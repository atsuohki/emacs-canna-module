;;; canna.el --- Interface to the Canna Server -*- lexical-binding: t; -*-

;; This program is based on the following `canna.el',
;; modified to use `input-method-function' directly
;; like quail, but this is not a quail package!
;;                                              Atsuo OHKI, 2004/03/28
;; `canna-undo' can undo just after KAKUTEI.

;;==================================================================
;; Copyright (C) 1994 Akira Kon, NEC Corporation.
;; Copyright (C) 1996,1997,1998 MORIOKA Tomohiko
;; Copyright (C) 1997 Stephen Turnbull

;; Author: Akira Kon <kon@d1.bs2.mt.nec.co.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Version: $Revision: 1.18 $
;; Keywords: Canna, Japanese, input method, mule, multilingual

;; This file is part of Emacs-Canna.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Egg offered some influences to the implementation of Canna on
;; Nemacs/Mule, and this file contains a few part of Egg which is
;; written by S.Tomura, Electrotechnical Lab.  (tomura@etl.go.jp)

;; This program is rewritten for Emacs/mule and XEmacs/mule by MORIOKA
;; Tomohiko.
;;==================================================================

;;; Code:

;; ============================= ;;
;; C coded functions & variables ;;
;; ============================= ;;

(declare-function canna-key-proc "canna.c" (ch))
(declare-function canna-set-bunsetsu-kugiri "canna.c" (&optional num))
(declare-function canna-initialize "canna.c" (&optional num server rcfile))
(declare-function canna-finalize "canna.c" ())
(declare-function canna-touroku-string "canna.c" (str))
(declare-function canna-set-width "canna.c" (width))
(declare-function canna-change-mode "canna.c" (num))
(declare-function canna-store-yomi "canna.c" (yomi &optional roma))
(declare-function canna-do-function "canna.c" (num &optional ch))
(declare-function canna-parse "canna.c" (str))
(declare-function canna-query-mode "canna.c" ())
;; #ifdef WITH_KKCP
(declare-function canna-henkan-begin "canna.c" (yomi))
(declare-function canna-henkan-next "canna.c" (bunsetsu))
(declare-function canna-bunsetu-henkou "canna.c" (bunsetsu bunlen))
(declare-function canna-henkan-kakutei "canna.c" (bun kouho))
(declare-function canna-henkan-end "canna.c" ())
(declare-function canna-henkan-quit "canna.c" ())
;; #end /* WITH_KKCP */

(defvar canna-empty-info nil)
(defvar canna-through-info nil)
(defvar canna-func-extend-mode nil)
(defvar canna-func-functional-insert nil)
(defvar canna-func-henkan nil)
(defvar canna-func-japanese-mode nil)
(defvar canna-henkan-length 0)
(defvar canna-henkan-revlen 0)
(defvar canna-henkan-revpos 0)
(defvar canna-henkan-string "")
(defvar canna-ichiran-length 0)
(defvar canna-ichiran-revlen 0)
(defvar canna-ichiran-revpos 0)
(defvar canna-ichiran-string "")
(defvar canna-kakutei-romaji "")
(defvar canna-kakutei-string "")
(defvar canna-kakutei-yomi "")
(defvar canna-mode-alpha-mode nil)
(defvar canna-mode-bushu-mode nil)
(defvar canna-mode-hex-mode nil)
(defvar canna-mode-kigo-mode nil)
(defvar canna-mode-string "")

(defvar canna-coding-system "euc-jp"
  "coding system Canna server uses -- euc-jp(default) or euc-jisx2013")

;;;===========================================================;;;
;;; dynamically load canna.so with module-load() if necessary ;;;
;;;
(and (not (boundp 'CANNA))
     (fboundp 'module-load)
     (module-load (expand-file-name "canna.so" exec-directory)))

(or (boundp 'CANNA)
    (featurep 'CANNA)
    (error "Canna is not built into this Emacs"))
;;;
;;;===========================================================;;;

;;; ====================== ;;;
;;; Canna global variables ;;;
;;; ====================== ;;;

(defvar canna:*region-start* nil)   ;; 変換候補表示開始位置
(defvar canna:*region-end*   nil)   ;; 変換候補表示終了位置
(defvar canna:*spos-undo-text* nil) ;; 確定候補始点
(defvar canna:*epos-undo-text* nil) ;; 確定候補終点
(defvar canna:*undo-text-yomi* nil) ;; (確定候補 . (読み . ローマ字))
(defvar canna:*last-kouho* 0)
(defvar canna:*initialized* nil)
(defvar canna:*previous-window* nil)
(defvar canna:*cursor-was-in-minibuffer* nil)
(defvar canna:*menu-buffer* " *menu*")
(defvar canna:*saved-minibuffer* nil)
(defvar canna:*saved-redirection* nil)
(defvar canna:*minibuffer-window-selected* nil)
(defvar canna:*invoke-subcommand* nil) ;; invoke subfunctions

(defconst canna:*keytrans*
  '((return     . ?\r )
    (newline    . ?\n )
    (delete     . ?\C-? )
    (deletechar . ?\C-? ) ;; from 23.x??
    (backspace  . ?\b )
    (tab        . ?\t )
    (escape     . ?\e )
    (kanji      . ?\  )
    (up    . ?\C-p ) (S-up    . ?\C-p ) (C-up    . ?\C-p )
    (down  . ?\C-n ) (S-down  . ?\C-n ) (C-down  . ?\C-n )
    (left  . ?\C-b ) (S-left  . ?\C-b ) (C-left  . ?\C-b )
    (right . ?\C-f ) (S-right . ?\C-f ) (C-right . ?\C-f )
    ))

;;; ===================== ;;;
;;; Canna local variables ;;;
;;; ===================== ;;;

(defvar-local canna:*japanese-mode* nil
  "T if Canna mode is ``japanese-canna''.")
(put 'canna:*japanese-mode* 'permanent-local t)

(defvar canna:*japanese-mode-in-minibuffer* nil
  "T if Canna mode is ``japanese-canna'' in minibuffer.")

(defvar canna:*exit-japanese-mode* nil
  "T if Canna mode is temporarily ``japanese-canna'' for the next read loop.")

(defvar-local canna:*fence-mode* nil
  "T if Canna conversion is in progress.")
(put 'canna:*fence-mode* 'permanent-local t)

(defvar-local canna:*use-region-as-henkan-region* nil)
(put 'canna:*use-region-as-henkan-region* 'permanent-local t)

(defconst canna-rcs-version
  "$Id: canna.el,v 1.00 2012/04/29 00:00:00 ohki Exp $")

;; (defconst canna-rcs-version
;;  "$Id: canna.el,v 1.18 1998/10/27 09:59:18 morioka Exp $")

(defun canna-version ()
  "Display version of canna.el in mini-buffer."
  (interactive)
  (message (concat
	    (substring canna-rcs-version
		       5
		       (if (string-match "[0-9] [a-z]" canna-rcs-version)
			   (1+ (match-beginning 0))))
	    " ...")))

;;; ============ ;;;
;;; かんなの変数 ;;;
;;; ============ ;;;

;;
;; (canna:customize
;;    body1
;;    body2 )
;;
;; (canna:customize
;;    variable-name initial-value "docstring" ... )
;;
;; replace `t' with `nil' to disable customization
;; *NOTE* this works as #ifdef of C preprocessor
(eval-and-compile
  (defconst canna-enable-customization t "enable customization"))

(defmacro canna:customize (b1 b2 &rest rest) "" nil
	  (cond ((null rest)
		 (if canna-enable-customization
		     b2
		   b1))

		(canna-enable-customization
		 `(defcustom ,b1 ,b2 ,@rest))

		(t `(defvar ,b1 ,b2 ,(car rest)))))
;;
;; define canna group for customization
(canna:customize
 nil
 (defgroup canna nil
   "Canna related customization variables.
  you must restart emacs after saving customization."
   :group 'emacs
   :group 'mule
   :group 'leim
   ))

(canna:customize canna-do-keybind-for-functionkeys t
	"bind functionkeys to Canna menu.\n\
(need restart to take effect)"
	:type 'boolean
	:group 'canna )

(canna:customize canna-use-space-key-as-henkan-region t
	"use space key as a trigger for converting region.\n\
(need \\[canna-reset] to take effect)\n\
If transient mark mode is off, you must type C-SPC twice to begin region."
	:type 'boolean
	:group 'canna )

(canna:customize canna-enable-canna-undo-key nil
	"enable canna-undo with key sequence \\[undo].\n\
(need \\[canna-reset] to take effect)"
	:type 'boolean
	:group 'canna )

(canna:customize canna-server nil
	"Canna server host.\n\
(need \\[canna-reset] to take effect)"
	:type '(choice (const :tag "default" nil)
		       (string :tag "host name" ""))
	:group 'canna )

(canna:customize canna-file nil
	"Canna user initialization file.\n\
(need \\[canna-reset] to take effect)"
	:type '(choice (const :tag "default" nil)
		       (file :must-match t))
	:group 'canna )

(canna:customize canna-underline nil
	"underline the converting region.\n\
(need \\[canna-setup-color] to take effect)"
	:type 'boolean
	:group 'canna )

;; "*Non-nil の時、字種変換で半角かなに変換しない"
(canna:customize canna-inhibit-hankakukana nil
	"inhibit hankaku-kana in selecting character type.\n\
(need \\[canna-reset] to take effect)"
	:type 'boolean
	:group 'canna )

(canna:customize canna-dump nil
	"dump key input for debuging."
	:type 'boolean
	:group 'canna )

;;; ================== ;;;
;;; モードラインの修正 ;;;
;;; ================== ;;;

(defmacro canna:adjust-mode-string (str)
  `(if (string= ,str "      ") "Canna " ,str))

(defvar canna:*kanji-mode-string* "[ あ ]")
(defvar canna:*alpha-mode-string* (canna:adjust-mode-string "      "))
(defvar canna:*saved-mode-string* "[ あ ]")

(defvar canna:*show-minibuffer-mode* nil)

(defvar-local mode-line-canna-mode canna:*alpha-mode-string*)
(put 'mode-line-canna-mode 'permanent-local t)

(defvar mode-line-canna-mode-in-minibuffer canna:*alpha-mode-string*)

(defvar canna:*minibuffer-preprompt-overlay* nil "overlay property for mode")

; iseach uses originating buffer & minibuffer simultaneously
(defvar canna:*isearch-in-progress* nil)

;; same name as TAKANA
;; たかなでは t がデフォルトだけど、nil をデフォルトにしておこうかな。
(canna:customize display-minibuffer-mode-in-minibuffer nil
	"display the Canna mode for minibuffer input in minibuffer itself or not."
	:type 'boolean
	:group 'canna )

(defmacro canna:find-lower-left-window ()
  ;; return the lower left window of current frame, if it has minibuffer window,
  ;; nil otherwise.
  `(if (canna:memq-recursive (window-frame) (minibuffer-frame-list))
      (let ((w (window-tree)))
	(if (windowp (car w)) (car w)
	  (setq w (car w))
	  (while (and (consp w) (>= (length w) 3))
	    (cond ((eq (car w) t)   (setq w (car (last w))))
		  ((eq (car w) nil) (setq w (elt w 2)))
		  (t (message "invalid window tree list")
		     (setq w nil))))
	  w))))

(defun mode-line-canna-mode-update (str)
  ;; (if (string= "      " str) (setq str "Canna"))
  (setq str (canna:adjust-mode-string str)
	mode-line-canna-mode-in-minibuffer str
	mode-line-canna-mode str)

  (if (setq canna:*show-minibuffer-mode*
	    (or canna:*minibuffer-window-selected*
		canna:*isearch-in-progress*))
      (if display-minibuffer-mode-in-minibuffer
	  (with-current-buffer
	   (window-buffer (minibuffer-window))
	   (or canna:*minibuffer-preprompt-overlay*
	       (setq canna:*minibuffer-preprompt-overlay*
		     (make-overlay (point-min) (point-min))))
	   (unless (overlay-buffer canna:*minibuffer-preprompt-overlay*)
	     (move-overlay canna:*minibuffer-preprompt-overlay*
			   (point-min) (point-min)))
	   (overlay-put canna:*minibuffer-preprompt-overlay*
			'before-string str))

	;; display in mode line of some buffer
	(let ((w (canna:find-lower-left-window)))
	  (if (windowp w)
	      ;; update the mode-line of the buffer in the lower left window
	      (with-current-buffer (window-buffer w)
		(set-buffer-modified-p (buffer-modified-p)))
	    ;; mode-lines of all buffer -- hope a few frames/windows
	    (force-mode-line-update t))))

    ;; update the mode-line of the buffer
    (set-buffer-modified-p (buffer-modified-p))))

;; to display mode of Canna properly in isearch-mode
;; enter to isearch-mode
(add-hook 'isearch-mode-hook
	  #'(lambda ()
	      (setq canna:*isearch-in-progress* t)
	      ;; to make isearch-mode messages look properly
	      (setq
	       isearch-message-function
	       #'(lambda (&optional c-q-hack ellipis)
		   (let ((msg (let ((inhibit-message t))
				(isearch-message c-q-hack ellipis))))
		     (if display-minibuffer-mode-in-minibuffer
			 (setq msg (concat
				    (propertize mode-line-canna-mode
						'face 'minibuffer-prompt)
				    msg)))
		     ;; do as iseach-message
		     (if c-q-hack msg
		       (let ((message-log-max nil)) (message "%s" msg))))))
	      (mode-line-canna-mode-update mode-line-canna-mode)))

;; exit from isearch-mode
(add-hook 'isearch-mode-end-hook
	  #'(lambda ()
	      (setq canna:*isearch-in-progress* nil)
	      (setq isearch-message-function nil)
	      (mode-line-canna-mode-update mode-line-canna-mode)))

;; memq を強調するなら、以下だが、
;;(defun canna:memq-recursive (a l)
;;  (or (eq a l)
;;      (and (consp l)
;;	   (or (canna:memq-recursive a (car l))
;;	       (canna:memq-recursive a (cdr l))))))
;; 次の定義を使おう...
(defun canna:memq-recursive (a l)
  (if (atom l) (eq a l)
    (or (canna:memq-recursive a (car l))
	(canna:memq-recursive a (cdr l)))))

(defun canna:create-mode-line ()
  "Add string of Canna status into mode-line."
  (or (canna:memq-recursive 'mode-line-canna-mode mode-line-format)
      (setq-default
       mode-line-format
       (append (list (list 'canna:*show-minibuffer-mode*
			   (list 'display-minibuffer-mode-in-minibuffer
				 "-"
				 "m")
			   "-")
		     (list 'canna:*show-minibuffer-mode*
			   (list 'display-minibuffer-mode-in-minibuffer
				 'mode-line-canna-mode
				 'mode-line-canna-mode-in-minibuffer)
			   'mode-line-canna-mode))
	       mode-line-format)))
  (mode-line-canna-mode-update mode-line-canna-mode))

;;; ============================ ;;;
;;; minibuffer related functions ;;;
;;; ============================ ;;;

;; enter to minibuffer
(add-hook 'minibuffer-setup-hook
	  #'(lambda ()
	      ;; minibufferのデフォルトはcurrent-input-method
	      (setq canna:*japanese-mode*
		    (string= current-input-method "japanese-canna")
		    canna:*japanese-mode-in-minibuffer* canna:*japanese-mode*
		    canna:*minibuffer-window-selected* t)
	      (mode-line-canna-mode-update
	       (if canna:*japanese-mode*
		   canna:*kanji-mode-string* canna:*alpha-mode-string*))))

;; exit from minibuffer
(add-hook 'minibuffer-exit-hook
	  #'(lambda ()
	      ;; Exit minibuffer turning off Canna Japanese mode.
	      (setq current-input-method nil
		    canna:*japanese-mode* nil
		    canna:*japanese-mode-in-minibuffer* nil
		    canna:*minibuffer-window-selected* nil)
	      (mode-line-canna-mode-update
	       (if canna:*isearch-in-progress*
		   mode-line-canna-mode
		   canna:*alpha-mode-string*))))

;;; ======== ;;;
;;; 色の設定 ;;;
;;; ======== ;;;

;;*Non-nil でカラーディスプレイで色を付ける.
;; t の時はデフォルトの色を使用する。
;; 色を指定したい時は, 3要素のリスト
;;     (読みの色 変換対象の色 選択対象の色)
;; を指定する。
;; ただし、各対象の色名は、
;;     "前景色"                ← 前景色のみを指定
;;     "前景色/背景色"         ← 前景色と背景色をスラッシュ(/)で区切る
;;      ("前景色" . "背景色")  ← リストではなくコンスセル！
;;                                背景色を指定しない場合は nil を指定
;; のいづれかで指定する
;; ユーザ設定値のデフォルトは t の場合と同じ
(canna:customize canna-use-color nil
	"display converting words with various color.\n\
(need \\[canna-setup-color] to take effect)"
	:type '(choice (const :tag "no color" nil)
		       (const :tag "default color" t)
		       (list :tag "color for various part"
			     (cons :tag "yomi"
				   (color :tag "(foreground)" "red")
				   (choice :tag "(background)"
					   (const nil) (color)))
			     (cons :tag "taishou"
				   (color :tag "(foreground)" "blue")
				   (choice :tag "(background)"
					   (color "lavender") (const nil)))
			     (cons :tag "select"
				   (color :tag "(foreground)" "DarkOliveGreen1")
				   (choice :tag "(background)"
					   (color "cadet blue") (const nil)))))
	:group 'canna )

(canna:customize hilit-background-mode nil
	"force reverse video mode\n\
(need \\[canna-setup-color] to take effect)"
	:type '(choice (const :tag "normal" nil)
		       (const :tag "reverse" 'dark))
	:group 'canna )

(defvar canna:color-p nil "色が使えるか")
(defvar canna:attr-mode nil "現在のディスプレイモード")
(defvar canna:attr-yomi nil "読みの色属性")
(defvar canna:attr-taishou nil "変換対象部分の色属性")
(defvar canna:attr-select nil
  "ミニバッファ分離時のメニューの選択対象番号の色属性")
(defvar canna:attribute-alist		;colored by tagu@ae.keio.ac.jp
  '((yomi (normal . "red")
	  (reverse . "moccasin"))
    (taishou (normal . "blue/lavender")
	     (reverse . "yellow/cadet blue"))
    (select (normal . "DarkOliveGreen1/cadet blue")
	    (reverse . "light sea green/burlywood1")))
  "かんな変換時の配色のalist")

(defvar-local canna:*yomi-overlay* nil)
(put 'canna:*yomi-overlay* 'permanent-local t)

(defvar-local canna:*henkan-overlay* nil)
(put 'canna:*henkan-overlay* 'permanent-local t)

(defvar-local canna:*select-overlay* nil)
(put 'canna:*select-overlay* 'permanent-local t)

;;; ================== ;;;
;;; 色づけのための関数
;;; ================== ;;;

;;
;; use defmacro to make it inlined when compiled.
;;
(defmacro canna:yomi-attr-on (start end)
  `(if (overlayp canna:*yomi-overlay*)
       (move-overlay canna:*yomi-overlay* ,start ,end)
     (overlay-put (setq canna:*yomi-overlay*
			(make-overlay ,start ,end nil nil t))
		  'face
		  (if canna:color-p 'attr-yomi 'underline))))

(defmacro canna:yomi-attr-off ();
  `(if (overlayp canna:*yomi-overlay*)
       (delete-overlay canna:*yomi-overlay*)))

(defmacro canna:henkan-attr-on (start end)
  `(if (overlayp canna:*henkan-overlay*)
       (move-overlay canna:*henkan-overlay* ,start ,end)
     (overlay-put (setq canna:*henkan-overlay*
			(make-overlay ,start ,end nil nil t))
		  'face
		  (if canna:color-p 'attr-taishou 'region))))

(defmacro canna:henkan-attr-off ()
  `(if (overlayp canna:*henkan-overlay*)
       (delete-overlay canna:*henkan-overlay*)))

(defmacro canna:select-attr-on (start end)
  `(if (overlayp canna:*select-overlay*)
       (move-overlay canna:*select-overlay* ,start ,end)
     (overlay-put (setq canna:*select-overlay*
			(make-overlay ,start ,end nil nil t))
		  'face
		  'attr-select)))

(defmacro canna:select-attr-off ()
  `(if (overlayp canna:*select-overlay*)
       (delete-overlay canna:*select-overlay*)))

;;; ======================== ;;;
;;; グローバル関数の書き替え ;;;
;;; ======================== ;;;

;; kill-emacs

(add-hook 'kill-emacs-hook 'canna:finalize)

;;; ============================================ ;;;
;;; かんなでundoできるようにするための変数と関数 ;;;
;;; ============================================ ;;;

(defvar-local canna:*buffer-undo-list* nil)
(put 'canna:*buffer-undo-list* 'permanent-local t)

(defun canna:buffer-disable-undo (buffer)
  (with-current-buffer buffer
    (setq canna:*buffer-undo-list* buffer-undo-list) ; save undo-list
    (buffer-disable-undo)))

(defun canna:buffer-enable-undo (buffer)
  (with-current-buffer buffer
    (if canna:*buffer-undo-list*
	(setq buffer-undo-list canna:*buffer-undo-list*)) ; restore undo-list
    (buffer-enable-undo)))

;; evaluate forms with buffer-undo-enabled to `undo' work correctly
;; but this causes kakutei words and region words to be undone too.
(defmacro canna:with-undo-enabled (&rest forms)
  `(let ((b (current-buffer)))
     (canna:buffer-enable-undo b)
     ,@forms
     (canna:buffer-disable-undo b)))

;;; ======================================== ;;;
;;; 「かんな」サーバの変換結果を表示する関数 ;;;
;;; ======================================== ;;;

(defun canna:delete-last-preedit ()
  (if (and (> canna:*last-kouho* 0)
	   (numberp canna:*region-start*)
	   (numberp canna:*region-end*))
      (progn
	(if canna-underline
	    ;; まず、属性を消す。
	    (progn
	      (canna:henkan-attr-off)
	      (canna:yomi-attr-off)))
	(delete-region canna:*region-start* canna:*region-end*)))

  (setq canna:*region-start* nil
	canna:*region-end* nil
	canna:*last-kouho* 0))

(defun canna:insert-fixed (strs)
  (cond ((> strs 0)
	 (cond ((and canna-kakutei-yomi
		     (not (string= canna-kakutei-yomi canna-kakutei-string)))
		(setq canna:*undo-text-yomi*
		      (cons canna-kakutei-string
			    (cons canna-kakutei-yomi
				  canna-kakutei-romaji))
		      canna:*spos-undo-text* (point))
		(insert canna-kakutei-string))

	       (t
		(insert canna-kakutei-string))))))

(defun canna:insert-preedit ()
  (cond ((> canna-henkan-length 0)
	 (setq canna:*region-start* (point))
	 (unless canna-underline
	   (insert "||")
	   (setq canna:*region-end* (point))
	   (backward-char 1))
	 (insert canna-henkan-string)
	 (if canna-underline
	     (progn
	       (setq canna:*region-end* (point))
	       (canna:yomi-attr-on canna:*region-start* canna:*region-end*)))
	 (setq canna:*last-kouho* canna-henkan-length)))

  ;; 候補領域では強調したい文字列が存在するものと考えら
  ;; れる。強調したい文字はEmacsではカーソルポジションにて表示
  ;; することとする。強調したい文字がないのであれば、カーソル
  ;; は一番後の部分(入力が行われるポイント)に置いておく。

  ;; カーソルを移動する。
  (if (not canna-underline)
      (backward-char
       (- canna:*last-kouho*
	  ;; カーソル位置は、反転表示部分が存在しないのであれば、
	  ;; 候補文字列の最後の部分とし、反転表示部分が存在するの
	  ;; であれば、その部分の始めとする。
	  (cond ((zerop canna-henkan-revlen)
		 canna:*last-kouho*)
		(t canna-henkan-revpos))))

    (if (and (> canna-henkan-revlen 0)
	     (> canna-henkan-length 0))
	;; 候補の長さが0でなく、
	;; 反転表示の長さが0でなければ、
	;; その部分を反転表示する。
	(let ((start (+ canna:*region-start*
			(if canna-underline 0 1)
			canna-henkan-revpos)))
	  (if canna-underline
	      (canna:henkan-attr-on start
				    (+ start canna-henkan-revlen)))))))

(defun canna:display-candidates (strs)
  (cond ((stringp strs) ; エラーが起こった場合
	 (beep)
	 (message strs))

	(canna-henkan-string
	 ;; もし候補表示が前の結果から変わっていなくないときは......
	 ;; 取り合えず最初は前に書いておいた中間結果を消す。
	 (canna:delete-last-preedit)
	 ;; 確定した文字列があればそれを挿入する。
	 (canna:insert-fixed strs)
	 ;; 次は候補についての作業である。
	 ;; 候補を挿入する。候補は縦棒二本にて挟まれる。
	 (canna:insert-preedit)))

  ;; モードを表す文字列が存在すればそれをモードとして取り扱う。
  (if (stringp canna-mode-string)
      (mode-line-canna-mode-update canna-mode-string))

  ;; ミニバッファに書くことが存在するのであれば、それをミニバッファ
  ;; に表示する。
  (cond (canna-ichiran-string
	 (canna:minibuffer-input canna-ichiran-string
				 canna-ichiran-length
				 canna-ichiran-revpos
				 canna-ichiran-revlen))

	(canna:*cursor-was-in-minibuffer*
	 ;; Emacs 23.1 requires following order!
	 (set-window-buffer (minibuffer-window)
			    (get-buffer-create canna:*menu-buffer*))
	 (select-window (minibuffer-window)))))

(defun canna:minibuffer-input (str len revpos revlen)
  "Displaying misc informations for kana-to-kanji input."

  ;; 作業をミニバッファに移すのに際して、現在のウィンドウの情報を保存
  ;; しておく。
  (setq canna:*previous-window* (selected-window))

  ;; 自分に来る前がミニバッファかどうかを変数にでもいれておいた方がいいなあ。

  (unless canna:*cursor-was-in-minibuffer*
    ;; ミニバッファウィンドウに候補一覧用のバッファを割り当てる。
    (setq canna:*saved-minibuffer* (window-buffer (minibuffer-window)))
    ;; modified by 守岡 知彦 <morioka@jaist.ac.jp>, 1996/6/7
    (setq canna:*saved-redirection* (frame-focus (selected-frame)))
    (redirect-frame-focus (selected-frame)
			  (window-frame (minibuffer-window))))

  ;; Emacs 23.1 requires following order!
  (set-window-buffer (minibuffer-window)
		     (get-buffer-create canna:*menu-buffer*))
  (select-window (minibuffer-window))

  (canna:select-attr-off)
  (setq canna:*cursor-was-in-minibuffer* t)
  (delete-region (point-min) (point-max))
  (unless (eq canna:*previous-window* (selected-window))
      (setq canna:*minibuffer-window-selected* nil))

  ;; もともとのミニバッファに表示されていたモード文字列は
  ;; 一覧によって隠されるので、改めてモード文字列を表示しなおす。
  (if (and display-minibuffer-mode-in-minibuffer
           (eq canna:*previous-window* (selected-window)))
      (insert (canna:adjust-mode-string (canna-query-mode))))

  (insert str)

  ;; ミニバッファで反転表示するべき文字のところにカーソルを移動する。
  (cond ((> revlen 0)
	 (backward-char (- len revpos))))
  ;;(message "%s" (selected-frame)) (sit-for 3)
  (raise-frame (window-frame (minibuffer-window)))
  (and canna:color-p (not (eobp))
       (canna:select-attr-on (point)
			     (save-excursion (forward-char 1) (point))))

  ;; ミニバッファに表示するべき文字列がヌル文字列なのであれば、前のウィ
  ;; ンドウに戻る。
  (if (or (zerop len) canna-empty-info)
      (progn
	(setq canna:*cursor-was-in-minibuffer* nil)

	;; ミニバッファウィンドウのバッファを元に戻す。
	(set-window-buffer (minibuffer-window) canna:*saved-minibuffer*)
	(setq  canna:*saved-minibuffer* nil)
	;; modified by 守岡 知彦 <morioka@jaist.ac.jp>, 1996/6/7
	;; とりあえず XEmacs では動かさないようにしておこう (^_^;
	(redirect-frame-focus (window-frame canna:*previous-window*)
			      canna:*saved-redirection*)
	(if (and canna-empty-info (> len 0))
	    (progn
	      (message str)))
	(select-window canna:*previous-window*))))

;;; ====================================== ;;;
;;; かんなの変換が確定するまでの入力ループ ;;;
;;; ====================================== ;;;

;;
;; use defmacro to make it inlined when compiled.
;;
(defmacro canna:dump-head (key in_mini)
  `(with-current-buffer (get-buffer-create "canna:dump")
     (goto-char (point-min))
     (insert "\n")
     (backward-char)
     (if ,in_mini (insert "{M}"))
     (insert (format " %S" ,key))))

(defmacro canna:dump-key (key)
  `(with-current-buffer (get-buffer-create "canna:dump")
     (insert (format " %S" ,key))))

;; `read-key()' has a bug flickering a minibuffer window,
;; because of mis-use of message clearing.
;; workround: set inhibit-message non-nil
(defmacro canna:read-key (&optional prompt)
  `(let ((inhibit-message t))
    (read-key ,prompt)))

;; starting with a given `key',
;; feeds input to a Canna server until conversion complete.
;; while conversion is in progress, buffer undo is disabled.
;; once completed, any buffer changes are cleared and
;; a list of converted charaters is returned as a result.
;;
(defun canna-input-method (key)
  ;; do as `(quail-input-method key)'
  (if (or (and buffer-read-only
               (not (or inhibit-read-only
                        (get-char-property (point) 'inhibit-read-only))))
          (and overriding-terminal-local-map
               ;; If the overriding map is `universal-argument-map', that
               ;; must mean the user has pressed 'C-u KEY'.  If KEY has a
               ;; binding in `universal-argument-map' just return
               ;; (list KEY), otherwise act as if there was no
               ;; overriding map.
               (or (not (eq (cadr overriding-terminal-local-map)
                            universal-argument-map))
                   (lookup-key overriding-terminal-local-map (vector key))))
          overriding-local-map)
      (list key)

    ;; key is an ascii printable character (including space)
    ;;                     (point)
    ;; on entry:   -------><--------
    ;; after loop: ------->xxxxxxx<--------
    ;; on return:  -------><--------
    ;; `xxxxxxx' is the result returned as a list of characters.
    ;;
    ;; in case of canna:henkan-region, canna:undo and canna:rk-trans-region,
    ;; on entry:   ------->region<--------
    ;; after loop: ------->xxxxxxx<--------
    ;; on return:  -------><--------
    ;; point is at the either side of the region,
    ;; so start-pos is forced to point at the beginning of the region.

    (undo-auto--boundaries 'command)	; undo each conversion as a unit
    (let ((modified-p (buffer-modified-p))
	  (start-pos (point))
	  do-loop
	  (saved-imf input-method-function)	; save input method (should myself)
	  (echo-keystrokes 0))			; prevent keystroke echo

      (setq input-method-previous-message nil
	    input-method-function nil)		; prevent recursive call

      ;; enter canna-mode when appropriate
      (if (or (and canna:*minibuffer-window-selected*
		   canna:*japanese-mode-in-minibuffer*)
	      (and (not canna:*minibuffer-window-selected*)
		   canna:*japanese-mode*))
	  (canna:enter-canna-mode))

      ;; process canna-* commands invoked just before
      ;; little bit paranoia
      (if (setq do-loop canna:*fence-mode*)
	  (if canna:*invoke-subcommand*
	      (cond ((eq last-command 'canna-henkan-region)
		     (deactivate-mark)	; turn off region face of TMmode
		     (setq start-pos (region-beginning))
		     (canna:henkan-region (region-beginning) (region-end)))
		    ((eq last-command 'canna-touroku-region)
		     (canna:touroku-region (region-beginning) (region-end)))
		    ((eq last-command 'canna-extend-mode)
		     (canna:extend-mode))
		    ((eq last-command 'canna-kigou-mode)
		     (canna:kigou-mode))
		    ((eq last-command 'canna-hex-mode)
		     (canna:hex-mode))
		    ((eq last-command 'canna-bushu-mode)
		     (canna:bushu-mode))
		    ((eq last-command 'canna-undo)
		     (setq start-pos canna:*spos-undo-text*)
		     (canna:undo))
		    ((eq last-command 'canna-rk-trans-region)
		     (deactivate-mark)	; turn off region face of TMmode
		     (setq start-pos (region-beginning))
		     (canna:rk-trans-region (region-beginning) (region-end))))
	    ;; else (null canna:*invoke-subcommand*)
	    ;; first Space key is canna-henkan-region
	    (if (and (eq key ?\  )
		     canna-use-space-key-as-henkan-region
		     canna:*use-region-as-henkan-region*
		     (let ((use-empty-active-region nil)) (use-region-p))
		     (canna:without-newline (region-beginning) (region-end)))
		(progn (deactivate-mark)	; turn off region face of TMmode
		       (setq canna:*invoke-subcommand* t
			     start-pos (region-beginning))
		       (canna:henkan-region (region-beginning) (region-end)))))
	;; internal error
	;; -- canna:*invoke-subcommand* is non-null, but not in japanese mode
	(if canna:*invoke-subcommand*
	    (message "かんな：canna:*japanese-mode*がnilなのは変！")))

      (setq canna:*use-region-as-henkan-region* nil)
      ;; ignore a dummy key input
      (if canna:*invoke-subcommand*
	  (setq key (canna:read-key nil)
		canna:*invoke-subcommand* nil))
      (if canna-dump
	  (canna:dump-head key canna:*minibuffer-window-selected*))

      ;; main input conversion loop
      (unwind-protect
	  (progn (while do-loop
		   ;; switch back to previous window
		   ;; when menus are displayed in minibuffer
		   (if canna:*cursor-was-in-minibuffer*
		       (progn  ;; Emacs 23.1 requires following order!
			 (set-window-buffer (minibuffer-window)
					    canna:*saved-minibuffer*)
			 (select-window canna:*previous-window*)))
		   ;; process key events only!
		   (if (or (characterp (event-basic-type key))
			   ;; In emacs-20.x,  event symbols are returned
			   ;; when isearch-*  read in minibuffer.
			   ;; No need for emacs-21.x.
			   (and (symbolp key)
				(setq key (assq key canna:*keytrans*))
				(setq key (cdr key))))
		       (canna:display-candidates (canna-key-proc key)))
		   (setq key nil)
		   (if canna-empty-info
		       (setq do-loop nil)
		     (setq key (canna:read-key nil))
		     (if canna-dump
			 (canna:dump-key key))))
		 ;; return the result or unprocessed key as list
		 (if (< start-pos (point))
		     (string-to-list (buffer-substring-no-properties
				      start-pos (point)))
		   (if key (list key))))
	;; unwind forms
	(if (< start-pos (point))
	    (delete-region start-pos (point)))
	(set-buffer-modified-p modified-p)
	(setq input-method-function saved-imf)	; re-establish input method
	(canna:quit-canna-mode)))))

;;; ==================== ;;;
;;; 変換機能のオン／オフ ;;;
;;; ==================== ;;;

(defun canna-toggle-japanese-mode ()
  "Toggle Canna japanese mode."
  (cond (canna:*japanese-mode*
	 (setq canna:*japanese-mode* nil
	       input-method-function nil
	       canna:*use-region-as-henkan-region* nil
	       canna:*saved-mode-string* mode-line-canna-mode)
	 (canna:abandon-undo-info)
	 (mode-line-canna-mode-update canna:*alpha-mode-string*))

	(t
	 (setq canna:*japanese-mode* t
	       input-method-function 'canna-input-method
	       canna:*use-region-as-henkan-region* nil)
	 (if (fboundp 'canna-query-mode)
	     (let ((new-mode
		    (canna:adjust-mode-string (canna-query-mode))))
	       (if (string-equal new-mode "")
		   (setq canna:*kanji-mode-string* canna:*saved-mode-string*)
		 (setq canna:*kanji-mode-string* new-mode))))
	 (mode-line-canna-mode-update canna:*kanji-mode-string*)))
  (setq canna:*japanese-mode-in-minibuffer* canna:*japanese-mode*))

(defun canna:initialize ()
  (let ((init-val nil))
    (cond (canna:*initialized*) ; initialize されていたら何もしない
	  (t
	   (setq canna:*initialized* t
		 init-val (canna-initialize (if canna-underline 0 1)
					    canna-server canna-file))
	   (cond ((car (cdr (cdr init-val)))
		  (canna:output-warnings (car (cdr (cdr init-val))))))
	   (cond ((car (cdr init-val))
		  (error (car (cdr init-val)))))))

    (if (fboundp 'canna-query-mode)
	(progn
	  (canna-change-mode canna-mode-alpha-mode)
	  (setq canna:*alpha-mode-string*
		(canna:adjust-mode-string (canna-query-mode)))))

    (canna-do-function canna-func-japanese-mode)

    (if (fboundp 'canna-query-mode)
	(setq canna:*kanji-mode-string*
	      (canna:adjust-mode-string (canna-query-mode))))

    init-val))

(defun canna:finalize ()
  (cond ((null canna:*initialized*)) ; initialize されていなかったら何もしない
	(t
	 (message "『かんな』の辞書をセーブします....")
	 (setq canna:*initialized* nil)
	 (let ((init-val (canna-finalize)))
	   (cond (init-val
		  (canna:output-warnings init-val))))
	 (message "『かんな』の辞書をセーブします....done"))))

(defun canna:enter-canna-mode ()
  (unless canna:*initialized*
    (message "『かんな』の初期化を行っています....")
    (canna:initialize)
    (message "『かんな』の初期化を行っています....done"))

  (canna-set-width (- (window-width (minibuffer-window))
		      (if (and display-minibuffer-mode-in-minibuffer
			       (eq (selected-window) (minibuffer-window)))
			  (string-width
			   (let ((new-mode
				  (canna:adjust-mode-string (canna-query-mode))))
			     (if (string-equal new-mode "")
				 canna:*saved-mode-string*
			       new-mode)))
			0)))
  (setq canna:*fence-mode* t)
  (canna:buffer-disable-undo (current-buffer)))

(defun canna:quit-canna-mode ()
  (setq canna:*fence-mode* nil)
  (if canna:*exit-japanese-mode*
      (progn (setq canna:*exit-japanese-mode* nil)
	     (if  current-input-method
		 (toggle-input-method))))
  (canna:buffer-enable-undo (current-buffer))
  (setq canna:*last-kouho* 0
	canna:*region-start* nil
	canna:*region-end* nil))

;;; ============ ;;;
;;; 補助コマンド ;;;
;;; ============ ;;;

(defun canna:without-newline (start end)
  (and (< start end)
       (or (and (<= (point-at-bol) start) (= (point) end))
           (and (= start (point)) (<= end (point-at-eol))))))

(defun canna:all-ascii (start end)
  (let ((cnt 0))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
		  (or (and (eq ?\  (char-after))
			   (setq cnt (1+ cnt)))
		      t)
		  (<= ?\  (char-after) ?~ ))
	(forward-char))
      ;; not all spaces
      (and (= (point) end) (not (= cnt (- end start)))))))

(defun canna:abandon-undo-info ()
  (setq canna:*undo-text-yomi* nil
	canna:*spos-undo-text* nil
	canna:*epos-undo-text* nil))

;; enable input-method temorarily if it is not currently enabled
(defmacro canna:enable-once ()
  `(if (eq deactivate-current-input-method-function 'canna-deactivate)
       (progn (unless current-input-method
		(toggle-input-method)
		(setq canna:*exit-japanese-mode* t))
	      t)
     (message "かんな が有効になっていません")
     (beep)
     nil))

;;; マークコマンド
(defun canna-set-mark-command (arg)
  "Besides setting mark, set mark as a HENKAN region if it is in\n\
the japanese mode."
  (interactive "P")
  (setq this-command 'set-mark-command) ;; mark!
  (set-mark-command arg)
  (if (and canna-use-space-key-as-henkan-region
	   canna:*japanese-mode*
           ;; is set-mark-command really executed ?
           (eq this-command 'set-mark-command))
      (progn
	(setq canna:*use-region-as-henkan-region* (mark t))
	(if (current-message)
	    (message "%s(変換領域開始)" (current-message))))))

;;;
;;; following canna-XXX functions do
;;;	1) check condition of invokation
;;;	2) prepare to enter canna-input-method right after return
;;;	   (canna:enable-once and push back a dummy key input.)
;;;	3) then canna-input-method calls a corresponding canna:XXX function
;;;

(defmacro canna:prepare-enter-input-method (key)
  `(if (canna:enable-once)
       (setq unread-command-events (cons ,key  unread-command-events)
	     canna:*invoke-subcommand* t)))

(defun canna-henkan-region (start end)
  "Convert a text which is indicated by region into a kanji text."
  (interactive "r")
  (if (and (< start end)
	   (canna:without-newline start end))
      (canna:prepare-enter-input-method ?H)
    (message "リージョンが不正です。ヌルリージョンか、改行が含まれています。")
    (beep)))

(defun canna:henkan-region (start end)
  "subfunction to Convert a text which is indicated by region into a kanji text."
  (if (canna:all-ascii start end)
      (canna:rk-trans-region start end)
    ;; normal henkan-region
    (canna-store-yomi (buffer-substring-no-properties start end))
    (canna:with-undo-enabled
     (delete-region start end))
    (canna:display-candidates (canna-do-function canna-func-henkan))))

(defun canna-touroku-region (start end)
  "Register a word which is indicated by region into a kana-to-kanji\n\
dictionary."
  (interactive "r")
  (if (and (< start end)
	   (canna:without-newline start end))
      (canna:prepare-enter-input-method ?T)
    (message "リージョンが不正です。ヌルリージョンか、改行が含まれています。")
    (beep)))

(defun canna:touroku-region (start end)
  "subfunction called after `canna-touroku-region'."
  (canna:display-candidates
   (canna-touroku-string (buffer-substring-no-properties start end))))

(defun canna-extend-mode ()
  "To enter an extend-mode of Canna."
  (interactive "*")
  (canna:prepare-enter-input-method ?E))

(defun canna:extend-mode ()
  "subfunction called after `canna-extend-mode'."
  (canna:display-candidates
   (canna-do-function canna-func-extend-mode)))

(defun canna-kigou-mode ()
  "Enter symbol choosing mode."
  (interactive "*")
  (canna:prepare-enter-input-method ?K))

(defun canna:kigou-mode ()
  "subfunction called after `canna-kigou-mode'."
  (canna:display-candidates (canna-change-mode canna-mode-kigo-mode)))

(defun canna-hex-mode ()
  "Enter hex code entering mode."
  (interactive "*")
  (canna:prepare-enter-input-method ?X))

(defun canna:hex-mode ()
  "subfunction called after `canna-hex-mode'."
  (canna:display-candidates (canna-change-mode canna-mode-hex-mode)))

(defun canna-bushu-mode ()
  "Enter special mode to convert by BUSHU name."
  (interactive "*")
  (canna:prepare-enter-input-method ?B))

(defun canna:bushu-mode ()
  "subfunction called after `canna-bushu-mode'."
  (canna:display-candidates (canna-change-mode canna-mode-bushu-mode)))

;; available just after KAKUTEI
(defun canna-undo (&optional arg)
  "undo last kana-kanji conversion to YOMI, or original undo"
  (interactive "*P")
  (if (and (null arg)
	   canna:*japanese-mode*
	   canna:*undo-text-yomi*
	   (numberp canna:*spos-undo-text*)
	   (save-excursion
	     (and (goto-char canna:*spos-undo-text*)
		  (setq canna:*epos-undo-text*
			(search-forward (car canna:*undo-text-yomi*)
					(point-at-eol)
					t))
		  (= canna:*spos-undo-text* (match-beginning 0)))))
      (progn (message "読みに戻します！")
	     (canna:prepare-enter-input-method ?U))
    ;; else
    (if canna-enable-canna-undo-key
	(funcall-interactively 'undo arg)
      (message "読みに戻せません！")
      (beep))))

(defun canna:undo ()
  (cond ((and (numberp canna:*spos-undo-text*)
	      (numberp canna:*epos-undo-text*)
	      (< canna:*spos-undo-text* canna:*epos-undo-text*))
	 (goto-char canna:*spos-undo-text*)
	 (canna:with-undo-enabled
	  (delete-region canna:*spos-undo-text* canna:*epos-undo-text*))
	 (canna:display-candidates
	  (canna-store-yomi (cadr canna:*undo-text-yomi*)))))
  (canna:abandon-undo-info))

;; リージョンにあるローマ字を『かんな』に食わす。
;; 結果として、『かんな』の読みモードになる。
;; リージョンに存在している空白文字と制御文字は捨てられる。

(defun canna-rk-trans-region (start end)
  "Insert alpha-numeric string as it is sent from keyboard."
  (interactive "*r")
  (if (and (< start end)
	   (canna:without-newline start end))
      (canna:prepare-enter-input-method ?R)
    (message "リージョンが不正です。ヌルリージョンか、改行が含まれています。")
    (beep)))

(defun canna:rk-trans-region (start end)
  "subfunction called after `canna-rk-trans-region'."
  (goto-char end)
  (let ((res (canna:rk-region start end)))
    (if (numberp res)
	(progn (canna:with-undo-enabled
		(delete-region start end))
	       (canna:display-candidates res)))))

(defun canna:rk-region (start end)
  "Convert region into kana."
  (let ((res 0) ch)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
		  (numberp res))
	(if (< ?\  (setq ch (char-after)))
	    (setq res (canna-do-function canna-func-functional-insert ch)))
	(forward-char)))
    res))

;;; ================== ;;;
;;; 『かんな』の初期化 ;;;
;;; ================== ;;;
(defun canna-setup-color ()
  ;; 配色設定 (by yuuji@ae.keio.ac.jp)
  (interactive)
  (setq canna:color-p (and canna-use-color
			   (or (and (eq window-system 'x)
			       	    (x-display-color-p))
			       (tty-display-color-p))))
  ;;カラーの時underlineモードと同じ状態で初期化する必要がある
  (setq canna-underline (or canna:color-p canna-underline))
  (cond
   (canna:color-p
    (setq canna:attr-mode (cond
			   ((or (and (boundp 'hilit-background-mode)
				     (eq hilit-background-mode 'dark))
				(string-match
				 "on\\|t"
				 (or (and (eq window-system 'x)
					  (x-get-resource
					   "ReverseVideo" "reverseVideo"))
				     "")))
			    'reverse)	;反転しているなら 'reverse
			   (t 'normal))
	  canna:attr-yomi (if (listp canna-use-color)
			      (car canna-use-color)
			    (cdr (assq canna:attr-mode
				       (assq 'yomi canna:attribute-alist))))
	  canna:attr-taishou (if (listp canna-use-color)
				 (car (cdr canna-use-color))
			       (cdr (assq
				     canna:attr-mode
				     (assq 'taishou canna:attribute-alist))))
	  canna:attr-select (if (listp canna-use-color)
				(car (cdr (cdr canna-use-color)))
			      (cdr (assq canna:attr-mode
					 (assq 'select canna:attribute-alist)))))
    ;;色づけ用faceの作成
    (mapc #'(lambda (face)
	      (let* ((color (symbol-value
			     (intern (concat "canna:" (symbol-name face)))))
		     backp)
		(make-face face)
		(cond ((stringp color)
		       (setq backp (string-match "/" color))
		       (set-face-foreground face (substring color 0 backp))
		       (if backp
			   (set-face-background face
						(substring color (1+ backp)))))

		      ((and (consp color) (stringp (car color)))
		       (set-face-foreground face (car color))
		       (if (stringp (cdr color))
			   (set-face-background face (cdr color))))

		      (t
		       (copy-face color face)))))
	  '(attr-yomi attr-taishou attr-select))))
  ;;配色設定終了
  ;; reset each buffer's overlay
  (mapc #'(lambda (buf)
	    (if (buffer-name buf)
		(with-current-buffer buf
		  (kill-local-variable 'canna:*yomi-overlay*)
		  (kill-local-variable 'canna:*henkan-overlay*)
		  (kill-local-variable 'canna:*select-overlay*))))
	(buffer-list)))

(defun canna:check-remap (map key)
  (if (keymapp map)
      (let ((rmap (assq 'remap map)))
	(if (and (consp rmap)
		 (consp (cdr rmap)) (eq 'keymap (cadr rmap))
		 (setq rmap (assq key rmap)))
	    (cdr rmap)))))

(defun canna-reset ()
  (interactive)
  (message "『かんな』の辞書をセーブします。");
  (canna:finalize)
  (message "『かんな』の再初期化を行っています....")
  (setq canna:*region-start* nil
	canna:*region-end* nil
	canna:*last-kouho* 0)
  (canna:abandon-undo-info)
  (canna-setup-color)
  (canna:initialize)

  (if (eq (canna:check-remap global-map 'set-mark-command) 'canna-set-mark-command)
      (global-set-key [remap set-mark-command] nil))
  (if canna-use-space-key-as-henkan-region
      (global-set-key [remap set-mark-command] 'canna-set-mark-command))

  (if (eq (canna:check-remap global-map 'undo) 'canna-undo)
      (global-set-key [remap undo] nil))
  (if canna-enable-canna-undo-key
      (global-set-key [remap undo] 'canna-undo))

  (message "『かんな』の再初期化を行っています....done"))

(defun canna ()
  "Enable Kana-Kanji conversion with Canna.\n\
You can also use following Canna related commands.\n\
\n\
canna-touroku-region	register a japanese word in a region to a dictionary\n\
canna-henkan-region	convert a text in a region into a kanji text\n\
canna-undo		undo the last conversion\n\
canna-rk-trans-region	put alphanumeric characters in a region into Canna\n\
canna-extend-mode	enter Canna menu (\\[canna-extend-mode])\n\
canna-kigou-mode	enter KIGOU input mode (\\[canna-kigou-mode])\n\
canna-hex-mode		enter Kanji code input mode (\\[canna-hex-mode])\n\
canna-bushu-mode	enter BUSHU input mode (\\[canna-bushu-mode])"
  (message "『かんな』を初期化しています....")
  (cond ((and (fboundp 'canna-initialize) (fboundp 'canna-change-mode))
	 ;; Canna が使える時は次の処理をする。
	 (canna-setup-color)
	 ;; 『かんな』システムの初期化
	 (canna:initialize)
	 (if canna-do-keybind-for-functionkeys
	     (progn
	       (global-set-key "\e[28~" 'canna-extend-mode) ; HELP on EWS4800
	       (global-set-key "\e[2~"  'canna-kigou-mode)  ; INS  on EWS4800
	       (global-set-key "\e[12~" 'canna-extend-mode)
	       (global-set-key "\e[13~" 'canna-kigou-mode)
	       (global-set-key "\e[14~" 'canna-hex-mode)
	       (global-set-key "\e[15~" 'canna-bushu-mode)
	       (define-key global-map [help] 'canna-extend-mode)
	       (define-key global-map [insert] 'canna-kigou-mode)
	       ;; f1 is for Help
	       ;; f2 is for two-column, but ...
	       (define-key global-map [f2] 'canna-extend-mode)
	       (define-key global-map [f3] 'canna-kigou-mode)
	       (define-key global-map [f4] 'canna-hex-mode)
	       (define-key global-map [f5] 'canna-bushu-mode)))

	 ;; key binding for
	 (if canna-use-space-key-as-henkan-region
	     (global-set-key [remap set-mark-command] 'canna-set-mark-command))
	 (if canna-enable-canna-undo-key
	     (global-set-key [remap undo] 'canna-undo))

	 ;; モード行の作成
	 (canna:create-mode-line)
	 (mode-line-canna-mode-update canna:*alpha-mode-string*))

	((fboundp 'canna-initialize)
	 (beep)
	 (with-output-to-temp-buffer "*canna-warning*"
	   (princ "この Emacs/Mule では new-canna が使えません")
	   (terpri)
	   (help-print-return-message)))

	(t ; 『かんな』システムが使えなかった時の処理
	 (beep)
	 (with-output-to-temp-buffer "*canna-warning*"
	   (princ "この Emacs/Mule では canna が使えません")
	   (terpri)
	   (help-print-return-message))))
  (message "『かんな』を初期化しています....done"))

(defun canna:output-warnings (mesg)
  (with-output-to-temp-buffer "*canna-warning*"
    (while mesg
      (princ (car mesg))
      (terpri)
      (setq mesg (cdr mesg)))
    (help-print-return-message)))

(provide 'canna)

;;;=========================================;;;
;;; *CAUTION*
;;; following code change the status which
;;; could not be undone on the fly!
;;;=========================================;;;
;; handle EUC-JP <-> UTF-8 code conversion ;;
;;
;; use base64 encoding to exchange euc-coded string transparently with canna.c
;; In canna.c, mule_extract_string() and mule_make_string() handle
;; base64 decoding/encoding.
;;

(defun canna:encode (str)
  (cond ((and (stringp str)
	      (< 0 (length str)))
	 (base64-encode-string (encode-coding-string str canna-coding-system 1)
			       t))
	(t str)))

(defun canna:decode (l)
  (cond ((and (stringp l)
	      (< 0 (length l)))
	 (decode-coding-string (base64-decode-string l) canna-coding-system 1))
	((consp l)
	 (let ((c0 (car l))
	       (c1 (cdr l)))
	   (if (stringp c0) (setcar l (canna:decode c0))
	     (if (consp c0) (canna:decode c0)))
	   (if (stringp c1) (setcdr l (canna:decode c1))
	     (if (consp c1) (canna:decode c1))))
	 l)
	(t l)))

(defun canna:storeResults(val)
  (if (not (numberp val)) (canna:decode val)
    (mapc #'(lambda (var)
	      (set var (canna:decode (symbol-value var))))
	  '(canna-kakutei-string
	    canna-kakutei-yomi
	    canna-kakutei-romaji
	    canna-henkan-string
	    canna-ichiran-string
	    canna-mode-string))
    val))
;;
;; wrappers for lisp functions defined in canna.so.
;; they take EUC-string arguments or return EUC-string values
;;
;; using advicing

(defmacro canna:wrapper (fn args &optional result)
  (let* ((argdecl (cons nil nil))  (ad argdecl)
	 (argcall (cons nil nil))  (ac argcall))
    ;; default is canna:decode
    (or result
	(setq result 'canna:decode))
    ;; make arguments list for declaration & function call
    (mapc #'(lambda (arg)
	      (cond ((eq arg '&optional)
		     (setcdr ad (cons arg nil)) (setq ad (cdr ad)))
		    ((consp arg)
		     (setcdr ad (cons (cadr arg) nil)) (setq ad (cdr ad))
		     (setcdr ac (cons arg nil))        (setq ac (cdr ac)))
		    (t
		     (setcdr ad (cons arg nil)) (setq ad (cdr ad))
		     (setcdr ac (cons arg nil)) (setq ac (cdr ac)))))
	  args)
    (setq argdecl (cdr argdecl)
	  argcall (cdr argcall))
    `(advice-add ',fn
		 :around
		 #'(lambda (func ,@argdecl)
		     (let (last-coding-system-used
			   (inhibit-eol-conversion t))
		       (,result (funcall func ,@argcall)))))
    ))

;;
;; establish/delete wrappers
;;
(if (module-function-p (symbol-function 'canna-key-proc))
    ;; establish wrappers
    (progn
     (canna:wrapper canna-key-proc (ch) canna:storeResults)
     (canna:wrapper canna-initialize (&optional num server rcfile))
     (canna:wrapper canna-finalize ())
     (canna:wrapper canna-touroku-string ((canna:encode str)) canna:storeResults)
     (canna:wrapper canna-change-mode (num) canna:storeResults)
     (canna:wrapper canna-store-yomi ((canna:encode yomi)
				      &optional (canna:encode roma))
		    canna:storeResults)
     (canna:wrapper canna-do-function (num &optional ch) canna:storeResults)
     (canna:wrapper canna-parse ((canna:encode str)))
     (canna:wrapper canna-query-mode ())

     ;; #ifdef WITH_KKCP
     (unless (null (symbol-function 'canna-henkan-begin))
       (canna:wrapper canna-henkan-begin ((canna:encode yomi)))
       (canna:wrapper canna-henkan-next (bunsetsu))
       (canna:wrapper canna-bunsetu-henkou (bunsetsu bunlen)) )
     ;; #end /* WITH_KKCP */
     )
  ;; delete unused functions
  (unintern "canna:encode" nil)
  (unintern "canna:decode" nil)
  (unintern "canna:storeResults" nil))

(unintern "canna:wrapper" nil)

;;;
;;;=========================================;;;

;;; canna.el ends here
