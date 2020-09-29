;;; canna-leim.el --- Canna-related code for LEIM
;; Copyright (C) 1997  Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Copyright (C) 1997 Free Software Foundation, Inc.
;;
;; Shamelessly ripped off from
;;
;; skk-leim.el --- SKK related code for LEIM
;; Copyright (C) 1997
;; Murata Shuuichirou <mrt@mickey.ai.kyutech.ac.jp>
;;
;; Author: Stephen Turnbull <turnbull@sk.tsukuba.ac.jp>
;; Version: canna-leim.el,v 1.2 1997/10/27 10:08:49 steve Exp
;; Keywords: japanese, input method, LEIM
;; Last Modified: 1997/10/27 10:08:49

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either versions 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs, see the file COPYING.  If not, write to the Free
;; Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; TODO
;;
;;  Add pointers to Canna documentation in LEIM format

;; need these here?
(let ((save-imf input-method-function))
  (setq input-method-function nil)
  (if (assoc 'input-method-function (buffer-local-variables))
      (setq-default input-method-function save-imf)
    (make-variable-buffer-local 'input-method-function)
    (put 'input-method-function 'permanent-local t)))

(require 'canna)

(defun canna-help ()
  "Show key bindings available while converting by canna."
  (interactive)
  (with-output-to-temp-buffer (help-buffer)
    (princ "かんな フェンスモード時のキーマップ[~/.cannaでカスタマイズ]\n\n")
    (princ "SPC\t\t変換、次候補\n")
    (princ "RTN, C-m\t確定\n")
    (princ "BS, C-h\t\tカーソル左文字削除、漢字を読みに戻す\n")
    (princ "C-\\\t\t字種設定: [ あ ]→[全ア]→[半ア]→[全英]→[半英]→[ あ ]\n")
    (princ "C-a\t\t読みの左端へ移動、左端候補へ移動、左端文節選択\n")
    (princ "C-b\t\tカーソル左移動、左候補へ移動、左文節選択\n")
    (princ "C-c\t\t文節編集\n")
    (princ "C-d\t\tカーソル右文字削除、末尾の一文字選択\n")
    (princ "C-e\t\t読みの右端へ移動、右端候補へ移動、右端文節選択\n")
    (princ "C-f\t\tカーソル右移動、右候補へ移動、右文節選択\n")
    (princ "C-g\t\t取りやめ\n")
    (princ "C-i\t\t文節縮め\n")
    (princ "C-j\t\t部分確定\n")
    (princ "C-k\t\tカーソル以降の文字列削除、先頭の一文字選択\n")
    (princ "C-l\t\t小文字変換\n")
    (princ "C-n\t\t次文字種、次候補表示、次の候補一覧表示\n")
    (princ "C-o\t\t文節伸ばし\n")
    (princ "C-p\t\t前文字種、前候補表示、前の候補一覧表示\n")
    (princ "C-q\t\t引用入力\n")
    (princ "C-u\t\t大文字変換\n")
    (princ "C-w\t\t候補一覧、部首一覧\n")
    (princ "C-y\t\t(jisx0208/0212の)16進コード変換\n")
    ))

(defun canna-activate (&optional name)
  (setq describe-current-input-method-function 'canna-help
        deactivate-current-input-method-function 'canna-deactivate)
  (unless (featurep 'leim-canna-initialized)
    (canna)
    (provide 'leim-canna-initialized))
  (cond ((not canna:*japanese-mode*) (canna-toggle-japanese-mode))))

(defun canna-deactivate ()
  (cond (canna:*japanese-mode* (canna-toggle-japanese-mode))) )

(register-input-method
 'japanese-canna "Japanese"
 'canna-activate "かんな"
 "Canna - a kana to kanji conversion program" )

(provide 'canna-leim)

;;; canna-leim.el ends here
