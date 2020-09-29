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
    (princ "$B$+$s$J(B $B%U%'%s%9%b!<%I;~$N%-!<%^%C%W(B[~/.canna$B$G%+%9%?%^%$%:(B]\n\n")
    (princ "SPC\t\t$BJQ49!"<!8uJd(B\n")
    (princ "RTN, C-m\t$B3NDj(B\n")
    (princ "BS, C-h\t\t$B%+!<%=%k:8J8;z:o=|!"4A;z$rFI$_$KLa$9(B\n")
    (princ "C-\\\t\t$B;z<o@_Dj(B: [ $B$"(B ]$B"*(B[$BA4%"(B]$B"*(B[$BH>%"(B]$B"*(B[$BA41Q(B]$B"*(B[$BH>1Q(B]$B"*(B[ $B$"(B ]\n")
    (princ "C-a\t\t$BFI$_$N:8C<$X0\F0!":8C<8uJd$X0\F0!":8C<J8@aA*Br(B\n")
    (princ "C-b\t\t$B%+!<%=%k:80\F0!":88uJd$X0\F0!":8J8@aA*Br(B\n")
    (princ "C-c\t\t$BJ8@aJT=8(B\n")
    (princ "C-d\t\t$B%+!<%=%k1&J8;z:o=|!"KvHx$N0lJ8;zA*Br(B\n")
    (princ "C-e\t\t$BFI$_$N1&C<$X0\F0!"1&C<8uJd$X0\F0!"1&C<J8@aA*Br(B\n")
    (princ "C-f\t\t$B%+!<%=%k1&0\F0!"1&8uJd$X0\F0!"1&J8@aA*Br(B\n")
    (princ "C-g\t\t$B<h$j$d$a(B\n")
    (princ "C-i\t\t$BJ8@a=L$a(B\n")
    (princ "C-j\t\t$BItJ,3NDj(B\n")
    (princ "C-k\t\t$B%+!<%=%k0J9_$NJ8;zNs:o=|!"@hF,$N0lJ8;zA*Br(B\n")
    (princ "C-l\t\t$B>.J8;zJQ49(B\n")
    (princ "C-n\t\t$B<!J8;z<o!"<!8uJdI=<(!"<!$N8uJd0lMwI=<((B\n")
    (princ "C-o\t\t$BJ8@a?-$P$7(B\n")
    (princ "C-p\t\t$BA0J8;z<o!"A08uJdI=<(!"A0$N8uJd0lMwI=<((B\n")
    (princ "C-q\t\t$B0zMQF~NO(B\n")
    (princ "C-u\t\t$BBgJ8;zJQ49(B\n")
    (princ "C-w\t\t$B8uJd0lMw!"It<s0lMw(B\n")
    (princ "C-y\t\t(jisx0208/0212$B$N(B)16$B?J%3!<%IJQ49(B\n")
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
 'canna-activate "$B$+$s$J(B"
 "Canna - a kana to kanji conversion program" )

(provide 'canna-leim)

;;; canna-leim.el ends here
