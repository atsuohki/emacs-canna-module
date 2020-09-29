;; ----------------------------------------------------------------------
;; setup language & input method
(if (and (not noninteractive)
	 (load "canna-leim" t))
    (custom-set-variables
     '(current-language-environment "Japanese")
     '(default-input-method "japanese-canna")
     ))
;; ----------------------------------------------------------------------
