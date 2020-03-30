;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
;;
;; You can add more settings inside the file .dir-locals-2.el.
;; Emacs loads this file in addition to the .dir-locals.el.
;;

;; The following impose configuration for PEL:
;; - line length = 80
;; - indentation uses space characters only (no hard tabs)
;; - License is GPL V3.0
;; - DVCS used: Git
;;   - Note: you can also use another (D)VCS from the command
;;           line. If you want to use it within Emacs then
;;           change the vc-dir-backend in the file, but do
;;           not submit it back to the PEL depot.
;;
((nil             .
                  ((fill-column . 80)
                   (lice:default-license  . "gpl-3.0")
                   (lice:copyright-holder . "Pierre Rouleau")))
 (emacs-lisp-mode .
                  ((indent-tabs-mode . nil)))
 (vc-dir-mode     .
                  ((vc-dir-backend  . Git)))
 )

;; -----------------------------------------------------------------------------
