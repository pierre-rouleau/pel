;;; pel-key-chord.el --- PEL Key-Chord Support

;;; Commentary:
;;

(require 'pel--base)     ; use: pel-concat-strings-in-list
(require 'pel--options)  ; use: pel-key-chords

(defun pel-activate-key-chord-from-spec (key-chord-spec)
  "Activate the KEY-CHORD-SPEC.
The KEY-CHORD-SPEC must be a list of 3 elements:
- a symbol: either 'global or the name of a major-mode
- a string of 2 characters key-chord
- what to do for the key-chord, one of:
  - a function or lambda
  - A string describing the key sequence to execute.

Return one of:
- nil if the specification is in error: it was ignored,
- t if a global or major-mode key-chord was passed and was ok:
  it was defined,
- a 3 element list:  (mode-symbol string string|function) for
  a major mode key-chord: the key-chord-define for it must be scheduled
  via a hook because the major mode symbol is currently not bound."
  (if (and (require 'key-chord nil :noerror)
           (fboundp 'key-chord-define-global)
           (fboundp 'key-chord-define))
      (let ((kc-mode  (car key-chord-spec))
            (kc       (nth 1 key-chord-spec))
            (kc-exec  (nth 2 key-chord-spec)))
        (if (eq kc-mode 'global)
            (progn
              (key-chord-define-global kc kc-exec)
              t)
          (let ((kc-mode-map (pel-map-symbol-for kc-mode)))
            (if (and (fboundp kc-mode)
                     (boundp kc-mode-map)
                     (keymapp (symbol-value kc-mode-map)))
              (progn
                (key-chord-define (symbol-value kc-mode-map) kc kc-exec)
                t)
            (list kc-mode kc kc-exec)))))
    (error "Failed loading key-chord!")))


(defun pel-activate-key-chords-in (key-chords-spec)
  "Activate all global key-chords in KEY-CHORDS-SPEC.
Return list of mode symbols for which the activation must be deferred."
  (let ((deferred-modes ())) ; list of modes that must be deferred
    (dolist (spec key-chords-spec)
      (let ((activation-result (pel-activate-key-chord-from-spec spec)))
        ;;   activation-result := (mode-symbol string string|function)
        (unless (eq activation-result t)
          (let ((mode    (car activation-result))
                (keyspec (cdr activation-result)))
            (unless (memq mode deferred-modes)
              (setq deferred-modes (cons mode deferred-modes)))))))
    deferred-modes))

(defun pel--activate-deferred-key-chords ()
  "..."
  (message "pel--activate-deferred-key-chords")
  (pel-activate-key-chords-in pel-key-chords))

(defun pel-activate-all-key-chords ()
  "Activate all key-chords defined in pel-key-chords."
  (let ((deferred-modes (pel-activate-key-chords-in pel-key-chords)))
    (dolist (mode deferred-modes)
      (message "deferring %s" mode)
      (eval-after-load (symbol-name mode)
        '(pel--activate-deferred-key-chords)))))

;; -----------------------------------------------------------------------------
(provide 'pel-key-chord)

;;; pel-key-chord.el ends here
