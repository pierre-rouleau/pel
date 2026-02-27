;;; pel-autoloads.el --- automatically extracted autoloads
;;
;;; Commentary:
;;
;;  File for package system

;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "build-pel" "build-pel.el" (0 0 0 0))
;;; Generated autoloads from build-pel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "build-pel" '("build-pel")))

;;;***

;;;### (autoloads nil "install-pel" "install-pel.el" (0 0 0 0))
;;; Generated autoloads from install-pel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "install-pel" '("upload-pel-to-local-archive")))

;;;***

;;;### (autoloads nil "pel" "pel.el" (0 0 0 0))
;;; Generated autoloads from pel.el

(autoload 'pel-init "pel" "\
Initialize PEL, map its keys, autoload its functions.

If CACHED-ABBREV-FILE-NAME is non-nil it should hold a the value
the name of the abbreviation file, the name that the variable
`abbrev-file-name' had before it was changed in the init file.
This is done to delay the loading of the abbreviation file, speeding
up Emacs initialization time when the file is big.

Only the PEL features activated via the `pel-use-...' customization variables
from the  \"Pel Package Use\" subgroup of the \"Pel\" group are loaded and the
respective PEL keys are mapped.  The others are not.

If you need to activate new features, use \\[customize] to customize variables
inside the \"Pel\" group.  The \"Pel Package Use\" subgroup contains the
customization variables that control PEL activated features.

You can customize PEL feature only after execution of the `pel-init' command.
After a customization change its best to restart Emacs, however if your
modifications simply activate new features, you may be able to simply
re-execute `pel-init' again to activate them.

\(fn &optional CACHED-ABBREV-FILE-NAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel" '("pel--cached-abbrev-file-name")))

;;;***

;;;### (autoloads nil "pel--base" "pel--base.el" (0 0 0 0))
;;; Generated autoloads from pel--base.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel--base" '("pel-")))

;;;***

;;;### (autoloads nil "pel--keys-macros" "pel--keys-macros.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pel--keys-macros.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel--keys-macros" '("pel-" "define-pel-global-prefix")))

;;;***

;;;### (autoloads nil "pel--macros" "pel--macros.el" (0 0 0 0))
;;; Generated autoloads from pel--macros.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel--macros" '("while-n" "pel-")))

;;;***

;;;### (autoloads nil "pel--options" "pel--options.el" (0 0 0 0))
;;; Generated autoloads from pel--options.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel--options" '("pel-")))

;;;***

;;;### (autoloads nil "pel-align" "pel-align.el" (0 0 0 0))
;;; Generated autoloads from pel-align.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-align" '("pel-")))

;;;***

;;;### (autoloads nil "pel-applescript" "pel-applescript.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from pel-applescript.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-applescript" '("pel-")))

;;;***

;;;### (autoloads nil "pel-autocomplete" "pel-autocomplete.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pel-autocomplete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-autocomplete" '("pel-")))

;;;***

;;;### (autoloads nil "pel-autoload" "pel-autoload.el" (0 0 0 0))
;;; Generated autoloads from pel-autoload.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-autoload" '("pel--autoload-init")))

;;;***

;;;### (autoloads nil "pel-benchmark" "pel-benchmark.el" (0 0 0 0))
;;; Generated autoloads from pel-benchmark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-benchmark" '("pel-show-init-time")))

;;;***

;;;### (autoloads nil "pel-bookmark" "pel-bookmark.el" (0 0 0 0))
;;; Generated autoloads from pel-bookmark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-bookmark" '("pel-bookmark-in-current-file-p")))

;;;***

;;;### (autoloads nil "pel-cc" "pel-cc.el" (0 0 0 0))
;;; Generated autoloads from pel-cc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-cc" '("pel-cc-")))

;;;***

;;;### (autoloads nil "pel-ccp" "pel-ccp.el" (0 0 0 0))
;;; Generated autoloads from pel-ccp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-ccp" '("pel-")))

;;;***

;;;### (autoloads nil "pel-comment" "pel-comment.el" (0 0 0 0))
;;; Generated autoloads from pel-comment.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-comment" '("pel-")))

;;;***

;;;### (autoloads nil "pel-comment-adorn" "pel-comment-adorn.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pel-comment-adorn.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-comment-adorn" '("pel-")))

;;;***

;;;### (autoloads nil "pel-commonlisp" "pel-commonlisp.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from pel-commonlisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-commonlisp" '("pel-cl-init")))

;;;***

;;;### (autoloads nil "pel-completion" "pel-completion.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from pel-completion.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-completion" '("pel-")))

;;;***

;;;### (autoloads nil "pel-cua" "pel-cua.el" (0 0 0 0))
;;; Generated autoloads from pel-cua.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-cua" '("pel-cua-")))

;;;***

;;;### (autoloads nil "pel-cursor" "pel-cursor.el" (0 0 0 0))
;;; Generated autoloads from pel-cursor.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-cursor" '("pel-")))

;;;***

;;;### (autoloads nil "pel-custom" "pel-custom.el" (0 0 0 0))
;;; Generated autoloads from pel-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-custom" '("pel-")))

;;;***

;;;### (autoloads nil "pel-diff" "pel-diff.el" (0 0 0 0))
;;; Generated autoloads from pel-diff.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-diff" '("pel-ediff-")))

;;;***

;;;### (autoloads nil "pel-elisp" "pel-elisp.el" (0 0 0 0))
;;; Generated autoloads from pel-elisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-elisp" '("pel-")))

;;;***

;;;### (autoloads nil "pel-elisp-analyze" "pel-elisp-analyze.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pel-elisp-analyze.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-elisp-analyze" '("pel-run-ert")))

;;;***

;;;### (autoloads nil "pel-emacs" "pel-emacs.el" (0 0 0 0))
;;; Generated autoloads from pel-emacs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-emacs" '("pel-emacs-")))

;;;***

;;;### (autoloads nil "pel-erlang" "pel-erlang.el" (0 0 0 0))
;;; Generated autoloads from pel-erlang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-erlang" '("pel-")))

;;;***

;;;### (autoloads nil "pel-skels-erlang" "pel-skels-erlang.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pel-skels-erlang.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-skels-erlang" '("pel-")))

;;;***

;;;### (autoloads nil "pel-ert" "pel-ert.el" (0 0 0 0))
;;; Generated autoloads from pel-ert.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-ert" '("pel-")))

;;;***

;;;### (autoloads nil "pel-etags" "pel-etags.el" (0 0 0 0))
;;; Generated autoloads from pel-etags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-etags" '("pel-file-or-compressed-file-for")))

;;;***

;;;### (autoloads nil "pel-face-ut" "pel-face-ut.el" (0 0 0 0))
;;; Generated autoloads from pel-face-ut.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-face-ut" '("pel-")))

;;;***

;;;### (autoloads nil "pel-file" "pel-file.el" (0 0 0 0))
;;; Generated autoloads from pel-file.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-file" '("pel-")))

;;;***

;;;### (autoloads nil "pel-file-recent" "pel-file-recent.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from pel-file-recent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-file-recent" '("pel-" "ido-recentf-open")))

;;;***

;;;### (autoloads nil "pel-filedir" "pel-filedir.el" (0 0 0 0))
;;; Generated autoloads from pel-filedir.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-filedir" '("pel-")))

;;;***

;;;### (autoloads nil "pel-filex" "pel-filex.el" (0 0 0 0))
;;; Generated autoloads from pel-filex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-filex" '("pel-open-in-os-app")))

;;;***

;;;### (autoloads nil "pel-fill" "pel-fill.el" (0 0 0 0))
;;; Generated autoloads from pel-fill.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-fill" '("pel-")))

;;;***

;;;### (autoloads nil "pel-font" "pel-font.el" (0 0 0 0))
;;; Generated autoloads from pel-font.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-font" '("pel-font-")))

;;;***

;;;### (autoloads nil "pel-frame-control" "pel-frame-control.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pel-frame-control.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-frame-control" '("pel-")))

;;;***

;;;### (autoloads nil "pel-fs" "pel-fs.el" (0 0 0 0))
;;; Generated autoloads from pel-fs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-fs" '("pel-")))

;;;***

;;;### (autoloads nil "pel-go" "pel-go.el" (0 0 0 0))
;;; Generated autoloads from pel-go.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-go" '("pel-go-")))

;;;***

;;;### (autoloads nil "pel-goto-addr" "pel-goto-addr.el" (0 0 0 0))
;;; Generated autoloads from pel-goto-addr.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-goto-addr" '("pel-goto-")))

;;;***

;;;### (autoloads nil "pel-graphviz-dot" "pel-graphviz-dot.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pel-graphviz-dot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-graphviz-dot" '("pel-")))

;;;***

;;;### (autoloads nil "pel-help" "pel-help.el" (0 0 0 0))
;;; Generated autoloads from pel-help.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-help" '("pel-show-")))

;;;***

;;;### (autoloads nil "pel-hide-docstring" "pel-hide-docstring.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pel-hide-docstring.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-hide-docstring" '("pel-")))

;;;***

;;;### (autoloads nil "pel-hideshow" "pel-hideshow.el" (0 0 0 0))
;;; Generated autoloads from pel-hideshow.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-hideshow" '("pel-")))

;;;***

;;;### (autoloads nil "pel-highlight" "pel-highlight.el" (0 0 0 0))
;;; Generated autoloads from pel-highlight.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-highlight" '("pel-")))

;;;***

;;;### (autoloads nil "pel-ido" "pel-ido.el" (0 0 0 0))
;;; Generated autoloads from pel-ido.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-ido" '("pel-set-ido-use-fname-at-point")))

;;;***

;;;### (autoloads nil "pel-imenu" "pel-imenu.el" (0 0 0 0))
;;; Generated autoloads from pel-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-imenu" '("pel-")))

;;;***

;;;### (autoloads nil "pel-imenu-dbg" "pel-imenu-dbg.el" (0 0 0 0))
;;; Generated autoloads from pel-imenu-dbg.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-imenu-dbg" '("pel-imenu-print-vars")))

;;;***

;;;### (autoloads nil "pel-imenu-ido" "pel-imenu-ido.el" (0 0 0 0))
;;; Generated autoloads from pel-imenu-ido.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-imenu-ido" '("pel-")))

;;;***

;;;### (autoloads nil "pel-indent" "pel-indent.el" (0 0 0 0))
;;; Generated autoloads from pel-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-indent" '("pel-")))

;;;***

;;;### (autoloads nil "pel-kbmacros" "pel-kbmacros.el" (0 0 0 0))
;;; Generated autoloads from pel-kbmacros.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-kbmacros" '("pel-")))

;;;***

;;;### (autoloads nil "pel-key-chord" "pel-key-chord.el" (0 0 0 0))
;;; Generated autoloads from pel-key-chord.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-key-chord" '("pel-")))

;;;***

;;;### (autoloads nil "pel-line-control" "pel-line-control.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from pel-line-control.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-line-control" '("pel-")))

;;;***

;;;### (autoloads nil "pel-lisp" "pel-lisp.el" (0 0 0 0))
;;; Generated autoloads from pel-lisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-lisp" '("pel-")))

;;;***

;;;### (autoloads nil "pel-lispy" "pel-lispy.el" (0 0 0 0))
;;; Generated autoloads from pel-lispy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-lispy" '("pel-l")))

;;;***

;;;### (autoloads nil "pel-list" "pel-list.el" (0 0 0 0))
;;; Generated autoloads from pel-list.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-list" '("pel-")))

;;;***

;;;### (autoloads nil "pel-list-comp" "pel-list-comp.el" (0 0 0 0))
;;; Generated autoloads from pel-list-comp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-list-comp" '("pel-")))

;;;***

;;;### (autoloads nil "pel-make" "pel-make.el" (0 0 0 0))
;;; Generated autoloads from pel-make.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-make" '("makefile-nmake-" "pel-make-")))

;;;***

;;;### (autoloads nil "pel-mark" "pel-mark.el" (0 0 0 0))
;;; Generated autoloads from pel-mark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-mark" '("pel-")))

;;;***

;;;### (autoloads nil "pel-mode-line" "pel-mode-line.el" (0 0 0 0))
;;; Generated autoloads from pel-mode-line.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-mode-line" '("my-mode-line-" "mode-line-benchmark" "pel-mode-line-time-info")))

;;;***

;;;### (autoloads nil "pel-navigate" "pel-navigate.el" (0 0 0 0))
;;; Generated autoloads from pel-navigate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-navigate" '("pel-")))

;;;***

;;;### (autoloads nil "pel-net" "pel-net.el" (0 0 0 0))
;;; Generated autoloads from pel-net.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-net" '("pel-install-file")))

;;;***

;;;### (autoloads nil "pel-numkpad" "pel-numkpad.el" (0 0 0 0))
;;; Generated autoloads from pel-numkpad.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-numkpad" '("pel-")))

;;;***

;;;### (autoloads nil "pel-open" "pel-open.el" (0 0 0 0))
;;; Generated autoloads from pel-open.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-open" '("pel-")))

;;;***

;;;### (autoloads nil "pel-package" "pel-package.el" (0 0 0 0))
;;; Generated autoloads from pel-package.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-package" '("pel-")))

;;;***

;;;### (autoloads nil "pel-pathmng" "pel-pathmng.el" (0 0 0 0))
;;; Generated autoloads from pel-pathmng.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-pathmng" '("pel-emacs-load-path")))

;;;***

;;;### (autoloads nil "pel-plantuml" "pel-plantuml.el" (0 0 0 0))
;;; Generated autoloads from pel-plantuml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-plantuml" '("pel-")))

;;;***

;;;### (autoloads nil "pel-pp" "pel-pp.el" (0 0 0 0))
;;; Generated autoloads from pel-pp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-pp" '("pel-pp-")))

;;;***

;;;### (autoloads nil "pel-ppindent" "pel-ppindent.el" (0 0 0 0))
;;; Generated autoloads from pel-ppindent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-ppindent" '("pel-ppindent-")))

;;;***

;;;### (autoloads nil "pel-prompt" "pel-prompt.el" (0 0 0 0))
;;; Generated autoloads from pel-prompt.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-prompt" '("pel-")))

;;;***

;;;### (autoloads nil "pel-python" "pel-python.el" (0 0 0 0))
;;; Generated autoloads from pel-python.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-python" '("pel-python-qualified-module-name" "py-qualified-module-name")))

;;;***

;;;### (autoloads nil "pel-read" "pel-read.el" (0 0 0 0))
;;; Generated autoloads from pel-read.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-read" '("pel-")))

;;;***

;;;### (autoloads nil "pel-regexp" "pel-regexp.el" (0 0 0 0))
;;; Generated autoloads from pel-regexp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-regexp" '("pel-insert-")))

;;;***

;;;### (autoloads nil "pel-register" "pel-register.el" (0 0 0 0))
;;; Generated autoloads from pel-register.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-register" '("pel-")))

;;;***

;;;### (autoloads nil "pel-rst" "pel-rst.el" (0 0 0 0))
;;; Generated autoloads from pel-rst.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-rst" '("pel-")))

;;;***

;;;### (autoloads nil "pel-safe-list" "pel-safe-list.el" (0 0 0 0))
;;; Generated autoloads from pel-safe-list.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-safe-list" '("caar-safe" "cadr-safe")))

;;;***

;;;### (autoloads nil "pel-scroll" "pel-scroll.el" (0 0 0 0))
;;; Generated autoloads from pel-scroll.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-scroll" '("pel-")))

;;;***

;;;### (autoloads nil "pel-search" "pel-search.el" (0 0 0 0))
;;; Generated autoloads from pel-search.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-search" '("pel-")))

;;;***

;;;### (autoloads nil "pel-search-regexp" "pel-search-regexp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pel-search-regexp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-search-regexp" '("pel-")))

;;;***

;;;### (autoloads nil "pel-seq" "pel-seq.el" (0 0 0 0))
;;; Generated autoloads from pel-seq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-seq" '("pel-all-fboundp")))

;;;***

;;;### (autoloads nil "pel-skels" "pel-skels.el" (0 0 0 0))
;;; Generated autoloads from pel-skels.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-skels" '("pel-")))

;;;***

;;;### (autoloads nil "pel-skels-c" "pel-skels-c.el" (0 0 0 0))
;;; Generated autoloads from pel-skels-c.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-skels-c" '("pel-")))

;;;***

;;;### (autoloads nil "pel-skels-elisp" "pel-skels-elisp.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from pel-skels-elisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-skels-elisp" '("pel-")))

;;;***

;;;### (autoloads nil "pel-skels-generic" "pel-skels-generic.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pel-skels-generic.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-skels-generic" '("pel-")))

;;;***

;;;### (autoloads nil "pel-skels-rst" "pel-skels-rst.el" (0 0 0 0))
;;; Generated autoloads from pel-skels-rst.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-skels-rst" '("pel-")))

;;;***

;;;### (autoloads nil "pel-speedbar" "pel-speedbar.el" (0 0 0 0))
;;; Generated autoloads from pel-speedbar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-speedbar" '("pel-")))

;;;***

;;;### (autoloads nil "pel-spell" "pel-spell.el" (0 0 0 0))
;;; Generated autoloads from pel-spell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-spell" '("pel-")))

;;;***

;;;### (autoloads nil "pel-xref" "pel-xref.el" (0 0 0 0))
;;; Generated autoloads from pel-xref.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-xref" '("pel-xref-show-status")))

;;;***

;;;### (autoloads nil "pel-tempo" "pel-tempo.el" (0 0 0 0))
;;; Generated autoloads from pel-tempo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-tempo" '("pel-tempo-")))

;;;***

;;;### (autoloads nil "pel-text-insert" "pel-text-insert.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from pel-text-insert.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-text-insert" '("pel-")))

;;;***

;;;### (autoloads nil "pel-text-transform" "pel-text-transform.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from pel-text-transform.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-text-transform" '("pel-")))

;;;***

;;;### (autoloads nil "pel-undo" "pel-undo.el" (0 0 0 0))
;;; Generated autoloads from pel-undo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-undo" '("pel-")))

;;;***

;;;### (autoloads nil "pel-uuid" "pel-uuid.el" (0 0 0 0))
;;; Generated autoloads from pel-uuid.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-uuid" '("pel-")))

;;;***

;;;### (autoloads nil "pel-window" "pel-window.el" (0 0 0 0))
;;; Generated autoloads from pel-window.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-window" '("pel-")))

;;;***

;;;### (autoloads nil "pel-xr" "pel-xr.el" (0 0 0 0))
;;; Generated autoloads from pel-xr.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel-xr" '("pel-")))

;;;***

;;;### (autoloads nil "pel__hydra" "pel__hydra.el" (0 0 0 0))
;;; Generated autoloads from pel__hydra.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel__hydra" '("pel-")))

;;;***

;;;### (autoloads nil "pel_keys" "pel_keys.el" (0 0 0 0))
;;; Generated autoloads from pel_keys.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pel_keys" '("pel")))

;;;***

;;;### (autoloads nil "sexp" "sexp.el" (0 0 0 0))
;;; Generated autoloads from sexp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sexp" '("triangle-using-cond" "abc" "last-in" "first-in")))

;;;***

;;;### (autoloads nil "t" "t.el" (0 0 0 0))
;;; Generated autoloads from t.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t" '("pel-clean-package-selected-packages-in")))

;;;***

;;;### (autoloads nil "tlambda" "tlambda.el" (0 0 0 0))
;;; Generated autoloads from tlambda.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tlambda" '("sum-fibonacci" "evenp")))

;;;***

;;;### (autoloads nil nil ("au.el" "pel-pkg.el" "pel-stats.el") (0
;;;;;;  0 0 0))

;;;***


;;; Generated autoloads from pel.el

(autoload 'pel-init "pel" "\
Initialize PEL, map its keys, autoload its functions.

If CACHED-ABBREV-FILE-NAME is non-nil it should hold a the value
the name of the abbreviation file, the name that the variable
`abbrev-file-name' had before it was changed in the init file.
This is done to delay the loading of the abbreviation file, speeding
up Emacs initialization time when the file is big.

Only the PEL features activated via the `pel-use-...' customization variables
from the  \"Pel Package Use\" subgroup of the \"Pel\" group are loaded and the
respective PEL keys are mapped.  The others are not.

If you need to activate new features, use \\[customize] to customize variables
inside the \"Pel\" group.  The \"Pel Package Use\" subgroup contains the
customization variables that control PEL activated features.

You can customize PEL feature only after execution of the `pel-init' command.
After a customization change its best to restart Emacs, however if your
modifications simply activate new features, you may be able to simply
re-execute `pel-init' again to activate them.

(fn &optional CACHED-ABBREV-FILE-NAME)" t)
(register-definition-prefixes "pel" '("pel--cached-abbrev-file-name"))


;;; Generated autoloads from pel--base.el

(register-definition-prefixes "pel--base" '("pel"))


;;; Generated autoloads from pel--keys-macros.el

(register-definition-prefixes "pel--keys-macros" '("define-pel-" "pel-"))


;;; Generated autoloads from pel--macros.el

(register-definition-prefixes "pel--macros" '("pel-" "while-n"))


;;; Generated autoloads from pel--options.el

(register-definition-prefixes "pel--options" '("pel-"))


;;; Generated autoloads from pel--syntax-macros.el

(register-definition-prefixes "pel--syntax-macros" '("pel-"))


;;; Generated autoloads from pel-align.el

(register-definition-prefixes "pel-align" '("pel-"))


;;; Generated autoloads from pel-autoload.el

(register-definition-prefixes "pel-autoload" '("pel-"))


;;; Generated autoloads from pel-benchmark.el

(register-definition-prefixes "pel-benchmark" '("pel-"))


;;; Generated autoloads from pel-bookmark.el

(register-definition-prefixes "pel-bookmark" '("pel-bookmark-"))


;;; Generated autoloads from pel-browse.el

(register-definition-prefixes "pel-browse" '("pel-"))


;;; Generated autoloads from pel-buffer.el

(register-definition-prefixes "pel-buffer" '("pel-"))


;;; Generated autoloads from pel-c-comment.el

(register-definition-prefixes "pel-c-comment" '("pel-"))


;;; Generated autoloads from pel-c-preproc.el

(register-definition-prefixes "pel-c-preproc" '("pel-"))


;;; Generated autoloads from pel-c-utils.el

(register-definition-prefixes "pel-c-utils" '("pel-"))


;;; Generated autoloads from pel-cc.el

(register-definition-prefixes "pel-cc" '("pel-cc-"))


;;; Generated autoloads from pel-cc-find.el

(register-definition-prefixes "pel-cc-find" '("pel-"))


;;; Generated autoloads from pel-cc-linux-kernel.el

(register-definition-prefixes "pel-cc-linux-kernel" '("pel-"))


;;; Generated autoloads from pel-cc-navigate.el

(register-definition-prefixes "pel-cc-navigate" '("pel-"))


;;; Generated autoloads from pel-ccp.el

(register-definition-prefixes "pel-ccp" '("pel-"))


;;; Generated autoloads from pel-comint.el

(register-definition-prefixes "pel-comint" '("pel-comint-"))


;;; Generated autoloads from pel-comment.el

(register-definition-prefixes "pel-comment" '("pel-"))


;;; Generated autoloads from pel-comment-adorn.el

(register-definition-prefixes "pel-comment-adorn" '("pel-"))


;;; Generated autoloads from pel-commonlisp.el

(register-definition-prefixes "pel-commonlisp" '("pel-cl-"))


;;; Generated autoloads from pel-comp.el

(register-definition-prefixes "pel-comp" '("pel-"))


;;; Generated autoloads from pel-completion.el

(register-definition-prefixes "pel-completion" '("pel-"))


;;; Generated autoloads from pel-cpp.el

(register-definition-prefixes "pel-cpp" '("pel-"))


;;; Generated autoloads from pel-cua.el

(register-definition-prefixes "pel-cua" '("pel-cua-"))


;;; Generated autoloads from pel-cursor.el

(register-definition-prefixes "pel-cursor" '("pel-"))


;;; Generated autoloads from pel-custom.el

(register-definition-prefixes "pel-custom" '("pel-"))


;;; Generated autoloads from pel-d.el

(register-definition-prefixes "pel-d" '("pel-d-insert-shebang-line"))


;;; Generated autoloads from pel-diff.el

(register-definition-prefixes "pel-diff" '("pel"))


;;; Generated autoloads from pel-dtreplace.el

(register-definition-prefixes "pel-dtreplace" '("pel-"))


;;; Generated autoloads from pel-elisp.el

(register-definition-prefixes "pel-elisp" '("pel-"))


;;; Generated autoloads from pel-elisp-analyze.el

(register-definition-prefixes "pel-elisp-analyze" '("pel-run-ert"))


;;; Generated autoloads from pel-elisp-eval.el

(register-definition-prefixes "pel-elisp-eval" '("pel-eval-last-sexp-and-copy"))


;;; Generated autoloads from pel-elpa.el

(register-definition-prefixes "pel-elpa" '("pel-el"))


;;; Generated autoloads from pel-emacs.el

(register-definition-prefixes "pel-emacs" '("pel-"))


;;; Generated autoloads from pel-emacs-analyze.el

(register-definition-prefixes "pel-emacs-analyze" '("pel-show-lisp-control-variables"))


;;; Generated autoloads from pel-erlang.el

(register-definition-prefixes "pel-erlang" '("pel-"))


;;; Generated autoloads from pel-ert.el

(register-definition-prefixes "pel-ert" '("pel-"))


;;; Generated autoloads from pel-etags.el

(register-definition-prefixes "pel-etags" '("pel-file-or-compressed-file-for"))


;;; Generated autoloads from pel-face-ut.el

(register-definition-prefixes "pel-face-ut" '("pel-"))


;;; Generated autoloads from pel-ffind.el

(register-definition-prefixes "pel-ffind" '("pel-"))


;;; Generated autoloads from pel-ffind-inpath.el

(register-definition-prefixes "pel-ffind-inpath" '("pel-ffind-inpath"))


;;; Generated autoloads from pel-file.el

(register-definition-prefixes "pel-file" '("pel-"))


;;; Generated autoloads from pel-file-recent.el

(register-definition-prefixes "pel-file-recent" '("ido-recentf-open" "pel-"))


;;; Generated autoloads from pel-filedir.el

(register-definition-prefixes "pel-filedir" '("pel-"))


;;; Generated autoloads from pel-filex.el

(register-definition-prefixes "pel-filex" '("pel-open-"))


;;; Generated autoloads from pel-fill.el

(register-definition-prefixes "pel-fill" '("pel-"))


;;; Generated autoloads from pel-font.el

(register-definition-prefixes "pel-font" '("pel-font-"))


;;; Generated autoloads from pel-frame-control.el

(register-definition-prefixes "pel-frame-control" '("pel-"))


;;; Generated autoloads from pel-fs.el

(register-definition-prefixes "pel-fs" '("pel-"))


;;; Generated autoloads from pel-go.el

(register-definition-prefixes "pel-go" '("pel-go-"))


;;; Generated autoloads from pel-goto-addr.el

(register-definition-prefixes "pel-goto-addr" '("pel-goto-"))


;;; Generated autoloads from pel-graphviz-dot.el

(register-definition-prefixes "pel-graphviz-dot" '("pel-"))


;;; Generated autoloads from pel-hash.el

(register-definition-prefixes "pel-hash" '("pel-"))


;;; Generated autoloads from pel-help.el

(register-definition-prefixes "pel-help" '("pel-"))


;;; Generated autoloads from pel-hex.el

(register-definition-prefixes "pel-hex" '("pel-bibyte"))


;;; Generated autoloads from pel-hide-docstring.el

(register-definition-prefixes "pel-hide-docstring" '("pel-"))


;;; Generated autoloads from pel-hideshow.el

(register-definition-prefixes "pel-hideshow" '("pel-"))


;;; Generated autoloads from pel-highlight.el

(register-definition-prefixes "pel-highlight" '("pel-"))


;;; Generated autoloads from pel-ibuffer.el

(register-definition-prefixes "pel-ibuffer" '("dirname" "get-all-buffer-directories" "ibuffer-set-filter-groups-by-directory" "pel-map-ibuffer-mode-filters"))


;;; Generated autoloads from pel-ido.el

(register-definition-prefixes "pel-ido" '("pel-"))


;;; Generated autoloads from pel-iedit.el

(register-definition-prefixes "pel-iedit" '("pel-"))


;;; Generated autoloads from pel-iedit-modes-support.el

(register-definition-prefixes "pel-iedit-modes-support" '("pel-"))


;;; Generated autoloads from pel-imenu.el

(register-definition-prefixes "pel-imenu" '("pel-"))


;;; Generated autoloads from pel-imenu-dbg.el

(register-definition-prefixes "pel-imenu-dbg" '("pel-imenu-print-vars"))


;;; Generated autoloads from pel-imenu-ido.el

(register-definition-prefixes "pel-imenu-ido" '("pel-"))


;;; Generated autoloads from pel-indent.el

(register-definition-prefixes "pel-indent" '("pel-"))


;;; Generated autoloads from pel-ini.el

(register-definition-prefixes "pel-ini" '("pel-ini-"))


;;; Generated autoloads from pel-itemize.el

(register-definition-prefixes "pel-itemize" '("pel-itemize-lines"))


;;; Generated autoloads from pel-kbmacros.el

(register-definition-prefixes "pel-kbmacros" '("pel-"))


;;; Generated autoloads from pel-key-chord.el

(register-definition-prefixes "pel-key-chord" '("pel-"))


;;; Generated autoloads from pel-lfe.el

(register-definition-prefixes "pel-lfe" '("pel-lfe-eval-buffer"))


;;; Generated autoloads from pel-line-control.el

(register-definition-prefixes "pel-line-control" '("pel-"))


;;; Generated autoloads from pel-lisp.el

(register-definition-prefixes "pel-lisp" '("pel-"))


;;; Generated autoloads from pel-lispy.el

(register-definition-prefixes "pel-lispy" '("pel-"))


;;; Generated autoloads from pel-list.el

(register-definition-prefixes "pel-list" '("pel-"))


;;; Generated autoloads from pel-lsp.el

(register-definition-prefixes "pel-lsp" '("pel-toggle-lsp-"))


;;; Generated autoloads from pel-lua.el

(register-definition-prefixes "pel-lua" '("pel-lua-insert-shebang-line"))


;;; Generated autoloads from pel-make.el

(register-definition-prefixes "pel-make" '("makefile-nmake-" "pel-"))


;;; Generated autoloads from pel-man.el

(register-definition-prefixes "pel-man" '("pel-man-at-point"))


;;; Generated autoloads from pel-mark.el

(register-definition-prefixes "pel-mark" '("pel-"))


;;; Generated autoloads from pel-navigate.el

(register-definition-prefixes "pel-navigate" '("pel-"))


;;; Generated autoloads from pel-nim.el

(register-definition-prefixes "pel-nim" '("pel-nim-insert-shebang-line"))


;;; Generated autoloads from pel-numkpad.el

(register-definition-prefixes "pel-numkpad" '("pel-"))


;;; Generated autoloads from pel-open.el

(register-definition-prefixes "pel-open" '("pel-"))


;;; Generated autoloads from pel-outline.el

(register-definition-prefixes "pel-outline" '("pel-outline-print-vars"))


;;; Generated autoloads from pel-package.el

(register-definition-prefixes "pel-package" '("loaded-file-name" "pel-"))


;;; Generated autoloads from pel-pathmng.el

(register-definition-prefixes "pel-pathmng" '("pel-"))


;;; Generated autoloads from pel-perl.el

(register-definition-prefixes "pel-perl" '("pel-perl-"))


;;; Generated autoloads from pel-pike.el

(register-definition-prefixes "pel-pike" '("pel-pike-"))


;;; Generated autoloads from pel-plantuml.el

(register-definition-prefixes "pel-plantuml" '("pel-"))


;;; Generated autoloads from pel-pp.el

(register-definition-prefixes "pel-pp" '("pel-pp-"))


;;; Generated autoloads from pel-process.el

(register-definition-prefixes "pel-process" '("pel-process-"))


;;; Generated autoloads from pel-prompt.el

(register-definition-prefixes "pel-prompt" '("pel-"))


;;; Generated autoloads from pel-psw.el

(register-definition-prefixes "pel-psw" '("pel-psw-navigate-files"))


;;; Generated autoloads from pel-python.el

(register-definition-prefixes "pel-python" '("pel-python-insert-shebang-line"))


;;; Generated autoloads from pel-read.el

(register-definition-prefixes "pel-read" '("pel-"))


;;; Generated autoloads from pel-regexp.el

(register-definition-prefixes "pel-regexp" '("pel-insert-"))


;;; Generated autoloads from pel-register.el

(register-definition-prefixes "pel-register" '("pel-"))


;;; Generated autoloads from pel-reload.el

(register-definition-prefixes "pel-reload" '("pel-"))


;;; Generated autoloads from pel-rpm-spec.el

(register-definition-prefixes "pel-rpm-spec" '("pel-"))


;;; Generated autoloads from pel-rst.el

(register-definition-prefixes "pel-rst" '("pel-"))


;;; Generated autoloads from pel-ruby.el

(register-definition-prefixes "pel-ruby" '("pel-ruby-insert-shebang-line"))


;;; Generated autoloads from pel-scheme.el

(register-definition-prefixes "pel-scheme" '("pel-"))


;;; Generated autoloads from pel-screen.el

(register-definition-prefixes "pel-screen" '("pel-screen-log-fix-rendering"))


;;; Generated autoloads from pel-scroll.el

(register-definition-prefixes "pel-scroll" '("pel-"))


;;; Generated autoloads from pel-search.el

(register-definition-prefixes "pel-search" '("pel-"))


;;; Generated autoloads from pel-search-regexp.el

(register-definition-prefixes "pel-search-regexp" '("pel-"))


;;; Generated autoloads from pel-seed7.el

(register-definition-prefixes "pel-seed7" '("pel-seed7-insert-shebang-line"))


;;; Generated autoloads from pel-seq.el

(register-definition-prefixes "pel-seq" '("pel-all-fboundp"))


;;; Generated autoloads from pel-server.el

(register-definition-prefixes "pel-server" '("pel-shutdown-server"))


;;; Generated autoloads from pel-setup.el

(register-definition-prefixes "pel-setup" '("pel-"))


;;; Generated autoloads from pel-setup-27.el

(register-definition-prefixes "pel-setup-27" '("pel-"))


;;; Generated autoloads from pel-setup-base.el

(register-definition-prefixes "pel-setup-base" '("pel-"))


;;; Generated autoloads from pel-sh.el

(register-definition-prefixes "pel-sh" '("pel-"))


;;; Generated autoloads from pel-shell.el

(register-definition-prefixes "pel-shell" '("pel-shell"))


;;; Generated autoloads from pel-skels.el

(register-definition-prefixes "pel-skels" '("pel-"))


;;; Generated autoloads from pel-skels-c.el

(register-definition-prefixes "pel-skels-c" '("pel-"))


;;; Generated autoloads from pel-skels-clisp.el

(register-definition-prefixes "pel-skels-clisp" '("pel-"))


;;; Generated autoloads from pel-skels-cpp.el

(register-definition-prefixes "pel-skels-cpp" '("pel-"))


;;; Generated autoloads from pel-skels-elisp.el

(register-definition-prefixes "pel-skels-elisp" '("pel-"))


;;; Generated autoloads from pel-skels-erlang.el

(register-definition-prefixes "pel-skels-erlang" '("pel-"))


;;; Generated autoloads from pel-skels-generic.el

(register-definition-prefixes "pel-skels-generic" '("pel-"))


;;; Generated autoloads from pel-skels-rst.el

(register-definition-prefixes "pel-skels-rst" '("pel-"))


;;; Generated autoloads from pel-smartparens.el

(register-definition-prefixes "pel-smartparens" '("pel-"))


;;; Generated autoloads from pel-speedbar.el

(register-definition-prefixes "pel-speedbar" '("pel-"))


;;; Generated autoloads from pel-spell.el

(register-definition-prefixes "pel-spell" '("pel-"))


;;; Generated autoloads from pel-spell-iedit.el

(register-definition-prefixes "pel-spell-iedit" '("pel-spell-iedit-check-conflict"))


;;; Generated autoloads from pel-sudo-edit.el

(register-definition-prefixes "pel-sudo-edit" '("pel-"))


;;; Generated autoloads from pel-syntax.el

(register-definition-prefixes "pel-syntax" '("pel-"))


;;; Generated autoloads from pel-tcl.el

(register-definition-prefixes "pel-tcl" '("pel-tcl-"))


;;; Generated autoloads from pel-tempo.el

(register-definition-prefixes "pel-tempo" '("pel-tempo-"))


;;; Generated autoloads from pel-text-insert.el

(register-definition-prefixes "pel-text-insert" '("pel-"))


;;; Generated autoloads from pel-text-transform.el

(register-definition-prefixes "pel-text-transform" '("pel-"))


;;; Generated autoloads from pel-time.el

(register-definition-prefixes "pel-time" '("pel-"))


;;; Generated autoloads from pel-timestamp.el

(register-definition-prefixes "pel-timestamp" '("pel-"))


;;; Generated autoloads from pel-undo.el

(register-definition-prefixes "pel-undo" '("pel-"))


;;; Generated autoloads from pel-uuid.el

(register-definition-prefixes "pel-uuid" '("pel-"))


;;; Generated autoloads from pel-vc.el

(register-definition-prefixes "pel-vc" '("pel-vc-"))


;;; Generated autoloads from pel-vcs.el

(register-definition-prefixes "pel-vcs" '("pel-"))


;;; Generated autoloads from pel-whitespace.el

(register-definition-prefixes "pel-whitespace" '("pel-"))


;;; Generated autoloads from pel-window.el

(register-definition-prefixes "pel-window" '("pel-"))


;;; Generated autoloads from pel-xr.el

(register-definition-prefixes "pel-xr" '("pel-"))


;;; Generated autoloads from pel-xref.el

(register-definition-prefixes "pel-xref" '("pel-"))


;;; Generated autoloads from pel-yang.el

(register-definition-prefixes "pel-yang" '("pel-yang-"))


;;; Generated autoloads from pel__hydra.el

(register-definition-prefixes "pel__hydra" '("pel-"))


;;; Generated autoloads from pel_keys.el

(register-definition-prefixes "pel_keys" '("pel"))


;;; Generated autoloads from pel--indent.el

(register-definition-prefixes "pel--indent" '("pel-"))


;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pel-autoloads.el ends here
