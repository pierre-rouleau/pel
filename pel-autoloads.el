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

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pel-autoloads.el ends here
