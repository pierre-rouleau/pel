;;; -*- no-byte-compile: t -*-
(define-package "pel" "0.0.1" "Pragmatic Environment Library"
  ;; The handling of Emacs package versions varies widely from package to package.
  ;; Unfortunately, version numbers that appear somewhat *stable* or sometimes
  ;; very misleading.
  ;; For example you can find the following ace-window.el file from these 3 sites:
  ;; - on Melpa-stable : 17415 bytes:   0.9.0, May 12-2015:
  ;; - on GNU Elpa     : 19116 bytes:   0.9.0, June-05-2015
  ;; - on Melpa        : 35195 bytes:   20200304.1344 (last commit on March 4, 2020) still labelled 0.9.0
  ;;
  ;; So... for this the version number is useless.
  ;; This makes it very hard to obtain stability in dependencies, and check validity, to say the least.
  ;;
  ;; My strategy so far is that I try as much as what I can on a stable package
  ;; I use often.  If I find something that is really out of date on Melpa-stable or GNU Elpa
  ;; I use Melpa, but I tried to get a sense of how stable the project is and understand how
  ;; the team is dealing with changes (when I have time to check).
  ;;
  ;; I check for byte-compiler warnings loading the PEL package and its dependencies
  ;; from a fresh environment and try to handle the various possibilities I detect from that.
  ;;
  ;; I'm documenting the versions below when I prepare a PEL package to help me learn
  ;; the evolution of those packages over time.
  ;;
  ;; I would *like* to be able to version PEL using the semantic versionning methodology
  ;; and use Git lables to tag the versions.  That might mean that I'll have to spend more time
  ;; on PEL that I might have in the future, we'll see.
  ;;
  ;; If anyone finds an issue related to a version of a dependency, please let me know.
  ;;
  ;; -- Pierre Rouleau
  ;;
  '((emacs "26.1")
    (use-package         "2.4")             ; melpa-stable. Melpa:= 20191126.2034
    (ace-window          "20200304.1344")   ; melpa         Melpa-stable := 0.9.0
    (auto-complete       "1.5.1")           ; melpa-stable. Melpa := 20170125.245
    (bind-key            "2.4")             ; melpa-stable. Melpa := 20191110.416
    (bm                  "201905")          ; melpa-stable. Melpa := 20190807.1217
    (c-eldoc             "20181109.439")    ; melpa.        Melpa-stable:= nothing
    (cargo               "0.4.1")           ; melpa.        Melpa := 20191224.47
    (company             "0.9.12")          ; melpa-stable. Melpa := 2020228.1919
    (dired-narrow        "20181114.1723")   ; melpa.        Melpa-stable := nothing
    (edts                "0.1.0")           ; melpa-stable. Melpa        := 020200304.1709
    (elisp-lint          "0.3.0")           ; melpa-stable. Melpa        := 20200217.38
    (elpy                "1.32.0")          ; melpa-stable. Melpa        := 20200202.2031
    (esup                "0.7.1")           ; melpa-stable. Melpa        := 20200130.2034
    (expand-region       "0.11.0")          ; melpa-stable. Melpa        := 20200304.1839
    (free-keys           "1.0.0")           ; melpa-stable. Melpa        := 20160726.2050
    (goto-last-change    "1.2.1")           ; melpa-stable. Melpa        := 20150109.1823
    (graphviz-dot-mode   "0.4.2")           ; melpa-stable. Melpa        := 20200304.432
    (highlight-defined   "0.1.5")           ; melpa-stable. Melpa        := 20181106.1718
    (lice                "0.2")             ; melpa-stable. Melpa        := 20191011.631
    (macrostep           "0.9")             ; melpa-stable. Melpa        := 20161120.2106
    (nhexl-mode          "1.4")             ; gnu
    (parinfer            "0.4.10")          ; melpa-stable. Melpa        := 20180904.844
    (popup               "0.5.3")           ; melpa-stable. Melpa        := 20160709.1429
    (popup-kill-ring     "20131020.1854")   ; melpa.        Melpa-stable := nothing
    (racer               "1.2")             ; melpa-stable. Melpa        := 20191001.2344
    (rainbow-delimiters  "2.1.4")           ; melpa-stable. Melpa        := 20191018.1233
    (rg                  "1.8.1")           ; melpa-stable. Melpa        := 20200307.1623
    (rust-mode           "0.4.0")           ; melpa-stable. Melpa        := 20200303.932
    (slime               "2.24")            ; melpa-stable. Melpa        := 20200228.1656
    (smooth-scrolling    "2.0.0")           ; melpa-stable. Melpa        := 20161002.1949
    ;; sr-speedbar of 2014 has a bug that was fixed in the 2016 version.
    (sr-speedbar         "20161025.831")    ; melpa         Melpa-stable := 20140914.2339
    (undo-tree           "0.7.4")           ; gnu
    (which-key           "3.4.0"))           ; melpa-stable. Melpa        := 20200216.1350

  :url "https://github.com/pierre-rouleau/pel"
  :authors '(("Pierre Rouleau" . "prouleau.swd@gmail.com")))
