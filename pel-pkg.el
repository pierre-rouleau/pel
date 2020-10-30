;;; -*- no-byte-compile: t -*-
(define-package "pel" "0.3.1" "Pragmatic Environment Library"
  ;; The handling of Emacs package versions varies widely from package
  ;; to package.  Unfortunately, version numbers that appear somewhat
  ;; *stable* or sometimes very misleading.
  ;; For example you can find the following ace-window.el file from
  ;; these 3 sites:
  ;; - on Melpa-stable : 17415 bytes:   0.9.0, May 12-2015:
  ;; - on GNU Elpa     : 19116 bytes:   0.9.0, June-05-2015
  ;; - on Melpa        : 35195 bytes:   20200304.1344
  ;;                     (last commit on March 4, 2020) still labelled 0.9.0
  ;;
  ;; I would have preferred to use fixed versions and be able to verify
  ;; (ideally automatically) that all is OK with the versions identified.
  ;; Unfortunately I don't really have the time to do that.
  ;; So... I kind of went with the flow and use MELPA for now...
  ;;
  ;; If anyone finds an issue related to a version of a dependency,
  ;; please let me know.
  ;;
  ;; -- Pierre Rouleau
  ;;
  '((emacs "26.1")
    (use-package         "20191126.2034") ; melpa  Melpa-stable: 2.4
    (ace-link            "20200518.957")  ; melpa  Melpa-stable: 0.5.0
    (ace-window          "20200304.1344") ; melpa  Melpa-stable: 0.9.0
    (adoc-mode           "20160314.2130")
    (ag                  "20190726.9")    ; melpa
    (all-the-icons       "20200923.1339")
    (all-the-icons-ibuffer "20200612.1642")
    (all-the-icons-dired "20200403.1018")
    (all-the-icons-ivy   "20190508.1803")
    (alchemist           "20180312.1304")
    (anzu                "20200514.1801") ; melpa
    (apples-mode         "20110121.418")
    (ascii-table         "20200329.1744") ; melpa
    (auto-complete       "20170125.245")  ; melpa  Melpa-stable: 1.5.1
    (avy                 "20200624.1148") ; melpa
    (bind-key            "20191110.416")  ; melpa  Melpa-stable: 2.4
    (bm                  "20190807.1217") ; melpa  Melpa-stable: 201905
    (c-eldoc             "20181109.439")  ; melpa. Melpa-stable: N/A
    (cargo               "20191224.47")   ; melpa. Melpa-stable: 0.4.1
    (company             "2020228.1919")  ; melpa. Melpa-stable: 0.9.12
    (counsel             "20200520.1809") ; melpa.
    (d-mode              "20191009.903")
    (desktop+            "20170107.2132")
    (desktop-registry    "20140119.2143")
    (dired-narrow        "20181114.1723") ; melpa. Melpa-stable: N/A
    (edts                "020200304.1709"); melpa. Melpa-stable: 0.1.0
    (elixir-mode         "20200121.623")
    (elpy                "20200202.2031") ; melpa. Melpa-stable: 1.32.0
    (erlang              "20200313.1030")
    (esup                "20200130.2034") ; melpa. Melpa-stable: 0.7.1
    (expand-region       "20200304.1839") ; melpa. Melpa-stable: 0.11.0
    (exunit              "20190919.1238")
    (fill-column-indicator "20200806.2239") ; melpa Not needed for Emacs 27.1 and later
    (flycheck            "20200516.1719") ; melpa
    (flycheck-plantuml   "20171018.111")  ; melpa
    (free-keys           "20160726.2050") ; melpa. Melpa-stable: 1.0.0
    (forth-mode          "20170527.1930") ; melpa
    (goto-last-change    "20150109.1823") ; melpa. Melpa-stable: 1.2.1
    (graphviz-dot-mode   "20200304.432")  ; melpa. Melpa-stable: 0.4.2
    (helm                "20200517.509")  ; melpa.
    (helm-cscope         "20190615.41")
    (helm-xref           "20201004.1817")
    (highlight-defined   "20181106.1718") ; melpa. Melpa-stable: 0.1.5
    (hydra               "20200306.913")
    (ivy                 "20200520.1809") ; melpa.
    (js2-mode            "20200725.112")
    (julia-snail         "20200420.221")
    (keycast             "20200612.2247") ; melpa
    (key-seq             "20150907.756")  ; melpa
    (key-chord           "20160227.1238") ; melpa
    (lfe-mode            "20170121.1254")
    (lice                "20191011.631")  ; melpa. Melpa-stable: 0.2
    (macrostep           "20161120.2106") ; melpa. Melpa-stable: 0.9
    (magit               "20200408.2341") ; melpa. Melpa-stable: 2.90.1
    (monky               "20190619.1637") ; melpa. Melpa-stable: 0.1 (old)
    (multiple-cursors    "20191210.1759") ; melpa
    (neotree             "20200324.1946") ; melpa.
    (nhexl-mode          "1.4")           ; gnu
    (parinfer            "20180904.844")  ; melpa. Melpa-stable: 0.4.10
    (pcre2el             "20161120.2103") ; melpa
    (plantuml-mode       "20191102.2056") ; melpa
    (popup               "0.5.3")         ; melpa-stable. Melpa: 20160709.1429
    (popup-kill-ring     "20131020.1854") ; melpa. Melpa-stable: N/A
    (projectile          "20200616.1659") ; melpa.
    (racer               "20191001.2344") ; melpa. Melpa-stable: 1.2
    (rainbow-delimiters  "20191018.1233") ; melpa. Melpa-stable: 2.1.4
    (regex-tool          "20170104.1918") ; melpa
    (ripgrep             "20190215.841")  ; melpa
    (rg                  "20200307.1623") ; melpa. Melpa-stable: 1.8.1
    (rust-mode           "20200303.932")  ; melpa. Melpa-stable: 0.4.0
    (rtags-xref          "20200310.1909")
    (slime               "20200228.1656") ; melpa. Melpa-stable: 2.24
    (smart-dash          "20200104.1620") ; melpa.
    (smooth-scrolling    "20161002.1949") ; melpa. Melpa-stable: 2.0.0
    ;; sr-speedbar of 2014 has a bug that was fixed
    ;; in the 2016 version.
    (sr-speedbar         "20161025.831")  ; melpa. Melpa-stable: 20140914.2339
    (swiper              "20200503.1102") ; melpa.
    (treemacs            "20201004.1125")
    (undo-tree           "0.7.4")         ; gnu
    (v-mode              "20200823.535")
    (visual-regexp       "20190414.814")  ; melpa
    (visual-regexp-steroids "20190414.814") ; melpa
    (vterm               "20200418.1610")
    (which-key           "20200216.1350") ; melpa. Melpa-table : 3.4.0
    (xcscope             "20200828.1027")
    (xr                  "1.19")          ; gnu
    (yasnippet           "20200604.246")  ; melpa
    (yasnippet-snippets  "20200802.1658") ; melpa
    (ztree               "20191108.2234")); melpa.

  :url "https://github.com/pierre-rouleau/pel"
  :authors '(("Pierre Rouleau" . "prouleau001@gmail.com")))

;; The following packages are useful for development but not
;; required to use PEL
;;  (elisp-lint          "0.3.0")         ; melpa-stable. Melpa : 20200217.38
