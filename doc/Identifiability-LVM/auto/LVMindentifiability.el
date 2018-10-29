(TeX-add-style-hook
 "LVMindentifiability"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("algpseudocode" "noend")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "listings"
    "inputenc"
    "fontenc"
    "lmodern"
    "textcomp"
    "color"
    "enumerate"
    "graphicx"
    "grffile"
    "wrapfig"
    "capt-of"
    "rotating"
    "caption"
    "longtable"
    "multirow"
    "multicol"
    "pdflscape"
    "setspace"
    "geometry"
    "amssymb"
    "amsmath"
    "amsfonts"
    "dsfont"
    "array"
    "ifthen"
    "hyperref"
    "natbib"
    "authblk"
    "algorithm"
    "algpseudocode"
    "colortbl"
    "epstopdf"
    "xspace"
    "xifthen"
    "xargs"
    "stmaryrd"
    "prodint")
   (TeX-add-symbols
    '("trans" 1)
    '("defBoldVar" 2)
    '("defUOperator" 5)
    '("defOperator" 7)
    "Real"
    "Rational"
    "Natural"
    "independent")
   (LaTeX-add-labels
    "sec:org3f6f343"
    "sec:org3dd5345"
    "sec:org68b96b5"
    "sec:org8e5bde5"
    "sec:orgc4cc5b6"))
 :latex)

