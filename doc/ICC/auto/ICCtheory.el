(TeX-add-style-hook
 "ICCtheory"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("geometry" "top=3cm" "bottom=3cm" "left=3cm" "right=3cm") ("inputenc" "utf8") ("ulem" "normalem")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "1_MathNotations"
    "article"
    "art10"
    "listings"
    "color"
    "amsmath"
    "array"
    "fontenc"
    "natbib"
    "geometry"
    "inputenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref")
   (LaTeX-add-labels
    "sec:org26476cb"
    "sec:orgd6bd315"
    "sec:org77f573c"
    "sec:org7a5f1c8"
    "sec:orgc8b4baa"
    "sec:org2ec4104"
    "sec:org5e331b9"
    "sec:org4216525"
    "sec:org0aefc87"
    "sec:orgc38faac"
    "sec:org1599827"
    "sec:orge56c6fd"
    "sec:org6f62c7d"
    "sec:org3e20e15"
    "sec:org1686ca2"
    "sec:org6f86828"
    "sec:org12779b1"
    "sec:orgca469b2"
    "sec:org2880b8e"
    "sec:org0fd120e"))
 :latex)

