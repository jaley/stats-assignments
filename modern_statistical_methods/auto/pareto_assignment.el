(TeX-add-style-hook
 "pareto_assignment"
 (lambda ()
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "amsmath"
    "amssymb"
    "minted"
    "graphicx")
   (LaTeX-add-labels
    "fig:q1c_histogram"
    "fig:q2_histogram"
    "fig:q3_premiums"
    "fig:q3_claims"
    "fig:q4_customers")))

