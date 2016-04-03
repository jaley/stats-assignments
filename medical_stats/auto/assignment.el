(TeX-add-style-hook
 "assignment"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "listings"
    "hyperref")
   (LaTeX-add-bibitems
    "vandam"
    "prevalence"
    "cox"
    "github")))

