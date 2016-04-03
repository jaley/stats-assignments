(TeX-add-style-hook
 "assignment"
 (lambda ()
   (add-to-list 'LaTeX-verbatim-environments-local "minted")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "minted"
    "graphicx"
    "appendix"
    "amsmath"
    "booktabs")
   (LaTeX-add-labels
    "fig:rainfall_10years"
    "tbl:model_comparison"
    "tbl:hmm_decoding"
    "tbl:hmm_state_2"
    "tbl:var_portmanteau"
    "apdx:rainfall_preliminary"
    "apdx:hmm_fitting"
    "apdx:hmm_decoding"
    "apdx:lat_var"
    "apdx:lat_arima"
    "apdx:lat_tfm")))

