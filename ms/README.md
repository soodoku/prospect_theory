# Manuscript build

Run `make paper` from the repository root. The target rebuilds all analysis outputs and then compiles `main.tex` with XeLaTeX, Biber, and `latexmk`.

The manuscript imports generated estimates from `tabs/` and the main figure from `figs/`. Do not edit generated tables or macros by hand.
