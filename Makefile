.PHONY: all analysis test lint paper check clean

all: check

analysis:
	Rscript scripts/99_run_all.R

test: analysis
	Rscript tests/testthat.R

lint:
	Rscript -e 'lintr::lint_dir("scripts"); lintr::lint_dir("tests")'

paper: analysis
	latexmk -cd -xelatex -interaction=nonstopmode -halt-on-error ms/main.tex

check: test lint paper

clean:
	latexmk -cd -C ms/main.tex
