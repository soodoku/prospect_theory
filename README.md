# Description Invariance in Risky-Choice Framing

Replication materials, public data, and manuscript for an archival analysis of the Asian Disease Problem across five randomized survey experiments: a national area-probability panel (FFRISP, 2009), a volunteer panel that fielded nine versions of the problem (Luth, 2009), and three factorial experiments that ran an identical instrument (Stanford discovery, Stanford replication, and UCSB replication, 2018). Together they contribute 7,249 respondents and 23 independent gain-loss contrasts.

The direction of the framing effect is a stable fact; its size is not. All 23 contrasts are positive, averaging 24.7 percentage points, enough to move roughly one respondent in four. The size tracks what the option descriptions leave unstated. Relative to each survey's original wording, fully stating the certain option removed more than half of the effect (18.1 points across the four surveys that fielded it), while reversing the order of the risky option's outcomes changed essentially nothing (1.3 points across all five surveys). The framing contrast also grows modestly with education (4.0 points per category, exploratory). Results are organized by manipulation, with the surveys as replications; every condition label, manipulation family, and design-factor code comes from one registry (`scripts/00_config.R`) that every exhibit and test consumes.

## Data

The three respondent-level Parquet files under `data/` are public and committed to this repository. They are minimized at build: each carries only the columns the analysis consumes (treatment assignment, outcome, condition registry, validity and attention flags, education, political knowledge, and the FFRISP survey weight with anonymized stratum and PSU ranks). Age, gender, race and ethnicity, region, response durations, raw source codes, and all identifiers stay in the restricted sources. `case_id` is assigned sequentially after a deterministic sort of each source export, so rebuilding from the sources reproduces the same identifiers, but nothing in the released files links a row to a panel identifier. `data/data-dictionary.md` documents the schema and the minimization policy, and `tests/testthat/test-data-contracts.R` fails the build if a restricted column reappears.

The raw survey exports contain panel identifiers and open text and are not distributed; each `data/*/source/manifest.csv` records their names and SHA-256 checksums so an authorized copy can be verified. The open-ended coding workbook is likewise restricted; its aggregate reliability outputs are committed.

## Reproduce

The R environment is pinned with `renv`. From a clone:

```sh
R -q -e 'renv::restore()'
Rscript scripts/99_run_all.R
```

Without the restricted sources, the runner skips the build and audit stages with a message and reruns every analysis, synthesis, and exhibit stage from the committed respondent files. With authorized sources placed at the paths in the manifests, the same command rebuilds everything from the raw exports, including the source-integrity checks and the Qualtrics block-label verification of the 2018 treatment assignment.

`make check` runs the pipeline, the test suite, the linter, and compiles `ms/main.pdf` with XeLaTeX and `latexmk`. Individual targets: `make analysis`, `make test`, `make lint`, `make paper`. Every number in the manuscript is a generated macro; no table, figure, or headline value is hand-edited.

## Repository layout

```text
data/             Public respondent Parquet files, data contracts, source manifests
data/wording/     Curated treatment text for every arm, graded by evidentiary status
figs/             Generated figures
lit/              Bibliography and literature evidence map
logs/             Deterministic build logs
meta/             Archive-wide and manipulation-effect syntheses
ms/               Manuscript source and compiled PDF
scripts/          Ordered R pipeline (scripts/README.md maps stages to outputs)
tabs/             Generated estimates, LaTeX tables, and manuscript macros
tests/            Data-contract, fixed-target, and taxonomy tests
```

The retrospective specification is `design.md`, and `submission-checklist.md` lists the items that still need author records before journal submission. The authors additionally maintain private evidence and claim ledgers tying each manuscript claim to its surviving sources, estimand, script, and exhibit.

## License and citation

Code is under the MIT License (`LICENSE`); released data, documentation, and manuscript text are under CC-BY 4.0 (`LICENSE-DATA`). Please cite:

> Protzko, John, Gaurav Sood, and Jon A. Krosnick. "Description Invariance in Risky-Choice Framing: Evidence from Five Survey Experiments." Working paper and replication materials, https://github.com/soodoku/prospect_theory.
