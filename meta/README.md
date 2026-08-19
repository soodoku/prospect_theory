# Archive-wide meta-analysis

This module synthesizes every independent gain-versus-loss contrast recovered
from this repository. It is deliberately separate from the primary-study
analysis and from the broader literature review.

`scripts/11_archive_meta_analysis.R` reads generated effect files from the two
2009 wording studies and the three 2018 Protzko experiments. It writes its own
analysis data, tables, macros, and figure beneath `meta/`.

This is not a literature-wide systematic review. Published and unpublished
studies outside this archive require a separate search, screening, extraction,
and duplicate-coding workflow before they can enter a quantitative synthesis.
