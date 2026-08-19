# Submission checklist

## Resolved from the archive

- [x] Identify the FFRISP fieldwork organization, target population, sample stages, within-household selection, panel enrollment, and monthly follow-up.
- [x] Document the FFRISP recruitment consent workflow and incentives without claiming an unverified protocol number.
- [x] Reconstruct all 12 FFRISP completion counts and add them to the appendix pipeline.
- [x] Verify the deployed FFRISP and Luth instruments and document the Luth condition 8 error.
- [x] Audit the open-ended coding from the independent Anne and Jeremy codes. Do not use the workbook's undocumented reconciled score or reproduce verbatim responses.
- [x] Preserve the 2009 manuscript's author, affiliation, and correspondence record in the evidence ledger.
- [x] Map the framing literature through August 18, 2026, verify the core bibliographic records, and distinguish robust directional evidence from context-dependent effect magnitude and unresolved mechanisms.

## Required author records or decisions

- [ ] Recover the human-subjects protocol numbers, approval records, and approved consent language for both surveys. The FFRISP report documents consent procedures but not the protocol identifier; no Luth ethics record has been found.
- [ ] Resolve authorship. Every surviving 2009 draft names Jon Krosnick and Gaurav Sood, while the current draft also names John Protzko. Confirm order, affiliations, corresponding author, and CRediT contributions.
- [x] Recover, checksum, and source-reconstruct the four Protzko data files. The three study exports yield 4,585 valid first choices. The received anonymous combined file adds one blank row and changes one source-missing choice to zero.
- [ ] Recover the November 2019 Protzko manuscript, original analysis scripts, questionnaire, codebook, preregistration, and second OSF link. These are required to compare historical specifications and recover exact displayed wording and sampling procedures.
- [ ] Confirm the National Science Foundation award number and final acknowledgments. The FFRISP report establishes NSF funding but provides no award number.
- [ ] Recover a source tied specifically to the October 7 to October 14, 2009 Luth fielding that names the panel vendor, recruitment method, invitations, incentives, and dispositions. The old draft's December and January passages describe other fieldwork and cannot be imported.
- [ ] Recover documentation for the archived FFRISP month 12 weight. The report documents only the base weight and month 1 raking procedure.
- [x] Obtain authorization for release of respondent-level derived data. On August 18, 2026, author Gaurav Sood directed the public release of minimized derived respondent files with in-pipeline disclosure controls; the builds now emit only analysis-consumed columns with anonymized design codes, enforced by `tests/testthat/test-data-contracts.R`. The original exports remain restricted, and the repository history that previously exposed raw exports was replaced at the v0.1.0 release. Written confirmation from the co-authors remains advisable before journal submission.
- [ ] Choose a target journal and apply its article class, word limit, reporting checklist, anonymization rules, data policy, and reference style after the scientific content is approved.

## Final package checks

- [ ] Replace `Draft: \today` with a fixed manuscript date or remove it for anonymous review.
- [ ] Add journal-specific title page, declarations, funding statement, competing-interest statement, and author contribution statement.
- [x] Run `make check` and confirm `renv::status()` reports no problems in the project environment.
- [ ] Restore `renv.lock` into a separate empty library and rerun the pipeline there. The active project library has been repaired and passes all checks, but a genuinely clean-library build remains to be run.
- [x] Confirm every citation resolves, inspect the bibliography, and scan the TeX log for missing references, overfull boxes, and font substitutions.
- [x] Render every manuscript page and inspect table widths, figure legibility, float order, blank pages, and appendix cross-references.
- [ ] Create the journal upload set only after deciding whether source data, derived data, or code alone may be shared.
- [x] Keep the archive-wide meta-analysis in a separate module with an explicit scope boundary; do not describe it as a literature-wide systematic meta-analysis.
