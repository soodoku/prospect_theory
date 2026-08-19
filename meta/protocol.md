# Protocol: archive-wide synthesis

## Question and unit of analysis

The synthesis estimates the average gain-frame minus loss-frame difference in
the proportion choosing the certain option across every valid, independent
randomized contrast recovered in this archive.

The unit of analysis is a gain-loss contrast. The FFRISP and Luth wording cells
use disjoint respondents. Each Protzko contrast uses two disjoint cells within a
study and a fixed combination of internal-flip and explicit-balancing factors.
The resulting sampling errors are therefore treated as independent. Shared
protocols may still generate heterogeneity in true effects.

## Eligibility

- Binary choice between the certain and risky options.
- Random assignment to extensionally matched gain and loss descriptions.
- Recoverable cell-level observations from an immutable source file.
- A valid deployed instrument and a nonmissing choice.

The defective Luth natural-frequency pair is excluded. The Protzko analysis
uses the three study-level exports, maps `-99` to missing, and does not import
the one missing-to-zero mutation in the received combined file.

## Effect and model

The effect is the risk difference: certain-choice proportion in the gain frame
minus the corresponding proportion in the loss frame. Each standard error is
the HC2 standard error from a linear probability model. The primary synthesis
is a random-effects model fitted by REML with Knapp--Hartung inference. A
fixed-effect model is a sensitivity analysis. The report includes the pooled
estimate, confidence interval, prediction interval, tau-squared, I-squared,
and Cochran's Q.

No funnel-asymmetry or publication-bias adjustment is planned. These contrasts
form a designed archival collection rather than a set selected through journal
publication, and publication-bias diagnostics would answer the wrong question.

## Scope boundary

The pooled estimate describes this archive, not all Asian Disease Problem
experiments. A literature-wide meta-analysis must have a dated reproducible
search, deduplication, title/abstract and full-text decisions, duplicate outcome
extraction, an effect-dependence plan, and a public exclusion ledger.
