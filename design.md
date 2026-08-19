# Retrospective analysis specification

This document records the estimands and decision rules used by the rebuilt pipeline. It is not a preregistration. The surveys were fielded in 2009 and 2018, and earlier scripts, outputs, and the received 2018 combined file had already been inspected when this specification was written. Confirmatory language is therefore inappropriate. The randomized treatment contrasts remain causally identified under the assumptions below, but analysis choices should be read as retrospective.

## Research questions

1. Does the original risky-choice framing contrast reproduce in a probability-based online panel and in a screened volunteer panel?
2. How much does the gain-minus-loss contrast change under small changes to option wording, outcome order, response order, and scenario detail?
3. As exploratory analyses, do education or political knowledge predict variation in the framing contrast?
4. In the three 2018 experiments, does reversing the internal outcome order or explicitly balancing the option descriptions change the framing contrast?
5. What average and between-contrast heterogeneity summarize all independent gain-loss comparisons recovered in this archive?

## Estimands

| Component | Definition |
|---|---|
| Unit | One survey respondent |
| Treatment | Random assignment to a gain or loss frame within a matched wording pair |
| Outcome | Indicator for choosing the certain option |
| Contrast | Gain-frame mean minus loss-frame mean, reported in percentage points |
| Primary populations | Completed FFRISP probability-panel respondents with valid panel identifiers; completed Luth volunteer-panel respondents who passed the pre-treatment attention check |
| Analysis time | Surveys fielded in 2009 |
| Main estimator | Unweighted difference in means, implemented as a linear probability model with HC2 standard errors |
| Uncertainty | Two-sided 95 percent confidence interval |

The randomized contrast identifies the average effect of assignment among observed respondents if assignment was implemented as recorded, one respondent's assignment did not affect another respondent's choice, and outcome observation was not differentially affected by frame. Generalizing an effect magnitude beyond each analyzed sample requires stronger assumptions and is kept separate from internal validity.

## Inclusion and exclusion rules

- FFRISP retains completed records with numeric panel identifiers. Four test or nonpanel identifiers are excluded. Item nonresponse is excluded pair by pair.
- Luth retains every completed record in the canonical data file. The primary analysis requires the requested pre-treatment attention-check response (`qcheck = 5`). A sensitivity analysis includes all completed respondents.
- Luth conditions 7 and 8 are excluded as a pair. In the deployed questionnaire, the condition 8 stem names Programs C and D while its response choices name Programs A and B. The later corrected questionnaire does not establish what respondents in the deployed survey saw.
- Demographic and political-survey matches are not required for the main randomized comparisons. Each moderator model uses its available-case denominator.

## Main analysis family

The main exhibit reports three FFRISP wording-pair contrasts and eight valid Luth wording-pair contrasts. The original wording is highlighted as a direct reproduction. The remaining variants probe robustness of the effect magnitude. Claims rely on effect estimates and intervals rather than a count of individually significant results.

Within each sample, a robust joint test of all frame-by-wording interaction terms assesses whether the contrast is constant across wordings. This omnibus test is the inferential check for wording heterogeneity.

## Sensitivity analyses

- Re-estimate the FFRISP original-wording contrast with month 12 survey weights and the documented stratified, clustered sample design. Two respondents without weights are excluded only from weighted analyses.
- Re-estimate the Luth original-wording contrast among all completed respondents, without the attention-check screen.
- Report sample flow and all outcome missingness. The six missing Luth outcomes among attention-check passers occur only in the already invalid wording pair.

## Protzko experiments

The 2018 source archive contains two Stanford exports and one UCSB export. The Stanford surveys record two frame questions per respondent. The analysis uses only the first response, matching the construction of the received anonymous combined file and preserving a between-subject comparison uncontaminated by the second question. The UCSB export contributes its single recorded choice.

The three recorded factors are gain versus loss frame, original versus internally flipped ordering, and absence versus presence of explicit balancing. The outcome is choosing the certain option. Source code `-99` is missing, never a risky choice. Four of 4,589 source respondents have a missing first choice, leaving 4,585 complete cases.

The primary factorial model includes study indicators and all interactions among frame, internal flip, and explicit balancing, with HC2 standard errors. Study-specific gain-minus-loss effects are also reported for each of the four design combinations. The received four-variable combined file is a reconstruction target only: it appends one blank row and changes one source-missing choice to zero.

## Archive-wide synthesis

The separate `meta/` module pools the 11 valid 2009 wording contrasts and 12 Protzko factorial contrasts as risk differences. The primary model is random effects by REML with Knapp--Hartung inference; a fixed-effect model is a sensitivity analysis. The synthesis reports a confidence interval for the mean, a prediction interval, tau-squared, I-squared, and Cochran's Q. It describes this archive and is not a literature-wide meta-analysis.

## Exploratory moderation

Education is coded as an ordered four-category measure in both samples. Political knowledge is the proportion correct on five contemporaneous factual items in Luth. Political knowledge is not an intelligence measure.

Moderator models pool valid wording pairs and include wording indicators, all wording-by-frame interactions, the moderator main effect, and one moderator-by-frame interaction. Positive reported interactions mean that the gain-minus-loss contrast increases at higher moderator values. Raw and Holm-adjusted p-values are reported for the three moderator tests. These analyses are descriptive and exploratory because the specification is retrospective, moderators have missing data, and the political-knowledge measure comes from a separate survey.

## Stopping and interpretation rules

- Do not restore the invalid Luth pair to obtain an additional contrast.
- Do not call political knowledge intelligence or cognitive ability.
- Do not interpret a nonsignificant difference between estimates as proof of equality.
- Distinguish evidence that a framing contrast exists from evidence about its size across wordings or samples.
- Use the open-ended material only for an appendix reliability audit. Do not use the undocumented reconciled score, reproduce response text, recode the experimental outcome, or infer a psychological mechanism from post-choice explanations.
