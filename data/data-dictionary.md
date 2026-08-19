# Analysis data dictionary

## Released files and minimization

The three respondent Parquet files are public and committed to the
repository. They are minimized at build: each carries only the columns the
analysis consumes, so age, gender, race and ethnicity, region, Hispanic
origin, response durations, raw source codes, and source row positions never
leave the restricted `source/` directories. `case_id` is assigned
sequentially after a deterministic sort of each source export, so rebuilding
from the sources reproduces the same identifiers, but nothing in the released
files links a row back to a panel identifier; the residual information in a
`case_id` is the row's position in the source export, which roughly follows
response order. The FFRISP `stratum` and `psu` columns relabel the archived
design codes by stratum size with ties broken by first appearance in the
file. Both ordering quantities are derivable from the released file itself,
so the labels add no information about the archived codes and do not preserve
their ordering, while the clustering structure the weighted sensitivity
analysis needs is intact.
`tests/testthat/test-data-contracts.R` fails the build if any restricted
column reappears in a released file.

## Registry columns shared by all respondent files

Every respondent file also carries the arm-registry columns joined from
`scripts/00_config.R` by sample and wording pair or design cell: `family`,
`family_label`, `variant`, `effect_id`, `wording` (canonical condition label),
`internal_flip_code`, `explicit_balance_code`, `response_order_code`,
`certain_survivors`, and `display_order`. The codes classify each arm on the
cross-survey design factors; `data/recode-ledger.md` records the definitions
and `tests/testthat/test-taxonomy.R` guards the assignments. Generated data
artifacts in `tabs/` and `meta/` are Parquet files, not CSV.

## Open questions

- Two FFRISP respondents have no month 12 survey weight. The unweighted randomized comparison keeps them when their framing outcome is observed; the weighted descriptive sensitivity excludes them. The archived methods report documents base weights and month 1 raking but not the provenance or construction of the month 12 variable.
- The Luth recruitment disposition and the reason 225 experiment respondents lack the demographic survey are not documented in the available files. Moderator analyses therefore report their own denominators and a missingness sensitivity.
- The Luth political survey contains a five-item political-knowledge battery. It is not an intelligence test and is never labeled as intelligence in the analysis or manuscript.

## FFRISP respondent file

One row is one completed FFRISP survey respondent with a numeric panel identifier that links uniquely to the profile file. The source has 902 completed records; four non-FFRISP or test identifiers are excluded, leaving 898 rows.

| name | source | type | unit and universe | values or transformation | missing |
|---|---|---|---|---|---|
| `case_id` | generated | character | respondent; all analysis rows | deidentified sequential key | 0 |
| `sample` | generated | character | respondent; all rows | `FFRISP probability panel` | 0 |
| `condition` | survey `Qktcomp` | integer | respondent; all rows | randomized condition 1 to 6 | 0 |
| `wording_id` | `condition` | integer | respondent; all rows | paired conditions 1 to 3 | 0 |
| `wording` | questionnaire | character | respondent; all rows | original, risky-option order reversed, expanded drug wording | 0 |
| `frame` | `condition` | character | respondent; all rows | odd conditions are gain; even conditions are loss | 0 |
| `loss_frame` | `frame` | integer | respondent; all rows | 1 loss, 0 gain | 0 |
| `certain_choice` | displayed `qkt1` to `qkt6` item | integer | respondent; assigned condition | 1 if Program or Drug A, 0 if B | 11 item nonresponses |
| `instrument_valid` | questionnaire audit | logical | respondent; all rows | true for all six conditions | 0 |
| `profile_matched` | profile join | logical | respondent; all rows | all 898 match | 0 |
| `survey_weight` | weights `w12_finalweights` | double | month 12 respondent | normalized month-specific survey weight | 2 item missings |
| `stratum` | profile `vstratum` | integer | profiled respondent | strata relabeled by size then first appearance; preserves clustering, not the archived code or its order | 0 |
| `psu` | profile `vpsu` | integer | profiled respondent | primary sampling units relabeled by size then first appearance within stratum | 0 |
| `education` | profile `ppeduc` | character | profiled respondent | less than high school; high school; some college or associate; bachelor's or higher | 0 |

## Luth respondent file

One row is one completed Luth experiment respondent. The file retains all 2,330 completed records; 2,017 pass the pre-treatment attention check (`qcheck = 5`). Primary analyses apply that screen explicitly rather than deleting failed records during construction.

| name | source | type | unit and universe | values or transformation | missing |
|---|---|---|---|---|---|
| `case_id` | generated | character | respondent; all completed rows | deidentified sequential key | 0 |
| `sample` | generated | character | respondent; all rows | `Luth volunteer panel` | 0 |
| `attention_pass` | survey `qcheck` | logical | respondent; all rows | true for the requested response, code 5 | 0 |
| `condition` | survey `qra` | integer | respondent; all rows | randomized condition 1 to 18 | 0 |
| `wording_id` | `condition` | integer | respondent; all rows | paired conditions 1 to 9 | 0 |
| `wording` | deployed questionnaire | character | respondent; all rows | wording manipulation assigned | 0 |
| `frame` | `condition` | character | respondent; all rows | odd conditions are gain; even conditions are loss | 0 |
| `loss_frame` | `frame` | integer | respondent; all rows | 1 loss, 0 gain | 0 |
| `certain_choice` | displayed `q1a` to `q18a` item | integer | respondent; assigned condition | corrects for reversed response order in conditions 9, 10, 17, and 18 | 9 overall; 6 among attention-check passers, all in the invalid pair |
| `instrument_valid` | deployed questionnaire audit | logical | respondent; all rows | false for conditions 7 and 8 because condition 8 names C and D in the stem but A and B in the answers | 0 |
| `demographic_matched` | demographic join | logical | respondent; all rows | 2,065 overall; 1,792 of 2,017 attention-check passers | 0 |
| `knowledge_matched` | political-survey join | logical | respondent; all rows | 2,327 overall; 2,014 of 2,017 attention-check passers | 0 |
| `education` | demographic `qd11` | character | demographic matches | less than high school; high school; some college or associate; bachelor's or higher | 276 overall; 234 among attention-check passers |
| `political_knowledge` | political survey `qk1` to `qk5` | double | respondents who completed that survey | mean of five scored items, 0 to 1 | 261 overall; 222 among attention-check passers |
| `knowledge_survey_completed` | political survey status | logical | all experiment respondents | true for 2,069 overall and 1,795 attention-check passers | 0 |

## Protzko respondent file

One row is one source respondent's first framing choice. The two Stanford exports contribute 3,089 rows and the UCSB export contributes 1,500, for 4,589 rows. Four first choices are missing, leaving 4,585 analysis rows. No direct source identifier, IP address, ZIP code, user agent, or open-text field is exported.

| name | source | type | unit and universe | values or transformation | missing |
|---|---|---|---|---|---|
| `case_id` | generated | character | respondent; all rows | study-prefixed sequential key | 0 |
| `study` | source filename | character | respondent; all rows | Stanford discovery; Stanford replication; UCSB replication | 0 |
| `certain_choice` | Stanford `certain1st`; UCSB `certain` | integer | respondent's first assigned item | 1 certain, 0 risky; `-99` mapped to missing | 4 |
| `gain_frame` | Stanford `gainframe1st`; UCSB `gainframe` | integer | respondent; all rows | 1 gain, 0 loss | 0 |
| `loss_frame` | `gain_frame` | integer | respondent; all rows | `1 - gain_frame` | 0 |
| `internal_flip` | `flip` | integer | respondent; all rows | 1 internal flip, 0 original internal order | 0 |
| `explicit_balance` | `explicit` | integer | respondent; all rows | 1 explicit balancing, 0 no explicit balancing | 0 |
| `analysis_complete` | `certain_choice` | logical | respondent; all rows | true when the first choice is observed | 0 |
| `education_index` | source `education` | integer | respondent; all rows | four ordered categories, 0 to 3, aligned with the 2009 education coding | source item nonresponse |
