# Join contracts

## FFRISP survey to profile

- Left: 898 completed survey rows with numeric panel identifiers.
- Right: profile rows with nonmissing `abtsrbiid`; the key is unique.
- Cardinality: many-to-one, enforced by `dplyr`.
- Expected and actual output: 898 rows.
- Match: 898 of 898. Month 12 weights match 896 of 898.

The two missing monthly weights remain in the unweighted randomized comparison and are excluded only from weighted sensitivity estimates.

## Luth experiment to demographic survey

- Left: 2,330 completed experiment rows; `uniq` is unique.
- Right: 2,065 demographic rows; `identifier` is unique.
- Cardinality: one-to-one, enforced as many-to-one from the experiment.
- Expected and actual output: 2,330 rows.
- Match: 2,065 of 2,330 overall; 1,792 of 2,017 (88.8%) among attention-check passers.

The experiment remains the left table because the framing estimand describes randomized experiment respondents. Demographic nonmatches remain in the main framing analysis and are absent from moderator analyses.

## Luth experiment to political survey

- Left: the same 2,330 experiment rows.
- Right: 2,327 political-survey rows; `identifier` is unique.
- Cardinality: one-to-one, enforced as many-to-one from the experiment.
- Expected and actual output: 2,330 rows.
- Match: 2,327 of 2,330 overall. Among attention-check passers, 2,014 of 2,017 match and 1,795 completed the political survey.

Political knowledge is defined only for the 1,795 completed political-survey matches. A matched dropout is not recoded as a zero score.
