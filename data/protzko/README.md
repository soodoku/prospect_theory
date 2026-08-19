# Protzko data module

The complete authorized local replication package consists of the four
immutable Stata files in `source/`, the tracked source manifest, and pipeline
stages 08 through 11. Running `make analysis` verifies the hashes, profiles the
sources without exporting identifiers, builds the deidentified respondent
file, estimates the factorial contrasts, and updates the separate archive-wide
synthesis.

The original study exports are not safe for an unrestricted public Git
repository. They contain IP addresses, response and panel identifiers, ZIP
codes, user-agent strings, and open-text survey metadata. They remain ignored
by Git but are present in the authorized local package. A private or controlled
repository may ship them with the transformation scripts if the consent,
license, and institutional disclosure rules permit it.

The received `study2prospecttheorycombinedanon.dta` file is preserved as source
evidence, not used as the canonical analysis input. The build reconstructs it
from the three study-level exports and records the one blank row and one
missing-to-zero difference.
