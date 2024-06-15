# Climate Actions by Scientists
This repository contains code to reproduce all analyses and figures for the paper Dablander, F., Sachisthal, M., & Haslbeck, J.M.B. (2024). Going Beyond Research: Climate Actions by Climate and Non-Climate Researchers.

- `helpers.R` includes useful functions.
- `analyses/`
    - `climate_action_analyses.Rmd` runs the analyses reported in the paper.
- `data/`
    - `DataS1_Anonymized.RDS` is the raw anonymized data.
    - `DataS2_Imputed.RDS` is the imputed data.
    - `DataS3_Final.RDS` is the final data set used for quantitative analysis.
    - `Codebook_DataS1_Quantitative.xlx` is the codebook for the quantitative data (`DataS1_Anonymized`).
    - `comparisons.csv` has the average (adjusted) differences between climate and non-climate researcers.
    - `comparisons.csv` includes the average (adjusted) differences in climate actions between climate and non-climate researcers.
    - `comparisons_binom.csv` includes the average (adjusted) differences in the number of climate actions between climate and non-climate researcers.
- `models/` is where the estimated models are being saved to.
- `figures/` is where the figures are being saved to.