# RMT: Quantifying heteroscedasticity

This repository contains the code, data, and additional analyses for the simulations reported in chapter 4 of the thesis Robust Methods in the Credibility Movement of Psychological Science. The associated OSF page can be accessed here: <https://osf.io/h3bwv/>

The repository is organised as follows:

-   `r_docs`

    -   `simulation_code.qmd` contains the code necessary for running the simulations reported in the paper, as well as supplementary simulations. Note that "Simulation 0" is the main reported simulation, while simulations 1 and 2 are additional analyses. This scripts exports files that are then used to create summaries and plots. Note that in the code, VP1 refers to homoscedastic pattern, VP2 is the butterfly pattern, VP3 is the inverse butterfly pattern and VP4 is the funnel pattern.

    -   `simulation_summaries.qmd` - contains code for reproducing plots and summaries as reported in the paper. The code reads data exported from `simulation_code.qmd` . Summaries from the paper are under simulation 1. Supplementary analyses are under simulation 1 and 2

-   `data`

    -   `simulation_exports` - a folder containing all the exports from `simulation_code.qmd` . Objects labelled as `sim_0_` relate to the simulation reported in the paper. Remaining files are for the supplementary analyses.

-   `scripts`

    -   `helpers.R` - small helper functions

    -   `quantile_loess.R` function to fit the quantile curves for the QLI method.
