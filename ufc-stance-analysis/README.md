# UFC Stance Analysis

A statistical analysis of UFC fighters exploring whether fighting stance influences competitive success. The short answer: it does not. The longer answer is more interesting.

The analysis runs three linear regression models with increasing complexity, a t-test comparing Orthodox and Southpaw win percentages, and a set of visualizations covering performance metrics by stance. The conclusion is that stance explains less than 1% of variance in wins. What actually predicts success is defense. Strike defense and takedown defense are consistently the strongest predictors across all models.

Built in R using tidyverse, ggplot2, and plotly. Full analysis available as an interactive HTML report.

Files

- `UFC-analysis.Rmd` — Source code and analysis
- `UFC-analysis.html` — Rendered output

## View the Analysis

Download UFC-analysis.html and open it in your browser, or knit it locally:

```r
rmarkdown::render("UFC-analysis.Rmd")
```

Santiago Freile · May 2025
