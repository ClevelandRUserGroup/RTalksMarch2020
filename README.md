# R Lighting Talks for March 2020 Meetings
[Cleveland R User Group](http://meetu.ps/c/q7Xn/16P01/a)

1. Pick a data set from below
2. Do something interesting with it:
  * create some visualizations that tell a story about the data
  * do an exploratory analysis of the data
  * build a predictive model (machine learning)

## Data Source 1
### RStudio Learning Survey Data
[github.com/rstudio/learning-r-survey](https://github.com/rstudio/learning-r-survey)

## Data Source 2
### Kaggle Housing Prices practice competition
[kaggle.com/c/house-prices-advanced-regression-techniques](https://www.kaggle.com/c/house-prices-advanced-regression-techniques)

## Contribution Guide

The lighting talks will all be presented from one computer to reduce the
transition time between talks. Crucially, no code will be executed for the
presentation. In other words, in addition to your source code (e.g. R or Rmd
file(s)), you will need to submit the finished product to display
(recommendations below).

If you are comfortable with Git and GitHub, please submit a Pull Request to this
repository with your contribution. Commit all your contributions to a directory
titled with your last name followed by your first name, e.g.
`LastnameFirstname`. Please do not let the complexity of Git/GitHub discourage
you from contributing a lightning talk. Feel free to email
[Tim](mailto:tim@hoolihan.net) with your contribution, and he will add it to the
repository.

Below are recommendations for what to submit based on the output of your
analysis:

1. **Plot(s)** - Submit the plot in a web-friendly format such as PNG or JPEG.
   From the RStudio plots pane, you can click `Export`->`Save as image...` to
   export a PNG file. Alternatively you can use `png()`, `jpeg()`, or `ggsave()`
   directly in R. Also include the R script(s) you used to generate the plots.

1. **Reproducible Report** - If you used knitr/rmarkdown to generate a
   reproducible report of your analysis, submit the R Markdown source file and
   also a Markdown version of the report. Markdown is preferred because GitHub
   will automatically display the Markdown; whereas, it doesn't do this for HTML
   and other formats. For best results, use the output format
   `github_document()`.

1. **Shiny App** - If you develop a Shiny app, you will need to deploy it
   yourself, e.g. at [shinyapps.io](https://www.shinyapps.io/). Submit the R
   files you used to create the app as well as a README file with the URL to
   your deployed app.

## Questions?
[Contact Tim Hoolihan](mailto:tim@hoolihan.net)
