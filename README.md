## IMD19
Interactive map showing the Indices of Deprivation 2019 (and 2015) for Trafford.

<img src="screenshot.png" width="700">

The app can be viewed at <a href="https://trafforddatalab.shinyapps.io/imd19" target="_blank">https://trafforddatalab.shinyapps.io/imd19</a>. To run the app locally execute the following code:

``` r
shiny::runGitHub("trafforddatalab/imd19")
```

**NB** The application is scalable to other local authorities. Just swap out the local authority name in the [pre-processing script](data/pre-processing.R).
