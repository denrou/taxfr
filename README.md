
<!-- README.md is generated from README.Rmd. Please edit that file -->

# taxfr

The goal of taxfr is to provide a dataset of French taxes and revenus in
a tidy format.

## Installation

You can install the released version of taxfr from
[git](https://github.com/denrou/taxfr) with:

``` r
remotes::install_github("denrou/taxfr")
```

## Dataset

The dataset can be imported with:

``` r
data(tax)
tibble::glimpse(tax)
#> Observations: 2,794,890
#> Variables: 9
#> $ code_insee                                      <chr> NA, NA, NA, NA, …
#> $ departement                                     <chr> "ain", "ain", "a…
#> $ revenu_fiscal_de_reference_par_tranche_en_euros <ord> 0 à 10 000, 10 0…
#> $ impot_net_total                                 <dbl> -7980194, -47552…
#> $ annee                                           <dbl> 2011, 2011, 2011…
#> $ annee_revenu                                    <dbl> 2010, 2010, 2010…
#> $ type                                            <ord> Foyers fiscaux, …
#> $ nombre                                          <dbl> 68498, 19941, 33…
#> $ revenu                                          <dbl> 330764869, 21970…
```

## Source

Raw data can be found on the French Government website:
<https://www.impots.gouv.fr/portail/statistiques> under `Base de
données` -\> `Impôt sur le revenu par commune (IRCOM)`
