
# respeciate <img src="man/figures/logo.png" align="right" alt="" width="220" />

[![R build
status](https://github.com/atmoschem/respeciate/workflows/R-CMD-check/badge.svg)](https://github.com/atmoschem/respeciate/actions)

respeciate gives you access to air pollutant emissions profiles in the
[US/EPA Speciate
v5.4](https://www.epa.gov/air-emissions-modeling/speciate) and [EU/JRC
SPECIEUROPE v3.0](https://source-apportionment.jrc.ec.europa.eu/)
archives via R.

The installation is:

``` r
remotes::install_github("atmoschem/respeciate")
```

The currently packaged SPECIATE and SPECIEUROPE archives are:

``` r
library(respeciate)
# packaged archives
rsp_info()
#> respeciate: 0.4.0
#> source: SPECIATE 5.4
#>  [in respeciate since 0.4.0]
#>  Profiles: 6897; species: 3115
#> source: SPECIEUROPE 2.0
#>  [in respeciate since 0.3.1]
#>  Profiles: 285; species: 231
```

## Example

Searching the respeciate (SPECIATE + SPECIEUROPE) for a profile,
e.g. using a keyword:

``` r
rsp_find_profile("cement")
#> respeciate profile list: 82
#> [NO SPECIES]
#>   (CODE US:2720110) Cement Kiln (Gas-Fired)
#>   (CODE US:272012.5) Cement Kiln (Gas-Fired)
#>   (CODE US:2720130) Cement Kiln (Gas-Fired)
#>   (CODE US:27201C) Cement Kiln (Gas-Fired)
#>   (CODE US:2720310) Cement Kiln (Coal-Fired)
#>   (CODE US:272032.5) Cement Kiln (Coal-Fired)
#>     > showing 6 of 82
```

Limiting the search to just SPECIEUROPE:

``` r
rsp_find_profile("cement", source="eu")
#> respeciate profile list: 11
#> [NO SPECIES]
#>   (CODE EU:1) Cement
#>   (CODE EU:32) Cement kiln (coal fired)
#>   (CODE EU:71) Cement production dust
#>   (CODE EU:72) Cement production dust
#>   (CODE EU:73) Cement production dust
#>   (CODE EU:126) Cement kiln
#>     > showing 6 of 11
```

Getting the first profile in SPECIEUROPE:

``` r
prf <- rsp(1, source="eu")
prf
#> respeciate: count 1
#>   EU:1 (38 species) Cement
plot(prf)
```

![](man/figures/get-1.png)<!-- -->

Comparing that profile with pm profiles in (US EPA) SPECIATE:

``` r
rsp_match_profile(prf, rsp_us_pm(),  
                  output = "plot,summary", 
                  layout=c(5,2))
```

![](man/figures/output.options-1.png)<!-- -->

    #>    .profile.id                            .profile  n         pd        srd
    #> 1      US:4377                         Cement Kiln 28 0.29773918 0.05254516
    #> 2     US:91004 Draft Cement Production - Composite 28 0.17778801 0.06090050
    #> 3      US:4378                         Cement Kiln 28 0.36395399 0.06309018
    #> 4      US:4332                         Cement Kiln 28 0.23621184 0.06555357
    #> 5      US:4325                         Cement Kiln 27 0.31684644 0.07586627
    #> 6      US:4365                  Vegetative Burning 25 0.46094875 0.06733352
    #> 7      US:4348                   Unpaved Road Dust 26 0.08510414 0.07863248
    #> 8      US:4376                         Cement Kiln 28 0.39187984 0.07772304
    #> 9      US:4205                     Paved Road Dust 24 0.11408921 0.08893236
    #> 10     US:4327                         Cement Kiln 25 0.49511342 0.09367184
    #>          sid   nearness
    #> 1  0.2630862 0.01382390
    #> 2  0.2357942 0.01435999
    #> 3  0.2464707 0.01554988
    #> 4  0.2600407 0.01704660
    #> 5  0.2470702 0.01874429
    #> 6  0.3046534 0.02051339
    #> 7  0.2639644 0.02075617
    #> 8  0.2728634 0.02120777
    #> 9  0.2633172 0.02341742
    #> 10 0.2675759 0.02506432

Notes:

- The nearest match to the SPECIEUROPE EU:1 profile Cement from the US
  EPA SPECIATE PM subset is SPECIATE US:4377 Cement Kiln.
- In addition, 6/9 of the other nearest matches are cement-related
  sources.  
- The nearness metrics, pd (Pearson’s Distance), srd (Spearman Ranked
  Distance) and sid (Standardized Identity Distance), all tend to zero
  for better matches. See ?rsp_match_profile in the packaged respeciate
  documentation for details and references.
