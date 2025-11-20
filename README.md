
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
#> respeciate: 0.4.1
#> source: SPECIATE 5.4
#>  [in respeciate since 0.4.0]
#>  Profiles: 6897; species: 3115
#> source: SPECIEUROPE 3.0
#>  [in respeciate since 0.4.1]
#>  Profiles: 425; species: 232
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
#>   (CODE EU:126) Cement kiln
#>   (CODE EU:127) Cement kiln
#>   (CODE EU:130) Cement mill
#>   (CODE EU:131) Cement mill
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
    #> 1      US:4323                         Cement Kiln 24 0.17743824 0.04434783
    #> 2      US:4325                         Cement Kiln 26 0.28711775 0.05282953
    #> 3     US:91004 Draft Cement Production - Composite 27 0.16877615 0.05754845
    #> 4      US:4378                         Cement Kiln 27 0.35292141 0.05999083
    #> 5      US:4377                         Cement Kiln 27 0.30124903 0.05677656
    #> 6      US:4376                         Cement Kiln 27 0.36702881 0.05738706
    #> 7      US:4327                         Cement Kiln 24 0.47355560 0.06327460
    #> 8      US:4332                         Cement Kiln 27 0.18886917 0.07067622
    #> 9      US:4232                          Local Soil 24 0.42616717 0.06956522
    #> 10     US:4348                   Unpaved Road Dust 26 0.08510414 0.07863248
    #>          sid   nearness
    #> 1  0.2473368 0.01096885
    #> 2  0.2236303 0.01181428
    #> 3  0.2201589 0.01266980
    #> 4  0.2305776 0.01383254
    #> 5  0.2540946 0.01442662
    #> 6  0.2523656 0.01448252
    #> 7  0.2441244 0.01544687
    #> 8  0.2640956 0.01866528
    #> 9  0.2980568 0.02073439
    #> 10 0.2639644 0.02075617

Notes:

- The nearest match to the SPECIEUROPE EU:1 profile Cement from the US
  EPA SPECIATE PM subset is SPECIATE US:4377 Cement Kiln.
- In addition, 6/9 of the other nearest matches are cement-related
  sources.  
- The nearness metrics, pd (Pearson’s Distance), srd (Spearman Ranked
  Distance) and sid (Standardized Identity Distance), all tend to zero
  for better matches. See ?rsp_match_profile in the packaged respeciate
  documentation for details and references.
