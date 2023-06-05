# frankmakrdiss #

<!-- badges: start -->
[![R-CMD-check](https://github.com/frankmakr/frankmakrdiss/actions/workflows/r_check_standard.yml/badge.svg)](https://github.com/frankmakr/frankmakrdiss/actions/workflows/r_check_standard.yml)
<!-- badges: end -->

The *R* package *frankmakrdiss* supplements the doctoral dissertation:

Krumbholz, F. (2023).
*Psychological Sense of Community:*
*Inhaltsvalidität von Testverfahren in Abhängigkeit von Bezugsgemeinschaften.*
[Psychological sense of community:
Content validity of tests in dependence of community contexts]
[Unpublished Doctoral Dissertation].
FernUniversität in Hagen.

<details>

<summary>BibTeX entry</summary>

``` bibtex
# BibTeX
@unpublished{Krumbholz2023,
    author = {Krumbholz, F.},
    date = {2023},
    origtitle = {Psychological Sense of Community:,
        Inhaltsvalidität von Testverfahren
        in Abhängigkeit von Bezugsgemeinschaften},
    title = {Psychological Sense of Community},
    subtitle = {Content validity of tests
        in dependence of community contexts},
    titleaddon = {Unpublished doctoral dissertation},
    institution = {FernUniversität in Hagen},
    langid = {ngerman}
}
```

</details>

It provides the [data set](https://doi.org/zenodo.xxx)
and a set of functions
for an easier reproduction, replication, and adaptation
of the implemented analyses.



## Installation ##

Before installing *frankmakrdiss* you need *cmdstanr* and *CmdStan*.  
Please visit
<https://mc-stan.org/cmdstanr>
for detailed instructions.

``` r
# The cmdstanr dev team recommends running this in a fresh R session
# or restarting your current session

# Install cmdstanr
install.packages("cmdstanr",
  repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Install CmdStan
cmdstanr::install_cmdstan()
```

To install *frankmakrdiss* you can use
[*devtools*](https://devtools.r-lib.org)
or the lighter
[*remotes*](https://remotes.r-lib.org).

``` r
# install.packages("devtools")
devtools::install_github("frankmakr/frankmakrdiss")

# install.packages("remotes")
remotes::install_github("frankmakr/frankmakrdiss")
```



## Getting started ##

At this time
the documentation of *frankmakrdiss*
is not detailed enough to be self-explanatory.
That is why it is highly recommended
to read the doctoral dissertation before using this package.

This is an ongoing project.
So, please be patient.



## License ##

The code of this work is licensed under a
[BSD 3-Clause “New” or “Revised” License
(BSD-3-Clause)](LICENSE.md).
The content and the data set are licensed under a
[Creative Commons Attribution 4.0 International License
(CC-BY-4.0)](https://creativecommons.org/licenses/by/4.0/).
