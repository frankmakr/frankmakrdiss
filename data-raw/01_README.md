# CHANGELOG

The format of the CHANGELOG section is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0)
and uses
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2023-08-25

### New

+ File `01_README.md` with CHANGELOG, GENERAL INFORMATION,
  and DATA OVERVIEW sections
+ Files `02_comms_demo.csv`, `03_comms_data.csv`, `04_conval_demo.csv`,
  `05_conval_data.csv`, and `06_conval_comms.csv`



# GENERAL INFORMATION

## Author

Frank Krumbholz (frankmakr [AT] mailbox [DOT] org)

## Recommended Citation

### German

Krumbholz, F. (2023).
*Psychological Sense of Community:*
*Inhaltsvalidität von Testverfahren in Abhängigkeit von Bezugsgemeinschaften*
(Version 1.0.0)
[Datensatz].
<https://doi.org/10.5281/zenodo.8000035>

### English

Krumbholz, F. (2023).
*Psychological Sense of Community:*
*Inhaltsvalidität von Testverfahren in Abhängigkeit von Bezugsgemeinschaften*
[Psychological sense of community:
Content validity of tests in dependence of reference communities]
(Version 1.0.0)
[Data set].
<https://doi.org/10.5281/zenodo.8000035>

## License

This data set is licensed under a
[Creative Commons Attribution 4.0 International License
(CC-BY-4.0)](https://creativecommons.org/licenses/by/4.0/).



# DATA OVERVIEW

## File List

+ 01_README.md
+ 02_comms_demo.csv
+ 03_comms_data.csv
+ 04_conval_demo.csv
+ 05_conval_data.csv
+ 06_conval_comms.csv



## README

This file.



## comms_demo

The sample characteristics for the data set `comms_data`.

### Format

A table with 158 rows and 7 columns.

### Variables

+ **person**
  Person number
+ **gender**
  Gender coded as 1 = Male, 2 = Female, 3 = Divers
+ **age**
  Age in years
+ **state**
  One of the 16 German states the person is living in alphabetical order
  or 17 = Living outside from Germany
+ **studstatus**
  Student status used in the FernUniversität official statistics
  (Berichtswesen, 2022)
+ **noexpert**
  Formal theoretical knowledge of a theory of psychological sense of community
  coded as 0 = Knowledge, 1 = No knowledge
+ **numcomm**
  The number of communities the person mentioned
  with a maximum number of 10 entry fields for each person



## comms_data

Phrases for the communities in German
where persons have experienced themselves sense of community.

### Format

A table with 1,580 rows and 6 columns.

### Variables

+ **person**
  Person number from `comms_demo`
+ **field**
  Which of the 10 entry fields was used for the phrase
+ **k_start**
  The phrases before the classification process
+ **k_iter1**
  The phrases after iteration 1
+ **k_iter2**
  The phrases after iteration 2
+ **k_iter3**
  The phrases after iteration 3



## conval_demo

The sample characteristics for the data set `conval_data`.

### Format

A data frame with 1,465 rows and 7 columns.

### Variables

+ **person**
  Person number
+ **gender**
  Gender coded as 1 = Male, 2 = Female, 3 = Divers
+ **age**
  Age in years
+ **state**
  One of the 16 German states the person is living in alphabetical order
  or 17 = Living outside from Germany
+ **studstatus**
  Student status used in the FernUniversität official statistics
  (Berichtswesen, 2023)
+ **noexpert**
  Formal theoretical knowledge of a theory of psychological sense of community
  coded as 0 = Knowledge, 1 = No knowledge
+ **numcomm**
  The number of communities the person mentioned
  with a maximum number of 22 communities given



## conval_data

The rated content validity of the items of 4 different tests
in specific community contexts.

### Format

A table with 5,609 rows and 79 columns.

### Variables

+ **person**
  Person number from `conval_demo`
+ **comm**
  The specific community from `conval_comms`
  for which the test items were rated
+ **value**
  The grade to which the community is a community of shared values
+ **purpose**
  The grade to which the community is a community of purpose
+ **value_x_purpose**
  Interaction variable of `value` and `purpose`
+ **chavis_[1-25]**
  The grade to which the test item [1-25] from Chavis et al. (2008)
  is rated necessary to characterize sense of community
+ **omoto_[1-18]**
  The grade to which the test item [1-18] from Omoto and Snyder (2010)
  is rated necessary to characterize sense of community
+ **jason_[1-9]**
  The grade to which the test item [1-9] from Jason et al. (2015)
  is rated necessary to characterize sense of community
+ **halamova_[1-22]**
  The grade to which the test item [1-22] from Halamová et al. (2018)
  is rated necessary to characterize sense of community



## conval_comms

The phrases of the communities in German
for the variable `comm` in the data set `conval_data`.

### Format

A table with 22 rows and 2 columns.

### Variables

+ **long**
  The original phrases used in the study material
+ **short**
  Short phrases used in text, tables, and graphs



## References

Berichtswesen. (2022, March 25).
*Studierendenstatistik für Sommersemester 2018*
[Student statistics for summer semester 2018].
FernUniversität in Hagen.
<https://www.fernuni-hagen.de/uniintern/organisation/statistik/semesterstatistik/sose2018.shtml>.

Berichtswesen. (2023, March 31).
*Studierendenstatistik für Wintersemester 2019/20*
[Student statistics for winter semester 2019/20].
FernUniversität in Hagen.
<https://www.fernuni-hagen.de/uniintern/organisation/statistik/semesterstatistik/wise2019-20.shtml>.

Chavis, D. M., Lee, K. S., & Acosta, J. D. (2008).
*The Sense of Community Index (SCI) revised:*
*The reliability and validity of the SCI-2.*
Paper presented at the 2nd International Community Psychology Conference.
Lisboa, Portugal.
Retrieved October 17, 2018, from <https://senseofcommunity.com>.

Halamová, J. Kanovsky, M., & Naništová, E. (2018).
Development and psychometric analysis of the
sense of community descriptors scale.
*Psychological Intervention, 27(1), 44-55.*
<https://doi.org/10.5093/pi2018a8>.

Jason, L. A., Stevens, E., & Ram, D. (2015).
Development of a three-factor psychological sense of community scale.
*Journal of Community Psychology, 43(8), 973-985.*
<https://doi.org/10.1002/jcop.21726>.

Omoto, A. M., & Snyder, M. (2010).
Influences of psychological sense of community on
voluntary helping and prosocial action.
In S. Stürmer & M. Snyder (Eds.),
*The psychology of prosocial behavior:*
*Group processes, intergroup relations, and helping (pp. 223-243).*
Wiley-Blackwell.
<https://doi.org/10.1002/9781444307948.ch12>.
