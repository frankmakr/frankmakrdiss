This README file was generated on 2023-06-02

# Title of Record #

## GENERAL INFORMATION ##

### Author ###
Frank Krumbholz (frankmakr[AT]mailbox[DOT]org)

### License ###
These data sets are licensed under
Attribution-ShareAlike 4.0 International (CC-BY-SA-4.0).
To view a copy of this license visit
<https://creativecommons.org/licenses/by-sa/4.0>
or send a letter to
Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

### Recommended Citation ###

#### German version ####
Krumbholz, F. (2023).
*Psychological Sense of Community:*
*Inhaltsvalidität von Testverfahren in Abhängigkeit von Bezugsgemeinschaften*
[Datensatz].
<https://doi.org/zenodo.xxx>

#### English version ####
Krumbholz, F. (2023).
*Psychological Sense of Community:*
*Inhaltsvalidität von Testverfahren in Abhängigkeit von Bezugsgemeinschaften*
[Psychological sense of community:
Content validity of tests in dependence of community contexts]
[Data set].
<https://doi.org/zenodo.xxx>



## DATA OVERVIEW ##

### File List ###
- 01_README.md
- 02_comms_demo.csv
- 03_comms_data.csv
- 04_conval_demo.csv
- 05_conval_data.csv
- 06_conval_comms.csv



### comms_demo ###
The sample characteristics for the data set *comms_data*.

#### Format ####
A table with 158 rows and 7 columns.

#### Variables ####
- **person**
  Person number
- **gender**
  Gender coded as male = 1, female = 2, divers = 3
- **age**
  Age in years
- **state**
  One of the 16 German states the person is living in alphabetical order or 17 = living outside from Germany
- **studstatus**
  Student status used in the FernUniversität official statistics
- **noexpert**
  Knowledge of a theory of psychological sense of community coded as 0 = Knowledge, 1 = No knowledge
- **numcomm**
  The number of communities the person mentioned



### comms_data ###
Phrases for the communities in German
where persons have experienced themselves sense of community.

#### Format ####
A table with 1,580 rows and 6 columns.

#### Variables ####
- **person**
  Person number from *comms_demo*
- **field**
  Which of the 10 fields was used for the phrase
- **k_start**
  The phrases before the classification process
- **k_iter1**
  The phrases after iteration 1
- **k_iter2**
  The phrases after iteration 2
- **k_iter3**
  The phrases after iteration 3



### conval_demo ###
The sample characteristics for the data set *conval_data*.

#### Format ####
A data frame with 1,465 rows and 7 columns.

#### Variables ####
- **person**
  Person number
- **gender**
  Gender coded as male = 1, female = 2, divers = 3
- **age**
  Age in years
- **state**
  One of the 16 German states the person is living in alphabetical order or 17 = living outside from Germany
- **studstatus**
  Student status used in the FernUniversität official statistics
- **noexpert**
  Knowledge of a theory of psychological sense of community coded as 0 = Knowledge, 1 = No knowledge
- **numcomm**
  The number of communities the person mentioned



### conval_data ###
The content validity of the items of 4 different tests
was rated in specific community contexts.

#### Format ####
A table with 5,609 rows and 79 columns.

#### Variables ####
- **person**
  Person number from *conval_demo*
- **comm**
  The specific community from *conval_comms* for which the test items were rated
- **value**
  The grade to which the community is a community of shared values
- **purpose**
  The grade to which the community is a community of purpose}
- **value_x_purpose**
  Interaction variable of *value* and *purpose*
- **chavis_[1-25]**
  The grade to which the test item [1-25] from Chavis et al. (2008) is necessary to characterize sense of community
- **omoto_[1-18]**
  The grade to which the test item [1-18] from Omoto and Snyder (2010) is necessary to characterize sense of community
- **jason_[1-9]**
  The grade to which the test item [1-9] from Jason et al. (2015) is necessary to characterize sense of community
- **halamova_[1-22]**
  The grade to which the test item [1-22] from Halamová et al. (2018) is necessary to characterize sense of community



### conval_comms ###
The phrases of the communities in German
for the variable *comm* in the data set *conval_data*.

#### Format ####
A table with 22 rows and 2 columns.

#### Variables ####
- **long**
  The original phrases used in the study material
- **short**
  Short phrases used in text, tables, and graphs
