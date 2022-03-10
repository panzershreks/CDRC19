# Comparing death rates from Covid-19 in different countries II

## Contents:

1. [Project Information](#project-information)
2. [Repo Documentation](#repo-documentation)
    1. [`All Model Latex`](#all-model-latex)
    2. [`Combined DataFrame Work`](#combined-dataframe-work)
    3. [`Data_Dump`](#datadump)
    4. [`GLM (Random Forest (2nd))`](#glm-random-forest-2nd)
    5. [`GLM Data and Analysis`](#glm-data-and-analysis)
    6. [`GLM Take 2`](#glm-take-2)
    7. [`LM Data and Analysis`](#lm-data-and-analysis)
    8. [`Old/Middle-Man Data Frames`](#oldmiddle-man-data-frames)
    9. [`R_Scripts`](#rscripts)
    10. [`Working_Data`](#workingdata)

## Project Information

### Suitable degrees
Maths and Stats

### Project supervisor
Simon Wood

### Project group members

Matthew Dailey (s1837608) \
Theodora Richards (s1723589) \
Emma Saheed (s1858135) \
Eman Wong (s1974845) \
Zhihao Xu (s2135624)

### Project description

This project will involve finding publically available data on Covid-19 death rates (per 100 thousand people) for different countries, along side variables that may be associated with the differences in death rate, such as health system spending, hospital cross infection rates, GDP, economic inequality, extend of suppression measures undertaken etc. 

The assembled data set will then be analysed using statistical regression methods in R, to investigate the strength and statistical significance of any apparent associations.

A couple of example data sources are https://www.worldometers.info/coronavirus/ https://www.cia.gov/the-world-factbook/

### Project type
Group (group size 1 - 4)

### Prerequisites
You need to be competent with applied statistics, linear regression and R. You also need to be comfortable searching out data on the web, and getting it into a usable form.

### Recommended reading
Faraway, J Linear Models with R, CRC/Taylor and Francis

## Repo Documentation

Contained is this repository is all the code that was used in the creation of our report.

There are several folders:
1. [`All Model Latex`](#all-model-latex)
2. [`Combined DataFrame Work`](#combined-dataframe-work)
3. [`Data_Dump`](#datadump)
4. [`GLM (Random Forest (2nd))`](#glm-random-forest-2nd)
5. [`GLM Data and Analysis`](#glm-data-and-analysis)
6. [`GLM Take 2`](#glm-take-2)
7. [`LM Data and Analysis`](#lm-data-and-analysis)
8. [`Old/Middle-Man Data Frames`](#oldmiddle-man-data-frames)
9. [`R_Scripts`](#rscripts)
10. [`Working_Data`](#workingdata)

### `All Model Latex`

This folder contains the LaTeX of the tables for the final model summaries used in our report.

### `Combined DataFrame Work`

The subfolder structure is:
- `CSV Files`
    - `Clean`
    - `Old`
    - `Not Clean By Category`
- `Code`
- `Old`

#### `Combined DataFrame Work/CSV Files/Clean`

Contains our data sorted by category, after a cleaning step was done to ensure that only countries, which are entities in the United Nations are included.

#### `Combined DataFrame Work/CSV Files/Old`

**Deprecated**

#### `Combined DataFrame Work/CSV Files/Not Clean By Category`

Contains our data sorted by category, but before the cleaning step was done. Thus there are entities which are not countries included, such as "Western Europe".

#### `Combined DataFrame Work/Code`

Contains the code used for merging our initial pulled data from OWID, as well as for cleaning the countries included in our datasets.

#### `Combined DataFrame Work/Old`

**Deprecated**

### `Data_Dump`

Contains all the data pulled from our sources.

### `GLM (Random Forest (2nd))`

This folder contains the code and data for our RF models, which used the `R` `randomForest` library for our imputation method.

The subfolder structure is:
- `Bootstrap`
- `Categories Imputed Model`
- `Categories Miss Model`
- `Combined Model`
- `RF Split Into Categories Initial Work`
    - `RF Category Imputed Full CSV`
    - `RF Category Sig Imputed CSV`
    - `RF Category Sig Miss CSV`

#### `GLM (Random Forest (2nd))/Bootstrap`

Contains the script used for bootstrapping our model selection process, as well as CSVs which includes the tallies for each included variable using each model selection pipeline.

#### `GLM (Random Forest (2nd))/Categories Imputed Model`

Contains the `R` script for model RF(b). This uses the data stored in `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Imputed CSV`.

#### `GLM (Random Forest (2nd))/Categories Miss Model`

Contains the `R` script for model RF(c). This uses the data stored in `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Miss CSV`

#### `GLM (Random Forest (2nd))/Combined Model`

Contains the `R` scripts for model RF(a). This uses the data: `GLM Take 2/Combined Model/combined_all_missing.csv`.

#### `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work`

The `R` scripts in this directory handle the processing of the clean data, by category, and an initial `randomForest` imputation and model selection is run. This process gives us the subset of variables to feed into the following step of the model selection pipeline in RF(b) and RF(c). The data for this subset is then stored in `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Imputed CSV` and `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Miss CSV`, to generate RF(b) and RF(c) respectively.

#### `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Imputed Full CSV`

Contains the imputed CSVs for all variables initially included in the `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work` scripts.

#### `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Imputed CSV`

The CSVs to be used in the second stage of the RF(b) model selection pipeline.

#### `GLM (Random Forest (2nd))/RF Split Into Categories Initial Work/RF Category Sig Miss CSV`

The CSVs to be used in the second stage of the RF(c) model selection pipeline.

### `GLM Data and Analysis`

**Deprecated**

This folder contains the `R` scripts and CSVs that were used for our initial GLM modelling. However, we were using the `missForest` package, and included the response along with our explanatory variables during the imputation. This led to relationships between our variables and the response that we found were "too good to be true", and we determined that it was due to the response variable being included in the data used for imputation. Thus, we then left the response out when imputing our data, and the `R` scripts and CSVs used in that process are in `GLM Take 2`.

### `GLM Take 2`

This folder contains the code and data for our RF models, which used the `R` `missForest` library for our imputation method.

The subfolder structure is:
- `Bootstrap`
- `Categories Imputed Model`
- `Categories Miss Model`
- `Combined Model`
- `Split Into Categories Initial Work`
    - `Category Imputed Full CSV`
    - `Category Sig Imputed CSV`
    - `Category Sig Miss CSV`
- `Summary Statistics`
    - `Report Graphs`

#### `GLM Take 2/Bootstrap`

Contains the script used for bootstrapping our model selection process, as well as CSVs which includes the tallies for each included variable using each model selection pipeline.

#### `GLM Take 2/Categories Imputed Model`

Contains the `R` script for model MF(b). This uses the data stored in `GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV`.

#### `GLM Take 2/Categories Miss Model`

Contains the `R` script for model MF(c). This uses the data stored in `GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV`

#### `GLM Take 2/Combined Model`

Contains the `R` scripts and CSVs used for model RF(a). The final script is `Full Model Imputation.R`, uses the data: `combined_all_missing.csv`.

#### `GLM Take 2/Split Into Categories Initial Work`

The `R` scripts in this directory handle the processing of the clean data, by category, and an initial `missForest` imputation and model selection is run. This process gives us the subset of variables to feed into the following step of the model selection pipeline in MF(b) and MF(c). The data for this subset is then stored in `GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV` and `GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV`, to generate MF(b) and MF(c) respectively.

#### `GLM Take 2/Split Into Categories Initial Work/Category Imputed Full CSV`

Contains the imputed CSVs for all variables initially included in the `GLM Take 2/Split Into Categories Initial Work` scripts.

#### `GLM Take 2/Split Into Categories Initial Work/Category Sig Imputed CSV`

The CSVs to be used in the second stage of the MF(b) model selection pipeline.

#### `GLM Take 2/Split Into Categories Initial Work/Category Sig Miss CSV`

The CSVs to be used in the second stage of the MF(c) model selection pipeline.

#### `GLM Take 2/Summary Statistics`

Contains the `R` script used to generate the summary statistics used in the report.

#### `GLM Take 2/Summary Statistics/Report Graphs`

Contains the images generated by `GLM Take 2/Summary Statistics/Category Summary Statistics.R` to be used in the report.

### `LM Data and Analysis`

**Deprecated**

This folder contains the `R` scripts and CSVs that were used for our initial LM modelling. However, we found that the model assumptions that were necessary in linear modelling were not satisfied, and that a linear model was not suitable for our purposes.

### `Old/Middle-Man Data Frames`

**Deprecated**

Contains our initial CSV files after pre-processing. This is not used anymore.

### `R_Scripts`

This folder contains subfolders with code, CSV files, graphics, etc. generated by each member. The graphics used throughout the report, aside from the summary statistics, is found in `R_Scripts/MatthewR`. For our model selection process, several helper functions were written to drop variables based on their VIF, as well as to perform backwards selection using AICc while fitting models using the `glm2` package. These helper functions are contained in `R_Scripts/EmanR/automate_vif.R` and `R_Scripts/EmanR/step2.R`.

### `Working_Data`

This folder contains all the data, sorted by category, after an initial pruning process, in which irrelevant datasets in `Data_Dump` were disregarded.