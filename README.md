# CollegeDataClassification.R
## College Data Classification

This repository contains the R code used to perform various classification methods on a college dataset. The goal of this project is to classify whether a college is private or not based on various college attributes. 

The classification methods used are:

- Bagging
- Random Forests
- K-Nearest Neighbors (KNN)
- Quadratic Discriminant Analysis (QDA)
- Linear Discriminant Analysis (LDA)

The project includes four different scenarios for pre-processing and scaling of data:

1. Scenario A: Centering of all variables 
2. Scenario B: Log-transformation of skewed variables followed by centering of all variables
3. Scenario C: Standardization of all variables
4. Scenario D: Log-transformation of skewed variables followed by standardization of all variables

For each scenario and each classification method, the script calculates the hit rate, sensitivity, and specificity rates.

### Dependencies

The following R packages are required to run this script:

- ISLR2
- MASS
- randomForest
- class

### Running the Script

To execute the script, you can source the file `CollegeDataClassification.R` in your R environment.

### Data

The dataset used in this project comes from the `ISLR2` package and includes various attributes of colleges in the U.S.

### Results

The results from each scenario and each model are printed in the console. These results include the hit rate, sensitivity, and specificity for each classification method.
