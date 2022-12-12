# PLSDA Regression (Partial Least Square Discriminant Analysis) 

### TABLE OF CONTENTS
-   [Project Description](#project-description)
    -   [PLSDA Definition](#plsda-definition)
    -   [NIPALS Algorithm](#nipals-algorithm)
-  [Using the package PLSDA](#using-the-package-plsda)
    -   [Installing the package](#installing-the-package)
    -   [Preparing the data](#preparing-the-data)
    -   [Fitting the model](#fitting-the-model)
    -   [Testing the model and predicting](#testing-the-model-and-predicting)
    -   [Determining the VIP](#determining-the-vip)
    -   [Graphics Options](#graphics-options)
    -   [Help](#help)
-  [Navigating the Shiny App](#navigating-the-shiny-app)
    

       
### PROJECT DESCRIPTION
----------------------
This project is part of our cursus in Data Science at the University of Lyon 2, Master 2 SISE.  The main objective is to develop an R package and Shiny application capable of realizing a PLSDA. 
The user can download the R package directly from Git or execute the Shiny App (we will see how later) 


#### PLSDA DEFINITION 
----------------------
From the mixOmics website we find the following definition :  
"PLS was designed with a canonical (exploratory) approach and a regression (explanatory) approach in mind. Partial Least Squares – Discriminant Analysis (PLS-DA) was hence developed to allow the powerful PLS algorithm to be used for classification [1, 2]. It performs very similarly to PLS, just that the response vector y contains categorical vectors rather than continuous vectors. PLS-DA has the same advantages that PLS does, such that it operates efficiently over large dataframes and is not negatively influenced by collinearity."

#### NIPALS ALGORITHM
---------------------
From statistics4u.com : 
"The NIPALS Algorithm ("Nonlinear Iterative vartial Least Squares") has been developed by H. Wold at first for PCA and later-on for PLS. It is the most commonly used method for calculating the principal components of a data set. It gives more numerically accurate results when compared with the SVD of the covariance matrix, but is slower to calculate."



### USING THE PACKAGE PLSDA 
-----------------------------------
#### INSTALLING THE PACKAGE
---------------------------
To install the package you need to download it from GitHub
```sh
library(devtools)
install_github('Skarbkit/SISE_ProjectR_PLSDA', subdir='/PackagePLSDA')
```

Then when it is successfully installed, you need to load it
```sh
library(PackagePLSDA)
```
To test the following functions we will use the dataset 'iris' (available in R)
```sh
summary(iris)
```
#### PREPARING THE DATA
-------------------------------
The version of our packages does not accept missing values for now. You need to clean it before using it.
With the function split_train_test() you can separate your data set into two with a treshold of p.

```sh
new_df <- split_train_test(data = iris,p=0.7) 
summary(new_df)
```

<img width="900" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/udlDKMd.jpg">
<br/>

#### FITTING THE MODEL
------------------------------------
```sh
model1 <- plsda.fit(formula = Species ~ . , data = new_df$df_train , ncomp = 2 , center = TRUE , reduce = FALSE )
summary(model1)
```
<img width="900" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/F4CXWjy.jpg">
<br/>

pls.fit() uses function dummies() when creating the model.


#### TESTING THE MODEL AND PREDICTING
------------------------------------
```sh
pred <- predict.plsda(model1,new_df$df_test[,1:4],type="posterior")
```

#### GRAPHICS OPTIONS
------------------------------------
 The package comes with graphics functions.
```sh
 individuals_plot(res.pls, Axe1 = 1, Axe2 = 2) 
```
<img width="700" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/9dBe4Cp.jpg">
<br/>

```sh
plsda_scree_plot(res.pls) 
```
<img width="700" alt="Capture d’écran 2021-11-29 à 15 59 47" src="https://i.imgur.com/B8mYjcD.jpg">
<br/>

#### HELP
-----------------------------------------
Don't forget to use the command help(<function>) to open a help window for each function.


### NAVIGATING THE SHINY APP
-----------------------------

The tutorial to maneuver the app can be found in video form here :  [Video currently being uploaded]
The app is hosted here : https://aeutarici.shinyapps.io/appshiny/
  
-----------------------------------------
 ### contributors
Fatimetou Haidara
Skarbkit Florian
AxelEutarici
