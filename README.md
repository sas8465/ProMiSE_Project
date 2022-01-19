# ProMiSE_Project

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository hold all relevant code used for data preprocessing, visualization, and analysis for the ProMiSE Project.

## Table of content

- [Preprocessing](#preprocessing)
    - [Data Formats](#data-formats)
    - [Preprocessing Pipelines](#workflows)
- [Visualization](#visualization)
    - [Visualization Formats](#visualization-formats)
    - [Visualization Pipelines](#server)
- [Analysis](#analysis)


## Preprocessing

The application is divided into three applications:

    <p align="center">
      <img src="https://github.com/sas8465/ProMiSE_Project/blob/main/images/Slide1.PNG" width="850" height="500" />
    </p>
- Offer Curves:

    <p align="center">
      <img src="https://github.com/sas8465/ProMiSE_Project/blob/main/images/Slide2.PNG" width="850" height="500" />
    </p>
    <p align="center">
      <img src="https://github.com/sas8465/ProMiSE_Project/blob/main/images/Slide3.PNG" width="850" height="500" />
    </p>

## Visualization

The code is contains three components:

- UI:
    - Following R-Shiny conventions. All tittles, text, and user input buttons are programmed in this portion of the code. 
- Server:
    - This portion the R code holds the instructions that allow the user inputs from the R-Shiny UI to be translated into Python and prompts the Python code to be ran.  
- Edgeworth Functions:
    - This Python portion of the code contains the economic modelling in Python and manages all the combinations of parameters that are input by the user.  
- .Rprofile:
    - This document is not to be changed. It instructs R Shiny to initiate a Python server in order to Python and R interface to function. It is a default file and does not need to be change regardless of changes in the code.  

## Analysis

The code is contains three components:

- UI:
    - Following R-Shiny conventions. All tittles, text, and user input buttons are programmed in this portion of the code. 
- Server:
    - This portion the R code holds the instructions that allow the user inputs from the R-Shiny UI to be translated into Python and prompts the Python code to be ran.  
- Edgeworth Functions:
    - This Python portion of the code contains the economic modelling in Python and manages all the combinations of parameters that are input by the user.  
- .Rprofile:
    - This document is not to be changed. It instructs R Shiny to initiate a Python server in order to Python and R interface to function. It is a default file and does not need to be change regardless of changes in the code.  
