# ProMiSE_Project

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository hold all relevant code used for data preprocessing, visualization, and analysis for the ProMiSE Project.

This repository holds all the necessary code to run an Edgeworth Box teaching application developed in R Shiny. 

The interactive application allows students and teachers to illustrate key economic intuitions by visualising the general equilibrium in a 2 consumers x 2 goods endowment economy. Users can selectively display all important ingredients, like indifference curves and budget constraints, and change a wide range of parameter values. The app furthermore allows to illustrate the Pareto set, the contract curve, and the first and second welfare theorems - in an interactive way.

This project is one of three applications developed by Daniel Leal and Frank Pisch at the University of St. Gallen in 2020/21.

## Table of content

- [Application](#application)
    - [Edgeworth Box](#edgeworth-box)
    - [Offer Curves](#offer-curves)
    - [Welfare](#welfare)
- [Code](#code)
    - [UI](#ui)
    - [Server](#server)
    - [Edgeworth Function](#edgeworth-functions)
    - [.Rprofile](#git-ignore)
- [Tutorial](#tutorial)
- [License](#license)


## Application

The application is divided into three applications:

- Edgeworth Box:
    - This portion contains the standard Edgeworth Box representation. Its purpose is to showcase the interaction between two agents with different initial endowments and the effect of changing the relative price of good on their consumption.  
    <p align="center">
      <img src="https://github.com/sas8465/Edgeworth-Box-Shiny-App/blob/main/images/Edgeworth%20Box.png" width="850" height="500" />
    </p>
- Offer Curves:
    - This application demonstrates each agent’s individual offer curve and how they are constructed. An offer curve shows the quantity of one type of product that an agent will offer for each quantity of another type of product that it receives.
    <p align="center">
      <img src="https://github.com/sas8465/Edgeworth-Box-Shiny-App/blob/main/images/Offer%20Curves.png" width="850" height="500" />
    </p>
- Welfare:
    - This portion illustrates the First and Second Welfare Theorems using the Edgeworth Box setup.
    <p align="center">
      <img src="https://github.com/sas8465/Edgeworth-Box-Shiny-App/blob/main/images/Welfare.png" width="850" height="500" />
    </p>

## Code

The code is contains three components:

- UI:
    - Following R-Shiny conventions. All tittles, text, and user input buttons are programmed in this portion of the code. 
- Server:
    - This portion the R code holds the instructions that allow the user inputs from the R-Shiny UI to be translated into Python and prompts the Python code to be ran.  
- Edgeworth Functions:
    - This Python portion of the code contains the economic modelling in Python and manages all the combinations of parameters that are input by the user.  
- .Rprofile:
    - This document is not to be changed. It instructs R Shiny to initiate a Python server in order to Python and R interface to function. It is a default file and does not need to be change regardless of changes in the code.  

Tutorial to deploy application: https://youtu.be/ZAmtmZNZWbQ
