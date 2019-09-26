Project: 
Japan Typhoon Regional Frequency Analysis - Utilising JMA data

Author: 
Oliver O.

Aim: 
The aim of this project was to visualise the frequency of typhoons which have historically made landfall in Japan. 
The main data set I have used is the Japan Meteorological Agency's best storm track data set, containing 6-hourly observations of typhoons since 1951 until 2019. 
In my previous job, this was part of my project on validation of Japan typhoon models developed by catastrophe model vendors. 
Since the JMA data is publicly available (https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/besttrack.html), I decided to publish the visualisation code to (once again) show how simple and powerful R Shiny is.

Contents:
This project has two parts: 

> Part A - the 'ETL' part
In this part I have combined, transformed, and prepared data sets required for visualisation.
The key code is in 'ETL code.R', and requires all the .csv files to run properly - make sure to change the working directory.

> Part B - the R shiny part
This part contains the visualisation using R shiny.
The code to run is in 'ShinyApp.R', and requires all the .Rda files in this repo to run - make sure to change the working directory (best to put all the Part B files in one folder, open the ShinyApp code, and delete the setwd(...) line).


