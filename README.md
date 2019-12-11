# MCM
This folder contains all routine to extract, organise data from MCM (Multi criteria mapping) engagements. Some are in the form of Excel macros, and other in R code. MCM is a software based decision analysis tool for the appraisal of policies (more info at: https://www.multicriteriamapping.com). A detailed explanation on how to use the routines can be found in the MCM manual (<link here>). All the routines here are provided with an example case, so that you can try the routine out and see whether the results you get are similar to the ones we have provided. 

## 1.	Get Data from engagements 
There are two types of routines to extract data from engagements:
1. Excel macro to extract data from online engagement: 
The macros in __'MCM Data with raw data test.xlsm'__ enables to extract data from data copied from the individual rank reports. An example can be found in the spreadsheet, and the __result data__ is available in __'MCM Data with raw data test & results.xlsm'__. It enables to extract quantitative and qualitative data. 
 
2. R scripts to extract data from offline engagements:
The script __'1. Get Data from engagements.R'__ enables to extract data from all offline engagements in a dedicated folder (one example is provided in the folder data). The combined data can be found in the output file named __'MCM_Overall_Rawdata.csv'__ which extract both the quantitative data and qualitative (an example can be found in the folder 'MCM/csv'). 

## 2.	Create analaysis from engagement
1. A first step before the analysis is to merge all the data from offline and online engagements into one dataset. This is done using the R script __'2.TransformDataOnly.R'__. This script assumes that you have transformed your online and offline engagement files using the two above scripts, but also manually extracted the weighting of each online engagement. An example of the weighting file can be in 'MCM/weighting'. It is important to note that the structure of this file should be kept, and the names of the criteria and engagment name should match exactly the ones in your online engagement file (e.g. MCM/online engagements/Product Development.csv').

2. A second script, __'3. Distribution analysis.R'__, produce the __'pairwise inclination charts'__ and the __'merit orders'__. It assumed that the __'MCM_finalscore.csv'__ has been generated from the previous script. The __'pairwise inclination charts'__ can be found 'MCM/analysis/Graphs' folder, with individual data scores which can be found in the 'MCM/analysis/Data' folder. Likewise the __'merit order'__ chart will be generated in the 'MCM/analysis/' called __'Merit_Overall.png'__, and accompanying data can be found in the folder 'MCM/analysis/Metrics'. The folders do not need to be created beforehand, the script does this automatically. 

__Have fun with MCM :-)__
