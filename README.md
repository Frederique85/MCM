# MCM
This folder contains all routines to extract and organise data from MCM (Multicriteria mapping) engagements. Some are in the form of Excel macros, some in MCM engagement files, and others in R code. MCM is a software based decision analysis tool for the appraisal of different options (more info at: https://www.multicriteriamapping.com). A detailed explanation of how to use the routines can be found in the MCM manual at the bottom of this page: https://www.sussex.ac.uk/mcm/publications/. All the routines here are provided with an example case, so that you can try the routine out and see whether the results you get are similar to the ones we have provided. 

## 1.	Get data from engagements 
There are two types of routines to extract data from engagements:
A. Excel macro to extract data from online engagements: 
The macros in __'MCM Data with raw data test.xlsm'__ enables extraction of data from data copied from the MCM individual rank reports. An example can be found in the spreadsheet, and the __result data__ is available in __'MCM Data with raw data test & results.xlsm'__. It enables extraction of quantitative and qualitative data scores and notes data. 
 
B. R script to extract data from both online and offline engagements:
The script __'1. Get data from engagements.R'__ enables extraction of data from offline engagements in a dedicated folder (one example is provided) and online data as gathered in step A above, also provided for this example in dedicated folders for both quant and qual. The combined data can be found in the output file named __'Merged_MCM_Dataset_w_Comments.csv'__ which contains both the quantitative data and qualitative data (an example can be found in the folder 'MCM/csv'). 

## 2.	Transform data
A first step before the analysis is to merge all the data from offline and online engagements into one dataset. This is done using the R script __'2.Transform data.R'__. This script assumes that you have extracted data from your online and offline engagement files using the instructions in step 1 above, but also manually extracted the weighting of each online engagement and the core options for the project. An example of the weighting file can be in 'MCM/weighting'. It is important to note that the structure of this file should be kept, and the names of the criteria and engagement name should match exactly the ones in your online engagement file (e.g. MCM/online engagements/Product development.csv'). An example of the core options file can be in 'MCM/core options'. Again, it is important to note that the structure of this file should be kept, and the names of the options should match exactly the ones in your MCM project.

## 3.	Distribution analysis
A third script, __'3. Distribution analysis.R'__, produces the __'pairwise inclination charts'__ and the __'merit orders'__. It assumes that the __'MCM_Final_Ranks.csv'__ has been generated from the previous script (2). Once the script has been run, the __'pairwise inclination charts'__ can be found in the 'MCM/analysis/pairwise graphs' folder, with individual data scores found in the 'MCM/analysis/pairwise data' folder. Likewise the __'merit order'__ chart will be generated in the 'MCM/analysis/merit order' folder called __'Merit_Overall.png'__, and accompanying data can be found in the same folder 'MCM/analysis/merit order'. The analysis folders do not need to be created beforehand, the script does this automatically. This script can be edited to provide additional charts if you have groupings such as countries or stakeholders. 

__Have fun with MCM :-)__
