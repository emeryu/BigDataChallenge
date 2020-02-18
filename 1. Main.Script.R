################################################################################

#                              MAIN SCRIPT                                     #

################################################################################

#----------------------------TABLE OF CONTENTS----------------------------------

# - Overview
# - Packages downloaded
# - Libraries
# - Data Download 
# - Workflow 
# - Source the rest of the code

################################################################################

#---------------------------------OVERVIEW--------------------------------------

#The following contents within this main directory include the packages, 
#libraries, dataset and the workflow that is used in this analysis. For the 
#purpose of this analysis the scripts should be run in the folling order to gain 
#replicatory results:

#    1. MainScript:(Data organization with the overview of the workflow used
#                   in this project)
#
#    2. DataCleaning: (How to clean the data that is used for these analyses)
#
#
#    3. DataAnalysisandGraphs: (This includes the code used to run the linear 
#                               regressions and the graph production)

#------------------------------PACKAGE DOWNLOAD---------------------------------

# This is a list of the package used for these analyses

install.packages("tidyverse")

#---------------------------------LIBRARIES-------------------------------------

# Load the packages in the libraries 

library (tidyverse)

#--------------------------------DATA DOWLOAD-----------------------------------

# The raw data file was originally found from:
#     https://datadryad.org/stash/dataset/doi:10.5061/dryad.hp
# 38ct1?fbclid=IwAR2T0qNARXX80OK_fTSZ2B1B3hP4Wr2rCm9BawzTMmfgRSiTTihqGu3mZPM  
# The datasheet was then downloaded as a csv file. When downloading this 
# dataset as a csv, we saved the first worksheet titled Osteometrics. 

# The file of the raw data can be accessed with the following code.

working.dir <- getwd()

# This should be set to the working directory of
# the "BigDataChallenge_Master" folder after downloading this repo. 

raw.data <- read.csv("Raw.Data.csv")

#-----------------------------------WORKFLOW------------------------------------

# In our working firectory we created 4 pathways to different folders with 
# specific outputs 

#           - 1.Raw.Data  -> the path to this folder is: rd.path
#                 (This folder contains a copy of the original downloaded
#                   data set without changes.)
#
#           - 2.Clean.Data -> the path to this folder is: cd.path
#                 (This folder contains  the cleaned data set.)
#
#           - 3.Analysis -> the path to this folder is: an.path
#                 (This folder contains saved outputs of our anaylsis.)
#
#           - 4.Graphs -> the path to this folder is: gr.path
#                 (This folder contains all visualizations of data.)

output.folders <- c("1.Raw.Data","2.Clean.Data","3.Analysis","4.Graphs")

# Check to see if the folders exist in the working directory and if they don't, 
# use the following loop. 
# The following loop checks the output.folders list and checks to see 
# if the folders exist in the working directory. If they don't it will create 
# them. 

# Make the folders using this loop code 
for(i in 1:length(output.folders)) 
  if(file.exists(output.folders[i]) == FALSE) 
    dir.create(output.folders[i])

#-------- Pathways----------

# The following is a directory of the pathways to each of our output folders

# Path to 1.Raw.Data folder
rd.path <- paste(working.dir,"/",output.folders[1], "/", sep="")

# Path to 2.Clean.Data
cd.path <- paste(working.dir,"/",output.folders[2], "/", sep="")

# Path to 3.Analysis
an.path <- paste(working.dir,"/",output.folders[3], "/", sep="")

# Path to 4.Graphs
gr.path <- paste(working.dir,"/",output.folders[4], "/", sep="")

# Now we can save our raw data into the raw data file. 

write.csv(raw.data, paste(rd.path, "Raw.Data.csv"),
          row.names = FALSE)
#-----------Source the other scripts----------- 
source("2. Data.Cleaning.R")
source("3. Analysis.Graphs.R")
