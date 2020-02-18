
################################################################################

#                             ANALYSIS AND GRAPHS                              #

################################################################################


# Here is the linear regression models and graphs in a loop that are stored in 
#graphs folder. 
#
#

#=======Change the names of the headers so that the x axis reads nicely.======== 
names(bis)[names(bis)== "elev"] <- "Elevation (meters)"
names(bis)[names(bis)== "long"] <- "Longitude"
names(bis)[names(bis)== "lat"] <- "Latitude"
names(bis)[names(bis)== "temp"] <- "Temperature (C)"
names(bis)
#=====Here is code that needs to be run in order to store the graphs in the==== 
# correct folder
working.dir <- getwd() 
output.folders <- c("1.Raw.Data","2.Clean.Data","3.Analysis","4.Graphs")
gr.path <- paste(working.dir,"/",output.folders[4], "/", sep="")

#====================Here are our variables===================================== 
expl.variable <- c(1,2,3,4) #The position of each explanatory column 
# (elevation=2 etc.)
var.names <- colnames(bis)[expl.variable] #here is where the names are assigned 
# to the postion 
#here is the color palet for the graphs. 
color <- c("#FFD700","#CD5C5C","#DC143C","#20B2AA") # for the plot            	
color2 <- c("#20B2AA","#DC143C","#FFD700","#CD5C5C") #for the abline 

#=========== Here is where our regression results will be stored.========== 
regr.results <- cbind.data.frame(intercept = rep(NA, length(expl.variable)), 
                                 coeffic = rep(NA, length(expl.variable)))
#==========Here is where we create all our graphs and paste in folder===========
for(i in 1:4){ #for loop 1 through 4 because we have 4 postions 
  
  model.t <- lm(bis$mass ~ bis[, expl.variable[i]] ) # linear regression model 
  # individual regression result 
  regr.results[i,]  <- model.t$coefficients
  
  pdf(file =paste(gr.path, var.names[i], ".pdf", sep = ""), width = 5, 
      height = 5) # open as a pdf and save to graph folder. 
  plot(bis[, expl.variable[i]], bis$mass, main = var.names[i], ylab = "Mass (kg)", 
       xlab= var.names[i], col = color[i]) # plot the explanatory variable against mass 
  abline(model.t, lwd= 2, col = color2[i]) # add a regression line based off the coefficients 
  dev.off() #close pdf 
}

#========Here is where we put all our results in our results folder.======== 
for(i in 1:4){ #create a for loop to create all the test statistics 
  
model.t <- lm(bis$mass ~ bis[, expl.variable[i]] ) #create a model for each explanatory variable.
  
sum.t <- capture.output(summary(model.t)) #capture the output of the summary of model.t . 
  
cat(sum.t, file= paste(an.path,var.names[i],".txt" , sep = "")) #cat = cancatonate and paste as a txt file in the results folder.  
}

#===============================END============================================





