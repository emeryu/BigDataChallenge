
################################################################################

#                             ANALYSIS AND GRAPHS                              #

################################################################################


# Here is the linear regression models and graphs in a loop that are stored in 
#graphs folder. 
#
#

#Change the names of the headers so that the x axis reads nicely. 
names(bis)[names(bis)== "elev"] <- "Elevation"
names(bis)[names(bis)== "long"] <- "Longitude"
names(bis)[names(bis)== "lat"] <- "Latitude"
names(bis)[names(bis)== "temp"] <- "Temperature"

# Here is code that needed to be run in order to store the graphs in the 
# correct folder
working.dir <- getwd() 
output.folders <- c("1.Raw.Data","2.Clean.Data","3.Analysis","4.Graphs")
gr.path <- paste(working.dir,"/",output.folders[4], "/", sep="")

#Here are our variables 
expl.variable <- c(1,2,3,4) #The position of each explanatory column 
# (elevation=2 etc.)
var.names <- colnames(bis)[expl.variable] #here is where the names are assigned 
# to the postion 

# Here is where we run the regression and get the results. 
regr.results <- cbind.data.frame(intercept = rep(NA, length(expl.variable)), 
                                 coeffic = rep(NA, length(expl.variable)))

for(i in 1:4){ #for loop 1 through 4 because we have 4 postions 
  
  #i <- 1 #set i to one when running the 4 loop 
  model.t <- lm(bis$mass ~ bis[, expl.variable[i]] ) # the coefficients for each 
  # individual regression result 
  regr.results[i,]  <- model.t$coefficients
  
  pdf(file =paste(gr.path, var.names[i], ".pdf", sep = ""), width = 5, 
      height = 5) # open as a pdf and save to graph folder. 
  plot(bis[, expl.variable[i]], bis$mass, main = var.names[i], ylab = "Mass", 
       xlab= var.names[i]) # plot the explanatory variable against mass 
  abline(model.t, lwd= 2) # add a regression line based off the coefficients 
  dev.off() #close pdf 
}

#Here is where the we put all our results in our results folder. 
table <- data.frame(regr.results)
write.csv(table, paste(an.path, "results.csv"))
