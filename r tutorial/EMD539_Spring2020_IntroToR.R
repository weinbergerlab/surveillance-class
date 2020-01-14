################################################################################
# EMD 539 Intro to PH Surveillance (Spring 2020)                               #
#                                                                              #
#         Date: Friday, January 17,  2020                                      #
#     Current Version Edited by: Kelsie Cassell Jan 13, 2020                  #
#     Original Code by: Kayoko Shioda (kayoko.shioda@yale.edu) Jan 18,2019    #
################################################################################

#------------------------------------------------------------------------------#
# Install and load a package
#------------------------------------------------------------------------------#

install.packages("ggplot2")
library(ggplot2)


#------------------------------------------------------------------------------#
# Set a working directory
#------------------------------------------------------------------------------#

setwd("~/Documents/Surveillance_TF_2020/IntroR") # Mac
setwd('C:/Users/kayokoshioda/Desktop/Spring2019/EMD539') # Windows


#------------------------------------------------------------------------------#
# Import/Export data 
#------------------------------------------------------------------------------#

meas <- read.csv('meas.csv',header=T)

# if you want to output a dataframe to csv
measles.output = write.csv(meas, "~/Documents/Surveillance_TF_2020/IntroR", row.names = F)

head(meas)
names(meas) # just use names for dataframes
str(meas)

# all of the variables are numeric but if a variable is a factor and you want to make it numeric, you do:
# as.numeric(as.character(dataframe$varX))

#------------------------------------------------------------------------------#
# Sum cases by year 
#------------------------------------------------------------------------------#

sum.47 = sum(meas[which(meas$year == 47),'London'])
mean(meas$London); median(meas$London); sd(meas$London); 

# The aggregate() function will output a new dataframe 
meas.year = setNames(aggregate(meas$London ~ meas$year, FUN = 'sum'), c('year', 'case_sum'))

# Make a more elaborate plot of this using base R 
par(mfrow=c(1,1)) # can alter this to plot multiple panels in one figure
plot(case_sum ~ year, data= meas.year, type="l", col="black", bty="l", main="Yearly measles", ylab="Counts", ylim=c(0,max(case_sum)))

# Practice Selection 
meas.year = meas.year[(meas.year$year >= 44 & meas.year$year <= 48),]


#-----------------------------------------------------------------------------------------------------------------------------------------------
# We can also aggregate by week and year and then add lines with different colors for each year 
#-----------------------------------------------------------------------------------------------------------------------------------------------

meas.year.wk = setNames(aggregate(meas$London ~ meas$year + meas$week, FUN = 'sum'), c('year', 'week', 'case_sum'))

par(mfrow=c(1,1))
max1 = max(meas.year.wk$case_sum)
plot(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == 44),], type="l", col="black", bty="l", main="weekly measles", ylab="Counts", ylim=c(0,max1))
lines(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == 45),], type="l", col="red")
lines(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == 46),], type="l", col="orange")
lines(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == 47),], type="l", col="blue")
lines(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == 48),], type="l", col="purple")
legend(x="topright",legend = c("'44","'45","'46","'47", "'48"), lty=c(1,1,1,1), col=c("black","red","orange","blue", 'purple'), bty="n", cex=0.8)


# Automate the plotting process with a basic for loop 
max1 = max(meas.year.wk$case_sum)
plot(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == 44),], type="l", col="black", bty="l", main="weekly measles", ylab="Counts", ylim=c(0,max1))
for(i in 45:48){
  lines(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == i),], type="l", col="black")
}
legend(x="topright",legend = c("'44","'45","'46","'47", "'48"), lty=c(1,1,1,1), col=c("black","red","orange","blue", 'purple'), bty="n", cex=0.8)

#-----------------------------------------------------------------------------------------------------------------------------------------------
# So we add a variable for color and practice our ifelse() statements 
#-----------------------------------------------------------------------------------------------------------------------------------------------

meas.year.wk$col1 = ifelse(meas.year.wk$year == 44, 'black',
                           ifelse(meas.year.wk$year == 45, 'red',
                                  ifelse(meas.year.wk$year == 46,'orange',
                                         ifelse(meas.year.wk$year == 47, 'blue',
                                                ifelse(meas.year.wk$year == 48, 'purple', 'NA')))))

max1 = max(meas.year.wk$case_sum)
plot(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == 44),], type="l", col="black", bty="l", main="weekly measles", ylab="Counts", ylim=c(0,max1))
for(i in 45:48){
  lines(case_sum ~ week, data= meas.year.wk[(meas.year.wk$year == i),], type="l", col=col1)
}
legend(x="topright",legend = c("'44","'45","'46","'47", "'48"), lty=c(1,1,1,1), col=c("black","red","orange","blue", 'purple'), bty="n", cex=0.8)










