#------------------------------------------------------------------------
# Data Open Project
# By: Mark Erenberg
#------------------------------------------------------------------------


library(data.table)
library(dplyr)
library(tidyr)
library(lattice)
library(mice)
library(imputeTS)
library(taRifx)
library(ggplot2)
library(plotly)
library(maps)

setwd("C:\\Users\\marke\\Downloads\\Data Open\\Datathon Materials 2")
chemicals <- fread("chemicals.csv",na.strings=c("NA","N/A",""))
industry <- fread("industry_occupation.csv",na.strings=c("NA","N/A",""))
earnings <- fread("earnings.csv",
                  na.strings=c("NA","N/A","",'2,500-','250,000+'))


# look at imported data types:
sapply(earnings,class)

# When the data set was loaded, characters were contained in the table. 
# For example, in the earnings table, 2500- and 250,000+ were used to indicate
# low values or high values outside of the 2500, 250000 boundaries. 

# How many of these values exist in each column?
sapply(earnings, function(x) length(which(x == '2,500-')))
sapply(earnings, function(x) length(which(x == '250,000+')))

# All in all, there is 1905 records with outliers, or approximately 8.7%.
# Since this number is relatively low, we can remove outliers, since we can
# assume that the data is MCAR. 

# This converts the numeric columns to numbers
earnings <- japply(earnings, 6:31, as.numeric)
sapply(earnings,class)

# Convert Year column to date:
earnings$year <- as.Date(as.character(earnings$year), "%Y")
chemicals$year <- as.Date(as.character(chemicals$year), "%Y")
industry$year <- as.Date(as.Date(as.character(industry$year), "%Y"))

# First determine if there is any data missing from data sets
sapply(chemicals, function(x) sum(is.na(x)))
sapply(earnings, function(x) sum(is.na(x)))


# Make all chemical unit measurements in micrograms:
for(i in 1:length(chemicals$value)){
  if(chemicals$unit_measurement[i] == "milligrams/L"){
    chemicals$value[i] == chemicals$value[i]/1000
  }
}


# Reshape the chemical table and subset 2010 or greater:
chemicals2 <- spread(chemicals,key="chemical_species",value="value")
chemicals <- chemicals[chemicals$year >= as.Date(as.character("2009"),"%Y")]


# Visualizations
ggplot(chemicals_2010, aes(x=year, y=value, group=chemical_species)) +
  geom_point(aes(color=chemical_species))

# To plot total chemical values per year:
agg_chem_value <- aggregate(chemicals$value, 
          by=list(Chemical=chemicals$chemical_species,Year=chemicals$year), 
          FUN=sum)
ggplot(agg_chem_value, aes(x=Year, y=x, group=Chemical)) +
  geom_line(aes(color=Chemical)) + 
  geom_point(aes(color=Chemical))+ 
  labs(y="Chemical Value in Micrograms/L",x="Year")+
  ggtitle("Total Chemical Values Per Year")+
  theme(legend.position='bottom')

# Investigate two things: Why are chemical levels high & what impact does this
#                         have?

# Investigating impact of chemical levels on median earnings:
# To plot every industry earnings over time:
earnings2 <- gather(earnings,key="industry",value="median_income",
                    -geo_id,-fips,-county,-year,-total_med)
earnings2$year <- as.Date(as.character(earnings2$year), "%Y")
agg_ind_earn <- aggregate(median_income~year+industry,earnings2,FUN=sum)
ggplot(agg_ind_earn, aes(x=year, y=median_income, group=industry)) +
  geom_line(aes(color=industry)) + 
  geom_point(aes(color=industry))+ 
  labs(y="Median Industry Incomes",x="Year")+
  ggtitle("Total Median Industry Incomes Per Year")+
  theme(legend.position='bottom')

# Because there are lot of dimensions, we can summarize them by looking at 
# the total features:
total_earnings <- earnings[,c(1,2,3,4,6,20,24,27)]
total_earnings2 <- gather(total_earnings,key="industry",value="median_income",
                          -geo_id,-fips,-county,-year)
agg_total_earn <- aggregate(median_income~year+industry,total_earnings2,FUN=sum)
ggplot(agg_total_earn, aes(x=year, y=median_income, group=industry)) +
  geom_line(aes(color=industry)) + 
  geom_point(aes(color=industry))+ 
  labs(y="Total Median Incomes For Industries",x="Year")+
  ggtitle("Total Median Industry Incomes Per Year")

# From this graph, we can see that arts, ent, acc, food, were the only industries
# that did not drastically increase over time, and had lower earnings comparatively.

# So to focus on the three major composite industries:
total_earnings = subset(total_earnings,select=-c(total_arts_ent_acc_food))
total_earnings2 <- gather(total_earnings,key="industry",value="median_income",
                          -geo_id,-fips,-county,-year)
agg_total_earn <- aggregate(median_income~year+industry,total_earnings2,FUN=sum)
ggplot(agg_total_earn, aes(x=year, y=median_income, group=industry)) +
  geom_line(aes(color=industry)) + 
  geom_point(aes(color=industry))+ 
  labs(y="Total Median Incomes For Industries",x="Year")+
  ggtitle("Total Median Industry Incomes Per Year")

# So as we can see the agriculture, fishing, and mining industries
# had the highest increases in median income after the year 2014.

# To examine which industries in agriculture, fishing, and mining were affected:
agri_earnings = earnings[,c(1,2,3,4,7:19)]
agri_earnings2 <- gather(agri_earnings,key="industry",value="median_income",
                          -geo_id,-fips,-county,-year)
agg_agri_earn <- aggregate(median_income~year+industry,agri_earnings2,FUN=sum)
ggplot(agg_agri_earn, aes(x=year, y=median_income, group=industry)) +
  geom_line(aes(color=industry)) + 
  geom_point(aes(color=industry))+ 
  labs(y="Total Median Incomes",x="Year")+
  ggtitle("Total Median Industry Incomes  For Agricultural Industries Per Year")

# From this plot, we can tell the following
#   1) Utilities, mining, information, and rent both dipped significantly in 2013,
#     but then rebounded in 2014 and continued increasing.
#   2) All other remaining industries increased steadilt over the 6 year window.

# To understand why, let's take a look at industry occupation:
agri_industry = industry[,c(1:3,5:12,18)]
agri_industry2 <- gather(agri_industry,key="industry",value="working_pop",
                         -geo_id,-fips,-county,-year)
agg_agri_indus <- aggregate(working_pop~year+industry,agri_industry2,FUN=sum)
ggplot(agg_agri_indus, aes(x=year, y=working_pop, group=industry)) +
  geom_line(aes(color=industry)) + 
  geom_point(aes(color=industry))+ 
  labs(y="Working Population",x="Year")+
  ggtitle("Working Population For Agricultural Industries Per Year")

# So there don't seem to be any significant changes in working population that
# might explain the increase in median earnings.


# Bar Chart for Top Trihalomethane Counties in 2014
tri_2014 <- chemicals[chemicals$chemical_species == 'Trihalomethane' &
                        chemicals$year == as.Date(as.character(2014), "%Y")]
tri_2014$county <- tolower(tri_2014$county)
tri_2014$loc <- paste0(tri_2014$county,"-",tri_2014$state)
agg_tri_2014_county <- aggregate(value~loc,tri_2014,FUN=sum)
agg_tri_2014_county <- agg_tri_2014_county[order(-agg_tri_2014_county$value),]

ggplot(agg_tri_2014_county[1:10,], aes(x=loc,y=value,fill=loc)) +
  geom_bar(stat='identity') +
  labs(y="Chemical Value",x="Location")+
  ggtitle("Top Trihalomethane Counties for 2014")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Chloropleth Map of Trihalomethane Across Counties
county_df <- map_data("county")
state_df <- map_data("state")
county_df$county <- gsub(" ", "", county_df$subregion)

# join election and county data
choropleth <- inner_join(county_df, df, by = "subregion")
choropleth <- choropleth[!duplicated(choropleth$order), ]

# Plot Choropleth Maps
ggplot(choropleth, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = win), 
               colour = alpha("white", 1/2), size = 0.1)  +
  geom_polygon(data = state_df, colour = "white", fill = NA) + 
  scale_fill_manual(values = c('blue','red')) +
  theme_void()

