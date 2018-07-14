#  folders structure
projects <- "/Users/namsan/Desktop/Spring\ 2018/PredictiveClass/projects"

# Shortcuts to folders of interest
CleanData <- paste0(projects,"/CleanData")
Dictionaries <- paste0(projects,"/Dictionaries")
RawData <- paste0(projects,"/RawData")
RCode <- paste0(projects,"/RCode")
RData <- paste0(projects,"/RData")
Output <- paste0(projects,"/Output")

tmp.library.list <- c("haven", "zoo", "fUnitRoots", "tseries", "urca", "lmtest", "forecast", "data.table", "readxl","reshape", "quantmod", "ggplot2", "reshape2", "plyr","scales")
for (i in 1:length(tmp.library.list)) {
  if (!tmp.library.list[i] %in% rownames(installed.packages())) {
    install.packages(tmp.library.list[i])
  }
  library(tmp.library.list[i], character.only = TRUE)
}
rm(tmp.library.list)


# =============================================================================
# IMPORT DATA
# =============================================================================
# import data 
incidents.2017 <- paste0(RawData,"/SFIncidents2017.csv")
incidents.2018 <- paste0(RawData,"/SFIncidents2018.csv")
sf.population <- paste0(RawData,"/SFpopulation_by_pddistrict.csv")

incidents2017 <- data.table(read.csv(incidents.2017, sep = ',', stringsAsFactors = F))
incidents2018 <- data.table(read.csv(incidents.2018, sep = ',', stringsAsFactors = F))
sfpopulation <- data.table(read.csv(sf.population, sep = ',', stringsAsFactors = F))

str(incidents2018)

#cases.311 <- paste0(RawData,"/311_Cases.csv")
#cases311 <- data.table(read.csv(cases.311, sep = ',', stringsAsFactors = F))
#str(cases311)

# check if variables from both sets are the same
names(incidents2017) <- tolower(names(incidents2017))
names(incidents2018) <- tolower(names(incidents2018))
names(incidents2017) == names(incidents2018)

# convert date to date format
incidents2017[, date := as.Date(date, format = "%m/%d/%Y")]
incidents2018[, date := as.Date(date, format = "%m/%d/%Y")]

# combine 2 data sets since we want both 2017 & 2018 data)
incidents <- rbind(incidents2017, incidents2018)

# =============================================================================
# CLEANING
# =============================================================================

table(incidents$category)

#codes <- incidents[grep('SECONDARY CODES', category),]

# Take criminal cases only
non.criminal <- c("FAMILY OFFENSES","NON-CRIMINAL","MISSING PERSON"
                  ,"BAD CHECKS","RUNAWAY","RECOVERED VEHICLE", "TREA")

incidents.crime <- incidents[-grep(paste(non.criminal, collapse = "|")
                                   ,category),]

# =============================================================================
# DESCRIPTIVE ANALYSIS
# =============================================================================
str(incidents.crime)

# Which district has the most criminal incidents per capita?

incidents.bydistrict <- setDT(data.frame(table(incidents.crime$pddistrict)))

setnames(incidents.bydistrict, c("Var1", "Freq"), c("pddistrict", "frequency"))

## merge with population data
incidents.bydistrict[sfpopulation, on = "pddistrict", population := i.population]

incidents.bydistrict[, crime.per.capita := frequency/population]

#tiff('Crimes per Capita in San Francisco (by District).tiff',units="in", width=6, height=4, res=300)
ggplot(data=incidents.bydistrict, 
       aes(reorder(pddistrict, crime.per.capita),crime.per.capita)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="Numbers of Crimes per Capita", x="Police Department District"
       , title="Crimes per Capita in San Francisco (by District)")  + 
  theme_bw()
#dev.off()

# How many of the incidents have been solved? In which districts? Which categories?
incidents.crime$condition <- ifelse(grepl("UNFOUNDED", incidents.crime$resolution
                                          ,ignore.case = T), "unsolved"
                            ,ifelse(grepl("NONE", incidents.crime$resolution
                                          , ignore.case = T ), "unsolved", "solved"))
# 37.34%
table(incidents.crime$condition) 

## How many incidents/crimes have been solved?

crime.condition <- setDT(data.frame(table(incidents.crime$condition
                                          , incidents.crime$pddistrict)))

setnames(crime.condition, c("Var1", "Var2", "Freq")
         , c("condition", "pddistrict", "frequency"))

crime.condition[sfpopulation, on = "pddistrict", population := i.population]

crime.condition[, crime.per.capita := frequency/population]

crime.condition$condition <- factor(crime.condition$condition
                                    , levels=c("unsolved","solved"))

#tiff('Crimes per Capita in San Francisco (by District)1.tiff',units="in", width=6, height=4, res=300)
ggplot(data=crime.condition, 
       aes(reorder(pddistrict, crime.per.capita),crime.per.capita, fill = condition)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="Numbers of Crimes per Capita", x="Police Department District"
       , title="Crimes per Capita in San Francisco (by District)") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") 
#dev.off()

## Which categories? 
table(incidents.crime$category)

crime.category <- setDT(data.frame(table(incidents.crime$condition
                                          , incidents.crime$category)))
setnames(crime.category, c("Var1", "Var2", "Freq")
         , c("condition", "category", "frequency"))

crime.category$condition <- factor(crime.category$condition
                                    , levels=c("unsolved","solved"))

# There are many categories. Here we plot only the largest frequency categories
crime.category[, sum.freq := sum(frequency), by = category]

crime.category.high <- crime.category[sum.freq > median(crime.category$sum.freq)]

str(crime.category)

#tiff('Crimes by Category in San Francisco.tiff',units="in", width=6, height=4, res=300)
ggplot(data=crime.category.high, 
       aes(reorder(category, frequency),frequency, fill = condition)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="Numbers of Crimes", x="Category Name"
       , title="Crimes by Category in San Francisco") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") 
#dev.off()

crime.category.high[, percentage := (frequency/sum.freq)*100]

#tiff('Crimes by Condition in San Francisco.tiff',units="in", width=6, height=4, res=300)
ggplot(data=crime.category.high, 
       aes(reorder(category, percentage),percentage, fill = condition)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y="Proportion of Crimes", x="Category Name"
       , title="Crimes by Condition in San Francisco") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") 
#dev.off()

## Do some criminal categories have significant differences in freq among districts?
crime.district <- setDT(data.frame(table(incidents.crime$condition
                                         , incidents.crime$category
                                         , incidents.crime$pddistrict)))
setnames(crime.district, c("Var1", "Var2", "Var3", "Freq")
         , c("condition","category", "pddistrict", "frequency"))

# Take "unsolved" criminal cases only
crimes <- c("VEHICLE THEFT","LARCENY/THEFT","VANDALISM"
            ,"ASSAULT", "SUSPICIOUS OCC")

crime.district <- crime.district[grep(paste(crimes, collapse = "|")
                                      ,category),]

crime.district[ , sum.freq := sum(frequency), by = pddistrict]
crime.district[, percentage := (frequency/sum.freq)*100]

#tiff('Particular Crimes by District in San Francisco.tiff',units="in", width=6, height=4, res=300)
ggplot(crime.district,aes(x=pddistrict,y=percentage,fill=category))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "RdYlBu")  +
  labs(y="Percentage", x="District"
       , title="Particular Crimes by District in San Francisco") +
  theme_bw()
#dev.off()

#tiff('Particular Crimes by District in San Francisco.tiff',units="in", width=6, height=4, res=300)
ggplot(crime.district,aes(x=pddistrict,y=frequency,fill=category))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_brewer(palette = "RdYlBu")  +
  labs(y="Count", x="District"
       , title="Particular Crimes by District in San Francisco") +
  theme_bw()
#dev.off()

# What time of the day, day of the week, month of the year has the most criminal incidents? Calendar heatmap

incidents.crime[, frequency := 1]

crime.summary <- incidents.crime[ ,sum(frequency), by= list(date, dayofweek)]

crime.summary <- crime.summary[date < "2018-05-01"]

setnames(crime.summary, "V1", "crime")

#tiff('Crime in San Francisco (time series).tiff',units="in", width=6, height=4, res=300)
ggplot(crime.summary , aes(date, crime)) + 
  geom_line() + 
  scale_y_continuous() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m") +
  labs(y="Count", x="Time"
       , title="Crime in San Francisco (time series)") +
  theme_bw()
#dev.off()

# monthly blox plot in 2017

#monthOrder <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
crime.summary$month <- factor(format(crime.summary$date, "%b"), levels = monthOrder)


crime.summary.2017 <- crime.summary[date < "2018-01-01"]
crime.summary.2017 <- crime.summary[, mean.crime := mean(crime), by = month]


#tiff('Average Crime in San Francisco by Month in 2017.tiff',units="in", width=6, height=4, res=300)
ggplot(crime.summary.2017, aes(month, mean.crime, group = 1)) +
  geom_line()+
  geom_point() +
  ggtitle("Average Crime in San Francisco by Month in 2017") +
  theme_bw()
#dev.off()


crime.summary$dayofweek <- factor(crime.summary$dayofweek, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"))

# day of week
#tiff('Crime in San Francisco by Weekday.tiff',units="in", width=6, height=4, res=300)
ggplot(crime.summary, aes(dayofweek, crime)) +
  geom_boxplot() + stat_boxplot(geom ='errorbar') + 
  ggtitle("Crime in San Francisco by Weekday")
#dev.off()



