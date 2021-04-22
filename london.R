# ========= Q1 ===========
# Read the dataset
london_crime <- read.csv('london-crime-data.csv', na='')

# Show the structure of the dataset
str(london_crime)

# Amalgamate the month and year variables into a new variable called Date
london_crime$Date <- paste(1,london_crime$month, london_crime$year, sep='/')


# ========= Q2 ===========
# Convert the variable names
# i.e:
#   borough ==> Borough
#   major_category ==> MajorCategory
#   etc.

# List column names first to identify the index of each column
names(london_crime)

# Change variable names
names(london_crime)[2] <- 'Borough'
names(london_crime)[3] <- 'MajorCategory'
names(london_crime)[4] <- 'SubCategory'
names(london_crime)[5] <- 'Value'
names(london_crime)[8] <- 'CrimeDate'

# Show columns names again to check the result
names(london_crime)


# ========= Q3 ===========
# Convert the CrimeDate variable 
# so that it is a variable of type Date
london_crime$CrimeDate <- as.Date(london_crime$CrimeDate, '%d/%m/%Y')

# Check the result
# Show the structure and content of the date variable
str(london_crime$CrimeDate)


# ========= Q4 ===========
# Plot a chart to show the summary of the borough information

# Convert to factor variable
london_crime$Borough <- factor(london_crime$Borough)

# Check the structure
str(london_crime$Borough)

# Plot the data
plot(london_crime$Borough, main="No. of Crime by Borough", 
     xlab='Borough', 
     ylab='No. of crime',
     col='#FFC300')

# Summary the data
summary(london_crime$Borough)

# Comment: 
# The borough has the highest number of crime is Croydon: 5226
# The borough has the lowest number of crime is City of London: 86


# ========= Q5 ===========
# Display the MajorCategory variable data in a pie chart

# Convert to factor variable
london_crime$MajorCategory <- factor(london_crime$MajorCategory)

# Check the structure
str(london_crime$MajorCategory)

# Summary the data
major_category <- summary(london_crime$MajorCategory)

# Plot the data
pie(major_category,  main="The percentage of crime by major category")

# List info to check min, max
major_category

# Comment:
# The major category had the highest level of crimes is 'Criminal Damage': 17727 
# The major category had the lowest level of crimes is 'Sexual Offences': 917 


# ========= Q6 ===========
# Categorise each borough in the London_crime dataset
# into the general area where it lies within London

# Initial Region variable with the default value is NA
london_crime$Region <- NA

# Assign Region value
london_crime$Region[london_crime$Borough == 'Barking and Dagenham'] <- 'East'
london_crime$Region[london_crime$Borough == 'Barnet'] <- 'North'
london_crime$Region[london_crime$Borough == 'Bexley'] <- 'East'
london_crime$Region[london_crime$Borough == 'Brent'] <- 'West'
london_crime$Region[london_crime$Borough == 'Bromley'] <- 'South'
london_crime$Region[london_crime$Borough == 'Camden'] <- 'North'
london_crime$Region[london_crime$Borough == 'Croydon'] <- 'South'
london_crime$Region[london_crime$Borough == 'Ealing'] <- 'West'
london_crime$Region[london_crime$Borough == 'Enfield'] <- 'North'
london_crime$Region[london_crime$Borough == 'Greenwich'] <- 'East'
london_crime$Region[london_crime$Borough == 'Hackney'] <- 'North'
london_crime$Region[london_crime$Borough == 'Hammersmith and Fulham'] <- 'West'
london_crime$Region[london_crime$Borough == 'Haringey'] <- 'North'
london_crime$Region[london_crime$Borough == 'Harrow'] <- 'West'
london_crime$Region[london_crime$Borough == 'Havering'] <- 'East'
london_crime$Region[london_crime$Borough == 'Hillingdon'] <- 'West'
london_crime$Region[london_crime$Borough == 'Hounslow'] <- 'West'
london_crime$Region[london_crime$Borough == 'Islington'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Kensington and Chelsea'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Kingston upon Thames'] <- 'East'
london_crime$Region[london_crime$Borough == 'Lambeth'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Lewisham'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Merton'] <- 'South'
london_crime$Region[london_crime$Borough == 'Newham'] <- 'East'
london_crime$Region[london_crime$Borough == 'Redbridge'] <- 'East'
london_crime$Region[london_crime$Borough == 'Richmond upon Thames'] <- 'West'
london_crime$Region[london_crime$Borough == 'Southwark'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Sutton'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Tower Hamlets'] <- 'South'
london_crime$Region[london_crime$Borough == 'Waltham Forest'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Wandsworth'] <- 'East'
london_crime$Region[london_crime$Borough == 'Westminster'] <- 'Central'

# Check Region is still contains NA value
na_region <- london_crime[is.na(london_crime$Region),]
na_region

# Comment:
# As you can see in the na_region dataset, 
# the Region value of the borough 'City of London' is still NA
# It should be assign to 'Central' value
london_crime$Region[london_crime$Borough == 'City of London'] <- 'Central'

# Recheck again
na_region <- london_crime[is.na(london_crime$Region),]
nrow(na_region)

# Comment:
# nrow(na_region) = 0 ==> Finished the task


# ========= Q7 ===========
# Display which region in London has the highest recorded crime rate

# Convert to factor variable
london_crime$Region <- factor(london_crime$Region)

# Check the structure
str(london_crime$Region)

# Plot the data
plot(london_crime$Region, main="No. of Crime by Region", 
     xlab='Region', 
     ylab='No. of crime',
     col='#FFC300')

# Summary the data
summary(london_crime$Region)

# Comment: 
# The region had the highest number of crimes is Central: 27864
# The region had the lowest number of crimes is South: 16214


# ========= Q8 ===========
# Extract out the subset of data

# The subset of data that had the highest number of crimes
highest_crime_by_region <- subset(london_crime, 
                                  london_crime$Region == 'Central')

# The subset of data that had the lowest level of crimes
lowest_crime_by_region <- subset(london_crime, 
                                  london_crime$Region == 'South')


# Critique and discuss the major crime category of both regions
# Show major category 
str(highest_crime_by_region$MajorCategory)
str(lowest_crime_by_region$MajorCategory)

# Comment:
# Both regions had 9 levels of major crime


# ========= Q9 ===========
# Plot the content of both of your data frames side by side

# Summay the crime by major category
sum_highest <- summary(highest_crime_by_region$MajorCategory)
sum_lowest <- summary(lowest_crime_by_region$MajorCategory)

# Bind both datasets together (prepare for plotting)
both_datasets <- cbind(sum_highest, sum_lowest)

# Plot the data
barplot(both_datasets, beside=T)

title(xlab = "Central region vs South region", col.lab = rgb(0, 0.5, 0))
title(ylab = "The number of crime", col.lab = rgb(0, 0.5, 0))

# Text on the x- axis should be presented vertically


# ========= Q10 ===========
# Using the write.csv() command, 
# save the modified london_crime data frame
# as london-crime-modified.csv
write.csv(london_crime, file = 'london-crime-modified.csv')

