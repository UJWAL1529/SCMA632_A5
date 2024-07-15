# Set the working directory and verify it
setwd("C:\\A5")
getwd()
install.packages("sf")

#install.packages(dplyr)
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("C:\\A5\\NSSO68.csv")

# Filtering for RJ
df <- data %>%
  filter(state_1 == "RJ")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
RJnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
RJnew$Meals_At_Home <- impute_with_mean(RJnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}
outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  RJnew <- remove_outliers(RJnew, col)
}

# Summarize consumption
RJnew$total_consumption <- rowSums(RJnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- RJnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}
district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "Ganganagar","2" = "Hanumangarh","3" = "Bikaner","4" = "Churu","5" = "Jhunjhunun","6" = "Alwar","7" = "Bharatpur","8" = "Dhaulpur","9" = "Karauli","10" = "Sawai Madhopur","11" = "Dausa","12" = "Jaipur","13" = "Sikar","14" = "Nagaur","15" = "Jodhpur","16" = "Jaisalmer","17" = "Barmer","18" = "Jalor","19" = "Sirohi","20" = "Pali","21" = "Ajmer","22" = "Tonk","23" = "Bundi","24" = "Bhilwara","25" = "Rajsamand","26" = "Udaipur","27" = "Dungarpur","28" = "Banswara","29" = "Chittaurgarh","30" = "Kota","31" = "Baran","32" = "Jhalawar")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

RJnew$District <- as.character(RJnew$District)
RJnew$Sector <- as.character(RJnew$Sector)
RJnew$District <- ifelse(RJnew$District %in% names(district_mapping), district_mapping[RJnew$District], RJnew$District)
RJnew$Sector <- ifelse(RJnew$Sector %in% names(sector_mapping), sector_mapping[RJnew$Sector], RJnew$Sector)

View(RJnew)

hist(RJnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Rajasthan State")

RJ_consumption <- aggregate(total_consumption ~ District, data = RJnew, sum) 
View(RJ_consumption)
??barplot
barplot(RJ_consumption$total_consumption, 
        names.arg = RJ_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed

# b) Plot {'any variable of your choice'} on the Rajasthan state map using NSSO68.csv data

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("C:\\A5\\RAJASTHAN_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data <- merge(RJ_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")


