# =============================================================================
# Cost and affordability of recommended diets in Rwanda
# using [near] real-time market data
# =============================================================================
#
# Authors: Rhys Manners, Kate Schneider Lecy, James Warner, Eric Matsijo,
#          Hilda Vasanthakaalame, Gilberthe Benimana, David J. Spielman
#
# Published: Food Policy, 141 (2026) 103085
# DOI: https://doi.org/10.1016/j.foodpol.2026.103085
#
# This script estimates the daily cost of the proposed Rwandan food-based
# dietary guidelines (FBDG) at the district-month level using eSoko market
# price data (April 2019 - March 2024), and reproduces the manuscript's
# figures and summary statistics.
#
# -----------------------------------------------------------------------------
# REQUIREMENTS
# -----------------------------------------------------------------------------
#   R version 4.3.3 or newer
#   Packages: tidyverse, readxl, sf, magick, animation, cowplot, zoo,
#             reshape2, tibble, scales, patchwork, tempdisagg
#   Install missing packages with install.packages(c("readxl", "sf", ...))
#
# -----------------------------------------------------------------------------
# INPUTS (expected under Data/)
# -----------------------------------------------------------------------------
#   Data/eSoko/2019-2023 Feb_ALL DATA (1).xlsx ###Available on esoko website
#   Data/eSoko/2023_All Data_eSoko Prices.xlsx ###Available on esoko website
#   Data/eSoko/2024_All Data_eSoko Prices.xlsx ###Available on esoko website
#   Data/Food Items/Food_Items_Raw_010825.xlsx ###In supplementary material
#   Data/Food Items/Food_Items_Check_010426.xlsx ### In supplementary material
#   Data/CPI/CPI_Food.xlsx ###Available from NISR website
#   Data/Wage/RLFS_Quintiles_Merge_2025.xlsx ###Available from NISR website
#

library(tidyverse)   
library(readxl)
library(stats)
library(sf)
library(magick)
library(animation)
library(cowplot)
library(zoo)
library(reshape2)
library(scales)
library(patchwork)
library(tempdisagg)

# -----------------------------------------------------------------------------
# Working directory: edit the path below to point at your local copy of the
# Analysis/ folder (the folder containing Data/ and Results/).
# -----------------------------------------------------------------------------
 setwd("/path/to/Rwanda Diet Costs Study/Analysis/")

# Create the Results/ output folder if it does not exist
if (!dir.exists("Results")) dir.create("Results", recursive = TRUE)
########
#####Data Import and Cleaning
########
data<-read_xlsx("Data/eSoko/2019-2023 Feb_ALL DATA (1).xlsx", sheet="2020-2023")###Importing 2020-2023 data
data_2019<-read_xlsx("Data/eSoko/2019-2023 Feb_ALL DATA (1).xlsx", sheet="2019")###Importing 2019 data
data<-rbind(data, data_2019)
rm(data_2019)

colnames(data)<-c("Province", "District", "Market", "Commodity", "Price", "Date")
data<-na.omit(data)
data$Price<-as.numeric(data$Price)

#####Adding date information to aggregate on
data<-data[!grepl("^44",data$Date),]###Removing random data with dates starting with 44....
data$Date<-as.Date(data$Date, "%m/%d/%Y")

####Adding 2023 data
data<- subset(data, format(Date, "%Y") != "2023")###Removing old 2023 data
data_2023<-read_xlsx("Data/eSoko/2023_All Data_eSoko Prices.xlsx")###Importing 2023 data
data_2023<-data_2023[!grepl("^44",data_2023$Date),]###Removing random data with dates starting with 44....
data_2023$Date<-as.Date(data_2023$Date, "%m/%d/%Y")

colnames(data_2023)<-c("Province", "District", "Market", "Commodity", "Price", "Date")
data_2023<-na.omit(data_2023)
data_2023$Price<-as.numeric(data_2023$Price)

data<-rbind(data, data_2023)
rm(data_2023)

####Adding full 2024 data
Jan_2024<-read_xlsx("Data/eSoko/2024_All Data_eSoko Prices.xlsx", sheet="January_2024")
Feb_2024<-read_xlsx("Data/eSoko/2024_All Data_eSoko Prices.xlsx", sheet="February_2024")
Mar_2024<-read_xlsx("Data/eSoko/2024_All Data_eSoko Prices.xlsx", sheet="March_2024")

data_2024<-rbind(Jan_2024, Feb_2024, Mar_2024)
rm(Jan_2024, Feb_2024, Mar_2024)

data_2024<-data_2024[!grepl("^44",data_2024$Date),]###Removing random data with dates starting with 44....
data_2024$Date<-as.Date(data_2024$Date)
colnames(data_2024)<-c("Province", "District", "Market", "Commodity", "Price", "Date")
data_2024<-na.omit(data_2024)
data_2024$Price<-as.numeric(data_2024$Price)

data<-rbind(data, data_2024)
rm(data_2024)

####Manipulating date data
data$year <- format(data$Date, "%y")
data$month<- format(data$Date, "%m")
data$Date_Update<-as.Date(data$Date, format="%m/%Y")
data <-data[order(data$Date_Update), ]
data$Date_Update_2<-format(data$Date_Update, "%m/%Y")
data$unique_month_value <- as.integer(factor(data$Date_Update_2, levels = unique(data$Date_Update_2)))
data<-data[!data$Date_Update_2=="12/2024",]###Removing December 2024 data, erroneously coded

#####Updating commodity names to English and adding the food groups
commodity_translation<-read_xlsx("Data/Food Items/Food_Items_Raw_010825.xlsx")
data$Commodity_Eng <- commodity_translation$Standard[match(data$Commodity, commodity_translation$Rwa)]
data$Food_Group <- commodity_translation$Food_Group[match(data$Commodity_Eng, commodity_translation$Standard)]
data<-data[data$Food_Group!="Remove",]
data<-na.omit(data)

##Original egg data is price per egg. Assuming each egg is 50g, so multiplying by 20 to get price per kg, like other items
data[data$Commodity_Eng == "Egg", "Price"] <- data[data$Commodity_Eng == "Egg", "Price"] * 20####Check this
data[data$Commodity_Eng == "Egg_improved", "Price"] <- data[data$Commodity_Eng == "Egg_improved", "Price"] * 20
data<-data[data$Commodity_Eng!="Egg_improved",]

####Nutrients that have been checked and the edible portion from USDA
Nutrient_Check<-read_xlsx("Data/Food Items/Food_Items_Check_010426.xlsx")
Nutrient_Check$Edible_Portion_Unified[is.na(Nutrient_Check$Edible_Portion_Unified)]<-90###default all empty data to 90%

#####Aggregating (mean) the energy content for each 'standardised' food item.
agg_df<-aggregate(cbind(Nutrient_Check$Energy_kcal_100g_edible_port, Nutrient_Check$Edible_Portion_Unified, Nutrient_Check$total_grams) ~Standard, data=Nutrient_Check, FUN=mean)
colnames(agg_df)<-c("Standard", "Energy (kcal/100g)", "Edible_Portion_Unified", "total_grams")
agg_df<- agg_df[!duplicated(agg_df$Standard), ]
agg_df$Standard_2<-agg_df$Standard

#Attach Food_Group_Split (Fruit/Vegetable/etc) from nutrient file
split_lookup <- Nutrient_Check[!duplicated(Nutrient_Check$Standard), c("Standard","Food_Group_Split")]
agg_df$Food_Group_Split <- split_lookup$Food_Group_Split[match(agg_df$Standard, split_lookup$Standard)]

########
###Calculating the mean price of each food item by district, year, and month grouping.

df_mean_price<-data %>% 
  group_by (District, Commodity_Eng, year, month, Date_Update_2,unique_month_value, Food_Group) %>% 
  summarise(mean(Price))

agg_df<-merge(df_mean_price, agg_df, by.x="Commodity_Eng", by.y="Standard_2") #merging the price and nutrient information

#################################
#######Conversion from########### 
######nominal to real prices#####
#################################

#####CPI Data. CPI food and beverage rates, taken from NISR, taken for national, rural, and urban areas.
CPI_rate<-read_xlsx("Data/CPI/CPI_Food.xlsx")
CPI_rate<-t(as.data.frame(CPI_rate))
colnames(CPI_rate)<-CPI_rate[1,]
CPI_rate<-CPI_rate[-1,]
CPI_rate<-as.data.frame(CPI_rate)
CPI_rate$date2<-as.Date(as.numeric(rownames(CPI_rate)), format="%Y-%m-%d", origin="1899-12-30")
CPI_rate$date2<-format(as.Date(CPI_rate$date2), "%m/%Y")

#Calculating Mean Annual CPI Values for converting wages from nominal to real
study_period_cpi<-CPI_rate[c(123:182),]
study_period_cpi_date<-study_period_cpi$date2
study_period_cpi<-as.data.frame(lapply(study_period_cpi[,1:3], as.numeric))
study_period_cpi$date2<-study_period_cpi_date

#Rebasing to August 2021
study_period_cpi[,1]<-(study_period_cpi[,1]/ study_period_cpi[29,1])*100
study_period_cpi[,2]<-(study_period_cpi[,2]/ study_period_cpi[29,2])*100 
study_period_cpi[,3]<-(study_period_cpi[,3]/ study_period_cpi[29,3])*100 

study_period_cpi$year<-sub(".*/", "", study_period_cpi$date2)
study_period_cpi<-study_period_cpi %>%
  mutate(
    Urban = as.numeric(Urban),
    Rural = as.numeric(Rural),
    National = as.numeric(National)
  )

aggregated_mean_annual_cpi <- study_period_cpi %>%
  group_by(year) %>%
  summarise(across(matches("Urban|Rural|National"), ~ mean(.[is.numeric(.)], na.rm = TRUE), .names = "mean_{.col}"))

study_period_cpi<-merge(study_period_cpi, aggregated_mean_annual_cpi, by="year")
colnames(study_period_cpi)<-c("Year", "Urban Monthly CPI", "Rural Monthly CPI","National Monthly CPI", "Date", "Mean Annual Urban CPI", "Mean Annual Rural CPI", "Mean Annual National CPI")

####Nominal to real eSoko price
#Converting from nominal to real prices (urban/rural/national prices)
agg_df$urban_real_prices <- (agg_df$`mean(Price)` / as.numeric(study_period_cpi$Urban[match(agg_df$Date_Update_2, study_period_cpi$Date)]))*100
agg_df$rural_real_prices <- (agg_df$`mean(Price)` / as.numeric(study_period_cpi$Rural[match(agg_df$Date_Update_2, study_period_cpi$Date)]))*100
agg_df$national_real_prices <- (agg_df$`mean(Price)` / as.numeric(study_period_cpi$National[match(agg_df$Date_Update_2, study_period_cpi$Date)]))*100
agg_df$nominal_prices<-agg_df$`mean(Price)`

#######Conversion to PPP USD#####
## World Bank annual PPP conversion factors (PA.NUS.PPP), 2019-2024
## https://data.worldbank.org/indicator/PA.NUS.PPP?end=2023&locations=RW&start=2019&view=chart

## Monthly rates interpolated using Denton-Cholette method (tempdisagg package)
## to avoid step-changes at year boundaries.

annual_ppp_values <- c(311.86, 321.47, 300.03, 324.63, 350.49, 355.09)
annual_ppp_years  <- 2019:2024
annual_ppp <- ts(annual_ppp_values, start = 2019, frequency = 1)
monthly_ppp <- predict(td(annual_ppp ~ 1, to = 12, method = "denton-cholette", conversion = "average"))

# Build lookup: MM/YYYY -> monthly PPP rate
monthly_ppp_df <- data.frame(
  date = sprintf("%02d/%04d", rep(1:12, length(annual_ppp_years)), rep(annual_ppp_years, each = 12)),
  ppp_rate = as.numeric(monthly_ppp)
)

# Keep forex for backward compatibility (annual rates, used in wage section)
forex<-as.data.frame(cbind(c("2019", "2020", "2021", "2022", "2023", "2024"), as.numeric(annual_ppp_values)))


########################
#Energy density per 100g
agg_df$Energy_Density <- agg_df$`Energy (kcal/100g)`

########################
#Edible portion
agg_df$Edible_Portion <- agg_df$Edible_Portion_Unified/100.

#######################
#Price per kcal
agg_df$price_kcal_urban<-(agg_df$urban_real_prices*(1/agg_df$Edible_Portion))/(agg_df$Energy_Density*10)
agg_df$price_kcal_rural<-(agg_df$rural_real_prices*(1/agg_df$Edible_Portion))/(agg_df$Energy_Density*10)
agg_df$price_kcal_national<-(agg_df$national_real_prices*(1/agg_df$Edible_Portion))/(agg_df$Energy_Density*10)
agg_df$price_kcal_nominal<-(agg_df$`mean(Price)`*(1/agg_df$Edible_Portion))/(agg_df$Energy_Density*10)

#######################
#Price per gram
agg_df$price_g_urban<-(agg_df$urban_real_prices*(1/agg_df$Edible_Portion))/(agg_df$total_grams*10)
agg_df$price_g_rural<-(agg_df$rural_real_prices*(1/agg_df$Edible_Portion))/(agg_df$total_grams*10)
agg_df$price_g_national<-(agg_df$national_real_prices*(1/agg_df$Edible_Portion))/(agg_df$total_grams*10)
agg_df$price_g_nominal<-(agg_df$`mean(Price)`*(1/agg_df$Edible_Portion))/(agg_df$total_grams*10)

#######################
#Food groups for analysis 
#use Food_Group_Split which keeps Fruit & Vegetable separate. This aligns with Herforth's 3 veg + 2 fruit item split.
agg_df$Food_Group_Analysis <- ifelse(
  agg_df$Food_Group_Split %in% c("Fruit","Vegetable"),
  agg_df$Food_Group_Split,
  agg_df$Food_Group
)

#######################
#Number of foods per group (following Herforth et al. 2024)
#F&V split: 3 vegetables + 2 fruits
assign_food_number <- function(text) {
  if (text == "Starchy") {
    return(2)
  } else if (text == "Vegetable") {
    return(3)
  } else if (text == "Fruit") {
    return(2)
  } else if (text == "Animal_source") {
    return(2)
  } else if (text == "Legumes_nuts_seeds") {
    return(1)
  } else if (text == "Oils_fats") {
    return(1)
  } else {
    return("unknown")
  }
}

agg_df$foods_per_group <- suppressWarnings(as.numeric(sapply(agg_df$Food_Group_Analysis, assign_food_number)))

########################
#Daily kcal requirement (Herforth et al. 2024 methodology)
#Rwanda FBDG grams → kcal via conversion factors, scaled to 2330 kcal

#Conversion factors:
#  Starchy:    rice, single item, EICV7 #1 by expenditure (3.59 kcal/g)
#  Vegetable:  unweighted mean across 16 items (0.324 kcal/g)
#  Fruit:      unweighted mean across 13 items (0.650 kcal/g)
#  ASF:        beef, single item, EICV7 #1 by expenditure (2.84 kcal/g)
#  Legumes:    dry bean, single item, EICV7 #1 by expenditure (3.0 kcal/g)
#  Oils:       peanut oil, single item, EICV7 #1 by expenditure (8.84 kcal/g)
#Single items for groups with low within-group energy density variance;
#unweighted mean for F&V where variance is high (Herforth et al. 2025).

#F&V gram allocation: 429.53g total, 85.9g per item, 3 veg items = 257.72g, 2 fruit items = 171.81g
assign_kcal_requirement <- function(text) {
  sf <- 2330 / (441.75*3.59 + 257.72*0.324 + 171.81*0.650 + 111.19*2.84 + 203.06*3.0 + 51.25*8.84)
  if (text == "Starchy") {
    return(441.75 * 3.59 * sf)
  } else if (text == "Vegetable") {
    return(257.72 * 0.324 * sf)
  } else if (text == "Fruit") {
    return(171.81 * 0.650 * sf)
  } else if (text == "Animal_source") {
    return(111.19 * 2.84 * sf)
  } else if (text == "Legumes_nuts_seeds") {
    return(203.06 * 3.0 * sf)
  } else if (text == "Oils_fats") {
    return(51.25 * 8.84 * sf)
  } else {
    return("unknown")
  }
}

agg_df$daily_kcal_requirement<-suppressWarnings(as.numeric(sapply(agg_df$Food_Group_Analysis, assign_kcal_requirement)))
agg_df$daily_kcal_requirement_item<-(agg_df$daily_kcal_requirement/agg_df$foods_per_group)


#Daily Cost
agg_df$daily_cost_urban<-agg_df$price_kcal_urban*agg_df$daily_kcal_requirement_item
agg_df$daily_cost_rural<-agg_df$price_kcal_rural*agg_df$daily_kcal_requirement_item
agg_df$daily_cost_national<-agg_df$price_kcal_national*agg_df$daily_kcal_requirement_item
agg_df$daily_cost_nominal<-agg_df$price_kcal_nominal*agg_df$daily_kcal_requirement_item


#################################
#######Oil Impute################
#################################

Food_CPI<-read_xlsx("Data/CPI/CPI_Food.xlsx")

# Oil Price CPI Imputation - Replace Fixed Fallback with Period-Specific Imputed Prices

# Extract National CPI row (3rd row) and convert to usable format
national_cpi_row <- Food_CPI[3, ]

# Convert to long format
cpi_data <- data.frame(
  excel_date = as.numeric(names(national_cpi_row)[-1]),  # Exclude first column
  national_cpi = as.numeric(national_cpi_row[-1])        # Exclude first column
)

# Convert Excel dates to R dates and create month/year format
cpi_data$date <- as.Date(cpi_data$excel_date, origin = "1899-12-30")
cpi_data$date_formatted <- format(cpi_data$date, "%m/%Y")

# Remove any rows with missing data and sort by date
cpi_data <- cpi_data[!is.na(cpi_data$national_cpi) & !is.na(cpi_data$date), ]
cpi_data <- cpi_data[order(cpi_data$date), ]

# Get oil data and use January 2022 as starting point (most consistent data)
oil_data <- agg_df[agg_df$Food_Group == "Oils_fats", ]

if(nrow(oil_data) > 0) {
  # Use Junw 2022 as the reference starting point (most consistent data)
  reference_start_period <- "06/2022"
  
  # Find the unique_month_value for June 2022
  jun_2022_data <- agg_df[agg_df$Date_Update_2 == reference_start_period, ]
  
  if(nrow(jun_2022_data) > 0) {
    reference_start_month <- jun_2022_data$unique_month_value[1]
  } else {
    # Fallback to finding first oil data if Jan 2022 not found
    oil_data_sorted <- oil_data[order(oil_data$unique_month_value), ]
    reference_start_month <- oil_data_sorted$unique_month_value[1]
    reference_start_period <- oil_data_sorted$Date_Update_2[1]
    print(paste("January 2022 not found, using first available oil data:", reference_start_period))
  }
  
  # Get data for 6 months starting from June 2022
  reference_months <- reference_start_month:(reference_start_month + 5)
  reference_oil_data <- oil_data[oil_data$unique_month_value %in% reference_months, ]
  
  if(nrow(reference_oil_data) > 0) {
    # Use mean daily_cost_nominal from first 6 months as reference
    reference_oil_price <- mean(reference_oil_data$daily_cost_nominal, na.rm = TRUE)
    reference_periods <- unique(reference_oil_data$Date_Update_2)
    
  } else {
    # If somehow no data in reference months, use all available
    reference_oil_price <- mean(oil_data$daily_cost_nominal, na.rm = TRUE)
    
  }
  
  # Set CPI reference to January 2022
  reference_cpi_period <- "06/2022"
  
} else {
  # Fallback to your known average daily cost
  reference_oil_price <- 1240.3884
  reference_cpi_period <- "06/2022"  # Use Jun 2022 as reference period
}

# Use January 2022 as reference for CPI
reference_cpi_match <- cpi_data[cpi_data$date_formatted == reference_cpi_period, ]

if(nrow(reference_cpi_match) > 0) {
  reference_cpi <- reference_cpi_match$national_cpi[1]
  
} else {
  # If no exact match for Jan 2022, find closest
  # Try to find 2022 data
  cpi_2022 <- cpi_data[grepl("2022", cpi_data$date_formatted), ]
  if(nrow(cpi_2022) > 0) {
    reference_cpi <- cpi_2022$national_cpi[1]
    reference_cpi_period <- cpi_2022$date_formatted[1]
    print(paste("Using closest 2022 period as reference:", reference_cpi_period, "with CPI:", round(reference_cpi, 2)))
  } else {
    # Ultimate fallback
    mid_point <- round(nrow(cpi_data) / 2)
    reference_cpi <- cpi_data$national_cpi[mid_point]
    reference_cpi_period <- cpi_data$date_formatted[mid_point]
    print(paste("Using mid-period as fallback reference:", reference_cpi_period, "with CPI:", round(reference_cpi, 2)))
  }
}

# Create CPI-adjusted fallback prices for each period in your data
all_periods <- unique(agg_df$Date_Update_2)
oil_fallback_prices <- data.frame()

for(period in all_periods) {
  # Find CPI for this period
  period_cpi_match <- cpi_data[cpi_data$date_formatted == period, ]
  
  if(nrow(period_cpi_match) > 0) {
    period_cpi <- period_cpi_match$national_cpi[1]
    
    # Calculate CPI-adjusted fallback price
    cpi_adjustment_factor <- period_cpi / reference_cpi
    fallback_price <- reference_oil_price * cpi_adjustment_factor
    
    oil_fallback_prices <- rbind(oil_fallback_prices, data.frame(
      Date_Update_2 = period,
      fallback_oil_price = fallback_price,
      period_cpi = period_cpi,
      cpi_adjustment_factor = cpi_adjustment_factor,
      stringsAsFactors = FALSE
    ))
  } else {
    # If no CPI match, use the original reference price
    oil_fallback_prices <- rbind(oil_fallback_prices, data.frame(
      Date_Update_2 = period,
      fallback_oil_price = reference_oil_price,
      period_cpi = NA,
      cpi_adjustment_factor = 1.0,
      stringsAsFactors = FALSE
    ))
  }
}


# Create named vector for easy lookup - this replaces your fixed 124.3884
oil_fallback_lookup <- setNames(oil_fallback_prices$fallback_oil_price, 
                                oil_fallback_prices$Date_Update_2)

#################################
#######Wage Data#################
#################################
nom_wages<-read_xlsx('Data/Wage/RLFS_Quintiles_Merge_2025.xlsx')
nom_wages<-as.data.frame(nom_wages[c(1:6),])
rownames(nom_wages)<-nom_wages[,1]
quintiles<-rownames(nom_wages)
nom_wages<-nom_wages[,-1]

#Removing commas from values to allow for them to become numeric
remove_commas_and_convert <- function(x) {
  as.numeric(gsub(",", "", x))
}

nom_wages<-lapply(nom_wages, remove_commas_and_convert)
nom_wages<-as.data.frame(nom_wages)
rownames(nom_wages)<-quintiles
colnames(nom_wages) <- gsub("Rwanda", "National", colnames(nom_wages))

daily_wage_nom<-(nom_wages*12)/365

######Converting nominal to real wages
years_req<-c("2019","2020", "2021", "2022", "2023", "2024")
region_req<-c("National", "Rural", "Urban")

region_list_real<-list()
year_list_real<-list()
region_list_nom<-list()
year_list_nom<-list()
year_list_real_rwf<-list()

for (i in 1:length(years_req)){
  selected_columns <- grep(years_req[i], colnames(nom_wages))
  df_filtered <- nom_wages[, selected_columns, drop = FALSE]
  cpi_filt<-aggregated_mean_annual_cpi[aggregated_mean_annual_cpi$year==years_req[i],]
  
  for (j in 1:length(region_req)){
    
    selected_region<-grep(region_req[j], colnames(df_filtered))
    df_filt_2 <- df_filtered[, selected_region, drop = FALSE]
    cpi_filt_2<-cpi_filt[grep(region_req[j], colnames(cpi_filt))]
    r_wage<-(df_filt_2/as.numeric(cpi_filt_2))*100
    region_list_real[[j]] <- r_wage
    region_list_nom[[j]]<- df_filt_2
    
  }
  
  region_df_real <- do.call(cbind, region_list_real)
  year_list_real[[i]]<-region_df_real/as.numeric(forex[i,2])####Converting to PPP USD
  year_list_real_rwf[[i]]<-region_df_real 
  
  region_df_nom<-do.call(cbind, region_list_nom)
  year_list_nom[[i]]<-region_df_nom/as.numeric(forex[i,2])
  
  
}

###Real daily wages
monthly_df_real <- do.call(cbind, year_list_real)
real_wages<-monthly_df_real
daily_wage_real<-(real_wages*12)/365

###Real daily wages - RWF
monthly_df_real_rwf <- do.call(cbind, year_list_real_rwf)
real_wages_rwf<-monthly_df_real_rwf
daily_wage_real_rwf<-(real_wages_rwf*12)/365


#################################
#######Cost of a recommended diet
#################################
agg_df <- agg_df[order(agg_df$unique_month_value), ] 

#Preparing the loop
district<-unique(agg_df$District)
unique_months<-unique(agg_df$unique_month_value)

#Preparing matrix for storing the price of the healthy diet across the 30 districts and for each of the available months of data
dietary_cost_district_month_urban<-matrix(NA, length(district), length(unique(agg_df$unique_month_value)))
rownames(dietary_cost_district_month_urban)<-district
colnames(dietary_cost_district_month_urban)<-unique(agg_df$Date_Update_2)

dietary_cost_district_month_rural<-matrix(NA, length(district), length(unique(agg_df$unique_month_value)))
rownames(dietary_cost_district_month_rural)<-district
colnames(dietary_cost_district_month_rural)<-unique(agg_df$Date_Update_2)

dietary_cost_district_month_national<-matrix(NA, length(district), length(unique(agg_df$unique_month_value)))
rownames(dietary_cost_district_month_national)<-district
colnames(dietary_cost_district_month_national)<-unique(agg_df$Date_Update_2)

dietary_cost_district_month_nominal<-matrix(NA, length(district), length(unique(agg_df$unique_month_value)))
rownames(dietary_cost_district_month_nominal)<-district
colnames(dietary_cost_district_month_nominal)<-unique(agg_df$Date_Update_2)

#Preparing matrix for storing the missing food groups from the 30 districts and for each of the available months of data
missing_data_groups<-matrix(NA, length(district), length(unique(agg_df$unique_month_value)))
rownames(missing_data_groups)<-district
colnames(missing_data_groups)<-unique(agg_df$Date_Update_2)

temp_food_group_costs_district_urban<-data.frame()
temp_food_group_costs_district_rural<-data.frame()
temp_food_group_costs_district_national<-data.frame()
temp_food_group_costs_district_nominal<-data.frame()

food_items_list<-list()
food_items_selected<-matrix(NA, 12,length(district))
colnames(food_items_selected)<-district


######Initiating the loop, initially on the district
for (i in 1:length(district)){
  
  temp<-agg_df[agg_df$District==district[i],] 
  
  #Preparing an empty dataframe for the group specific prices 
  temp_food_group_costs_urban<-data.frame()
  temp_food_group_costs_rural<-data.frame()
  temp_food_group_costs_national<-data.frame()
  temp_food_group_costs_nominal<-data.frame()
  
  ######Nested loop, looping for each month, within each district
  for (j in 1:length(unique_months)){
    
    temp_month<-temp[temp$unique_month_value==unique_months[j],] 
    
    
    Monthly_Cost<-matrix(NA, 12, 8)#####Matrix for storing the cheapest items in each group and the total costs
    rownames(Monthly_Cost)<-c("Staples","Staples", "Vegetable", "Vegetable", "Vegetable", "Fruit", "Fruit","Legume", "ASF", "ASF", "Oils", "Total")
    colnames(Monthly_Cost)<-c("Item_Urban", "Daily_Cost_Urban", "Item_Rural", "Daily_Cost_Rural","Item_National", "Daily_Cost_National", "Item_Nominal", "Daily_Cost_Nominal")
    
    #######################################
    ###Staples --> two items
    #######################################
    
    starchy<-temp_month[temp_month$Food_Group_Analysis=="Starchy",]
    urban_df <- head(starchy[order(starchy$daily_cost_urban), ], 2)
    rural_df <- head(starchy[order(starchy$daily_cost_rural), ], 2)
    national_df <- head(starchy[order(starchy$daily_cost_national), ], 2)
    nominal_df <- head(starchy[order(starchy$daily_cost_nominal),], 2)
    
    
    ####If insufficient items, set to NA (no duplication of items)
    if (nrow(urban_df) < 2)    { Monthly_Cost[1:2,1:2] <- NA } else { Monthly_Cost[1:2,1:2]<-cbind(urban_df$Commodity_Eng, urban_df$daily_cost_urban) }
    if (nrow(rural_df) < 2)    { Monthly_Cost[1:2,3:4] <- NA } else { Monthly_Cost[1:2,3:4]<-cbind(rural_df$Commodity_Eng, rural_df$daily_cost_rural) }
    if (nrow(national_df) < 2) { Monthly_Cost[1:2,5:6] <- NA } else { Monthly_Cost[1:2,5:6]<-cbind(national_df$Commodity_Eng, national_df$daily_cost_national) }
    if (nrow(nominal_df) < 2)  { Monthly_Cost[1:2,7:8] <- NA } else { Monthly_Cost[1:2,7:8]<-cbind(nominal_df$Commodity_Eng, nominal_df$daily_cost_nominal) }
    
    
    #######################################
    ####Vegetables --> three items
    #######################################
    
    vegetables<-temp_month[temp_month$Food_Group_Analysis=="Vegetable",]
    
    urban_df <- head(vegetables[order(vegetables$daily_cost_urban), ], 3)
    rural_df <- head(vegetables[order(vegetables$daily_cost_rural), ], 3)
    national_df <- head(vegetables[order(vegetables$daily_cost_national), ], 3)
    nominal_df <- head(vegetables[order(vegetables$daily_cost_nominal), ], 3)
    
    ####If insufficient items, set to NA (no duplication of items)
    if (nrow(urban_df) < 3)    { Monthly_Cost[3:5,1:2] <- NA } else { Monthly_Cost[3:5,1:2]<-cbind(urban_df$Commodity_Eng, urban_df$daily_cost_urban) }
    if (nrow(rural_df) < 3)    { Monthly_Cost[3:5,3:4] <- NA } else { Monthly_Cost[3:5,3:4]<-cbind(rural_df$Commodity_Eng, rural_df$daily_cost_rural) }
    if (nrow(national_df) < 3) { Monthly_Cost[3:5,5:6] <- NA } else { Monthly_Cost[3:5,5:6]<-cbind(national_df$Commodity_Eng, national_df$daily_cost_national) }
    if (nrow(nominal_df) < 3)  { Monthly_Cost[3:5,7:8] <- NA } else { Monthly_Cost[3:5,7:8]<-cbind(nominal_df$Commodity_Eng, nominal_df$daily_cost_nominal) }
    
    #######################################
    ####Fruits --> two items
    #######################################
    fruits<-temp_month[temp_month$Food_Group_Analysis=="Fruit",]
    
    urban_df <- head(fruits[order(fruits$daily_cost_urban), ], 2)
    rural_df <- head(fruits[order(fruits$daily_cost_rural), ], 2)
    national_df <- head(fruits[order(fruits$daily_cost_national), ], 2)
    nominal_df <- head(fruits[order(fruits$daily_cost_nominal), ], 2)
    
    ####If insufficient items, set to NA (no duplication of items)
    if (nrow(urban_df) < 2)    { Monthly_Cost[6:7,1:2] <- NA } else { Monthly_Cost[6:7,1:2]<-cbind(urban_df$Commodity_Eng, urban_df$daily_cost_urban) }
    if (nrow(rural_df) < 2)    { Monthly_Cost[6:7,3:4] <- NA } else { Monthly_Cost[6:7,3:4]<-cbind(rural_df$Commodity_Eng, rural_df$daily_cost_rural) }
    if (nrow(national_df) < 2) { Monthly_Cost[6:7,5:6] <- NA } else { Monthly_Cost[6:7,5:6]<-cbind(national_df$Commodity_Eng, national_df$daily_cost_national) }
    if (nrow(nominal_df) < 2)  { Monthly_Cost[6:7,7:8] <- NA } else { Monthly_Cost[6:7,7:8]<-cbind(nominal_df$Commodity_Eng, nominal_df$daily_cost_nominal) }
    
    
    #######################################
    ###Legume --> one item
    #######################################
    lns<-temp_month[temp_month$Food_Group_Analysis=="Legumes_nuts_seeds",]
    urban_df <- head(lns[order(lns$daily_cost_urban), ], 1)
    rural_df <- head(lns[order(lns$daily_cost_rural), ], 1)
    national_df <- head(lns[order(lns$daily_cost_national), ], 1)
    nominal_df <- head(lns[order(lns$daily_cost_nominal), ], 1)
    
    
    ####If insufficient items, set to NA (no duplication of items)
    if (nrow(urban_df) < 1)    { Monthly_Cost[8,1:2] <- NA } else { Monthly_Cost[8,1:2]<-cbind(urban_df$Commodity_Eng, urban_df$daily_cost_urban) }
    if (nrow(rural_df) < 1)    { Monthly_Cost[8,3:4] <- NA } else { Monthly_Cost[8,3:4]<-cbind(rural_df$Commodity_Eng, rural_df$daily_cost_rural) }
    if (nrow(national_df) < 1) { Monthly_Cost[8,5:6] <- NA } else { Monthly_Cost[8,5:6]<-cbind(national_df$Commodity_Eng, national_df$daily_cost_national) }
    if (nrow(nominal_df) < 1)  { Monthly_Cost[8,7:8] <- NA } else { Monthly_Cost[8,7:8]<-cbind(nominal_df$Commodity_Eng, nominal_df$daily_cost_nominal) }
    
    #######################################
    ###Animal_Source --> two items
    #######################################
    asf<-temp_month[temp_month$Food_Group_Analysis=="Animal_source",]
    urban_df <- head(asf[order(asf$daily_cost_urban), ], 2)
    rural_df <- head(asf[order(asf$daily_cost_rural), ], 2)
    national_df <- head(asf[order(asf$daily_cost_national), ], 2)
    nominal_df <- head(asf[order(asf$daily_cost_nominal), ], 2)
    
    ####If insufficient items, set to NA (no duplication of items)
    if (nrow(urban_df) < 2)    { Monthly_Cost[9:10,1:2] <- NA } else { Monthly_Cost[9:10,1:2]<-cbind(urban_df$Commodity_Eng, urban_df$daily_cost_urban) }
    if (nrow(rural_df) < 2)    { Monthly_Cost[9:10,3:4] <- NA } else { Monthly_Cost[9:10,3:4]<-cbind(rural_df$Commodity_Eng, rural_df$daily_cost_rural) }
    if (nrow(national_df) < 2) { Monthly_Cost[9:10,5:6] <- NA } else { Monthly_Cost[9:10,5:6]<-cbind(national_df$Commodity_Eng, national_df$daily_cost_national) }
    if (nrow(nominal_df) < 2)  { Monthly_Cost[9:10,7:8] <- NA } else { Monthly_Cost[9:10,7:8]<-cbind(nominal_df$Commodity_Eng, nominal_df$daily_cost_nominal) }
    
    #######################################
    ###Oil --> one item
    #######################################
    oil<-temp_month[temp_month$Food_Group_Analysis=="Oils_fats",]
    
    urban_df <- head(oil[order(oil$daily_cost_urban), ], 1)
    rural_df <- head(oil[order(oil$daily_cost_rural), ], 1)
    national_df <- head(oil[order(oil$daily_cost_national), ], 1)
    nominal_df <- head(oil[order(oil$daily_cost_nominal), ], 1)
    
    
    #######################################
    ###Missing
    #######################################
    ####Dealing with missing data (for missing food groups)
    # Given value for the number of food items that should be shortlisted
    given_value <- 1
  
    ###Urban
    # Check if the number of rows is less than the given value
    if (nrow(urban_df) < given_value) {
      # Repeat the first row to fill the missing rows
      num_missing_rows <- given_value - nrow(urban_df)
      repeat_rows <- urban_df[rep(1, num_missing_rows), ]
      
      if (num_missing_rows == given_value) {
        current_date <- unique(temp_month$Date_Update_2)[1]
        current_fallback_daily_cost <- oil_fallback_lookup[current_date]
        repeat_rows[is.na(repeat_rows)] <- current_fallback_daily_cost
      }
      
      filled_df <- rbind(urban_df, repeat_rows)
    } else {
      filled_df <- urban_df
    }
    Monthly_Cost[11, 1:2]<-cbind(filled_df$Commodity_Eng, filled_df$daily_cost_urban)
    
    
    ###Rural
    # Check if the number of rows is less than the given value
    if (nrow(rural_df) < given_value) {
      # Repeat the first row to fill the missing rows
      num_missing_rows <- given_value - nrow(rural_df)
      repeat_rows <- rural_df[rep(1, num_missing_rows), ]
      
      if (num_missing_rows == given_value) {
        current_date <- unique(temp_month$Date_Update_2)[1]
        current_fallback_daily_cost <- oil_fallback_lookup[current_date]
        repeat_rows[is.na(repeat_rows)] <- current_fallback_daily_cost
      }
      
      filled_df <- rbind(rural_df, repeat_rows)
    } else {
      filled_df <- rural_df
    }
    
    Monthly_Cost[11, 3:4]<-cbind(filled_df$Commodity_Eng, filled_df$daily_cost_rural)
    
    
    ###National
    # Check if the number of rows is less than the given value
    if (nrow(national_df) < given_value) {
      # Repeat the first row to fill the missing rows
      num_missing_rows <- given_value - nrow(national_df)
      repeat_rows <- national_df[rep(1, num_missing_rows), ]
      
      if (num_missing_rows == given_value) {
        current_date <- unique(temp_month$Date_Update_2)[1]
        current_fallback_daily_cost <- oil_fallback_lookup[current_date]
        repeat_rows[is.na(repeat_rows)] <- current_fallback_daily_cost
      }
      
      filled_df <- rbind(national_df, repeat_rows)
    } else {
      filled_df <- national_df
    }
    
    Monthly_Cost[11,5:6]<-cbind(filled_df$Commodity_Eng, filled_df$daily_cost_national)
    
    
    ###Nominal
    # Check if the number of rows is less than the given value
    if (nrow(national_df) < given_value) {
      # Repeat the first row to fill the missing rows
      num_missing_rows <- given_value - nrow(nominal_df)
      repeat_rows <- nominal_df[rep(1, num_missing_rows), ]
      
      if (num_missing_rows == given_value) {
        current_date <- unique(temp_month$Date_Update_2)[1]
        current_fallback_daily_cost <- oil_fallback_lookup[current_date]
        repeat_rows[is.na(repeat_rows)] <- current_fallback_daily_cost
      }
      
      filled_df <- rbind(nominal_df, repeat_rows)
    } else {
      filled_df <- nominal_df
    }
    
    Monthly_Cost[11,7:8]<-cbind(filled_df$Commodity_Eng, filled_df$daily_cost_nominal)
    
    
    
    #######################################
    ###Calculating the Total Cost
    #######################################
    
    Monthly_Cost[12, 2]<-sum(as.numeric(Monthly_Cost[,2]),na.rm=T)
    Monthly_Cost[12, 4]<-sum(as.numeric(Monthly_Cost[,4]),na.rm=T)
    Monthly_Cost[12, 6]<-sum(as.numeric(Monthly_Cost[,6]),na.rm=T)
    Monthly_Cost[12, 8]<-sum(as.numeric(Monthly_Cost[,8]),na.rm=T)
    
    # If ANY food group item has NA cost, set entire district-month to NA
    has_incomplete_diet <- any(is.na(as.numeric(Monthly_Cost[1:11, 8])))
    
    if (has_incomplete_diet) {
      dietary_cost_district_month_urban[i, j] <- NA
      dietary_cost_district_month_rural[i, j] <- NA
      dietary_cost_district_month_national[i, j] <- NA
      dietary_cost_district_month_nominal[i, j] <- NA
    } else {
      dietary_cost_district_month_urban[i, j]<-as.numeric(Monthly_Cost[12,2])
      dietary_cost_district_month_rural[i, j]<-as.numeric(Monthly_Cost[12,4])
      dietary_cost_district_month_national[i, j]<-as.numeric(Monthly_Cost[12,6])
      dietary_cost_district_month_nominal[i, j]<-as.numeric(Monthly_Cost[12,8])
    }
    
    #####Dropping diets costing less than 125RWF  -- no data collected. July 2020 has weird results.
    dietary_cost_district_month_urban[dietary_cost_district_month_urban<=125]<-NA
    dietary_cost_district_month_rural[dietary_cost_district_month_rural<=125]<-NA
    dietary_cost_district_month_national[dietary_cost_district_month_national<=125]<-NA
    dietary_cost_district_month_nominal[dietary_cost_district_month_nominal<=125]<-NA
    
    temp_food_group_costs_urban<-rbind(temp_food_group_costs_urban, Monthly_Cost[,1:2])
    temp_food_group_costs_rural<-rbind(temp_food_group_costs_rural, Monthly_Cost[,3:4])
    temp_food_group_costs_national<-rbind(temp_food_group_costs_national, Monthly_Cost[,5:6])
    temp_food_group_costs_nominal<-rbind(temp_food_group_costs_nominal, Monthly_Cost[,7:8])
    
    tot <- temp_food_group_costs_nominal[grep("Total", temp_food_group_costs_nominal$Item_Nominal), ]
    
    total_columns <- grep("Total", colnames(temp_food_group_costs_nominal), value = TRUE)
    
    
    
    #########
    ###Storing food items for the heatmap
    #
    food_items_selected[,i]<-as.vector(Monthly_Cost[,7])
    
  }
  temp_food_group_costs_district_urban<-rbind(temp_food_group_costs_district_urban, temp_food_group_costs_urban)
  temp_food_group_costs_district_rural<-rbind(temp_food_group_costs_district_rural, temp_food_group_costs_rural)
  temp_food_group_costs_district_national<-rbind(temp_food_group_costs_district_national, temp_food_group_costs_national)
  temp_food_group_costs_district_nominal<-rbind(temp_food_group_costs_district_nominal, temp_food_group_costs_nominal)
  
  food_items_list[[i]]<-temp_food_group_costs_nominal

}



##################################
#######Conversion to Current PPP USD (Denton-Cholette monthly rates)#####
## Following FAO/CoAHD methodology: nominal LCU -> current PPP dollars
## Using Denton-Cholette interpolated monthly PPP rates (defined above)
##################################
dietary_cost_district_month_urban<-as.data.frame(dietary_cost_district_month_urban)
dietary_cost_district_month_rural<-as.data.frame(dietary_cost_district_month_rural)
dietary_cost_district_month_national<-as.data.frame(dietary_cost_district_month_national)
dietary_cost_district_month_nominal<-as.data.frame(dietary_cost_district_month_nominal)

# Helper function: convert a district-month matrix to PPP using Denton monthly rates
convert_to_ppp <- function(df_rwf) {
  df_ppp <- df_rwf
  for(col_name in colnames(df_rwf)) {
    rate <- monthly_ppp_df$ppp_rate[monthly_ppp_df$date == col_name]
    if(length(rate) == 1) {
      df_ppp[[col_name]] <- df_rwf[[col_name]] / rate
    }
  }
  return(df_ppp)
}

#### Real RWF -> PPP (constant PPP, for reference)
dietary_cost_district_month_urban_final   <- convert_to_ppp(dietary_cost_district_month_urban)
dietary_cost_district_month_rural_final   <- convert_to_ppp(dietary_cost_district_month_rural)
dietary_cost_district_month_national_final <- convert_to_ppp(dietary_cost_district_month_national)

#### Nominal RWF -> Current PPP USD (FAO/CoAHD methodology)
## This is the primary PPP measure: nominal local currency / PPP rate
dietary_cost_district_month_nominal_ppp <- convert_to_ppp(dietary_cost_district_month_nominal)

# For backward compatibility, keep _nominal_final as well
dietary_cost_district_month_nominal_final <- dietary_cost_district_month_nominal_ppp

run_date <- format(Sys.Date(), "%d%m%y")
write.csv(dietary_cost_district_month_urban_final, paste0("Results/CoHD_District_Month_Urban_PPP_real_kcal_", run_date, ".csv"))
write.csv(dietary_cost_district_month_rural_final, paste0("Results/CoHD_District_Month_Rural_PPP_real_kcal_", run_date, ".csv"))
write.csv(dietary_cost_district_month_national_final, paste0("Results/CoHD_District_Month_National_real_kcal_", run_date, ".csv"))
write.csv(dietary_cost_district_month_nominal_ppp, paste0("Results/CoHD_District_Month_Nominal_CurrentPPP_kcal_", run_date, ".csv"))
write.csv(dietary_cost_district_month_nominal, paste0("Results/CoHD_District_Month_Nominal_Local_kcal_", run_date, ".csv"))


######################
####Summary Stats####
######################

# Annual mean, SD, and year-on-year change (national nominal)
nom_monthly <- colMeans(dietary_cost_district_month_nominal, na.rm = TRUE)
nom_years <- sub(".*/", "", names(nom_monthly))
# Exclude Jul 2020 due to levels of missing data, likely artefacet of COVID-19 restrictions
nom_monthly_clean <- nom_monthly[names(nom_monthly) != "07/2020"]
nom_years_clean <- sub(".*/", "", names(nom_monthly_clean))

cat("\n=== ANNUAL NATIONAL NOMINAL COST (RWF/day) ===\n")
cat(sprintf("%-6s  %8s  %8s  %10s\n", "Year", "Mean", "SD", "YoY Change"))
cat(paste(rep("-", 40), collapse = ""), "\n")
prev_mean <- NA
for (yr in sort(unique(nom_years_clean))) {
  v <- nom_monthly_clean[nom_years_clean == yr]
  m <- mean(v, na.rm = TRUE)
  s <- sd(v, na.rm = TRUE)
  yoy <- if (is.na(prev_mean)) "-" else sprintf("%+.1f%%", 100 * (m - prev_mean) / prev_mean)
  cat(sprintf("%-6s  %8.0f  %8.0f  %10s\n", yr, m, s, yoy))
  prev_mean <- m
}

# Helper function for annual summary with configurable decimal places
print_annual_summary <- function(mat, label, decimals = 0) {
  monthly <- colMeans(mat, na.rm = TRUE)
  monthly <- monthly[!grepl("07/2020|07\\.2020", names(monthly))]
  yrs <- sub(".*[/.]", "", names(monthly))
  
  cat(sprintf("\n=== %s ===\n", label))
  fmt_val <- paste0("%8.", decimals, "f")
  cat(sprintf("%-6s  %8s  %8s  %10s\n", "Year", "Mean", "SD", "YoY Change"))
  cat(paste(rep("-", 40), collapse = ""), "\n")
  prev <- NA
  for (yr in sort(unique(yrs))) {
    v <- monthly[yrs == yr]
    m <- mean(v, na.rm = TRUE); s <- sd(v, na.rm = TRUE)
    yoy <- if (is.na(prev)) "-" else sprintf("%+.1f%%", 100 * (m - prev) / prev)
    cat(sprintf(paste0("%-6s  ", fmt_val, "  ", fmt_val, "  %10s\n"), yr, m, s, yoy))
    prev <- m
  }
}

# PPP USD (2 decimal places)
print_annual_summary(dietary_cost_district_month_nominal_ppp, "ANNUAL NATIONAL CURRENT PPP USD", decimals = 2)

# Real RWF (national CPI deflated, 0 decimal places)
print_annual_summary(dietary_cost_district_month_national, "ANNUAL NATIONAL REAL RWF (national CPI)", decimals = 0)


####################################
######Rural-Urban analysis##########
####################################

# Define district classifications (uppercase to match data)
urban_districts <- c("GASABO", "KICUKIRO", "NYARUGENGE", "RUBAVU")
rural_districts <- c("BUGESERA", "KAYONZA", "NGOMA", "NYAGATARE", "RWAMAGANA", "GICUMBI", 
                     "KAMONYI", "MUHANGA", "NYANZA", "RUHANGO", "KARONGI", "GATSIBO", 
                     "KIREHE", "BURERA", "GAKENKE", "RULINDO", "GISAGARA", "NYAMAGABE", 
                     "NYARUGURU", "NGORORERO", "NYABIHU", "NYAMASHEKE", "RUTSIRO", 
                     "MUSANZE", "HUYE", "RUSIZI")

# DATASET 1: Current PPP USD (nominal RWF / PPP, FAO/CoAHD methodology)
# Split nominal PPP by urban/rural districts
urban_data_1 <- dietary_cost_district_month_nominal_ppp[rownames(dietary_cost_district_month_nominal_ppp) %in% urban_districts, ]
rural_data_1 <- dietary_cost_district_month_nominal_ppp[rownames(dietary_cost_district_month_nominal_ppp) %in% rural_districts, ]

# Calculate means and SDs
urban_mean_1 <- colMeans(urban_data_1, na.rm = TRUE)
urban_sd_1 <- apply(urban_data_1, 2, sd, na.rm = TRUE)
urban_national<-as.data.frame(cbind(urban_mean_1, urban_sd_1))
urban_national$date<-rownames(urban_national)
urban_national$date<-as.yearmon(rownames(urban_national), format = "%m/%Y")
urban_national$year <- format(urban_national$date, "%Y")

urban_national %>%
  group_by(year) %>%
  summarize(
    dcm_national_avg = mean(urban_mean_1, na.rm=T),
    dcm_sd_avg = mean(urban_sd_1, na.rm=T)
  )



rural_mean_1 <- colMeans(rural_data_1, na.rm = TRUE)
rural_sd_1 <- apply(rural_data_1, 2, sd, na.rm = TRUE)

urban_national<-as.data.frame(cbind(urban_mean_1, urban_sd_1))
urban_national$date<-rownames(urban_national)
urban_national$date<-as.yearmon(rownames(urban_national), format = "%m/%Y")
urban_national$year <- format(urban_national$date, "%Y")



# Create data frames
urban_df_1 <- data.frame(
  cost_mean = urban_mean_1,
  cost_sd = urban_sd_1,
  date = as.yearmon(names(urban_mean_1), format = "%m/%Y"),
  spectrum = "Urban"
)

rural_df_1 <- data.frame(
  cost_mean = rural_mean_1,
  cost_sd = rural_sd_1,
  date = as.yearmon(names(rural_mean_1), format = "%m/%Y"),
  spectrum = "Rural"
)

# Combine
dcm_national_spectrum <- rbind(rural_df_1, urban_df_1)
dcm_national_spectrum$year <- format(dcm_national_spectrum$date, "%Y")

# DATASET 2: dietary_cost_district_month_national
# Separate urban and rural districts
urban_data_2 <- dietary_cost_district_month_urban[rownames(dietary_cost_district_month_urban) %in% urban_districts, ]
rural_data_2 <- dietary_cost_district_month_rural[rownames(dietary_cost_district_month_rural) %in% rural_districts, ]

# Calculate means and SDs
urban_mean_2 <- colMeans(urban_data_2, na.rm = TRUE)
urban_sd_2 <- apply(urban_data_2, 2, sd, na.rm = TRUE)
rural_mean_2 <- colMeans(rural_data_2, na.rm = TRUE)
rural_sd_2 <- apply(rural_data_2, 2, sd, na.rm = TRUE)

# Create data frames
urban_df_2 <- data.frame(
  cost_mean = urban_mean_2,
  cost_sd = urban_sd_2,
  date = as.yearmon(names(urban_mean_2), format = "%m/%Y"),
  spectrum = "Urban"
)

rural_df_2 <- data.frame(
  cost_mean = rural_mean_2,
  cost_sd = rural_sd_2,
  date = as.yearmon(names(rural_mean_2), format = "%m/%Y"),
  spectrum = "Rural"
)

# Combine
dcm_nominal_spectrum <- rbind(rural_df_2, urban_df_2)
dcm_nominal_spectrum$year <- format(dcm_nominal_spectrum$date, "%Y")

# DATASET 3: dietary_cost_district_month_nominal
# Separate urban and rural districts
urban_data_3 <- dietary_cost_district_month_nominal[rownames(dietary_cost_district_month_nominal) %in% urban_districts, ]
rural_data_3 <- dietary_cost_district_month_nominal[rownames(dietary_cost_district_month_nominal) %in% rural_districts, ]

# Calculate means and SDs
urban_mean_3 <- colMeans(urban_data_3, na.rm = TRUE)
urban_sd_3 <- apply(urban_data_3, 2, sd, na.rm = TRUE)
rural_mean_3 <- colMeans(rural_data_3, na.rm = TRUE)
rural_sd_3 <- apply(rural_data_3, 2, sd, na.rm = TRUE)

# Create data frames
urban_df_3 <- data.frame(
  cost_mean = urban_mean_3,
  cost_sd = urban_sd_3,
  date = as.yearmon(names(urban_mean_3), format = "%m/%Y"),
  spectrum = "Urban"
)

rural_df_3 <- data.frame(
  cost_mean = rural_mean_3,
  cost_sd = rural_sd_3,
  date = as.yearmon(names(rural_mean_3), format = "%m/%Y"),
  spectrum = "Rural"
)

# Combine
dcm_nominal_2_spectrum <- rbind(rural_df_3, urban_df_3)
dcm_nominal_2_spectrum$year <- format(dcm_nominal_2_spectrum$date, "%Y")

# Print summary statistics
dcm_national_spectrum %>%
  group_by(year, spectrum) %>%
  summarize(
    cost_avg = mean(cost_mean, na.rm = TRUE),
    cost_sd_avg = mean(cost_sd, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  print()


####Annual stats
#PPP
annual_stats_1 <- dcm_national_spectrum %>%
  filter(spectrum=="Urban")%>%
  group_by(year, spectrum) %>%
  summarize(
    annual_mean = mean(cost_mean, na.rm = TRUE),
    annual_sd = mean(cost_sd, na.rm = TRUE),
    n_months = n(),
    .groups = 'drop'
  )%>%
  arrange(year, spectrum)

annual_stats_1 <-annual_stats_1 %>%
  group_by(spectrum) %>%
  arrange(year) %>%
  mutate(yoy_change_pct = ((annual_mean - lag(annual_mean)) / lag(annual_mean)) * 100) %>%
  ungroup()

annual_stats_1%>%
  filter(spectrum=="Urban")

#Real Local Currency
annual_stats_2 <- dcm_nominal_spectrum %>%
  filter(spectrum=="Rural")%>%
  group_by(year, spectrum) %>%
  summarize(
    annual_mean = mean(cost_mean, na.rm = TRUE),
    annual_sd = mean(cost_sd, na.rm = TRUE),
    n_months = n(),
    .groups = 'drop'
  ) %>%
  arrange(year, spectrum)

annual_stats_2 <-annual_stats_2 %>%
  group_by(spectrum) %>%
  arrange(year) %>%
  mutate(yoy_change_pct = ((annual_mean - lag(annual_mean)) / lag(annual_mean)) * 100) %>%
  ungroup()

#Nominal Local Currency
annual_stats_3 <- dcm_nominal_2_spectrum %>%
  filter(spectrum=="Rural")%>%
  group_by(year, spectrum) %>%
  summarize(
    annual_mean = mean(cost_mean, na.rm = TRUE),
    annual_sd = mean(cost_sd, na.rm = TRUE),
    n_months = n(),
    .groups = 'drop'
  ) %>%
  arrange(year, spectrum)

annual_stats_3 <-annual_stats_3 %>%
  group_by(spectrum) %>%
  arrange(year) %>%
  mutate(yoy_change_pct = ((annual_mean - lag(annual_mean)) / lag(annual_mean)) * 100) %>%
  ungroup()

# COMBINED SUMMARY TABLE
print("\n=== COMBINED ANNUAL SUMMARY ===")
combined_annual_summary <- bind_rows(
  annual_stats_1 %>% mutate(currency_type = "Current PPP USD"),
  annual_stats_2 %>% mutate(currency_type = "Real Local Currency"),
  annual_stats_3 %>% mutate(currency_type = "Nominal Local Currency")
) %>%
  select(currency_type, year, spectrum, annual_mean, annual_sd, n_months) %>%
  arrange(currency_type, year, spectrum)

print(combined_annual_summary)

# URBAN-RURAL DIFFERENCES BY YEAR
print("\n=== URBAN-RURAL DIFFERENCES BY YEAR ===")

# Current PPP USD differences
print("\nCurrent PPP USD - Urban vs Rural differences:")
usd_differences <- dcm_national_spectrum %>%
  select(year, spectrum, cost_mean) %>%
  group_by(year, spectrum) %>%
  summarize(annual_avg = mean(cost_mean, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = spectrum, values_from = annual_avg) %>%
  mutate(
    urban_rural_diff = Urban - Rural,
    urban_rural_ratio = Urban / Rural,
    urban_premium_pct = ((Urban - Rural) / Rural) * 100
  ) %>%
  select(year, Rural, Urban, urban_rural_diff, urban_rural_ratio, urban_premium_pct)

print(usd_differences)

# Real Local Currency differences  
print("\nReal Local Currency - Urban vs Rural differences:")
rwf_real_differences <- dcm_nominal_spectrum %>%
  select(year, spectrum, cost_mean) %>%
  group_by(year, spectrum) %>%
  summarize(annual_avg = mean(cost_mean, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = spectrum, values_from = annual_avg) %>%
  mutate(
    urban_rural_diff = Urban - Rural,
    urban_rural_ratio = Urban / Rural,
    urban_premium_pct = ((Urban - Rural) / Rural) * 100
  ) %>%
  select(year, Rural, Urban, urban_rural_diff, urban_rural_ratio, urban_premium_pct)

print(rwf_real_differences)

# Nominal Local Currency differences
print("\nNominal Local Currency - Urban vs Rural differences:")
rwf_nominal_differences <- dcm_nominal_2_spectrum %>%
  select(year, spectrum, cost_mean) %>%
  group_by(year, spectrum) %>%
  summarize(annual_avg = mean(cost_mean, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = spectrum, values_from = annual_avg) %>%
  mutate(
    urban_rural_diff = Urban - Rural,
    urban_rural_ratio = Urban / Rural,
    urban_premium_pct = ((Urban - Rural) / Rural) * 100
  ) %>%
  select(year, Rural, Urban, urban_rural_diff, urban_rural_ratio, urban_premium_pct)

print(rwf_nominal_differences)

# Print summary statistics
print("Current PPP USD - Rural vs Urban averages by year:")
dcm_national_spectrum %>%
  group_by(year, spectrum) %>%
  summarize(
    cost_avg = mean(cost_mean, na.rm = TRUE),
    cost_sd_avg = mean(cost_sd, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  print()


####################################
#######Plotting and secondary analysis
####################################

# Drop July 2020 from line plots only — 19/30 districts had incomplete data
# that month, so the plotted average is unreliable. The data itself is kept
# in the CSVs and downstream seasonality (where it gets interpolated).
dcm_national_spectrum   <- dcm_national_spectrum   %>% filter(date != as.yearmon("Jul 2020"))
dcm_nominal_spectrum    <- dcm_nominal_spectrum    %>% filter(date != as.yearmon("Jul 2020"))
dcm_nominal_2_spectrum  <- dcm_nominal_2_spectrum  %>% filter(date != as.yearmon("Jul 2020"))

###
#######Figure 1 -- Rural/Urban####
p1_spectrum <- ggplot(dcm_national_spectrum, aes(x = date, y = cost_mean, color = spectrum)) +
  geom_errorbar(aes(ymin = pmax(0, cost_mean - cost_sd), ymax = cost_mean + cost_sd),
                width = 0.025, alpha = 0.7) +
  geom_point(size = 2) +
  geom_line(aes(group = spectrum), linewidth = 0.8) +
  labs(x = "", y = "Cost of Recommended Diet \n(Current PPP$)") +
  scale_x_yearmon(format = "%b %Y", breaks = seq(min(dcm_national_spectrum$date), max(dcm_national_spectrum$date), by = 2/12)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, suffix = "")) +
  scale_color_manual(values = c("Rural" = "#2E8B57", "Urban" = "#FF6347")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(hjust = 1, size = 12),
    axis.title.y = element_text(size = 12),
    panel.border = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 3.5)) +
  labs(title = "c)")

p2_spectrum <- ggplot(dcm_nominal_spectrum, aes(x = date, y = cost_mean, color = spectrum)) +
  geom_errorbar(aes(ymin = pmax(0, cost_mean - cost_sd), ymax = cost_mean + cost_sd),
                width = 0.025, alpha = 0.7) +
  geom_point(size = 2) +
  geom_line(aes(group = spectrum), linewidth = 0.8) +
  labs(x = "", y = "Cost of Recommended Diet \n(Real Local Currency Unit (RWF))") +
  scale_x_yearmon(format = "%b %Y", breaks = seq(min(dcm_nominal_spectrum$date), max(dcm_nominal_spectrum$date), by = 2/12)) +
  scale_color_manual(values = c("Rural" = "#2E8B57", "Urban" = "#FF6347")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(hjust = 1, size = 12),
    axis.title.y = element_text(size = 12),
    panel.border = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 1300)) +
  labs(title = "b)")

p3_spectrum <- ggplot(dcm_nominal_2_spectrum, aes(x = date, y = cost_mean, color = spectrum)) +
  geom_errorbar(aes(ymin = pmax(0, cost_mean - cost_sd), ymax = cost_mean + cost_sd),
                width = 0.025, alpha = 0.7) +
  geom_point(size = 2) +
  geom_line(aes(group = spectrum), linewidth = 0.8) +
  labs(x = "", y = "Cost of Recommended Diet \n(Nominal Local Currency Unit (RWF))") +
  scale_x_yearmon(format = "%b %Y", breaks = seq(min(dcm_nominal_2_spectrum$date), max(dcm_nominal_2_spectrum$date), by = 2/12)) +
  scale_color_manual(values = c("Rural" = "#2E8B57", "Urban" = "#FF6347")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(hjust = 1, size = 12),
    axis.title.y = element_text(size = 12),
    panel.border = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 1300)) +
  labs(title = "a)")

# Combine plots
combined_plot_spectrum <- plot_grid(p3_spectrum, p2_spectrum, p1_spectrum, nrow = 3)
print(combined_plot_spectrum)

ggsave("Results/Fig_1.pdf", combined_plot_spectrum,
       width = 10, height = 12, dpi = 300, bg = "white")

###
#######Figure 2
num_intervals <- length(district)           # 30
n_months <- length(unique_months)           # 59 (after dropping July 2020)
month_labels <- unique(agg_df$Date_Update_2) # actual month labels, in order
n_items <- 12                                # 11 food items + 1 total row per month

group_cost_split <- data.frame(matrix(NA, nrow = n_months * n_items, ncol = num_intervals))

# Split the single long column into per-district columns
for (i in 1:num_intervals) {
  start_index <- (i - 1) * (n_months * n_items) + 1
  end_index <- start_index + (n_months * n_items) - 1
  group_cost_split[, i] <- temp_food_group_costs_district_nominal[start_index:end_index, 2]
}

group_cost_split <- apply(group_cost_split, 2, as.numeric)
# Keep NAs as NAs (do NOT convert to 0) so incomplete diets don't bias the totals
group_cost_split <- as.data.frame(group_cost_split)

group_cost_split$food_group <- rep(c("Starchy","Starchy", "Fruits_Vegetables", "Fruits_Vegetables","Fruits_Vegetables","Fruits_Vegetables","Fruits_Vegetables", "Legume", "ASF", "ASF", "Oils", "Total"), n_months)
group_cost_split$month <- rep(1:n_months, each = n_items)
colnames(group_cost_split)[1:num_intervals] <- district

long_data <- group_cost_split %>%
  pivot_longer(cols = all_of(district), names_to = "district", values_to = "value")

long_data$month <- as.yearmon(month_labels[long_data$month], format = "%m/%Y")
long_data$month_unique <- as.integer(factor(long_data$month))

write.csv(group_cost_split, paste0("Results/Group_cost_split_",run_date,".csv"))

# Propagate incomplete diet NAs: if any food group item is NA for a district-month,
# set all that district-month's group costs to NA (matches the total cost filter).
incomplete_dm <- long_data %>%
  group_by(district, month) %>%
  summarise(any_na = any(is.na(value)), .groups = "drop")

long_data <- long_data %>%
  left_join(incomplete_dm, by = c("district","month")) %>%
  mutate(value = ifelse(any_na, NA, value)) %>%
  select(-any_na)

summed_by_food_group <- long_data %>%
  group_by(food_group, month, month_unique, district) %>%
  summarise(total_value = sum(value, na.rm = FALSE), .groups = 'drop')

group_cost_split <- summed_by_food_group

oil_costs<-mean(subset(group_cost_split$total_value, (group_cost_split$food_group=="Oils" & group_cost_split$total_value > 0)))####calculating the mean of oil costs, where data are available.
group_cost_split$total_value<- ifelse(group_cost_split$food_group=="Oils" & group_cost_split$total_value == 0, oil_costs, group_cost_split$total_value)####assigning that mean where data aren't available.

total_per_dm <- group_cost_split %>%
  filter(food_group=="Total")%>%
  group_by(district, month) %>%
  summarise(total_diet_cost = sum(total_value, na.rm = FALSE),
            .groups = "drop") %>%
  mutate(year = format(month, "%Y"))

# Step 2: Annual summary - mean cost, max/min district-month per year
year_summary <- total_per_dm %>%
  group_by(year) %>%
  summarise(
    mean_cost     = mean(total_diet_cost, na.rm = TRUE),
    max_cost      = max(total_diet_cost, na.rm = TRUE),
    max_district  = district[which.max(total_diet_cost)],
    max_month     = as.character(month[which.max(total_diet_cost)]),
    min_cost      = min(total_diet_cost, na.rm = TRUE),
    min_district  = district[which.min(total_diet_cost)],
    min_month     = as.character(month[which.min(total_diet_cost)]),
    .groups = "drop"
  )

print(year_summary)

#Removing the total costs
group_cost_split<-group_cost_split[group_cost_split$food_group!="Total",]

#Add spectrum classification (needed for downstream filters before seasonality section)
urban_districts_list <- c("GASABO", "KICUKIRO", "NYARUGENGE", "RUBAVU")
group_cost_split$spectrum <- ifelse(
  toupper(group_cost_split$district) %in% urban_districts_list,
  "Urban", "Rural"
)



# Diagnostic counts for interpolation are printed below, after the
# interpolation step actually runs (around line 1660).

#Colour assignment
col_stack <- #c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")
  c(
    "#e41a1c",
    "#377eb8",
    "#4daf4a",
    "#984ea3",
    "#ff7f00")

food_groups_labs<-c("ASF"="Animal Sourced Foods", 
                    "Fruits_Vegetables"="Fruits & Vegetables", 
                    "Legume"="Legumes, Pulses & Nuts", 
                    "Oils"="Oils and Fats", 
                    "Starchy"="Starchy Staples")



#######Rural-Urban###############


# Define district classifications (uppercase to match data)
urban_districts <- c("GASABO", "KICUKIRO", "NYARUGENGE", "RUBAVU")
rural_districts <- c("BUGESERA", "KAYONZA", "NGOMA", "NYAGATARE", "RWAMAGANA", "GICUMBI", 
                     "KAMONYI", "MUHANGA", "NYANZA", "RUHANGO", "KARONGI", "GATSIBO", 
                     "KIREHE", "BURERA", "GAKENKE", "RULINDO", "GISAGARA", "NYAMAGABE", 
                     "NYARUGURU", "NGORORERO", "NYABIHU", "NYAMASHEKE", "RUTSIRO", 
                     "MUSANZE", "HUYE", "RUSIZI")

# Add spectrum classification to group_cost_split
# Drop July 2020 from the food group plot only — same reason as the line
# plots above. The data itself stays in group_cost_split (interpolated)
# for the seasonality analysis.
group_cost_split_spectrum <- group_cost_split %>%
  filter(month != as.yearmon("Jul 2020")) %>%
  mutate(
    spectrum = case_when(
      district %in% urban_districts ~ "Urban",
      district %in% rural_districts ~ "Rural",
      TRUE ~ "Unknown"  # For any districts not in either list
    )
  )

spectrum_averages <- group_cost_split_spectrum %>%
  group_by(food_group, month, spectrum) %>%
  summarise(
    avg_cost = mean(total_value, na.rm = TRUE),
    sd_cost = sd(total_value, na.rm = TRUE),
    n_districts = n(),
    .groups = 'drop'
  )

###Monthly percentage change
spectrum_averages %>%
  mutate(year = format(month, "%Y")) %>%
  group_by(food_group, spectrum, year) %>%
  summarise(annual_avg = mean(avg_cost, na.rm = TRUE), .groups = 'drop') %>%
  group_by(food_group, spectrum) %>%
  arrange(year) %>%
  summarise(
    start_year_avg = first(annual_avg),
    end_year_avg = last(annual_avg),
    pct_change_total = ((end_year_avg - start_year_avg) / start_year_avg) * 100,
    .groups = 'drop'
  )

spectrum_averages %>%
  filter(month %in% c(as.yearmon("April 2019"), as.yearmon("Mar 2024")))%>%
  group_by(food_group, spectrum) %>%
  arrange(month) %>%
  summarise(
    start_apr19 = first(avg_cost),
    end_mar24   = last(avg_cost),
    pct_change_total = ((end_mar24 - start_apr19) / start_apr19) * 100,
    .groups = 'drop'
  )

###Relative cost contribution
rcc<-spectrum_averages %>%
  mutate(year = format(month, "%Y")) %>%
  group_by(food_group, spectrum, year) %>%
  summarise(annual_avg = mean(avg_cost, na.rm = TRUE), .groups = 'drop') %>%
  group_by(spectrum, year) %>%
  mutate(
    total_cost = sum(annual_avg, na.rm = TRUE),
    cost_share_pct = (annual_avg / total_cost) * 100
  ) %>%
  select(food_group, spectrum, year, annual_avg, cost_share_pct)


# Color assignment
col_stack <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")

food_groups_labs <- c("ASF" = "Animal Sourced Foods",
                      "Fruits_Vegetables" = "Fruits & Vegetables", 
                      "Legume" = "Legumes, Pulses & Nuts",
                      "Oils" = "Oils and Fats",
                      "Starchy" = "Starchy Staples")


# Option 1: Update your food_groups_labs to match your data exactly
food_groups_labs <- c(
  "ASF" = "Animal Sourced Foods",
  "Fruits_Vegetables" = "Fruits & Vegetables", 
  "Legume" = "Legumes, Pulses & Nuts",  # Changed from "Legumes_nuts_seeds"
  "Oils" = "Oils and Fats",             # Changed from "Oils_fats"
  "Starchy" = "Starchy Staples"
  # Removed "Total" since it's not in your data
)

#Interpolate missing district × food group values (e.g., July 2020 for the 19
#incomplete districts). Uses linear interpolation between adjacent months.
#This fills NA values so downstream analyses (including seasonality) have a
#continuous time series per district × food group, while preserving observed
#data where it exists.


#############################
#######Figure 2 - Rural Urban
#############################

p4_faceted_no_outliers <- ggplot(group_cost_split_spectrum, aes(x = month, y = total_value, fill = spectrum)) +
  geom_boxplot(aes(group = month), outlier.shape = NA) +
  scale_fill_manual(values = c("Rural" = "#2E8B57", "Urban" = "#FF6347")) +
  scale_x_yearmon(format = "%b %Y", breaks = seq(min(group_cost_split_spectrum$month), max(group_cost_split_spectrum$month), by = 4/12)) +
  labs(x = "", y = "Nominal Cost of Recommended Diet Food Groups\nLocal Currency Unit (RWF)", fill = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(hjust = 1, size = 12),
    axis.title.y = element_text(size = 12),
    panel.border = element_blank(),
    plot.title = element_text(size = 12, face = "bold"),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    strip.text = element_text(size = 13),
    legend.position = "bottom"
  ) +
  labs(title = "") +
  ylim(0, 400) +
  facet_grid(food_group ~ spectrum, scales = "free", labeller = labeller(food_group = food_groups_labs))

print(p4_faceted_no_outliers)

ggsave(paste0("Results/Rwanda_Food_Group_Cost_dist_", run_date, ".pdf"), p4_faceted_no_outliers,
       width = 12, height = 15, dpi = 300, bg = "white")

###
#####Figure 3
#######Seasonality — District-Level STL###############
## Runs STL per district x food group, derives Total from parts,
## detects shock years, produces line plots + heatmaps (Figure 3)

# Ensure Total is removed (we derive it from parts)
group_cost_split <- group_cost_split[group_cost_split$food_group != "Total", ]

group_cost_split$year <- format(group_cost_split$month, "%Y")
group_cost_split$dates <- format(group_cost_split$month, "%b %Y")
group_cost_split$spectrum <- ifelse(
  toupper(group_cost_split$district) %in% c("GASABO", "KICUKIRO", "NYARUGENGE", "RUBAVU"),
  "Urban", "Rural"
)
group_cost_split$date_proper <- as.Date(paste0("01 ", group_cost_split$dates), format = "%d %b %Y")

food_groups_stl <- unique(group_cost_split$food_group)
districts_stl   <- unique(group_cost_split$district)

# Province lookup for district ordering in heatmaps
province_lookup <- tribble(
  ~district,       ~province,
  "GASABO",        "Kigali City",
  "KICUKIRO",      "Kigali City",
  "NYARUGENGE",    "Kigali City",
  "BUGESERA",      "Eastern",
  "GATSIBO",       "Eastern",
  "KAYONZA",       "Eastern",
  "KIREHE",        "Eastern",
  "NGOMA",         "Eastern",
  "NYAGATARE",     "Eastern",
  "RWAMAGANA",     "Eastern",
  "BURERA",        "Northern",
  "GAKENKE",       "Northern",
  "GICUMBI",       "Northern",
  "MUSANZE",       "Northern",
  "RULINDO",       "Northern",
  "GISAGARA",      "Southern",
  "HUYE",          "Southern",
  "KAMONYI",       "Southern",
  "MUHANGA",       "Southern",
  "NYAMAGABE",     "Southern",
  "NYANZA",        "Southern",
  "NYARUGURU",     "Southern",
  "RUHANGO",       "Southern",
  "KARONGI",       "Western",
  "NGORORERO",     "Western",
  "NYABIHU",       "Western",
  "NYAMASHEKE",    "Western",
  "RUBAVU",        "Western",
  "RUSIZI",        "Western",
  "RUTSIRO",       "Western"
)
## --- Core STL function ---
run_stl_district <- function(data) {
  data <- data %>% arrange(date_proper)
  if(nrow(data) < 24) return(NULL)
  start_date  <- min(data$date_proper)
  start_year  <- as.numeric(format(start_date, "%Y"))
  start_month <- as.numeric(format(start_date, "%m"))
  ts_data <- ts(data$total_value, start = c(start_year, start_month), frequency = 12)
  decomp  <- stl(ts_data, s.window = "periodic", robust = TRUE)
  seasonal <- decomp$time.series[, "seasonal"]
  data.frame(month_num = as.numeric(cycle(seasonal)), seasonal_value = as.numeric(seasonal)) %>%
    group_by(month_num) %>%
    summarise(seasonal_value = mean(seasonal_value, na.rm = TRUE), .groups = 'drop')
}

run_all_stl <- function(input_data) {
  fg_list <- unique(input_data$food_group)
  dist_list <- unique(input_data$district)
  results <- list()
  for(d in dist_list) {
    for(fg in fg_list) {
      subset_data <- input_data %>% filter(district == d, food_group == fg) %>% arrange(date_proper)
      stl_result <- run_stl_district(subset_data)
      if(is.null(stl_result)) next
      stl_result$district   <- d
      stl_result$food_group <- fg
      stl_result$spectrum   <- unique(subset_data$spectrum)
      stl_result$group_mean <- mean(subset_data$total_value, na.rm = TRUE)
      results[[paste(d, fg, sep = "_")]] <- stl_result
    }
  }
  bind_rows(results)
}

derive_seasonality <- function(abs_df) {
  total_abs <- abs_df %>%
    group_by(district, spectrum, month_num) %>%
    summarise(seasonal_value = sum(seasonal_value, na.rm = TRUE),
              group_mean = sum(group_mean, na.rm = TRUE), .groups = 'drop') %>%
    mutate(food_group = "Total", seasonal_pct = (seasonal_value / group_mean) * 100)
  individual_pct <- abs_df %>% mutate(seasonal_pct = (seasonal_value / group_mean) * 100)
  list(individual = individual_pct, total = total_abs)
}

average_by_spectrum <- function(individual_pct, total_abs) {
  group_avg <- individual_pct %>%
    group_by(food_group, spectrum, month_num) %>%
    summarise(mean_seasonal = mean(seasonal_pct, na.rm = TRUE),
              sd_seasonal = sd(seasonal_pct, na.rm = TRUE),
              n_districts = n(), .groups = 'drop')
  total_avg <- total_abs %>%
    group_by(food_group, spectrum, month_num) %>%
    summarise(mean_seasonal = mean(seasonal_pct, na.rm = TRUE),
              sd_seasonal = sd(seasonal_pct, na.rm = TRUE),
              n_districts = n(), .groups = 'drop')
  result <- bind_rows(group_avg, total_avg)
  result$month <- factor(month.abb[result$month_num], levels = month.abb)
  result
}


group_cost_split <- group_cost_split %>%
  arrange(district, food_group, month) %>%
  group_by(district, food_group) %>%
  mutate(
    interpolated = is.na(total_value),  # flag ORIGINAL NAs before filling
    total_value = zoo::na.approx(total_value, x = as.numeric(month), na.rm = FALSE),
    total_value = zoo::na.locf(total_value, na.rm = FALSE),
    total_value = zoo::na.locf(total_value, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()

cat(sprintf("Interpolated %d district-month-group cells (original NAs).\n",
            sum(group_cost_split$interpolated)))
cat(sprintf("Interpolation complete. NAs after: %d\n",
            sum(is.na(group_cost_split$total_value))))


## --- Shock year detection (leave-one-year-out) ---
cat("--- Shock Year Detection ---\n")
abs_all <- run_all_stl(group_cost_split)
seas_all <- derive_seasonality(abs_all)

baseline_rural_total <- seas_all$total %>%
  filter(spectrum == "Rural") %>%
  group_by(month_num) %>%
  summarise(baseline = mean(seasonal_pct, na.rm = TRUE), .groups = 'drop') %>%
  arrange(month_num) %>% pull(baseline)

all_years_stl <- unique(group_cost_split$year)
stability_results <- data.frame(Year = character(), Correlation = numeric(),
                                Mean_Abs_Diff = numeric(), stringsAsFactors = FALSE)

for(yr in all_years_stl) {
  filtered <- group_cost_split %>% filter(year != yr)
  abs_exc <- run_all_stl(filtered)
  seas_exc <- derive_seasonality(abs_exc)
  exc_pattern <- seas_exc$total %>%
    filter(spectrum == "Rural") %>%
    group_by(month_num) %>%
    summarise(val = mean(seasonal_pct, na.rm = TRUE), .groups = 'drop') %>%
    arrange(month_num) %>% pull(val)
  cr  <- cor(baseline_rural_total, exc_pattern, use = "complete.obs")
  mad <- mean(abs(baseline_rural_total - exc_pattern), na.rm = TRUE)
  stability_results <- rbind(stability_results,
                             data.frame(Year = yr, Correlation = round(cr, 3), Mean_Abs_Diff = round(mad, 2)))
  cat("  Excluded", yr, "— correlation:", round(cr, 3), "\n")
}

stability_results <- stability_results %>% filter(!is.na(Correlation)) %>% arrange(Correlation)
cat("\nStability results:\n"); print(stability_results)

shock_threshold <- 0.80
shock_years <- stability_results$Year[stability_results$Correlation < shock_threshold]
if(length(shock_years) > 0) {
  cat("\nShock years identified:", paste(shock_years, collapse = ", "), "\n")
} else {
  cat("\nNo shock years identified (all correlations >", shock_threshold, ")\n")
}

## --- Final seasonality (excluding shock years) ---
if(length(shock_years) > 0) {
  cat("\nRe-running STL excluding shock years:", paste(shock_years, collapse = ", "), "...\n")
  data_clean <- group_cost_split %>% filter(!year %in% shock_years)
} else {
  data_clean <- group_cost_split
}

abs_final <- run_all_stl(data_clean)
seas_final <- derive_seasonality(abs_final)
all_seasonality <- average_by_spectrum(seas_final$individual, seas_final$total)
if(length(shock_years) > 0) {
  all_seasonality$period <- paste("Excluding", paste(shock_years, collapse = ", "))
} else {
  all_seasonality$period <- "All Years"
}
cat("Final seasonality computed.\n")

## --- Figure 3: Line plots ---
col_stack_season <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
food_groups_labs_season <- c(
  "ASF" = "Animal Sourced Foods", "Fruits_Vegetables" = "Fruits & Vegetables",
  "Legume" = "Legumes, Pulses & Nuts", "Oils" = "Oils and Fats",
  "Starchy" = "Starchy Staples", "Total" = "Total Cost"
)

# Two-line labels for heatmap facet strips
food_groups_labs_heatmap <- c(
  "ASF" = "Animal\nSourced Foods", "Fruits_Vegetables" = "Fruits &\nVegetables",
  "Legume" = "Legumes,\nPulses & Nuts", "Oils" = "Oils\nand Fats",
  "Starchy" = "Starchy\nStaples", "Total" = "Total\nCost"
)

line_theme_season <- theme_minimal() +
  theme(axis.text = element_text(hjust = 1, size = 13),
        axis.title.y = element_text(size = 14.5),
        legend.position = "none",
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(size = 16, face = "bold"))

y_range_s <- range(all_seasonality$mean_seasonal, na.rm = TRUE)
y_lim_s <- c(floor(y_range_s[1]), ceiling(y_range_s[2]))

p_line_rural <- ggplot(all_seasonality %>% filter(spectrum == "Rural"),
                       aes(x = month, y = mean_seasonal, group = food_group, color = food_group)) +
  geom_line(size = 1) + geom_point(size = 1) +
  labs(title = "Rural", x = "", y = "Seasonal Deviation (%)") +
  scale_color_manual(values = c(col_stack_season, "black"), labels = food_groups_labs_season) +
  ylim(y_lim_s) + line_theme_season

p_line_urban <- ggplot(all_seasonality %>% filter(spectrum == "Urban"),
                       aes(x = month, y = mean_seasonal, group = food_group, color = food_group)) +
  geom_line(size = 1) + geom_point(size = 1) +
  labs(title = "Urban", x = "", y = "") +
  scale_color_manual(values = c(col_stack_season, "black"), labels = food_groups_labs_season) +
  ylim(y_lim_s) + line_theme_season

rural_seasonal<-all_seasonality%>%
  filter(spectrum=="Urban")

write.csv(all_seasonality, "Results/Seasonality.csv")

## --- Figure 3: Heatmaps ---
heatmap_data <- bind_rows(
  seas_final$individual %>% select(district, food_group, spectrum, month_num, seasonal_pct),
  seas_final$total %>% select(district, food_group, spectrum, month_num, seasonal_pct)
)
heatmap_data <- heatmap_data %>%
  mutate(district_label = str_to_title(district)) %>%
  left_join(province_lookup, by = "district")
heatmap_data$month_label <- factor(month.abb[heatmap_data$month_num], levels = month.abb)
heatmap_data$food_group_label <- food_groups_labs_heatmap[heatmap_data$food_group]
heatmap_data$food_group_label <- factor(heatmap_data$food_group_label, levels = food_groups_labs_heatmap)

write.csv(heatmap_data, "Results/Seasonality_District.csv")

province_order <- c("Kigali City", "Eastern", "Northern", "Southern", "Western")

rural_order <- heatmap_data %>% filter(spectrum == "Rural") %>%
  distinct(district, district_label, province) %>%
  mutate(province = factor(province, levels = province_order)) %>%
  arrange(province, district_label) %>% pull(district_label)

urban_order <- heatmap_data %>% filter(spectrum == "Urban") %>%
  distinct(district, district_label, province) %>%
  mutate(province = factor(province, levels = province_order)) %>%
  arrange(province, district_label) %>% pull(district_label)

heatmap_rural <- heatmap_data %>% filter(spectrum == "Rural")
heatmap_urban <- heatmap_data %>% filter(spectrum == "Urban")
heatmap_rural$district_label <- factor(heatmap_rural$district_label, levels = rev(rural_order))

# Pad urban heatmap to match rural height
n_padding <- length(rural_order) - length(urban_order)
padding_labels <- paste0("pad_", seq_len(n_padding))
padding_rows <- expand.grid(
  district_label = padding_labels,
  food_group_label = levels(factor(heatmap_urban$food_group_label, levels = food_groups_labs_heatmap)),
  month_label = factor(month.abb, levels = month.abb), stringsAsFactors = FALSE
) %>% mutate(seasonal_pct = NA, district = NA, food_group = NA,
             spectrum = "Urban", month_num = NA, province = NA)

heatmap_urban <- bind_rows(heatmap_urban, padding_rows)
heatmap_urban$district_label <- factor(heatmap_urban$district_label,
                                       levels = rev(c(urban_order, padding_labels)))
heatmap_urban$food_group_label <- factor(heatmap_urban$food_group_label, levels = food_groups_labs_heatmap)

urban_y_labels <- setNames(c(urban_order, rep("", n_padding)), c(urban_order, padding_labels))

heatmap_fill <- scale_fill_gradient2(
  low = "#5ab4ac", mid = "white", high = "#d8b365", midpoint = 0,
  limits = c(-50, 50), oob = squish, na.value = "white", name = "Seasonal\nDeviation (%)")

heatmap_theme <- theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "right",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.key.height = unit(1.2, "cm"),
        panel.grid = element_blank(),
        plot.margin = margin(2, 2, 2, 2))

p_heatmap_rural <- ggplot(heatmap_rural,
                          aes(x = month_label, y = district_label, fill = seasonal_pct)) +
  geom_tile(color = "white", linewidth = 0.2) + heatmap_fill +
  facet_wrap(~ food_group_label, nrow = 1) + labs(x = "", y = "") + heatmap_theme

p_heatmap_urban <- ggplot(heatmap_urban,
                          aes(x = month_label, y = district_label, fill = seasonal_pct)) +
  geom_tile(color = "white", linewidth = 0.2) + heatmap_fill +
  scale_y_discrete(labels = urban_y_labels, drop = FALSE) +
  facet_wrap(~ food_group_label, nrow = 1) + labs(x = "", y = "") + heatmap_theme

## --- Figure 3: Combined ---
p_legend_src <- ggplot(all_seasonality,
                       aes(x = month, y = mean_seasonal, group = food_group, color = food_group)) +
  geom_line() +
  scale_color_manual(values = c(col_stack_season, "black"), labels = food_groups_labs_season) +
  guides(color = guide_legend(title = "", nrow = 1)) +
  theme(legend.position = "bottom", legend.text = element_text(size = 13),
        legend.key = element_blank())
line_legend <- cowplot::get_legend(p_legend_src)

p_heatmap_rural_no_leg <- p_heatmap_rural + theme(legend.position = "none")

p_seasonality_combined <- (p_line_rural | p_line_urban) /
  wrap_elements(line_legend) /
  (p_heatmap_rural_no_leg | p_heatmap_urban) +
  plot_layout(heights = c(3, 0.3, 8))

print(p_seasonality_combined)

ggsave(paste0("Results/Seasonality_Combined_kcal_", run_date, ".pdf"), p_seasonality_combined,
       width = 19, height = 8, dpi = 300, bg = "white")


#######Geographic Analysis##############

########################################
#############Figure 4##################
########################################

# Load shapefile
rwa_shape <- st_read("Data/gadm41_RWA_shp (2)/gadm41_RWA_2.shp")

# Load Great Lakes shapefile
great_lakes <- st_read("Data/GADM/Great Lakes/Great_Lakes_Shape.shp")

# Country labels
country_labels <- c("Burundi", "DR Congo", "Tanzania", "Tanzania", "Uganda")
label_coords <- data.frame(
  Country = country_labels,
  lat = c(-2.7, -1.5, -2.7, -1.2, -1.25),
  lon = c(30.2, 29.0, 30.7, 30.8, 30.)
)

# Prepare annual averages per district x food group (including Total)
group_cost_split_with_totals_map <- bind_rows(
  group_cost_split,
  group_cost_split %>%
    group_by(district, month) %>%
    summarise(total_value = sum(total_value, na.rm = TRUE), .groups = "drop") %>%
    mutate(food_group = "Total")
)

annual_district_fg <- group_cost_split_with_totals_map %>%
  mutate(cal_year = as.numeric(format(as.Date(month), "%Y"))) %>%
  group_by(district, food_group, cal_year) %>%
  summarise(annual_mean = mean(total_value, na.rm = TRUE), n_months = n(), .groups = "drop") %>%
  mutate(district = toupper(district))

# Standardize district names in shapefile
rwa_shape$NAME_2 <- toupper(rwa_shape$NAME_2)
bbox <- st_bbox(rwa_shape)

# Food group labels and colour palettes (same as original maps)
fg_labels_map <- c(
  "ASF"               = "Animal Sourced Foods",
  "Fruits_Vegetables" = "Fruits & Vegetables",
  "Legume"            = "Legumes, Pulses & Nuts",
  "Oils"              = "Oils & Fats",
  "Starchy"           = "Starchy Staples",
  "Total"             = "Total Diet Cost"
)

color_palettes <- list(
  "ASF"               = colorRampPalette(c("white", "#e41a1c")),
  "Fruits_Vegetables" = colorRampPalette(c("white", "#377eb8")),
  "Legume"            = colorRampPalette(c("white", "#4daf4a")),
  "Oils"              = colorRampPalette(c("white", "#984ea3")),
  "Starchy"           = colorRampPalette(c("white", "#ff7f00")),
  "Total"             = colorRampPalette(c("white", "#018571"))
)

fg_order_map <- c("ASF", "Fruits_Vegetables", "Legume", "Oils", "Starchy", "Total")

row_plots <- list()

for (fg in fg_order_map) {
  fg_map_data <- merge(rwa_shape,
                       annual_district_fg %>% filter(food_group == fg),
                       by.x = "NAME_2", by.y = "district", all.x = TRUE)
  
  fg_vals <- annual_district_fg %>% filter(food_group == fg) %>% pull(annual_mean)
  fg_min <- floor(min(fg_vals, na.rm = TRUE) / 10) * 10
  fg_max <- ceiling(max(fg_vals, na.rm = TRUE) / 10) * 10
  
  show_strip <- (fg == fg_order_map[1])
  
  kigali_coords <- data.frame(lon = 30.0588, lat = -1.9441)
  
  p_row <- ggplot() +
    geom_sf(data = great_lakes, fill = "transparent", color = "gray60",
            linewidth = 0.3, alpha = 0.5) +
    geom_sf(data = fg_map_data, aes(fill = annual_mean),
            color = "black", linewidth = 0.15) +
    scale_fill_gradientn(
      colors = color_palettes[[fg]](100),
      limits = c(fg_min, fg_max),
      name = "RWF/day",
      labels = scales::number_format(big.mark = ",")
    ) +
    geom_point(data = kigali_coords, aes(x = lon, y = lat),
               shape = 16, size = 2.5, color = "black", stroke = 1.5) +
    geom_text(data = kigali_coords, aes(x = lon, y = lat, label = "Kigali"),
              vjust = -1, hjust = 0.5, size = 4, fontface = "bold") +
    geom_text(data = label_coords, aes(x = lon, y = lat, label = Country),
              size = 3.5, color = "gray30", fontface = "italic", alpha = 0.8) +
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +
    facet_wrap(~ cal_year, nrow = 1) +
    labs(title = NULL, y = fg_labels_map[fg]) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      axis.title.x      = element_blank(),
      axis.title.y      = element_text(size = 14, face = "bold", angle = 90, vjust = 0.5),
      panel.grid        = element_blank(),
      strip.text        = if (show_strip) element_text(size = 14, face = "bold") else element_blank(),
      legend.position   = "right",
      legend.key.height = unit(1.2, "cm"),
      legend.key.width  = unit(0.5, "cm"),
      legend.text       = element_text(size = 11),
      legend.title      = element_text(size = 12),
      panel.spacing     = unit(0.1, "lines")
    )
  
  row_plots[[fg]] <- p_row
}

combined_maps <- plot_grid(
  row_plots[["ASF"]],
  row_plots[["Fruits_Vegetables"]],
  row_plots[["Legume"]],
  row_plots[["Oils"]],
  row_plots[["Starchy"]],
  row_plots[["Total"]],
  ncol = 1,
  rel_heights = c(1.12, 1, 1, 1, 1, 1)
)

#print(combined_maps)
ggsave(paste0("Results/Rwanda_food_costs_maps_", run_date, ".png"), combined_maps,
       width = 18, height = 18, dpi = 300, bg = "white")

######
#####Border Impacts
border_impact<-dietary_cost_district_month_nominal
rownames(border_impact) <- tolower(rownames(border_impact))

# Convert the first letter of each row name to uppercase
rownames(border_impact) <- sapply(rownames(border_impact), function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
})

border_impact$border<-ifelse(rownames(border_impact) %in% c("Kirehe", "Nyagatare", "Burera", "Gicumbi", "Nyaruguru", "Gisagara", "Bugesera", "Rusizi", "Rusizi","Rubavu"), 1, 0)
border_impact$border_tz<-ifelse(rownames(border_impact) %in% c("Kirehe"), 1, 0)
border_impact$border_ug<-ifelse(rownames(border_impact) %in% c("Nyagatare", "Burera", "Gicumbi"), 1, 0)
border_impact$border_br<-ifelse(rownames(border_impact) %in% c("Nyaruguru", "Gisagara", "Bugesera", "Rusizi"), 1, 0)
border_impact$border_drc<-ifelse(rownames(border_impact) %in% c("Rusizi", "Rubavu"), 1, 0)


# Long format from the cost matrix — only use columns that match MM/YYYY
month_cols <- grep("^[0-9]{2}/[0-9]{4}$", colnames(border_impact), value = TRUE)

bi_long <- border_impact %>%
  tibble::rownames_to_column("district") %>%
  pivot_longer(cols = all_of(month_cols), names_to = "month", values_to = "cost")

# Drop July 2020 (incomplete) and any other NAs
bi_long <- bi_long %>% filter(month != "07/2020", !is.na(cost))

# Helper: paired-by-month test for one border group
# `non_border_districts` is fixed across all calls so the comparison pool
# stays the same. The reported non-border mean is computed over the FULL
# set of months (independent of border group's availability), so it's the
# same across all tests. The paired t-test still operates on months where
# both groups have data.
paired_border_test <- function(data, border_districts, non_border_districts, label) {
  # Non-border monthly means (over the full available month set)
  nb_monthly <- data %>%
    filter(district %in% non_border_districts) %>%
    group_by(month) %>%
    summarise(non_border = mean(cost, na.rm = TRUE), .groups = "drop")
  
  # Border monthly means for this group
  b_monthly <- data %>%
    filter(district %in% border_districts) %>%
    group_by(month) %>%
    summarise(border = mean(cost, na.rm = TRUE), .groups = "drop")
  
  # Reference: full non-border mean across all months
  full_nb_mean <- mean(nb_monthly$non_border, na.rm = TRUE)
  
  # Paired set for the t-test (only months where both have data)
  monthly <- nb_monthly %>%
    inner_join(b_monthly, by = "month") %>%
    filter(!is.na(border), !is.na(non_border)) %>%
    mutate(diff = border - non_border)
  
  tt <- t.test(monthly$border, monthly$non_border, paired = TRUE)
  
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("  Districts: %s\n", paste(border_districts, collapse = ", ")))
  cat(sprintf("  N paired months: %d\n", nrow(monthly)))
  cat(sprintf("  Mean border:     %.1f RWF/day  (SD: %.1f)\n", mean(monthly$border), sd(monthly$border)))
  cat(sprintf("  Mean non-border: %.1f RWF/day  (SD: %.1f)  (full set: %.1f)\n",
              mean(monthly$non_border), sd(monthly$non_border), full_nb_mean))
  cat(sprintf("  Mean difference: %+.1f RWF (%.1f%%)\n",
              mean(monthly$diff),
              100 * mean(monthly$diff) / mean(monthly$non_border)))
  cat(sprintf("  Paired t-test:   t = %.2f, df = %d, p = %.4f\n",
              tt$statistic, tt$parameter, tt$p.value))
  cat(sprintf("  95%% CI:          [%+.1f, %+.1f]\n",
              tt$conf.int[1], tt$conf.int[2]))
  invisible(monthly)
}

# Define non-border districts (those with no international border)
all_districts_lc <- unique(bi_long$district)
border_all_lc <- c("Kirehe","Nyagatare","Burera","Gicumbi","Nyaruguru",
                   "Gisagara","Bugesera","Rusizi","Rubavu")
non_border_districts <- setdiff(all_districts_lc, border_all_lc)

# Filter to non-border districts for the comparison pool
bi_long_nb_pool <- bi_long %>% filter(district %in% c(non_border_districts, border_all_lc))

# Run for each border country — non_border_districts stays fixed across all
paired_border_test(bi_long_nb_pool, c("Kirehe"), non_border_districts, "TANZANIA BORDER (Kirehe)")
paired_border_test(bi_long_nb_pool, c("Nyagatare","Burera","Gicumbi"), non_border_districts, "UGANDA BORDER (Nyagatare, Burera, Gicumbi)")
paired_border_test(bi_long_nb_pool, c("Nyaruguru","Gisagara","Bugesera"), non_border_districts, "BURUNDI BORDER (Nyaruguru, Gisagara, Bugesera)")
paired_border_test(bi_long_nb_pool, c("Rusizi","Rubavu"), non_border_districts, "DRC BORDER (Rusizi, Rubavu)")
paired_border_test(bi_long_nb_pool, border_all_lc, non_border_districts, "ANY INTERNATIONAL BORDER")


##RURAL vs URBAN ######
########################################
urban_impact<-dietary_cost_district_month_nominal
rownames(urban_impact) <- tolower(rownames(urban_impact))

# Convert the first letter of each row name to uppercase
rownames(urban_impact) <- sapply(rownames(urban_impact), function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
})


urban_impact$spectrum[rownames(urban_impact) %in% c("Gasabo", "Kicukiro", "Nyarugenge", "Rubavu")] <- "Urban"#Urban
urban_impact$spectrum[rownames(urban_impact) %in% c("Bugesera", "Kayonza","Ngoma", "Nyagatare", "Rwamagana", "Gicumbi", "Kamonyi", "Muhanga", "Nyanza", "Ruhango", "Karongi",
                                                    "Gatsibo", "Kirehe", "Burera", "Gakenke", "Rulindo", "Gisagara", "Nyamagabe", "Nyaruguru", "Ngororero", "Nyabihu", "Nyamasheke", "Rutsiro", "Musanze", "Huye", "Rusizi")] <- "Rural"     

urban_impact_comp<-melt(urban_impact, id.vars="spectrum")

ru_long <- urban_impact %>%
  tibble::rownames_to_column("district") %>%
  pivot_longer(cols = grep("^[0-9]{2}/[0-9]{4}$", colnames(.), value = TRUE),
               names_to = "month", values_to = "cost") %>%
  filter(month != "07/2020", !is.na(cost), !is.na(spectrum))

ru_monthly <- ru_long %>%
  group_by(month, spectrum) %>%
  summarise(mean_cost = mean(cost, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = spectrum, values_from = mean_cost) %>%
  filter(!is.na(Urban), !is.na(Rural)) %>%
  mutate(diff = Urban - Rural)

tt_ru <- t.test(ru_monthly$Urban, ru_monthly$Rural, paired = TRUE)


########################################
#############Affordability##############
########################################

####Figure 5
monthly_dietary_cost_national_final<-as.data.frame(colMeans(dietary_cost_district_month_nominal, na.rm=T))###If you want to use PPP USD, add "_final", change this if you PPP or nominal/real
monthly_dietary_cost_national_final$year<-sub(".*/", "", rownames(monthly_dietary_cost_national_final))
colnames(monthly_dietary_cost_national_final)<-c("dietary_cost_month_mean", "year")

########
affordability_national<-matrix(NA, nrow(monthly_dietary_cost_national_final), 54)
var_names<-list()

# Iterate through each row of monthly_dietary_cost_national
for (i in seq_len(nrow(monthly_dietary_cost_national_final))) {
  
  filter<-monthly_dietary_cost_national_final[i,]
  current_year <- as.numeric(filter$year)
  
  filtered_wage_data <- daily_wage_nom[, grepl(as.character(current_year), names(daily_wage_nom))]  # Assuming the year is in the column names
  filtered_wage_data$quintile<-rownames(filtered_wage_data)
  
  melted_wage_data <- melt(filtered_wage_data)
  var_names[[i]]<-melted_wage_data[,1:2]
  #melted_wage_data$group<-(melted_wage_data$variable)
  melted_wage_data$name<-(paste0(melted_wage_data$variable, "_", melted_wage_data$quintile))
  affordability_values <- (filter$dietary_cost_month_mean / melted_wage_data$value)*100
  
  affordability_national[i,]<- affordability_values
  
}

var_names<-do.call(rbind,var_names)

colnames(affordability_national)<-melted_wage_data$name
colnames(affordability_national) <- gsub(paste(c("_2019", "_2020", "_2021", "_2022", "_2023", "_2024"), collapse = "|"), "", colnames(affordability_national))
affordability_national<-as.data.frame(affordability_national)
affordability_national$date<-rownames(monthly_dietary_cost_national_final)[1:60]

aff_nat_melt<-melt(affordability_national)

aff_nat_melt$date<-as.yearmon(aff_nat_melt$date, format = "%m/%Y")
aff_nat_melt$date<-as.Date(aff_nat_melt$date, format = "%m/%Y")
aff_nat_melt <- aff_nat_melt %>%
  arrange(date)

aff_nat_melt$quintile<-var_names$quintile
aff_nat_melt$variable2<-var_names$variable
aff_nat_melt$variable2 <- gsub(paste(c("_2019", "_2020", "_2021", "_2022", "_2023", "_2024"), collapse = "|"), "", aff_nat_melt$variable2)



aff_nat_melt2 <- aff_nat_melt[aff_nat_melt$variable2 %in% c("National_Total", "National_Male", "National_Female"), ]
aff_nat_melt_rur_urb <- aff_nat_melt[aff_nat_melt$variable2 %in% c("Rural_Total", "Urban_Total"), ]


########################################
#############Figure 5##################
########################################

aff_pp<-
  aff_nat_melt_rur_urb%>%
  filter(quintile!="Total_Median") %>%
  filter(format(date, "%m/%Y") != "07/2020")  # drop July 2020 (19/30 districts incomplete)

p6<-ggplot(aff_pp, aes(x = date, y = value, color = quintile)) +
  #geom_line() +
  geom_point() +
  geom_smooth(aes(group = variable, color=quintile), method = "loess", se = FALSE) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black")+
  geom_hline(yintercept = 52, linetype = "dashed", color = "red")+
  #geom_smooth(aes(group = variable, color=variable), method = "loess", se = FALSE) +
  labs(x = "", y = "Affordability of Recommended Diets \n(% of Daily Wage)", 
       title = "") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +  
  #scale_x_date(date_labels = "%b %Y", date_breaks = seq(min(aff_nat_melt$date), max(aff_nat_melt$date), by = 2/12)) +
  
  scale_color_manual(values = c("#66c2a5",
                                "#fc8d62",
                                "#8da0cb",
                                "#e78ac3",
                                "#a6d854",
                                "#ffd92f"),
                     labels= c("1st", "2nd", "3rd", "4th", "5th", "Median"),
                     name="") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        axis.text.y = element_text(size=13),
        axis.title.y = element_text(size = 13),
        axis.line.x = element_line(),  # Add x-axis line
        axis.line.y = element_line(), 
        legend.text= element_text(size=11),
        legend.position="bottom",
        legend.direction = "horizontal",
        legend.title= element_text(size=1),
        strip.text = element_text(size = 13))+
  guides(color = "legend")+
  facet_wrap(~ variable2, ncol = 2, labeller = labeller(variable2 = c("National_Female"="Female", 
                                                                      "National_Male" ="Male", 
                                                                      "National_Total" ="National", 
                                                                      "Rural_Female"= "Rural Female", 
                                                                      "Rural_Male"="Rural Male", 
                                                                      "Rural_Total"="Rural",
                                                                      "Urban_Female"="Urban Female", 
                                                                      "Urban_Male" ="Urban Male", 
                                                                      "Urban_Total" ="Urban")))+
  guides(colour=guide_legend(nrow=1))

ggsave(paste0("Results/affordability_kcal_", run_date, ".pdf"), p6,
       width = 14, height = 6, dpi = 300, bg = "white")



########################################
## SENSITIVITY ANALYSIS: EDIBLE PORTION
########################################
## Five scenarios where the food-item-specific edible portion is altered:
## baseline (0%), +5%, +10%, -5%, -10%. Increases capped at 100%.

cat("\n========================================\n")
cat("EDIBLE PORTION SENSITIVITY ANALYSIS\n")
cat("========================================\n")

# Store original edible portion (fraction, 0-1)
original_EP <- agg_df$Edible_Portion

# Define scenarios: name and multiplier
ep_scenarios <- list(
  list(name = "-20%",     delta = -0.20),
  list(name = "-10%",     delta = -0.10),
  list(name = "-5%",      delta = -0.05),
  list(name = "Baseline", delta =  0.00),
  list(name = "+5%",      delta =  0.05),
  list(name = "+10%",     delta =  0.10),
  list(name = "+20%",     delta =  0.20)
)

# Function: re-run least-cost diet calculation for a given EP multiplier
# Returns national monthly mean costs (nominal RWF)
run_sensitivity_scenario <- function(agg_data, ep_delta, oil_fallback) {
  
  # Modify edible portion: multiply by (1 + delta), cap at 1.0
  agg_data$Edible_Portion <- pmin(original_EP * (1 + ep_delta), 1.0)
  
  # Recompute price per kcal (nominal only needed for sensitivity)
  agg_data$price_kcal_nominal <- (agg_data$`mean(Price)` * (1/agg_data$Edible_Portion)) / (agg_data$Energy_Density * 10)
  
  # Recompute daily cost (nominal)
  agg_data$daily_cost_nominal <- agg_data$price_kcal_nominal * agg_data$daily_kcal_requirement_item
  
  # Prepare output matrix
  districts <- unique(agg_data$District)
  months_unique <- unique(agg_data$unique_month_value)
  cost_mat <- matrix(NA, length(districts), length(months_unique))
  rownames(cost_mat) <- districts
  colnames(cost_mat) <- unique(agg_data$Date_Update_2)
  
  # Least-cost diet loop (nominal only)
  for (i in seq_along(districts)) {
    temp <- agg_data[agg_data$District == districts[i], ]
    
    for (j in seq_along(months_unique)) {
      tm <- temp[temp$unique_month_value == months_unique[j], ]
      mc <- rep(NA, 11)  # cost slots for 11 items
      
      # Staples: 2 cheapest
      grp <- tm[tm$Food_Group_Analysis == "Starchy", ]
      sel <- head(grp[order(grp$daily_cost_nominal), ], 2)
      if (nrow(sel) >= 2) mc[1:2] <- sel$daily_cost_nominal
      
      # Vegetables: 3 cheapest
      grp <- tm[tm$Food_Group_Analysis == "Vegetable", ]
      sel <- head(grp[order(grp$daily_cost_nominal), ], 3)
      if (nrow(sel) >= 3) mc[3:5] <- sel$daily_cost_nominal
      
      # Fruits: 2 cheapest
      grp <- tm[tm$Food_Group_Analysis == "Fruit", ]
      sel <- head(grp[order(grp$daily_cost_nominal), ], 2)
      if (nrow(sel) >= 2) mc[6:7] <- sel$daily_cost_nominal
      
      # Legumes: 1 cheapest
      grp <- tm[tm$Food_Group_Analysis == "Legumes_nuts_seeds", ]
      sel <- head(grp[order(grp$daily_cost_nominal), ], 1)
      if (nrow(sel) >= 1) mc[8] <- sel$daily_cost_nominal
      
      # ASF: 2 cheapest
      grp <- tm[tm$Food_Group_Analysis == "Animal_source", ]
      sel <- head(grp[order(grp$daily_cost_nominal), ], 2)
      if (nrow(sel) >= 2) mc[9:10] <- sel$daily_cost_nominal
      
      # Oil: 1 cheapest (with CPI fallback)
      grp <- tm[tm$Food_Group_Analysis == "Oils_fats", ]
      sel <- head(grp[order(grp$daily_cost_nominal), ], 1)
      if (nrow(sel) >= 1) {
        mc[11] <- sel$daily_cost_nominal
      } else {
        current_date <- unique(tm$Date_Update_2)[1]
        if (!is.null(current_date) && current_date %in% names(oil_fallback)) {
          mc[11] <- oil_fallback[current_date]
        }
      }
      
      # Total: only if all 11 items present
      if (!any(is.na(mc))) {
        total <- sum(mc)
        if (total > 125) cost_mat[i, j] <- total
      }
    }
  }
  
  return(cost_mat)
}

# Run all scenarios
sensitivity_results <- list()

for (sc in ep_scenarios) {
  cat(sprintf("  Running scenario: %s ...\n", sc$name))
  mat <- run_sensitivity_scenario(agg_df, sc$delta, oil_fallback_lookup)
  
  # Compute national monthly mean (across all 30 districts)
  nat_monthly <- colMeans(mat, na.rm = TRUE)
  sensitivity_results[[sc$name]] <- nat_monthly
  
  cat(sprintf("    Overall mean: %.1f RWF/day\n", mean(nat_monthly, na.rm = TRUE)))
}

# Restore original EP
agg_df$Edible_Portion <- original_EP

# Build data frame for plotting
sensitivity_plot_df <- do.call(rbind, lapply(names(sensitivity_results), function(sc_name) {
  vals <- sensitivity_results[[sc_name]]
  data.frame(
    date = as.yearmon(names(vals), format = "%m/%Y"),
    cost = as.numeric(vals),
    scenario = sc_name,
    stringsAsFactors = FALSE
  )
}))

# Remove Jul 2020 outlier
sensitivity_plot_df <- sensitivity_plot_df[sensitivity_plot_df$date != as.yearmon("Jul 2020"), ]

# Order scenarios for legend
sensitivity_plot_df$scenario <- factor(
  sensitivity_plot_df$scenario,
  levels = c("-20%", "-10%", "-5%", "Baseline", "+5%", "+10%", "+20%")
)

# Plot
p_sensitivity <- ggplot(sensitivity_plot_df, aes(x = date, y = cost, colour = scenario)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(
    values = c("-20%" = "#053061", "-10%" = "#2166AC", "-5%" = "#67A9CF", "Baseline" = "black",
               "+5%" = "#EF8A62", "+10%" = "#B2182B", "+20%" = "#67001F"),
    name = "Edible Portion\nScenario"
  ) +
  scale_x_yearmon(format = "%b %Y",
                  breaks = seq(min(sensitivity_plot_df$date), max(sensitivity_plot_df$date), by = 4/12)) +
  labs(
    x = NULL,
    y = "Daily Cost of Recommended Diet (RWF)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p_sensitivity)

ggsave("Results/Sensitivity_Edible_Portion.pdf", p_sensitivity,
       width = 10, height = 5, dpi = 300)
ggsave("Results/Sensitivity_Edible_Portion.png", p_sensitivity,
       width = 10, height = 5, dpi = 300)

cat("\nSensitivity plots saved to Results/\n")

# Summary table
cat("\n=== EDIBLE PORTION SENSITIVITY SUMMARY ===\n")
cat(sprintf("%-12s  %10s  %10s  %8s\n", "Scenario", "Mean (RWF)", "SD (RWF)", "vs Base%"))
baseline_mean <- mean(sensitivity_results[["Baseline"]], na.rm = TRUE)
for (sc_name in c("-20%", "-10%", "-5%", "Baseline", "+5%", "+10%", "+20%")) {
  sc_mean <- mean(sensitivity_results[[sc_name]], na.rm = TRUE)
  sc_sd   <- sd(sensitivity_results[[sc_name]], na.rm = TRUE)
  pct_diff <- 100 * (sc_mean - baseline_mean) / baseline_mean
  cat(sprintf("%-12s  %10.1f  %10.1f  %+7.1f%%\n", sc_name, sc_mean, sc_sd, pct_diff))
}


########################################
## HDB vs CoRD: NOMINAL RURAL/URBAN PLOT
########################################
## Supplementary figure comparing the HDB reference diet (method B) against
## the primary CoRD (method A / baseline) by rural-urban spectrum.

cat("\n========================================\n")
cat("HDB vs CoRD COMPARISON PLOT\n")
cat("========================================\n")

# Compute HDB cost matrix directly (avoids dependency on methodology comparison section)
hdb_kcal_targets <- c("Starchy" = 1160, "Vegetable" = 110, "Fruit" = 160,
                      "Animal_source" = 300, "Legumes_nuts_seeds" = 300, "Oils_fats" = 300)
agg_df$kcal_req_hdb     <- hdb_kcal_targets[agg_df$Food_Group_Analysis]
agg_df$kcal_req_item_hdb <- agg_df$kcal_req_hdb / agg_df$foods_per_group
agg_df$daily_cost_hdb_nom <- agg_df$price_kcal_nominal * agg_df$kcal_req_item_hdb

hdb_food_groups <- list(
  list(name = "Starchy", n = 2), list(name = "Vegetable", n = 3),
  list(name = "Fruit", n = 2), list(name = "Animal_source", n = 2),
  list(name = "Legumes_nuts_seeds", n = 1), list(name = "Oils_fats", n = 1)
)

hdb_mat <- matrix(NA, length(district), length(unique_months))
rownames(hdb_mat) <- district
colnames(hdb_mat) <- unique(agg_df$Date_Update_2)

for (i in seq_along(district)) {
  temp_d <- agg_df[agg_df$District == district[i], ]
  for (j in seq_along(unique_months)) {
    tm <- temp_d[temp_d$unique_month_value == unique_months[j], ]
    if (nrow(tm) == 0) next
    costs <- numeric(0)
    any_missing <- FALSE
    for (fg in hdb_food_groups) {
      fg_data <- tm[tm$Food_Group_Analysis == fg$name, ]
      if (nrow(fg_data) < fg$n) {
        if (fg$name == "Oils_fats" && nrow(fg_data) == 0) {
          current_date <- unique(tm$Date_Update_2)[1]
          costs <- c(costs, as.numeric(oil_fallback_lookup[current_date]))
        } else {
          any_missing <- TRUE
        }
      } else {
        ranked <- head(fg_data[order(fg_data$daily_cost_hdb_nom), ], fg$n)
        costs <- c(costs, ranked$daily_cost_hdb_nom)
      }
    }
    if (!any_missing && sum(costs) > 125) hdb_mat[i, j] <- sum(costs)
  }
}

hdb_urb_rows <- rownames(hdb_mat) %in% urban_districts
hdb_rur_rows <- !hdb_urb_rows

hdb_urb_mean <- colMeans(hdb_mat[hdb_urb_rows, , drop = FALSE], na.rm = TRUE)
hdb_urb_sd   <- apply(hdb_mat[hdb_urb_rows, , drop = FALSE], 2, sd, na.rm = TRUE)
hdb_rur_mean <- colMeans(hdb_mat[hdb_rur_rows, , drop = FALSE], na.rm = TRUE)
hdb_rur_sd   <- apply(hdb_mat[hdb_rur_rows, , drop = FALSE], 2, sd, na.rm = TRUE)

hdb_df <- rbind(
  data.frame(cost_mean = hdb_rur_mean, cost_sd = hdb_rur_sd,
             date = as.yearmon(names(hdb_rur_mean), format = "%m/%Y"),
             spectrum = "Rural", method = "HDB", stringsAsFactors = FALSE),
  data.frame(cost_mean = hdb_urb_mean, cost_sd = hdb_urb_sd,
             date = as.yearmon(names(hdb_urb_mean), format = "%m/%Y"),
             spectrum = "Urban", method = "HDB", stringsAsFactors = FALSE)
)

# CoRD data from dcm_nominal_2_spectrum (already computed earlier in script)
cord_df <- dcm_nominal_2_spectrum
cord_df$method <- "CoRD"

# Combine
hdb_cord_df <- rbind(
  cord_df[, c("cost_mean", "cost_sd", "date", "spectrum", "method")],
  hdb_df[, c("cost_mean", "cost_sd", "date", "spectrum", "method")]
)

# Remove Jul 2020
hdb_cord_df <- hdb_cord_df[hdb_cord_df$date != as.yearmon("Jul 2020"), ]

# Interaction label for colour/linetype
hdb_cord_df$group <- paste(hdb_cord_df$method, hdb_cord_df$spectrum)
hdb_cord_df$group <- factor(hdb_cord_df$group,
                            levels = c("CoRD Rural", "CoRD Urban", "HDB Rural", "HDB Urban"))



p_hdb_cord <- ggplot(hdb_cord_df, aes(x = date, y = cost_mean, colour = method)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(
    values = c("CoRD" = "#2E8B57", "HDB" = "#1B4F72"),
    name = NULL
  ) +
  scale_x_yearmon(format = "%b %Y",
                  breaks = seq(min(hdb_cord_df$date), max(hdb_cord_df$date), by = 2/12)) +
  facet_wrap(~ spectrum) +
  labs(
    x = NULL,
    y = "Daily Cost of Recommended Diet\n(Nominal Local Currency Unit (RWF))"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  ) +
  coord_cartesian(ylim = c(0, 1300))

print(p_hdb_cord)

ggsave("Results/HDB_vs_CoRD_Rural_Urban.pdf", p_hdb_cord,
       width = 14, height = 5, dpi = 300)
ggsave("Results/HDB_vs_CoRD_Rural_Urban.png", p_hdb_cord,
       width = 10, height = 5, dpi = 300)

cat("HDB vs CoRD plot saved to Results/\n")

# Annual summary table: CoRD vs HDB
hdb_cord_df$year <- format(hdb_cord_df$date, "%Y")

annual_table <- hdb_cord_df %>%
  group_by(year, method, spectrum) %>%
  summarise(mean_cost = mean(cost_mean, na.rm = TRUE),
            sd_cost = sd(cost_mean, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, spectrum, method)

table_lines <- c(
  "Annual Mean Costs: Rwandan FBDG vs Healthy Food Basket (Nominal RWF/day)",
  "",
  sprintf("%-6s  %-42s  %-42s", "", "Rural", "Urban"),
  sprintf("%-6s  %-16s  %-16s  %-8s  %-16s  %-16s  %-8s",
          "Year", "Rwandan FBDG", "Healthy Food", "Diff", "Rwandan FBDG", "Healthy Food", "Diff"),
  sprintf("%-6s  %-16s  %-16s  %-8s  %-16s  %-16s  %-8s",
          "", "Mean (\u00B1SD)", "Basket (\u00B1SD)", "(%)", "Mean (\u00B1SD)", "Basket (\u00B1SD)", "(%)"),
  paste(rep("-", 100), collapse = "")
)

for (yr in sort(unique(annual_table$year))) {
  cord_rur <- annual_table %>% filter(year == yr, method == "CoRD", spectrum == "Rural")
  hdb_rur  <- annual_table %>% filter(year == yr, method == "HDB", spectrum == "Rural")
  diff_rur <- 100 * (cord_rur$mean_cost - hdb_rur$mean_cost) / hdb_rur$mean_cost
  
  cord_urb <- annual_table %>% filter(year == yr, method == "CoRD", spectrum == "Urban")
  hdb_urb  <- annual_table %>% filter(year == yr, method == "HDB", spectrum == "Urban")
  diff_urb <- 100 * (cord_urb$mean_cost - hdb_urb$mean_cost) / hdb_urb$mean_cost
  
  table_lines <- c(table_lines,
                   sprintf("%-6s  %5.0f (\u00B1%3.0f)       %5.0f (\u00B1%3.0f)       %+5.1f%%   %5.0f (\u00B1%3.0f)       %5.0f (\u00B1%3.0f)       %+5.1f%%",
                           yr,
                           cord_rur$mean_cost, cord_rur$sd_cost, hdb_rur$mean_cost, hdb_rur$sd_cost, diff_rur,
                           cord_urb$mean_cost, cord_urb$sd_cost, hdb_urb$mean_cost, hdb_urb$sd_cost, diff_urb))
}

writeLines(table_lines, "Results/CoRD_vs_HDB_annual_table.txt")
cat("Table saved to Results/CoRD_vs_HDB_annual_table.txt\n")

