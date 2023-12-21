# Load the necessary library ===================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(stringr)
library(purrr)  
library(tidyverse)
library(forecast)
library(stats)
library(gridExtra)
library(corrplot)
####### Exploratory Data Analysis (EDA) ========================================================
# Load/read data 
Listing <- read.csv("//Users/st.amean/Desktop/ABNB DATA/listings.csv")
Zone <- read.csv("//Users/st.amean/Desktop/ABNB DATA/London_Zone.csv")
Calendar <- read.csv("//Users/st.amean/Desktop/ABNB DATA/Calendar_ABNB.csv")
str(Listing)
str(Zone)
str(Calendar)
## Listing DATA ========================================================
# Convert the "id" form numeric to character since thay are unique valiable
Listing$id <- as.character(Listing$id)
# Select the desired columns
Listing_subset <- Listing %>%
  select(id, name, host_id, host_since, neighbourhood_cleansed, property_type,
         room_type, accommodates, bathrooms_text, beds, price,
         minimum_nights, maximum_nights, availability_365, number_of_reviews)
# Check outlier of 0 availability 
# Create bins for availability_365 values
Listing_subset <- Listing_subset %>%
  mutate(availability_range = cut(availability_365,
                                  breaks = c(0, 100, 200, 300, 365, Inf),  # Adjusted 'breaks'
                                  labels = c("0", "1-100", "101-200", "201-300", "301-365"),
                                  include.lowest = TRUE))
# Group by availability_range and calculate the count of IDs in each range
availability_counts <- Listing_subset %>%
  group_by(availability_range) %>%
  summarise(count = n())
# Create a bar plot to see if there any outlier 
ggplot(availability_counts, aes(x = availability_range, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Availability Range", y = "Count of IDs") +
  theme_minimal() +
  ggtitle("Count of IDs by Availability Range")

# Remove the avalibility_365 = 0 & Remove "availability_range" column
Listing_filtered <- Listing_subset %>%
  filter(availability_365 != 0)
Listing_filtered <- Listing_filtered %>% select(-availability_range)

# Group by property_type and calculate the count of listing in each group
Listing_filtered %>%
  group_by(property_type) %>%
  count() %>%
  arrange(desc(n)) %>%
  top_n(10)
# Specify the property types you want to filter
desired_property_types <- c("Private room in condo", "Entire home", "Private room in home",
                            "Private room in rental unit", "Entire condo", "Entire rental unit")

# Filter only the rows with property_type in the desired list
filtered_listing_by_prop <- Listing_filtered %>%
  filter(property_type %in% desired_property_types)
#check room type
filtered_listing_by_prop %>%
  group_by(room_type) %>%
  count()%>%
  arrange(desc(n)) %>%
  top_n(10)
#check bath room
filtered_listing_by_prop %>%
  group_by(bathrooms_text) %>%
  count()%>%
  arrange(desc(n)) %>%
  top_n(10)
#Filter only the rows with bathroom in the desired list
#no shared bath,half-bath and only form top 10
desired_bathrooms <- c("1 bath","1.5 baths","2 baths","2.5 baths",
                       "3 baths","3.5 baths","1 private bath")

filtered_listing_by_prop <- filtered_listing_by_prop %>%
  filter(bathrooms_text %in% desired_bathrooms) 
#check N/A
colSums(is.na(filtered_listing_by_prop))
# Split the "name" column into separate columns

Name_split <- filtered_listing_by_prop %>%
  select(id, name)%>%
  separate(name, into = c("property_info", "rating", "bedroom_info", "bed_info", "bath_info"), sep = " · ")

#check N/A
colSums(is.na(Name_split))

View(Name_split)
# Reorder columns as needed by create new columns ane remove the old columns

Name_split <- Name_split %>%
  mutate(
    bath_info_new = ifelse(is.na(bath_info), bed_info, bath_info),
    bed_info_new = ifelse(is.na(bath_info), bedroom_info, bed_info),
    bedroom_info_new = ifelse(is.na(bath_info), rating, bedroom_info),
    rating_new = ifelse(is.na(bath_info), "No rating", rating)
  )


Name_split <- Name_split %>% # select new data after re-order columns
  select(id, property_info, rating_new, bedroom_info_new, bed_info_new, bath_info_new)


Name_split_recheck<-Name_split %>%
  filter(is.na(bath_info_new) | bath_info_new == "" |grepl("★(New|\\d+\\.\\d+)", bedroom_info_new))


# Reorder again after recheck columns as needed by create new columns ane remove the old columns
Name_split <- Name_split %>%
  mutate(
    bath_info = ifelse(is.na( bath_info_new), bed_info_new, bath_info_new),
    bed_info  = ifelse(is.na( bath_info_new), "No bed", bed_info_new))%>%
  select(
    id, property_info, rating_new, bedroom_info_new, bed_info, bath_info)


Name_split <- Name_split %>%
  mutate( 
    rating = ifelse((grepl("★(New|\\d+\\.\\d+)", bedroom_info_new)), bedroom_info_new, rating_new),
    bedroom_info = ifelse((grepl("★(New|\\d+\\.\\d+)", bedroom_info_new)), bed_info, bedroom_info_new),
    bed_info = ifelse((grepl("★(New|\\d+\\.\\d+)", bedroom_info_new)), "No bed" ,bed_info))%>%
  select(
    id, property_info, rating, bedroom_info, bed_info, bath_info
  )
#check N/A
colSums(is.na(Name_split))

View(Name_split)

# Merged data frame
# Name_split + filtered_listing_by_prop
Listing_detail <- merge(Name_split, 
                        filtered_listing_by_prop %>% 
                          select(id,Borough=neighbourhood_cleansed,room_type,
                                 price,minimum_nights,maximum_nights,availability_365), 
                        by = "id")
# Listing_detail + Zone
Listing_detail <- merge(Listing_detail, Zone , by = "Borough", all.x = TRUE)

# Move id to first column 
Listing_detail <- Listing_detail[c("id", setdiff(names(Listing_detail), "id"))]

## The "price" column data is in USD($), We need to checnge it to be GBP(£)

# Define the exchange rate from USD to GBP (Rate on 18 Nov 23)
usd_to_gbp_rate <- 0.8

# Convert the "price" column to numeric and calculate in GBP
Listing_detail$price <- as.numeric(gsub("[^0-9.]", "", Listing_detail$price))
Listing_detail$price <- Listing_detail$price * usd_to_gbp_rate

# Round the "price" column to the nearest £1
Listing_detail$price <- round(Listing_detail$price)

## Calendar DATA ========================================================

str(Calendar)
# Rename the "listing_id" column to "id" , so it will match with Listing df
Calendar <- Calendar %>%
  rename(id = listing_id)

# Convert the "id" form numeric to character since thay are unique valiable
Calendar$id <- as.character(Calendar$id)

# Convert the "date" column to a Date type
Calendar$date <- as.Date(Calendar$date)
# Extract month and year from the "date" column
Calendar <- Calendar %>%
  mutate(month = format(date, "%Y-%m"),
         year = format(date, "%Y"))
## The "price" column data is in USD($), We need to checnge it to be GBP(£)
# Convert the "price" column to numeric and calculate in GBP
Calendar$price <- as.numeric(gsub("[^0-9.]", "", Calendar$price))
Calendar$price <- Calendar$price * usd_to_gbp_rate

# Round the "price" column to the nearest £1
Calendar$price <- round(Calendar$price)

#Remove adjusted_price
Calendar <- Calendar %>% select(-adjusted_price)
### occupancy_rates
# Calculate the occupancy rate per year, grouped by listing_id
occupancy_rates <- Calendar %>%
  group_by(id, year) %>%
  summarize(occupancy_rate = (sum(available == "f") / n()) * 100)

# Check listing summay to scope focus group
listing_summary_year <- Calendar %>%
  group_by(id,year) %>%
  summarize(
    avg_price_booked = mean(price[available == "f"]),
    avg_price_available = mean(price[available == "t"])
  )
# Change all infinite,N/A,NaN to NA
numeric_cols <- sapply(listing_summary_year, is.numeric)
listing_summary_year[numeric_cols] <- lapply(listing_summary_year[numeric_cols], function(x) {
  x[is.na(x) | is.infinite(x)] <- NA
  return(x)
})

# Merge listing_summary with occupancy rates
listing_summary <- merge(occupancy_rates, listing_summary_year, by = c("id", "year"), all.x = TRUE, all.y = TRUE)

# Merge data with summary rate and price of each year
Listing_detail_summary <- merge(Listing_detail, listing_summary, by = c("id"), multiple = "all")

Listing_detail_summary %>%
  group_by(year) %>%
  filter(occupancy_rate == 100) %>%
  summarize(total_listings = n())

#check NA
colSums(is.na(Listing_detail_summary))

# Filter out Occupancy rate = 100% as focus group
#Removing rows with any NA in "avg_price_avaliable" column, Since it could mean occupancy_rate == 100 where the listing never been avaliable
DATA_23_Cleaned <- Listing_detail_summary %>%
  filter(year == "2023" & occupancy_rate != 100) %>%
  filter(!( is.na(avg_price_available)))

## Occupancy Rate Distribution
ggplot(data = Listing_detail_summary, aes(x = occupancy_rate)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Occupancy Rate Distribution", x = "Occupancy Rate", y = "Frequency")
## Occupancy Rate Distribution 2023
ggplot(data = DATA_23_Cleaned , aes(x = occupancy_rate)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Occupancy Rate Distribution_23", x = "Occupancy Rate", y = "Frequency")

colSums(is.na(DATA_23_Cleaned ))

DATA_23_Cleaned %>%
  filter(occupancy_rate == 0) %>%
  summarize(total_listings = n())

## The NA was beacuse of occupancy_rate == 0 where the listing never been booked
## Focus Airbnb Data  =============================================
## Get the Focus groups listing 
Focus_group <- DATA_23_Cleaned %>% 
  select(id,Borough,Zone,Sub_Regions,bed_room = bedroom_info,availability_365)

## Calendar data only 2023
Calendar_23 <- Calendar %>%
  filter(year == "2023")

## Merge Focus_group with Calendar 2023 data 
Focus_2023 <- merge(Focus_group, Calendar_23, by = c("id"), multiple = "all")
head(Focus_2023)
# Calculate the occupancy rate per year, grouped by id
Focus_2023 <- Focus_2023 %>%
  group_by(id, year) %>%
  mutate(occupancy_rate_annual = (sum(available == "f") / n()) * 100)
# Calculate the average price per year, grouped by id
Focus_2023 <- Focus_2023 %>%
  group_by(id, year) %>%
  mutate(avg_price = mean(price))
# Round the "avg_price" column to the nearest £1
Focus_2023$avg_price <- round(Focus_2023$avg_price)

## Filter out the minimum_night  over 1 
Focus_2023 <- Focus_2023 %>%
  filter(minimum_nights == 1)

Focus_Borough <- Focus_2023 %>%
  group_by(Borough, bed_room) %>%
  summarize(
    Count_of_listing = round(n_distinct(id) / 10) * 10,
    Mean = round(mean(price[available == "f"])),
    Lower_quartile = floor(quantile(price[available == "f"], 0.25)),
    Median = round(median(price[available == "f"])),
    Upper_quartile = ceiling(quantile(price[available == "f"], 0.75)),
    Annual_OCC = mean(occupancy_rate_annual)
  )
## Matching data style with the Rental data
Focus_Borough <- Focus_Borough %>%
  mutate(
    bed_room = gsub("1 bedroom", "1 Bedroom", bed_room),
    bed_room = gsub("2 bedrooms", "2 Bedrooms", bed_room),
    bed_room = gsub("3 bedrooms", "3 Bedrooms", bed_room)
  ) %>%
  rename(Bedroom.Category = bed_room)

# define the desired_categories
desired_categories <- c("Studio", "1 Bedroom", "2 Bedrooms", "3 Bedrooms")
# filter only the desired room 
Focus_Borough <- Focus_Borough %>%
  filter(Bedroom.Category %in% desired_categories)
# Confirm that their is no NA ; remove NA
Focus_Borough <- na.omit(Focus_Borough)

# Export the Focus_Borough DataFrame to a CSV file to a specific directory for fourther analysis
write.csv(Focus_Borough, file = "/Users/st.amean/Desktop/ABNB DATA/Airbnb_Focus.csv", row.names = FALSE)


# Plot to see the booking trend =============  
# Group the data by year_month and calculate the count of listings for each month
booked_counts <- Focus_2023 %>%
  filter(available == "f" )%>%
  group_by(month) %>%
  summarize(count = n())
# Convert the 'month' column to Date format
booked_counts$month <- as.Date(paste0(booked_counts$month, "-01"))
# Create a line chart
ggplot(booked_counts, aes(x = month, y = count)) +
  geom_line() +
  labs(
    x = " Month",
    y = "Count of Booked Listings",
    title = "Trend of Booked Listings Over Time"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(labels = scales::comma_format(scale = 1))
# by room type
desired_room <- c("Studio", "1 bedroom", "2 bedrooms", "3 bedrooms")
booked_counts_room <- Focus_2023 %>%
  filter(available == "f" )%>%
  filter(bed_room %in% desired_room)%>%
  group_by(month,bed_room) %>%
  summarize(count = n())
# Convert the 'month' column to Date format
booked_counts_room$month <- as.Date(paste0(booked_counts_room$month, "-01"))
# Create a line chart
ggplot(booked_counts_room, aes(x = month, y = count, color = bed_room)) +
  geom_line() +
  labs(
    x = "Month",
    y = "Count of booked listings",
    title = "Trend of booked listings by room type in a year"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  scale_y_continuous(labels = scales::comma_format(scale = 1))

# Summary table & Plot of Airbnb ============= 
#Table Summary of Airbnb price 
Airbnb_price_table <- Focus_Borough %>%
  group_by(Bedroom.Category) %>%
  summarize(
    Count_of_listing = sum(Count_of_listing),
    Mean = round(mean(Mean)),
    Lower_quartile = floor(mean(Lower_quartile)),
    Median = round(mean( Median)),
    Upper_quartile = ceiling(mean(Upper_quartile))
  )
# Add a row for the summary of all Bedroom.Category
Airbnb_price_table <- Airbnb_price_table %>%
  bind_rows(
    Airbnb_price_table %>%
      summarise(
        Bedroom.Category = "All",
        Count_of_listing = sum(Count_of_listing),
        Mean = round(mean(Mean)),
        Lower_quartile = floor(mean(Lower_quartile)),
        Median = round(mean(Median)),
        Upper_quartile = ceiling(mean(Upper_quartile))
      )
  )

# Identify the top 10 boroughs with the highest count of listings
top_10_bf_count <- Focus_Borough %>%
  group_by(Borough) %>%
  summarize(total_listings = sum(Count_of_listing, na.rm = TRUE)) %>%
  top_n(10, wt = total_listings) %>%
  pull(Borough)

# Filter data for the top 10 boroughs
top_10_data_count <- Focus_Borough %>%
  filter(Borough %in% top_10_bf_count)

# Create a grouped bar chart for the top 10 boroughs
ggplot(top_10_data_count, aes(x = reorder(Borough, -Count_of_listing), y = Count_of_listing, fill = Bedroom.Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Boroughs count of Listings",
       x = "Borough",
       y = "Count of Listings",
       fill = "Bedroom Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Identify the top 10 boroughs with the highest price of listings
top_10_bf_price <- Focus_Borough %>%
  group_by(Borough) %>%
  summarize(total_listings = mean(Mean, na.rm = TRUE)) %>%
  top_n(10, wt = total_listings) %>%
  pull(Borough)

# Filter data for the top 10 boroughs
top_10_data_price_bnb <- Focus_Borough %>%
  filter(Borough %in% top_10_bf_price)

# Create a grouped bar chart for the top 10 boroughs
ggplot(top_10_data_price_bnb, aes(x = reorder(Borough, -Mean), y = Mean, fill = Bedroom.Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Boroughs highest price",
       x = "Borough",
       y = "Price per night (£)",
       fill = "Bedroom Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Rental & Market trend Data ==============================================================
# Load/read data 
# Rental
Rental_Price <- read.csv("//Users/st.amean/Desktop/RENTAL LONDON/Private_Rental_23.csv")
# Market trend
AVG_price_index_type<- read.csv("//Users/st.amean/Desktop/RENTAL LONDON/House_price_index_type.csv")
AVG_price_Borough<- read.csv("//Users/st.amean/Desktop/RENTAL LONDON/Avg_price_Borough.csv")
AVG_index_Borough<- read.csv("//Users/st.amean/Desktop/RENTAL LONDON/Avg_Index_Borough.csv")
IPHRP <- read.csv("//Users/st.amean/Desktop/RENTAL LONDON/IPHRP_index_level.csv")


Rental_Price <- Rental_Price %>% select(Borough,Bedroom.Category,Count.of.rents,Mean,
                                        Lower.quartile,Median,Upper.quartile) 
Rental_Price <- Rental_Price %>%
  mutate(
    Bedroom.Category = gsub("One Bedroom", "1 Bedroom", Bedroom.Category),
    Bedroom.Category = gsub("Two Bedrooms", "2 Bedrooms", Bedroom.Category),
    Bedroom.Category = gsub("Three Bedrooms", "3 Bedrooms", Bedroom.Category)
  )
# filter only the desired room 
Rental_Price <- Rental_Price %>%
  filter(Bedroom.Category %in% desired_categories)
# Convert character columns to numeric, handling commas
Rental_Price$Count.of.rents <- as.numeric(gsub(",", "", Rental_Price$Count.of.rents))
Rental_Price$Mean <- as.numeric(gsub(",", "", Rental_Price$Mean))
Rental_Price$Lower.quartile <- as.numeric(gsub(",", "", Rental_Price$Lower.quartile))
Rental_Price$Median <- as.numeric(gsub(",", "", Rental_Price$Median))
Rental_Price$Upper.quartile <- as.numeric(gsub(",", "", Rental_Price$Upper.quartile))
# remove NA
Rental_Price <- na.omit(Rental_Price)

# Export the Rental_Price DataFrame to a CSV file to a specific directory for further analysis
write.csv(Rental_Price, file = "/Users/st.amean/Desktop/RENTAL LONDON/Rental_Price.csv", row.names = FALSE)


# Summary table & Plot of Rental ==== 
Rent_price_table <- Rental_Price %>%
  group_by(Bedroom.Category) %>%
  summarize(
    Count.of.rents = sum(as.numeric(gsub(",", "", Count.of.rents))),
    Mean = round(mean(as.numeric(gsub(",", "", Mean)))),
    Lower.quartile = floor(mean(as.numeric(gsub(",", "", Lower.quartile)))),
    Median = round(mean(as.numeric(gsub(",", "", Median)))),
    Upper.quartile = ceiling(mean(as.numeric(gsub(",", "", Upper.quartile))))
  )
# Add a row for the summary of all Bedroom.Category
Rent_price_table <- Rent_price_table %>%
  bind_rows(
    Rent_price_table %>%
      summarise(
        Bedroom.Category = "All",
        Count.of.rents = sum(Count.of.rents),
        Mean = round(mean(Mean)),
        Lower.quartile = floor(mean(Lower.quartile)),
        Median = round(mean(Median)),
        Upper.quartile = ceiling(mean(Upper.quartile))
      )
  )

# Identify the top 10 boroughs with the highest count of rentals
top_10_boroughs_rent <- Rental_Price %>%
  group_by(Borough) %>%
  summarize(total_rentals = sum(Count.of.rents, na.rm = TRUE)) %>%
  top_n(10, wt = total_rentals) %>%
  pull(Borough)

# Filter data for the top 10 boroughs
top_10_data_rent <- Rental_Price %>%
  filter(Borough %in% top_10_boroughs_rent)

# Create a bar chart for the count of rentals in the top 10 boroughs
ggplot(top_10_data_rent, aes(x = reorder(Borough, -Count.of.rents), y = Count.of.rents, fill = Bedroom.Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 rental boroughs ",
       x = "Borough",
       y = "Count of rentals",
       fill = "Bedroom Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Identify the top 10 boroughs with the highest price
top_10_boroughs_price <- Rental_Price %>%
  group_by(Borough) %>%
  summarize(total_rentals = mean(Mean, na.rm = TRUE)) %>%
  top_n(10, wt = total_rentals) %>%
  pull(Borough)

# Filter data for the top 10 boroughs
top_10_data_price <- Rental_Price %>%
  filter(Borough %in% top_10_boroughs_price)

# Create a bar chart for the count of rentals in the top 10 boroughs
ggplot(top_10_data_price, aes(x = reorder(Borough, -Mean), y = Mean, fill = Bedroom.Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 boroughs highest rental price",
       x = "Borough",
       y = "Price per Month (£)",
       fill = "Bedroom Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## IPHRP (index=100 @ Jan 2011) =====
IPHRP <- IPHRP %>% select(Date ,IPHRP ,IPHRP_Change) 
# remove NA
IPHRP <- na.omit(IPHRP)
# get table 
IPHRP_table <- summary(IPHRP[, c("IPHRP", "IPHRP_Change")])

# Convert Date to a Date type
IPHRP$Date <- as.Date(paste("01-", IPHRP$Date, sep = ""), format = "%d-%b-%y")

# Plot for IPHRP
ggplot(IPHRP, aes(x = Date, y = IPHRP)) +
  geom_line() +
  labs(title = "Index of Private Housing Rental Prices Over Time 2006 to 2023",
       x = "Year",
       y = "IPHRP") +
  theme_minimal() +
  scale_x_date(breaks = seq(as.Date("2006-01-01"), as.Date("2023-10-01"), by = "1 years"), date_labels = "%Y")

# Plot for IPHRP_Change
ggplot(IPHRP, aes(x = Date, y = IPHRP_Change)) +
  geom_line() +
  labs(title = "IPHRP movement Over Time 2006 to 2023",
       x = "Year",
       y = "IPHRP Movment") +
  theme_minimal()

#AVG IPHRP; (April2022-March2023) for LTL
IPHRP %>%
  filter(Date >= as.Date("2022-04-01") & Date <= as.Date("2023-03-01")) %>%
  summarize(LTL_IPHRP = mean(IPHRP))
### LTL_IPHRP = 111.6333

#AVG IPHRP; (Jan-Oct 2023) for Airbnb
IPHRP %>% 
  filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-10-01")) %>%
  summarize(STL_IPHRP = mean(IPHRP))
### STL_IPHRP = 115.85

## AVG_price_index_type =========================================================
#as numerric
AVG_price_index_type <- AVG_price_index_type %>%
  mutate(Price = as.numeric(gsub(",", "", Price)))

# Filter for "Flat" records
flat_data <- AVG_price_index_type %>%
  filter(Type == "Flat")

# Calculate the percentage of "Flat" prices and indices relative to all prices and indices
percentage_flat_prices <- mean(flat_data$Price) / mean(AVG_price_index_type$Price) * 100
percentage_flat_indices <- mean(flat_data$Index) / mean(AVG_price_index_type$Index) * 100

cat("Percentage of Flat prices relative to all prices:", round(percentage_flat_prices, 2), "%\n")
# Percentage of Flat prices relative to all prices: 69.68 %
cat("Percentage of Flat indices relative to all indices:", round(percentage_flat_indices, 2), "%\n")
# Percentage of Flat indices relative to all indices: 95.79 %

## AVG_price/index_borough ======================================================
#change to numeric
AVG_price_Borough <- AVG_price_Borough %>%
  mutate_at(vars(-Date), ~as.numeric(gsub(",", "", .)))
# Ensure that "Date" column is still character
AVG_price_Borough$Date <- as.character(AVG_price_Borough$Date)
## Housing DATA =============================================
# Create a line chart for price =====================
# Reshape data to longer format
AVG_price_long <- gather(AVG_price_Borough, key = "Borough", value = "Price", -Date)
# Convert Date to a Date type
AVG_price_long$Date <- as.Date(paste("01-", AVG_price_long$Date, sep = ""), format = "%d-%b-%y")
# Define a mapping of old names to new names
borough_mapping <- c(
  "Barking...Dagenham" = "Barking and Dagenham",
  "Hammersmith...Fulham" = "Hammersmith and Fulham",
  "Kensington...Chelsea" = "Kensington and Chelsea",
  "City.of.London" = "City of London",
  "Richmond.upon.Thames" = "Richmond upon Thames",
  "Tower.Hamlets" = "Tower Hamlets",
  "Kingston.upon.Thames" = 	"Kingston upon Thames",
  "Waltham.Forest"=	"Waltham Forest"
)
# Apply the mapping to the Borough column
AVG_price_long<- AVG_price_long %>%
  mutate(Borough = ifelse(Borough %in% names(borough_mapping), borough_mapping[Borough], Borough))

ggplot(AVG_price_long, aes(x = Date, y = Price, color = Borough)) +
  geom_line() +
  labs(title = "Price Trend for Each Borough",
       x = "Date",
       y = "Price") +
  theme_minimal()
# Create a line chart for index ======================
# Reshape data to longer format
AVG_Index_long <- gather(AVG_index_Borough, key = "Borough", value = "Index", -Date)
# Convert Date to a Date type
AVG_Index_long$Date <- as.Date(paste("01-", AVG_Index_long$Date, sep = ""), format = "%d-%b-%y")
# Apply the mapping to the Borough column
AVG_Index_long<- AVG_Index_long %>%
  mutate(Borough = ifelse(Borough %in% names(borough_mapping), borough_mapping[Borough], Borough))

# Create a line chart for the Index
ggplot(AVG_Index_long, aes(x = Date, y = Index, color = Borough)) +
  geom_line() +
  labs(title = "Index Trend for Each Borough",
       x = "Date",
       y = "Index") +
  theme_minimal()

# top 10 price ====================================
# Identify the top 10 boroughs with the highest average prices
top_10_boroughs <- AVG_price_long %>%
  group_by(Borough) %>%
  summarize(mean_price = mean(Price, na.rm = TRUE)) %>%
  top_n(10, wt = mean_price) %>%
  pull(Borough)

# Filter data for the top 10 boroughs
top_10_data <- AVG_price_long %>%
  filter(Borough %in% top_10_boroughs)

# Create a line chart for the top 10 boroughs
ggplot(top_10_data, aes(x = Date, y = Price, color = Borough)) +
  geom_line() +
  labs(title = "Trend for top 10 boroughs with highest house price",
       x = "Year",
       y = "Price (£)") +
  theme_minimal() +
  scale_x_date(breaks = seq(as.Date("1995-01-01"), as.Date("2023-09-01"), by = "2 years"), date_labels = "%Y")


# top 10 index ====================================
# Identify the top 10 boroughs with the highest average indices
top_10_boroughs_index <- AVG_Index_long %>%
  group_by(Borough) %>%
  summarize(mean_index = mean(Index, na.rm = TRUE)) %>%
  top_n(10, wt = mean_index) %>%
  pull(Borough)

# Filter data for the top 10 boroughs
top_10_data <- AVG_Index_long %>%
  filter(Borough %in% top_10_boroughs_index)

# Create a line chart for the top 10 boroughs
ggplot(top_10_data, aes(x = Date, y = Index, color = Borough)) +
  geom_line() +
  labs(title = "Trend for Top 10 Boroughs with highest Housing Index Price",
       x = "Year",
       y = "HPI") +
  theme_minimal() +
  scale_x_date(breaks = seq(as.Date("1995-01-01"), as.Date("2023-09-01"), by = "2 years"), date_labels = "%Y")

#### Rental income===============================================================
# number of nights in a year
number_of_nights_in_a_year <- 365
# Calculate Average Annual Rental Income
Focus_Borough <- Focus_Borough %>%
  mutate(Annual_Rental_Income = round((Annual_OCC/100) * number_of_nights_in_a_year * Mean))

# number of months in a year
number_of_months_in_a_year <- 12
# Calculate AveraggeAnnual Rental Income
Rental_Price <- Rental_Price %>%
  mutate(Annual_Rental_Income = number_of_months_in_a_year * Mean)

# Combine the two dataframes
Income_df <- Focus_Borough %>%
  left_join(Rental_Price %>% select(Borough, Bedroom.Category, Annual_Rental_Income_LTL = Annual_Rental_Income), 
            by = c("Borough", "Bedroom.Category"))

# Create a bar chart comparing Annual_Rental_Income and Annual_Rental_Income_LTL
comparison_chart <- ggplot(Income_df, aes(x = Borough, y = Annual_Rental_Income, fill = factor(Bedroom.Category))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_bar(aes(y = Annual_Rental_Income_LTL), stat = "identity", position = "dodge", color = "red", alpha = 0.5) +
  labs(title = "Comparison of Annual Rental Income",
       x = "Borough",
       y = "Annual Rental Income (£)",
       fill = "Bedroom Category") +
  theme_minimal() +
  facet_wrap(~Bedroom.Category, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Display the chart
print(comparison_chart)

# Housing price 2023 =========================================================
# The resulting Housing_price data frame will have average prices for each Borough in each month of 2023
Housing_price <- AVG_price_long %>%
  filter(year(Date) == 2023) %>%
  group_by(Borough) %>%
  summarize(Avg_Price = round(mean(Price)))
# Apply the mapping to the Borough column
Housing_price <- Housing_price %>%
  mutate(Borough = ifelse(Borough %in% names(borough_mapping), borough_mapping[Borough], Borough))

#Percentage of Flat prices relative to all prices: 69.68 % ; form calculation in AVG_price_index_type
# Calculate the Flat_price based on the Percentage of Flat prices; form 
Housing_price$Flat_price <- round(Housing_price$Avg_Price * 0.6968)

# Export DataFrame to a CSV file to a specific directory for further analysis
write.csv(Housing_price, file = "/Users/st.amean/Desktop/RENTAL LONDON/Housing_price.csv", row.names = FALSE)

#### Gross_Rental_Yield ====================================================
Focus_Borough <- merge(Focus_Borough, Housing_price, by = "Borough", all.x = TRUE)
# calculate GRY
Gross_Rental_Yield_bnb <- Focus_Borough %>%
  group_by(Borough) %>%
  summarize(
    Annual_Rental_Income_STL = round(mean(Annual_Rental_Income)),
    Housing_price = mean(Flat_price),
    Gross_Rental_Yield_STL = (Annual_Rental_Income_STL/Housing_price)*100
  ) %>%
  arrange(desc(Gross_Rental_Yield_STL))
Rental_Price <- merge(Rental_Price, Housing_price, by = "Borough", all.x = TRUE)

# calculate GRY
Gross_Rental_Yield <- Rental_Price %>%
  group_by(Borough) %>%
  summarize(
    Annual_Rental_Income_LTL = round(mean(Annual_Rental_Income)),
    Housing_price = mean(Flat_price),
    Gross_Rental_Yield_LTL = (Annual_Rental_Income_LTL/Housing_price)*100
  ) %>%
  arrange(desc(Gross_Rental_Yield_LTL))

# Combine the two dataframes
GRY_df <-Gross_Rental_Yield_bnb %>%
  left_join(Gross_Rental_Yield %>% select(Borough,Annual_Rental_Income_LTL,Gross_Rental_Yield_LTL), 
            by = c("Borough"))
# Reorder the Boroughs based on Gross_Rental_Yield_STL values
GRY_df$Borough <- reorder(GRY_df$Borough, GRY_df$Gross_Rental_Yield_STL, FUN = function(x) -x)

# Create a bar chart for Gross_Rental_Yield_STL and Gross_Rental_Yield_LTL
comparison_chart_gry <- ggplot(GRY_df, aes(x = Borough, y = Gross_Rental_Yield_STL, fill = "Gross_Rental_Yield_STL")) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_bar(aes(y = Gross_Rental_Yield_LTL, fill = "Gross_Rental_Yield_LTL"), stat = "identity", position = "dodge", color = "red", alpha = 0.5) +
  labs(title = "Comparison of Gross Rental Yield",
       x = "Borough",
       y = "Gross Rental Yield (%)",
       fill = "") +  # Remove legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Display the chart
print(comparison_chart_gry)

# Reorder the Boroughs based on Annual_Rental_Yield_STL values
GRY_df$Borough <- reorder(GRY_df$Borough, GRY_df$Annual_Rental_Income_STL, FUN = function(x) -x)

# Create a bar chart for Annual_Rental_Income_STL and Annual_Rental_Income_LTL (Overall income)
comparison_chart_income <- ggplot(GRY_df, aes(x = Borough, y = Annual_Rental_Income_STL, fill = "Annual_Rental_Income_STL")) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_bar(aes(y = Annual_Rental_Income_LTL, fill = "Annual_Rental_Income_LTL"), stat = "identity", position = "dodge", color = "red", alpha = 0.5) +
  labs(title = "Comparison of Overall Annnual Rental income",
       x = "Borough",
       y = "Annnual Rental income (£)",
       fill = "") +  # Remove legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Display the chart
print(comparison_chart_income)

### ROI =======================================================================
# Assume that 50% of income is expense the net profit will be 50% for STL
PF_STL <- 0.5
# Assume that 40% of income is expense the net profit will be 60% for LTL
PF_LTL <- 0.60
# Assume that down payment at 30% and closing cost+other cost at 10% of Property price 
# Innitial investment = (Property price*down payment)+ closing cost + other cost
DPM_percentage <- 0.30
CC_percentage <- 0.10

ROI_df <- GRY_df  %>% 
  mutate(STL_NPF = (Annual_Rental_Income_STL*PF_STL),
         LTL_NPF = (Annual_Rental_Income_LTL*PF_LTL),
         Initial_in = Housing_price*(DPM_percentage + CC_percentage),
         ROI_STL = (STL_NPF / Initial_in) * 100,
         ROI_LTL = (LTL_NPF / Initial_in) * 100
         )
# Reorder the Boroughs based on ROI_STL values
ROI_df$Borough <- reorder(ROI_df$Borough, ROI_df$ROI_STL, FUN = function(x) -x)

# Create a bar chart for ROI_STL and ROI_LTL
comparison_chart_ROI <- ggplot(ROI_df, aes(x = Borough, y = ROI_STL, fill = "ROI_STL")) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_bar(aes(y = ROI_LTL, fill = "ROI_LTL"), stat = "identity", position = "dodge", color = "red", alpha = 0.5) +
  labs(title = "Comparison of Return of Investment",
       x = "Borough",
       y = "Return of investment (%)",
       fill = "") +  # Remove legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Display the chart
print(comparison_chart_ROI)


#### Correlation ===============================================================
# Get Overall Annual OCC rate(STL) & Rental number (LTL)
Annual_OCC <- Focus_Borough %>%
  group_by(Borough) %>%
  summarize(Avg_Annual_OCC = mean(Annual_OCC, na.rm = TRUE))
Rental_num <- Rental_Price %>% 
  group_by(Borough) %>%
  summarize(Total_Rental = sum(Count.of.rents, na.rm = TRUE))
# Merge GRY_df, Rental_num and Annual_OCby Borough
Corre_df <- merge(ROI_df, Rental_num, by = "Borough")
Corre_df <- merge(Corre_df, Annual_OCC, by = "Borough")
#RENAME 
Corre_df <- Corre_df %>%
  rename(ARI_STL = Annual_Rental_Income_STL,
         GRY_STL = Gross_Rental_Yield_STL,
         ARI_LTL = Annual_Rental_Income_LTL,
         GRY_LTL = Gross_Rental_Yield_LTL,
         Rental = Total_Rental,
         OCC  = Avg_Annual_OCC,
         HP = Housing_price)

# Extracting numeric columns (excluding the "Borough" column)
numeric_columns <- Corre_df[sapply(Corre_df, is.numeric)]
# Compute the correlation matrix
correlation_matrix <- cor(numeric_columns)
# Plot the correlation matrix using corrplot
corrplot.mixed(correlation_matrix, order = 'AOE',tl.cex = 0.5,
               main = "Correlation Matrix",mar = c(0,0,1,1))


#### Forecasting =============================================================
## Housing price Forecasting=====================================================
str(AVG_price_long)
# Define the list of selected boroughs
#Selected intersting borough for analysis for both Airbnb and LTL 
selected_boroughs <- c("Southwark", "Westminster", "Camden", "Kensington and Chelsea", "Tower Hamlets")
# Subset your data for the selected boroughs
borough_data <- AVG_price_long %>%
  filter(Borough %in% selected_boroughs)
borough_data_Index <- AVG_Index_long %>%
  filter(Borough %in% selected_boroughs)

# Create a time series plot with multiple lines (one for each borough)====
## Price 
ggplot(data = borough_data, aes(x = Date, y = Price, color = Borough)) +
  geom_line() +
  labs(title = "Time Series Plot for Selected Boroughs (House Price)  ",
       x = "Year",
       y = "Price (£) ") +
  theme_minimal() +
  scale_color_discrete(name = "Borough")+
  scale_x_date(breaks = seq(as.Date("1995-01-01"), as.Date("2023-09-01"), by = "2 years"), date_labels = "%Y")
## HPI
ggplot(data = borough_data_Index, aes(x = Date, y = Index, color = Borough)) +
  geom_line() +
  labs(title = "Time Series Plot for Selected Boroughs (Housing Price Index)",
       x = "Year",
       y = "HPI") +
  theme_minimal() +
  scale_color_discrete(name = "Borough")+
  scale_x_date(breaks = seq(as.Date("1995-01-01"), as.Date("2023-09-01"), by = "2 years"), date_labels = "%Y")

#ARIMA Modeling for Each Borough======================
# Initialize an empty list to store forecasts 
forecast_list <- list()
# Set the forecast horizon to 5 years (60 months)
forecast_horizon <- 60

### Price 
# Loop through selected boroughs and perform forecasting
for (borough in selected_boroughs) {
  # Subset your data for the current borough
  borough_data <- AVG_price_long %>%
    filter(Borough == borough)
  
  # Convert the Date column to a Date object
  borough_data$Date <- as.Date(borough_data$Date)
  
  # Fit ARIMA model (you can adjust orders as needed)
  arima_model <- auto.arima(borough_data$Price)
  
  # Forecast for the next X years; depend on forecast_horizon
  forecast_values <- forecast(arima_model, h = forecast_horizon)
  
  # Store the forecast in the list
  forecast_list[[borough]] <- forecast_values
}
# Print and visualize the forecasts for each borough
# Create a list to store plots
plots_list <- list()
# Print and visualize the forecasts for each borough
for (i in 1:length(selected_boroughs)) {
  borough <- selected_boroughs[i]
  cat("Forecast for", borough, "for the next 5 years:\n")
  
  # Check if the forecast is not empty
  if (!is.null(forecast_list[[i]])) {
    print(forecast_list[[i]])
    
    # Convert forecast object to ggplot
    forecast_plot <- forecast::autoplot(forecast_list[[i]]) +
      ggplot2::labs(title = paste("Housing Price Forecast for", borough), x = "Data Point", y = "Price (£) ") +
      ggplot2::theme_minimal() + scale_y_continuous(labels = scales::comma_format(scale = 1))
    
    # Store the plot in the list
    plots_list[[length(plots_list) + 1]] <- forecast_plot
  }
}

# Check if there are non-empty plots
if (length(plots_list) > 0) {
  # Arrange plots in a grid
  gridExtra::grid.arrange(grobs = plots_list, ncol = 2)
} else {
  cat("No valid plots to arrange.\n")
}


### Housing Price Index (index=100 @ Jan 2015)
# Loop through selected boroughs and perform forecasting
for (borough in selected_boroughs) {
  # Subset your data for the current borough
  borough_data_Index <- AVG_Index_long %>%
    filter(Borough == borough)
  
  # Convert the Date column to a Date object
  borough_data_Index$Date <- as.Date(borough_data_Index$Date)
  
  # Fit ARIMA model (you can adjust orders as needed)
  arima_model <- auto.arima(borough_data_Index$Index)
  
  # Forecast for the next X years
  forecast_values <- forecast(arima_model, h = forecast_horizon)
  
  # Store the forecast in the list
  forecast_list[[borough]] <- forecast_values
}

# Print and visualize the forecasts for each borough
# Create a list to store plots
plots_list <- list()

# Print and visualize the forecasts for each borough
for (i in 1:length(selected_boroughs)) {
  borough <- selected_boroughs[i]
  cat("Forecast for", borough, "for the next 5 years:\n")
  
  # Check if the forecast is not empty
  if (!is.null(forecast_list[[i]])) {
    print(forecast_list[[i]])
    
    # Convert forecast object to ggplot
    forecast_plot <- forecast::autoplot(forecast_list[[i]]) +
      ggplot2::labs(title = paste("Housing Price Index Forecast for", borough), x = "Data Point", y = " HPI ") +
      ggplot2::theme_minimal()
    
    # Store the plot in the list
    plots_list[[length(plots_list) + 1]] <- forecast_plot
  }
}

# Check if there are non-empty plots
if (length(plots_list) > 0) {
  # Arrange plots in a grid
  gridExtra::grid.arrange(grobs = plots_list, ncol = 2)
} else {
  cat("No valid plots to arrange.\n")
}


# ARIMA Modeling for IPHRP
# Convert Date to a time series object
ts_data <- ts(IPHRP$IPHRP)  # Assuming monthly data

# Fit ARIMA model
IPHRP_arima_model <- auto.arima(ts_data)

# Forecast using the ARIMA model
forecast_result_IPHRP <- forecast(IPHRP_arima_model, h = 60)  # Forecast for the next 60 periods;5 years

# Print the forecast result
print(forecast_result_IPHRP)

# Plot the forecast
autoplot(forecast_result_IPHRP) +
  labs(title = "ARIMA Forecast for IPHRP", x = "Data point", y = "IPHRP") +
  theme_minimal()


###### Re-Check if ARIMA Forecast good for the data set =======
# Subset the historical data from Jan 1995 to Sep 2021
historical_data <- AVG_price_long %>%
  filter(Date >= as.Date("1995-01-01") & Date <= as.Date("2021-09-30"))
# Subset the future period for forecasting from Oct 2021 to Sep 2023
forecast_period <- seq(as.Date("2021-10-01"), as.Date("2023-09-30"), by = "months")

# Initialize an empty list to store forecasts
forecast_list <- list()

# Loop through selected boroughs and perform forecasting
for (borough in selected_boroughs) {
  # Subset your data for the current borough
  borough_data <- historical_data %>%
    filter(Borough == borough)
  
  # Convert the Date column to a Date object
  borough_data$Date <- as.Date(borough_data$Date)
  
  # Fit ARIMA model (you can adjust orders as needed)
  arima_model <- auto.arima(borough_data$Price)
  
  # Forecast for the future period (Oct 2021 - Sep 2023)
  forecast_values <- forecast(arima_model, h = length(forecast_period))
  
  # Create a time series object for the forecasted values with the appropriate time index
  forecast_time_series <- ts(forecast_values$mean, start = forecast_period[1], frequency = 12)
  
  # Store the forecast in the list
  forecast_list[[borough]] <- forecast_time_series
  
  # Print the forecasted values for the current borough
  cat("Forecasted values for", borough, ":\n")
  print(forecast_time_series)
}


## Evaluate Forecast Accuracy=================================
## HP EFA; MAE,MSE,RMSE ======================================
# Set the forecast horizon to the length of the forecast period
forecast_horizon <- length(forecast_period)

# Initialize empty data frames to store evaluation results
mae_results <- data.frame(Borough = character(0), MAE = numeric(0))
mse_results <- data.frame(Borough = character(0), MSE = numeric(0))
rmse_results <- data.frame(Borough = character(0), RMSE = numeric(0))

# Loop through selected boroughs and perform evaluation
for (borough in selected_boroughs) {
  # Extract actual housing prices for the evaluation period (future)
  actual_prices <- AVG_price_long %>%
    filter(Borough == borough & Date >= as.Date("2021-10-01") & Date <= as.Date("2023-09-30"))
  
  # Extract forecasted values for the same months
  forecasted_values <- forecast_list[[borough]]
  
  # Calculate forecast errors
  forecast_errors <- actual_prices$Price - forecasted_values
  
  # Calculate MAE, MSE, and RMSE
  mae <- mean(abs(forecast_errors), na.rm = TRUE)
  mse <- mean(forecast_errors^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  
  # Store evaluation results in data frames
  mae_results <- rbind(mae_results, data.frame(Borough = borough, MAE = mae))
  mse_results <- rbind(mse_results, data.frame(Borough = borough, MSE = mse))
  rmse_results <- rbind(rmse_results, data.frame(Borough = borough, RMSE = rmse))
  
  # Print the evaluation results for the current borough
  cat("Evaluation results for", borough, ":\n")
  cat("Mean Absolute Error (MAE):", mae, "\n")
  cat("Mean Squared Error (MSE):", mse, "\n")
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
}

# Print the overall evaluation results
cat("Overall Evaluation Results:\n")

# Print the overall evaluation results for MAE
cat("Overall Mean Absolute Error (MAE):\n")
print(mean(mae_results$MAE))
cat("\nMean Squared Error (MSE):\n")
print(mse_results)
cat("\nRoot Mean Squared Error (RMSE):\n")
print(rmse_results)

## HP EFA; MAPE ==============================================
# Initialize an empty data frame to store MAPE results
mape_results <- data.frame(Borough = character(0), MAPE = numeric(0))
# Calculate MAPE for each borough
for (borough in selected_boroughs) {
  actual_prices <- AVG_price_long %>%
    filter(Borough == borough & Date >= as.Date("2021-10-01") & Date <= as.Date("2023-09-30"))
  
  forecasted_values <- forecast_list[[borough]]
  
  # Calculate APE
  ape <- (abs(actual_prices$Price - forecasted_values) / actual_prices$Price) * 100
  
  # Calculate MAPE
  mape <- mean(ape, na.rm = TRUE)
  
  # Store MAPE results
  mape_results <- rbind(mape_results, data.frame(Borough = borough, MAPE = mape))
  
  # Print the MAPE for the current borough
  cat("MAPE for", borough, ":", mape, "%\n")
}

# Print the overall MAPE results
cat("Overall MAPE Results:\n")
print(mean(mape_results$MAPE))

## HIP EFA; MAE,MSE,RMSE ======================================
# Subset the HPI data for the same period
hpi_historical_data <- AVG_Index_long %>%
  filter(Date >= as.Date("1995-01-01") & Date <= as.Date("2020-12-31"))
# Subset the future period for forecasting from Oct 2021 to Sep 2023
forecast_period <- seq(as.Date("2021-01-01"), as.Date("2023-09-30"), by = "months")

# Initialize an empty list to store forecasts
forecast_list_HIP <- list()

# Loop through selected boroughs and perform forecasting
for (borough in selected_boroughs) {
  # Subset your data for the current borough
  borough_data <- hpi_historical_data %>%
    filter(Borough == borough)
  
  # Convert the Date column to a Date object
  borough_data$Date <- as.Date(borough_data$Date)
  
  # Fit ARIMA model (you can adjust orders as needed)
  arima_model <- auto.arima(borough_data$Index)
  
  # Forecast for the future period 
  forecast_values <- forecast(arima_model, h = length(forecast_period))
  
  # Create a time series object for the forecasted values with the appropriate time index
  forecast_time_series <- ts(forecast_values$mean, start = forecast_period[1], frequency = 12)
  
  # Store the forecast in the list
  forecast_list_HIP[[borough]] <- forecast_time_series
  
  # Print the forecasted values for the current borough
  cat("Forecasted values for", borough, ":\n")
  print(forecast_time_series)
}

# Set the forecast horizon to the length of the forecast period
forecast_period <- seq(as.Date("2021-01-01"), as.Date("2023-09-30"), by = "months")
forecast_horizon <- length(forecast_period)

# Initialize empty data frames to store evaluation results
mae_results <- data.frame(Borough = character(0), MAE = numeric(0))
mse_results <- data.frame(Borough = character(0), MSE = numeric(0))
rmse_results <- data.frame(Borough = character(0), RMSE = numeric(0))

# Loop through selected boroughs and perform evaluation
for (borough in selected_boroughs) {
  # Extract actual housing prices for the evaluation period (future)
  actual_Index <- AVG_Index_long %>%
    filter(Borough == borough & Date >= as.Date("2021-01-01") & Date <= as.Date("2023-09-30"))
  
  # Extract forecasted values for the same months
  forecasted_values <- forecast_list_HIP[[borough]]
  
  # Calculate forecast errors
  forecast_errors <- actual_Index$Index - forecasted_values
  
  # Calculate MAE, MSE, and RMSE
  mae <- mean(abs(forecast_errors), na.rm = TRUE)
  mse <- mean(forecast_errors^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  
  # Store evaluation results in data frames
  mae_results <- rbind(mae_results, data.frame(Borough = borough, MAE = mae))
  mse_results <- rbind(mse_results, data.frame(Borough = borough, MSE = mse))
  rmse_results <- rbind(rmse_results, data.frame(Borough = borough, RMSE = rmse))
  
  # Print the evaluation results for the current borough
  cat("Evaluation results for", borough, ":\n")
  cat("Mean Absolute Error (MAE):", mae, "\n")
  cat("Mean Squared Error (MSE):", mse, "\n")
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
}

# Print the overall evaluation results
cat("Overall Evaluation Results:\n")

# Print the overall evaluation results for MAE
cat("Overall Mean Absolute Error (MAE):\n")
print(mean(mae_results$MAE))
cat("\nMean Squared Error (MSE):\n")
print(mse_results)
cat("\nRoot Mean Squared Error (RMSE):\n")
print(rmse_results)

## HIP EFA; MAPE =============================================

# Initialize an empty data frame to store MAPE results
mape_results <- data.frame(Borough = character(0), MAPE = numeric(0))

# Calculate MAPE for each borough
for (borough in selected_boroughs) {
  actual_Index <- AVG_Index_long %>%
    filter(Borough == borough & Date >= as.Date("2021-01-01") & Date <= as.Date("2023-09-30"))
  
  forecasted_values <- forecast_list_HIP[[borough]]
  
  # Calculate APE
  ape <- (abs(actual_Index$Index - forecasted_values) / actual_Index$Index) * 100
  
  # Calculate MAPE
  mape <- mean(ape, na.rm = TRUE)
  
  # Store MAPE results
  mape_results <- rbind(mape_results, data.frame(Borough = borough, MAPE = mape))
  
  # Print the MAPE for the current borough
  cat("MAPE for", borough, ":", mape, "%\n")
}

# Print the overall MAPE results
cat("Overall MAPE Results:\n")
print(mean(mape_results$MAPE))


## IPHRP EFA; MAE,MSE,RMSE,MAPE ================================================
# Subset the historical data from Jan 1995 to Sep 2021
historical_data <- IPHRP %>%
  filter(Date >= as.Date("2006-01-01") & Date <= as.Date("2020-12-31"))
# Subset the future period for forecasting from Oct 2021 to Sep 2023
forecast_period <- seq(as.Date("2021-01-01"), as.Date("2023-10-31"), by = "months")

# Initialize an empty list to store forecasts
forecast_list_IPHRP <- list()

  # Fit ARIMA model (you can adjust orders as needed)
  arima_model_IPHRP <- auto.arima(historical_data$IPHRP)
  # Forecast for the future period 
  forecast_values <- forecast(arima_model_IPHRP, h = length(forecast_period))
  # Create a time series object for the forecasted values with the appropriate time index
  forecast_time_series <- ts(forecast_values$mean, start = forecast_period[1], frequency = 12)
  # Store the forecast in the list
  forecast_list_IPHRP[[borough]] <- forecast_time_series
  # Print the forecasted values 
  print(forecast_time_series)
  
# Set the forecast horizon to the length of the forecast period
forecast_horizon <- length(forecast_period)
  
    # Set actual valuse
    actual_IPHRP <- IPHRP %>%
      filter(Date >= as.Date("2021-01-01") & Date <= as.Date("2023-10-31"))
    # Extract forecasted values for the same months
    forecasted_values <- forecast_list_IPHRP[[borough]]
    # Calculate forecast errors
    forecast_errors <- actual_IPHRP$IPHRP - forecasted_values
    # Calculate MAE, MSE, and RMSE
    mae <- mean(abs(forecast_errors), na.rm = TRUE)
    mse <- mean(forecast_errors^2, na.rm = TRUE)
    rmse <- sqrt(mse)
    # Calculate APE
    ape <- (abs(actual_IPHRP$IPHRP - forecasted_values) / actual_IPHRP$IPHRP) * 100
    # Calculate MAPE
    mape <- mean(ape, na.rm = TRUE)
    
    # Print the evaluation results for the current borough
    cat("Mean Absolute Error (MAE):", mae, "\n")
    cat("Mean Squared Error (MSE):", mse, "\n")
    cat("Root Mean Squared Error (RMSE):", rmse, "\n")
    cat("Mean Average Percentage Erroe (MAPE):", mape, "\n")
  


