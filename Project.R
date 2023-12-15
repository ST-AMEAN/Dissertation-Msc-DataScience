# Load the necessary library
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(stringr)
library(purrr)  
### Data Exploration ========================================================
# Load/read data 
Listing <- read.csv("//Users/st.amean/Desktop/ABNB DATA/listings.csv")
Zone <- read.csv("//Users/st.amean/Desktop/ABNB DATA/London_Zone.csv")
Calendar <- read.csv("//Users/st.amean/Desktop/ABNB DATA/Calendar_ABNB.csv")
str(Listing)
str(Zone)
str(Calendar)
### Listing DATA ========================================================
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

#### Calendar DATA ========================================================

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

listing_summary_year <- Calendar %>%
  group_by(id,year) %>%
  summarize(
    avg_price_booked = mean(price[available == "f"]),
    min_price_booked = min(price[available == "f"]),
    max_price_booked = max(price[available == "f"]),
    avg_price_available = mean(price[available == "t"]),
    min_price_available = min(price[available == "t"]),
    max_price_available = max(price[available == "t"])
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

# Filter out Occupancy rate = 100% as a 1st focus group
DATA_23_Cleaned <- Listing_detail_summary %>%
  filter(year == "2023" & occupancy_rate != 100) %>%
  filter(!( is.na(avg_price_available) | is.na(min_price_available) | is.na(max_price_available)))

#Removing rows with any NA in "XXX_price_avaliable" column, Since it could mean occupancy_rate == 100 where the listing never been avaliable

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
###Focus Airbnb Data  =============================================
## Get the Focus groups listing 
Focus_group <- DATA_23_Cleaned %>% 
  select(id,Borough,Zone,Sub_Regions,bed_room = bedroom_info,availability_365)

## Merge Focus_group with Calendar 2023 data 
Calendar_23 <- Calendar %>%
  filter(year == "2023")
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

Focus_Borough <- Focus_2023 %>%
  group_by(Borough, bed_room) %>%
  summarize(
    Count_of_listing = round(n_distinct(id[available == "f"]) / 10) * 10,
    Mean = round(mean(price[available == "f"])),
    Lower_quartile = floor(quantile(price[available == "f"], 0.25)),
    Median = round(median(price)),
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
  labs(title = "Top 10 Boroughs listing price",
       x = "Borough",
       y = "Price per night",
       fill = "Bedroom Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






### Rental & Market trend Data ==============================================================
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

#Table Summary of rental price ==== 
Rent_price_table <- Rental_Price %>%
  group_by(Bedroom.Category) %>%
  summarize(
    Count.of.rents = sum(as.numeric(gsub(",", "", Count.of.rents))),
    Mean = round(mean(as.numeric(gsub(",", "", Mean)))),
    Lower.quartile = floor(mean(as.numeric(gsub(",", "", Lower.quartile)))),
    Median = round(mean(as.numeric(gsub(",", "", Median)))),
    Upper.quartile = ceiling(mean(as.numeric(gsub(",", "", Upper.quartile))))
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
  labs(title = "Top 10 rental Boroughs ",
       x = "Borough",
       y = "Count of Rentals",
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
  labs(title = "Top 10 Boroughs rental price",
       x = "Borough",
       y = "Price per Month",
       fill = "Bedroom Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#IPHRP =====
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
  labs(title = "IPHRP Over Time 2006 to 2023",
       x = "Year",
       y = "IPHRP") +
  theme_minimal()

# Plot for IPHRP_Change
ggplot(IPHRP, aes(x = Date, y = IPHRP_Change)) +
  geom_line() +
  labs(title = "IPHRP movement Over Time 2006 to 2023",
       x = "Year",
       y = "IPHRP Movment") +
  theme_minimal()







# AVG_price_index_type ====
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
cat("Percentage of Flat indices relative to all indices:", round(percentage_flat_indices, 2), "%\n")



# AVG_price/index_borough ====
#change to numeric
# Assuming AVG_price_Borough is your data frame
AVG_price_Borough <- AVG_price_Borough %>%
  mutate_at(vars(-Date), ~as.numeric(gsub(",", "", .)))
# Ensure that "Date" column is still character
AVG_price_Borough$Date <- as.character(AVG_price_Borough$Date)
str(IPHRP)

# Create a line chart for price ====
# Reshape data to longer format
AVG_price_long <- gather(AVG_price_Borough, key = "Borough", value = "Price", -Date)
# Convert Date to a Date type
AVG_price_long$Date <- as.Date(paste("01-", AVG_price_long$Date, sep = ""), format = "%d-%b-%y")


ggplot(AVG_price_long, aes(x = Date, y = Price, color = Borough)) +
  geom_line() +
  labs(title = "Price Trend for Each Borough",
       x = "Date",
       y = "Price") +
  theme_minimal()
# Create a line chart for index ====
# Convert Date to a Date type
AVG_Index_long$Date <- as.Date(paste("01-", AVG_Index_long$Date, sep = ""), format = "%d-%b-%y")

# Create a line chart for the Index
ggplot(AVG_Index_long, aes(x = Date, y = Index, color = Borough)) +
  geom_line() +
  labs(title = "Index Trend for Each Borough",
       x = "Date",
       y = "Index") +
  theme_minimal()

### top 10 price ====
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
  labs(title = "Price Trend for Top 10 Boroughs",
       x = "Date",
       y = "Price") +
  theme_minimal()

### top 10 index ====
# Identify the top 10 boroughs with the highest average indices
top_10_boroughs <- AVG_Index_long %>%
  group_by(Borough) %>%
  summarize(mean_index = mean(Index, na.rm = TRUE)) %>%
  top_n(10, wt = mean_index) %>%
  pull(Borough)

# Filter data for the top 10 boroughs
top_10_data <- AVG_Index_long %>%
  filter(Borough %in% top_10_boroughs)

# Create a line chart for the top 10 boroughs
ggplot(top_10_data, aes(x = Date, y = Index, color = Borough)) +
  geom_line() +
  labs(title = "Index Trend for Top 10 Boroughs",
       x = "Date",
       y = "Index") +
  theme_minimal()



### Rental income======
# Assume number of nights in a year
number_of_nights_in_a_year <- 365

# Calculate Annual Rental Income
Focus_Borough <- Focus_Borough %>%
  mutate(Annual_Rental_Income = (Annual_OCC/100) * number_of_nights_in_a_year * Mean)

# Assume number of months in a year
number_of_months_in_a_year <- 12

# Calculate Annual Rental Income
Rental_Price <- Rental_Price %>%
  mutate(Annual_Rental_Income = number_of_months_in_a_year * Mean)


### Housing price 2023 ==== 
Housing_price <- AVG_price_long %>%
filter(Date == as.Date("2023-09-01"))

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
Housing_price <- Housing_price %>%
  mutate(Borough = ifelse(Borough %in% names(borough_mapping), borough_mapping[Borough], Borough))

Focus_Borough <- merge(Focus_Borough, Housing_price, by = "Borough", all.x = TRUE)

# Airbnb Gross_Rental_Yield
Gross_Rental_Yield_bnb <- Focus_Borough %>%
  group_by(Borough) %>%
  summarize(
    Annual_Rental_Income = mean(Annual_Rental_Income),
    Housing_price = mean(Price),
    Gross_Rental_Yield = (Annual_Rental_Income/Housing_price)*100
  ) %>%
  arrange(desc(Gross_Rental_Yield))

# Create the bar chart
ggplot(Gross_Rental_Yield_bnb, aes(x = reorder(Borough, -Gross_Rental_Yield), y = Gross_Rental_Yield)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Airbnb Gross Rental Yield by Borough",
       x = "Borough",
       y = "Gross Rental Yield") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Gross_Rental_Yield
Rental_Price <- merge(Rental_Price, Housing_price, by = "Borough", all.x = TRUE)

Gross_Rental_Yield <- Rental_Price %>%
  group_by(Borough) %>%
  summarize(
    Annual_Rental_Income = mean(Annual_Rental_Income),
    Housing_price = mean(Price),
    Gross_Rental_Yield = (Annual_Rental_Income/Housing_price)*100
  ) %>%
  arrange(desc(Gross_Rental_Yield))
# Create the bar chart
ggplot(Gross_Rental_Yield, aes(x = reorder(Borough, -Gross_Rental_Yield), y = Gross_Rental_Yield)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Gross Rental Yield by Borough",
       x = "Borough",
       y = "Gross Rental Yield") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

