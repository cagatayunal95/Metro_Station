library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(leaflet)
library(ggmap)


#imported data

metro_stations <- read_excel("C:/Users/cagat/Desktop/DATALAR/metro/metro_stations.xlsx")
district_data <- read_excel("C:/Users/cagat/Desktop/DATALAR/metro/district_data.xlsx")


#seeing


View(metro_stations)
View(district_data)


#turkish letters to english


# Replace Turkish characters in 'İlçe' column
district_data$İlçe <- gsub("ç", "c", district_data$İlçe)
district_data$İlçe <- gsub("ğ", "g", district_data$İlçe)
district_data$İlçe <- gsub("ı", "i", district_data$İlçe)
district_data$İlçe <- gsub("ö", "o", district_data$İlçe)
district_data$İlçe <- gsub("ş", "s", district_data$İlçe)
district_data$İlçe <- gsub("ü", "u", district_data$İlçe)
district_data$İlçe <- gsub("Ç", "C", district_data$İlçe)
district_data$İlçe <- gsub("Ğ", "G", district_data$İlçe)
district_data$İlçe <- gsub("İ", "I", district_data$İlçe)
district_data$İlçe <- gsub("Ö", "O", district_data$İlçe)
district_data$İlçe <- gsub("Ş", "S", district_data$İlçe)
district_data$İlçe <- gsub("Ü", "U", district_data$İlçe)


View(district_data)


#For other data - different method


char_map <- c("ç" = "c", "ğ" = "g", "ı" = "i", "ö" = "o", "ş" = "s", "ü" = "u", 
              "Ç" = "C", "Ğ" = "G", "İ" = "I", "Ö" = "O", "Ş" = "S", "Ü" = "U")

for (turkish_char in names(char_map)) {
  english_char <- char_map[turkish_char]
  metro_stations$district <- gsub(turkish_char, english_char, metro_stations$district)
}


View(metro_stations)


##

metro_stations$district <- toupper(metro_stations$district)
district_data$İlçe <- toupper(district_data$İlçe)



View(district_data)
View(metro_stations)


##


metro_stations$district <- ifelse(metro_stations$district == "EYUP", "EYUPSULTAN", metro_stations$district)

##

merged_data <- merge(metro_stations, district_data, by.x = "district", by.y = "İlçe", all.x = TRUE)

View(merged_data)


##


merged_data_sf <- st_as_sf(merged_data, coords = c("lon", "lat"), crs = 4326)

ggplot(data = merged_data_sf) +
  geom_sf() +
  ggtitle("Map of Metro Stations in Istanbul")




ggplot(data = merged_data_sf) +
  geom_sf(aes(color = having_car)) +  # Color points based on car ownership
  geom_sf_label(aes(label = station_names), check_overlap = TRUE) + # Add station names
  scale_color_viridis_c() +  # Use a color scale for clarity
  ggtitle("Metro Stations in Istanbul with Car Ownership Rates")



###


leaflet(merged_data) %>%
  addTiles() %>%  # This adds the default OpenStreetMap tiles
  addMarkers(~lon, ~lat, popup = ~station_names)


leaflet(merged_data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(~lon, ~lat, popup = ~station_names)



###
register_google(key = "xxxx")
istanbul_map <- get_map(location = "Istanbul", zoom = 11, maptype = "terrain")


ggmap(istanbul_map) +
  geom_point(data = merged_data, aes(x = lon, y = lat), color = "red", size = 3, alpha = 0.7) +
  ggtitle("Metro Stations in Istanbul")


###


ggmap(istanbul_map) +
  geom_point(data = merged_data, aes(x = lon, y = lat, color = owing), size = 11, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +  # Customize the color gradient as needed
  ggtitle("Metro Stations in Istanbul with House Ownership Percentage")


###

ggmap(istanbul_map) +
  geom_point(data = merged_data, aes(x = lon, y = lat, color = having_car), size = 11, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +  # Customize the color gradient as needed
  ggtitle("Metro Stations in Istanbul with Car Ownership Percentage")

###

ggmap(istanbul_map) +
  geom_point(data = merged_data, aes(x = lon, y = lat, color = company_car), size = 11, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +  # Customize the color gradient as needed
  ggtitle("Metro Stations in Istanbul with Company Car Ownership Percentage")

###

ggmap(istanbul_map) +
  geom_point(data = merged_data, aes(x = lon, y = lat, color = Nufus), size = 11, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +  # Customize the color gradient as needed
  ggtitle("Metro Stations in Istanbul Nufus")

### 

districts <- c("ADALAR", "BAKIRKOY", "BESIKTAS", "BEYKOZ", "BEYOGLU", "CATALCA", "EYUPSULTAN", 
               "FATIH", "GAZIOSMANPASA", "KADIKOY", "KARTAL", "SARIYER", "SILIVRI", "SILE", 
               "SISLI", "USKUDAR", "ZEYTINBURNU", "BUYUKCEKMECE", "KAGITHANE", "KUCUKCEKMECE", 
               "PENDIK", "UMRANIYE", "BAYRAMPASA", "AVCILAR", "BAGCILAR", "BAHCELIEVLER", 
               "GUNGOREN", "MALTEPE", "SULTANBEYLI", "TUZLA", "ESENLER", "ARNAVUTKOY", 
               "ATASEHIR", "BASAKSEHIR", "BEYLIKDUZU", "CEKMEKOY", "ESENYURT", "SANCAKTEPE", "SULTANGAZI")


avg_rent_prices <- c(25384, 26118, 37500, 24220, 20909, 11765, 20162, 12750, 16471, 29514, 
                     18333, 43180, 12419, 20667, 20400, 18824, 14980, 16058, 16500, 16667, 
                     15600, 17692, 14529, 13324, 13618, 12916, 13999, 18182, 12500, 15929, 
                     12632, 11294, 20779, 20700, 18200, 16667, 11176, 14286, 12971)



rent_data <- data.frame(district = districts, avg_rent_price = avg_rent_prices)


####

# Merging the datasets
final_data <- merge(merged_data, rent_data, by = "district")

View(final_data)

####

# Linear regression model
model <- lm(avg_rent_price ~ having_car + not_having_car + Nufus, data = final_data)

# Summary of the model
summary(model)


# Diagnostic plots
par(mfrow = c(2, 2))
plot(model1)


###

options(scipen = 999)


###

model1 <- lm(avg_rent_price ~ having_car + not_having_car + Nufus, data = final_data)
summary(model1)

###

model2 <- lm(avg_rent_price ~ owing + rent, data = final_data)
summary(model2)


###

par(mfrow = c(2, 2))
plot(model2)
