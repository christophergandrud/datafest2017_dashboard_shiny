# SETUP CHUNK ------------------------------------------------------------------
# Load required packages. Install if needed.
library(flexdashboard)
library(gsheet)
library(ggmap)
library(dplyr)
library(leaflet)
library(sp)
library(rworldmap)
library(networkD3)
library(geosphere)
library(plotly)

# Download survey data -----
URL <- 'https://docs.google.com/spreadsheets/d/1QQkVYYdAPYjCQRO1Oupqze7Q8WULkfJ_cmDvJERumU4/edit#gid=317007960'
student_data <- gsheet2tbl(URL)

# Find hometown longitude and latitude -----
hometowns <- student_data$`What do you consider to be your "home town"?`
hometowns <- hometowns[!is.na(hometowns)]
hometown_coords <- geocode(hometowns)



# MAP CHUNK --------------------------------------------------------------------
# Map student hometowns
leaflet() %>% addTiles() %>%
    addMarkers(data = hometown_coords, lng = ~lon, lat = ~lat)



# NETWORK CHUNK ----------------------------------------------------------------
# Maximum distance for relationship
max_distance <- 1000000 # in meters

# Find distances between each home town
distances <- vector()
for (i in 1:nrow(hometown_coords)) {
    temp_indv <- hometown_coords[i, ]
    
    temp_distances <- distm(hometown_coords, temp_indv) %>% data.frame
    temp_distances$id <- i
    temp_distances$other_id <- 1:nrow(hometown_coords)
    names(temp_distances)[1] <- 'distance'
    distances <- rbind(distances, temp_distances)
}
distances <- distances[, c(2, 3, 1)]

# Remove self-self edges
distances <- subset(distances, id != other_id)

# Keep only nodes that are within 1,000 km of each other
distances <- subset(distances, distance <= max_distance)

# Plot network
simpleNetwork(distances[, 1:2], zoom = TRUE)



# BAR PLOT CHUNK ---------------------------------------------------------------