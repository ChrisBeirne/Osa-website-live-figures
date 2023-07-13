#library(rgdal) 
library(move) 
library(dplyr)
library(plotly)
library(viridis)
library(kableExtra)
library(lubridate)
library(htmlwidgets)
library(MetBrewer)


df <- data.frame(
  x = c(1,2,1),
  y = c(1,2,1),
  f = c(1,2,3)
)
 m <- df %>%
  plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 20),
    showlegend = FALSE
  ) %>%
  animation_button(visible = TRUE) %>%
  onRender("
        function(el,x) {
          Plotly.animate(el);
        }")
 
saveWidget(m, "index.html" , selfcontained = TRUE, libdir = NULL,
                       background = "white", knitrOptions = list())

# Import animal locations
# Import passcodes
MOVE_PASS <- Sys.getenv("MOVEBANK_PASSWORD")
MOVE_USE  <- Sys.getenv("MOVEBANK_USERNAME")

loginStored <- movebankLogin(username=MOVE_USE,
                             password=MOVE_PASS)

# Get animals
# Vultures
animals <-getMovebankAnimals(study=1573471517,login=loginStored)
# Ocelot
tmp <-getMovebankAnimals(study=2526574641,login=loginStored)
# Tapir
tmp2 <- getMovebankAnimals(study=1954804459,login=loginStored)
animals <- rbind(animals, tmp,tmp2)

# For some reason they are duplicated
animals[duplicated(animals)==F,]
# They vary by the field "sensor_type_id"
animals <- animals[animals$sensor_type_id==653 & is.na(animals$sensor_type_id)==F,]

# Clean up the name
animals$animalName <- paste0(sub('\\_.*', '', animals$animalName), "_", sub('\\ .*', '', animals$taxon_canonical_name))
animals$name <- sub('\\_.*', '', animals$animalName)

# Sort date objects
animals$timestamp_start <- ymd_hms(animals$timestamp_start)
animals$timestamp_end <- ymd_hms(animals$timestamp_end)

# Get all the data
# Vultures
mov_dat <- getMovebankData(study=1573471517, login=loginStored,  removeDuplicatedTimestamps=TRUE)
#OCelot
tmp <- getMovebankData(study=2526574641, login=loginStored,  removeDuplicatedTimestamps=TRUE)

#Tapir
tmp2 <- getMovebankData(study=1954804459, login=loginStored,  removeDuplicatedTimestamps=TRUE)

# Remove the obvious outlier
tmp2 <- tmp2[tmp2$location_lat>8,]

mov_dat <- moveStack(mov_dat, tmp, tmp2)

#Add the names
mov_dat$name <- trackId(mov_dat)


# Convert timezone
mov_dat$timestamp <- with_tz(timestamps(mov_dat), tz="America/Costa_Rica")

# Convert move stack to dataframe
dat <- as.data.frame(mov_dat)

# Add common names
dat$common_name <- NA
dat$common_name[dat$taxon_canonical_name=="Leopardus pardalis"] <- "Ocelot"
dat$common_name[dat$taxon_canonical_name=="Tapirus bairdii"] <- "Baird's Tapir"
dat$common_name[dat$taxon_canonical_name=="Cathartes aura"] <- "Turkey vulture"
dat$common_name[dat$taxon_canonical_name=="Cathartes melambrotus"] <- "Greater yellow-headed vulture"
dat$common_name[dat$taxon_canonical_name=="Coragyps atratus"] <- "Black vulture"
dat$common_name[dat$taxon_canonical_name=="Sarcoramphus papa"] <- "King vulture"

animals$common_name <- NA
animals$common_name[animals$taxon_canonical_name=="Leopardus pardalis"] <- "Ocelot"
animals$common_name[animals$taxon_canonical_name=="Tapirus bairdii"] <- "Baird's Tapir"
animals$common_name[animals$taxon_canonical_name=="Cathartes aura"] <- "Turkey vulture"
animals$common_name[animals$taxon_canonical_name=="Cathartes melambrotus"] <- "Greater yellow-headed vulture"
animals$common_name[animals$taxon_canonical_name=="Coragyps atratus"] <- "Black vulture"
animals$common_name[animals$taxon_canonical_name=="Sarcoramphus papa"] <- "King vulture"


##### Plot 1
df <- as.data.frame(table(animals$common_name))
df <- df[order(df$Freq),]
fig1 <- plot_ly(x = df$Var1, y = df$Freq, type = 'bar',
                marker = list(color = met.brewer(name="Archambault", nrow(df)), coloraxis="coloraxis"))

fig1

saveWidget(m, "speciesCount.html" , selfcontained = TRUE, libdir = NULL,
           background = "white", knitrOptions = list())



# 
# # Add a country column
# dat$country <- dat$location_lat<0
# dat$country[dat$country==T] <- "peru"
# dat$country[dat$country==F] <- "costa_rica"
# tmp_cr <- dat[dat$country=="costa_rica",]
# 
# 
# lastloc <- tmp_cr %>%
#   group_by(name) %>%
#   slice(n()) %>%
#   ungroup
# 
# ids <- unique(tmp_cr$name)
# 
# 
# m <- leaflet() %>%
#   # Add a satellite image layer
#   addProviderTiles(providers$CartoDB.VoyagerNoLabels,
#                    options = providerTileOptions(minZoom = 10, maxZoom = 13)) %>%
#                    setView(lng=-83.26358816666858, lat=8.708281742832918, zoom = 10)
# 
# m <- m %>%
#   addPolygons(data = oc, color = "pink", group = "property", weight=3,fillOpacity=1,stroke=F, popup="property") %>%
#   addCircleMarkers(data = cams_1, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="cameratraps") %>%
#   addCircleMarkers(data = cams_2, color = "blue", group = "camera trap", weight=1,opacity=1, radius=1, popup="cameratraps") %>%
#   addCircleMarkers(data = kids, color = "yellow", group = "kids club", weight=1,opacity=1, radius=1, popup="kids clubs")   %>%
#   addCircleMarkers(data = road, color = "purple", group = "road survey", weight=1,opacity=1, radius=1, popup="road survey")   %>%
#   addCircleMarkers(data = rest, color = "orange", group = "restoration", weight=1,opacity=1, radius=1, popup="restoration") %>%
#   # Add the last location point for each animal
#   addMarkers(lng=lastloc$location_long,
#              lat=lastloc$location_lat,
#              popup=lastloc$local_identifier,
#              icon = iconSet[lastloc$icon], group="animals")   %>%
#   addLayersControl(
#     overlayGroups = c("property","restoration","camera trap","kids club", "road survey", "restoration", "animals"),
#     options = layersControlOptions(collapsed = FALSE)
#   ) %>%
#   addFullscreenControl() %>%
#   addLegend(colors=c("pink", "blue", "yellow", "purple", "orange"), labels=c("OC property","camera trap","kids club", "road survey", "restoration partners"), opacity=1) %>%
#   suspendScroll(hoverToWake = TRUE, wakeTime = 2000)
# 
# m
# 
# saveWidget(m, "index.html" , selfcontained = TRUE, libdir = NULL,
#            background = "white", knitrOptions = list())
