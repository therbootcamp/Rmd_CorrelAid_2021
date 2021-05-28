
require(tidyverse)


a = readr::read_csv('~/Downloads/listings.csv')

sum(is.na(a$first_review))
sum(a$last_review > "2019-05-01",na.rm=T)
sum(is.na(a$first_review)|a$last_review > "2019-05-01", na.rm=T)

table(rev = a$last_review > "2019-05-01", ava = a$availability_ != 0)


a = a %>% filter(!is.na(first_review), last_review > "2019-11-01")



host = a %>% select(host_id, host_since, host_response_time, 
                      host_response_rate, host_is_superhost,host_listings_count)
# texts = a %>% select(name, summary, space, description,experiences_offered, neighborhood_overview,
#                      notes, transit, access, house_rules)
listing = a %>% select(price, first_review, property_type, accommodates, bedrooms, bathrooms, amenities,cleaning_fee, availability_90)
reviews = a %>% select(review_scores_rating,review_scores_accuracy,review_scores_cleanliness,review_scores_checkin,
                       review_scores_communication,review_scores_location,review_scores_value)
location = a %>% select(neighbourhood, neighbourhood_group_cleansed, latitude, longitude)

airbnb = listing %>% bind_cols(location, host, reviews) %>% 
  mutate(price = as.numeric(stringr::str_replace(str_replace(price,',',''),'\\$|,',''))) %>% 
  mutate(
    Küche = str_detect(amenities, "Kitchen"),
    Wifi = str_detect(amenities, "Wifi"),
    TV = str_detect(amenities, "TV"),
    Kaffeemaschine = str_detect(amenities, "Coffee maker"),
    Geschirrspüler = str_detect(amenities, "Dishwasher"),    
    Terrasse_Balkon = str_detect(amenities, "Patio or balcony"),
    Badewanne = str_detect(amenities, "Bathtub"),
    Check_in_24h = str_detect(amenities, "24-hour check-in")) %>% 
  select(-amenities) %>% 
  rename(
    Preis = price,
    Erstellungsdatum = first_review,
    Viertel = neighbourhood,
    Stadtteil = neighbourhood_group_cleansed,
    Breitengrad = latitude,
    Längengrad = longitude,
    Unterkunftsart = property_type,
    Schlafplätze = accommodates,
    Schlafzimme = bedrooms,
    Badezimmer = bathrooms,
    Reinigungsgebühr = cleaning_fee,
    Rating_gesamt = review_scores_rating,
    Rating_genauigkeit = review_scores_accuracy,
    Rating_sauberkeit = review_scores_cleanliness,
    Rating_checkin = review_scores_checkin,
    Rating_kommunikation = review_scores_communication,
    Rating_lage = review_scores_location,
    Rating_wertigkeit = review_scores_value,
    Host_id = host_id,
    Host_seit = host_since,
    Host_antwortzeit= host_response_time, 
    Host_antwortrate=host_response_rate, 
    Host_superhost = host_is_superhost,
    host_anzahl = host_listings_count,
    Verfügbarkeit_90Tage = availability_90
    )  %>% 
  filter(!is.na(Erstellungsdatum),!is.na(Host_seit))

set.seed(100)
airbnb$Wohnung_id = sample(10000:99999, nrow(airbnb))

airbnb = airbnb %>% select(Wohnung_id, everything())

write_csv(airbnb,'~/Dropbox (2.0)/Work/Software/RBootcamps/RmR_2020Jun/_materials/case/airbnb.csv')

sum(is.na(airbnb$Host_seit))

which(is.na(airbnb$Host_seit))




airbnb$Preis

as.numeric(stringr::str_replace(str_replace(airbnb$price[945],',',''),'\\$',''))

sort(table(unlist(stringr::str_split(a$amenities,','))))



tapply(a$price, a$neighbourhood_group_cleansed, mode, na.rm=T)
tapply(a$review_scores_location, a$neighbourhood_group_cleansed, mean, na.rm=T)


plot(airbnb$first_review, airbnb$price)




#install.packages("tidyverse")
library(tidyverse)

#install.packages("readr")
library(readr)

#install.packages("proj4")
library(proj4)

#install.packages("magick")
library(magick)

#install.packages("ggmap")
library(ggmap)

#install.packages("ggimage")
library(ggimage)


register_google(key = "AIzaSyBzQHdGjtB67GbTOabNQH1RfrPSCQpZBlU",
                account_type = "standard",write = TRUE)



styles = c(
  "style=feature:administrative.land_parcel%7Celement:labels%7Cvisibility:off",
  "style=feature:poi%7Celement:labels.text%7Cvisibility:off",
  "style=feature:poi.business%7Cvisibility:off",
  "style=feature:road%7Celement:labels.icon%7Cvisibility:off",
  #"style=feature:road.local%7Celement:labels%7Cvisibility:off",
  "style=feature:transit%7Cvisibility:off"
  )
style = paste(styles, sep = '&')

berlin = get_googlemap(center = c(lon = 13.404954, lat = 52.520008),
                  #size = c(1280, 1280),
                  zoom = 12, scale = 2,
                  maptype = "terrain",
                  style=style)
saveRDS(berlin, '_materials/case/berlin.RDS')


ggmap(m) + theme(axis.title = element_blank(), 
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank())

library(raster)

shapefile <- readOGR("~/Downloads/Bezirke_-_Berlin-shp/", "Berlin_Bezirke")

germG <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

## Crop to the desired extent, then plot
out <- crop(germG, extent(13.19, 13.62, 52.4, 52.65))

# Next the shapefile has to be converted to a dataframe for use in ggplot2
data <- fortify(out)

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoidGhlcmJvb3RjYW1wIiwiYSI6ImNrOXEzMXJhazBieHAzZm8yemI3Z3k5djEifQ.DqSqDc6RM6e-JSzYoPkhhA')

nc <- sf::st_read("~/Downloads/Bezirke_-_Berlin-shp/", "Berlin_Bezirke")
nc <- sf::st_transform(nc, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

out <- crop(nc, extent(13.19, 13.62, 52.4, 52.65))


plot_mapbox(nc,split=~Gemeinde_n,hoverinfo='Gemeinde_n')%>% layout(showlegend = FALSE)

g <- list(
  scope = 'germany',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

plot_ly(nc) %>% layout(geo=list(scope='deutschland')) 


ggmap(nc)

p = ggmap(m, extent ='device') + theme(axis.title = element_blank(), 
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = 'none') +
geom_polygon(data = data, 
          aes(x = long, y = lat, group = group, alpha = .1),
          fill = 'white',col='black', size = .2) 


ggplotly(width=1800)


plot_ly(out)
par()$usr


library(plotly)
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')

# geo styling
g <- list(
  scope = 'germany',
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

fig <- plot_geo(df, lat = ~lat, lon = ~long)
fig <- fig %>% add_markers(
  text = ~paste(airport, city, state, paste("Arrivals:", cnt), sep = "<br />"),
  color = ~cnt, symbol = I("square"), size = I(8), hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "Incoming flights<br />February 2011")
fig <- fig %>% layout(
  title = 'Most trafficked US airports<br />(Hover for airport)', geo = g
)

fig










  p <- ggmap(m, )
p + geom_point(aes(x = Lon, y = Lat,  colour = Facility), data = df, size = 0.5) + 
  theme(legend.position="bottom")


reviews = c()


review 

names(a)
a$market

table(a$mp_year)
length(table(a$cm_name))



# ```{r echo = F}
# ggmap(berlin) + 
#   theme(axis.title = element_blank(),
#         axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank()) +
#   coord_map(ylim=c(52.45,52.58)) +
#   geom_point(airbnb %>% filter(Erstellungsdatum > "2018-01-01", 
#                                Erstellungsdatum < "2019-12-31"), 
#              mapping = aes(x = Längengrad, y = Breitengrad, col = year(Erstellungsdatum)),size=.1) + 
#   theme(legend.position = 'none')
# ```

