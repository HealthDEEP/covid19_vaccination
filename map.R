#################
###### map vaccination
#################################

## Load packages
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# load data
world <- ne_countries(scale = "medium", returnclass = "sf")

asiaSEA<-subset(world,sovereignt %in%
                  c("Myanmar","Laos","Cambodia","Vietnam","Thailand",
                    "Malaysia","Indonesia","East Timor","Philippines","Brunei",
                    "Singapore"))

asiaSEA_points<- sf::st_centroid(asiaSEA)

asiaSEA_points <- cbind(asiaSEA, sf::st_coordinates(sf::st_centroid(asiaSEA)))


# data vaccines (2021-07-07)
# https://ourworldindata.org/covid-vaccinations

list_country <- read.csv("owid-covid-data.csv")

country_select <-c("Myanmar","Laos","Cambodia","Vietnam","Thailand",
                   "Malaysia","Indonesia","Timor","Philippines","Brunei",
                   "Singapore")


country_vac <- list_country  |>
  dplyr::select(iso_code,location, date, people_vaccinated_per_hundred) |>
  dplyr::filter(location %in% country_select) |>
  dplyr::rename(sov_a3=iso_code) |>
  tidyr::drop_na()  |>
  dplyr::group_by(location) |>
  dplyr::filter(people_vaccinated_per_hundred == max(people_vaccinated_per_hundred)) 



data_map_asia <- merge(asiaSEA, country_vac, by = "sov_a3", all.x = TRUE)

# map
library("ggrepel")
library(RColorBrewer)
library(scales)

#aggregate data to get mean latitude and mean longitude 

map <- ggplot(data =data_map_asia) +
  geom_sf(aes(fill =people_vaccinated_per_hundred),size=0.2,show.legend=TRUE)+
  # scale_fill_continuous() +
  #  scale_fill_distiller(name="Percent", palette = "YlGn", breaks = pretty_breaks(n = 5))+
  scale_fill_gradient(name="Percentage (%)", limits=c(0,70), low="lightblue", high="darkblue")+
  labs( x = "Longitude", y = "Latitude") +
  labs(title="Pourcentage population vaccinée au 1 juillet 2021") +
  # geom_point(data = asiaSEA_points, aes(x = X, y = Y),colour="red",size = 2)+ 
  # geom_text_repel(data = asiaSEA_points, aes(x = X, y = Y, label = sovereignt), fontface = "bold",
                  # nudge_x = c(-10, 0,0,0,0,0,0,0,0,0,0), 
                  # nudge_y = c(-10,0,0,0,0,0,0,0,0,0,0)
                  # nudge_x = 2,nudge_y = 3, segment.curvature = -0.1, segment.ncp = 3,
                  # segment.angle = 20, arrow = arrow(length = unit(0.02, "npc")),size = 4,
                  # box.padding = unit(2, "lines"))+
  annotation_scale(location = "bl", width_hint = 0.1) +
  theme_bw()

map


jpeg("Plot.jpeg", width = 18, height = 12, units = 'in', res = 300)
map # Make plot
dev.off()
