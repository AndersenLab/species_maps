library(tidyverse)
library(sf)
sf_use_s2(FALSE)

#Load date in format YYYYMMDD
date <- format(Sys.Date(), "%Y%m%d")


#Load the species sheets 
ce <- gsheet::gsheet2tbl(url = "https://docs.google.com/spreadsheets/d/10x-CcKNCl80F9hMcrGWC4fhP_cbekSzi5_IYBY2UqCc/edit#gid=538533765") %>%
    dplyr::filter(strain == isotype)%>%
    select(isotype, species, latitude, longitude)

cb <- gsheet::gsheet2tbl(url = "https://docs.google.com/spreadsheets/d/1IJHMLwuaxS_sEO31TyK5NLxPX7_qSd0bHNKverAv8-0/edit")%>%
    dplyr::filter(strain == isotype)%>%
    select(isotype, species, latitude, longitude)



world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(geounit != "Antarctica") 

ce_iso_loc <- gsheet::gsheet2tbl(url = "https://docs.google.com/spreadsheets/d/10x-CcKNCl80F9hMcrGWC4fhP_cbekSzi5_IYBY2UqCc/edit#gid=538533765") %>%
  dplyr::filter(!is.na(latitude)) %>%
    #classify country by latitutde and longitude
    sf::st_as_sf(coords = c("longitude", "latitude"), crs  =sf::st_crs(world), remove = F)

cb_iso_loc <- gsheet::gsheet2tbl(url = "https://docs.google.com/spreadsheets/d/1IJHMLwuaxS_sEO31TyK5NLxPX7_qSd0bHNKverAv8-0/edit")%>%
  dplyr::filter(!is.na(latitude)) %>%
    #classify country by latitutde and longitude
    sf::st_as_sf(coords = c("longitude", "latitude"), crs  =sf::st_crs(world), remove = F)

ct_iso_loc <- gsheet::gsheet2tbl(url = "https://docs.google.com/spreadsheets/d/1mqXOlUX7UeiPBe8jfAwFZnqlzhb7X-eKGK_TydT7Gx4/edit")%>%
  dplyr::filter(!is.na(latitude) & strain == isotype) %>%
    #classify country by latitutde and longitude
    sf::st_as_sf(coords = c("longitude", "latitude"), crs  =sf::st_crs(world), remove = F)

# Plot Maps
ce_iso_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white", lwd = 0.15) +
  ggplot2::theme(legend.position = "bottom") +
  geom_sf(data = ce_iso_loc, fill = "red", size = 1.75, stroke = 0.15, shape = 21)+
  theme_bw()

cb_iso_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white", lwd = 0.15) +
  ggplot2::theme(legend.position = "bottom") +
  geom_sf(data = cb_iso_loc, fill = "#228B22", size = 1.75, stroke = 0.15, shape = 21)+
  theme_bw() 

ct_iso_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = world, fill = "white", lwd = 0.15) +
  ggplot2::theme(legend.position = "bottom") +
  geom_sf(data = ct_iso_loc, fill = "blue", size = 1.75, stroke = 0.15, shape = 21)+
  theme_bw()

# Save Maps
ggsave(
    filename = glue::glue("{date}_ce_isotypes_map.png"),
    plot = ce_iso_map,
    width = 10,
    height = 10,
    units = "in",
    dpi = 300
)


ggsave(
    filename = glue::glue("{date}_cb_isotypes_map.png"),
    plot = cb_iso_map,
    width = 10,
    height = 10,
    units = "in",
    dpi = 300
)

ggsave(
    filename = glue::glue("{date}_ct_isotypes_map.png"),
    plot = ct_iso_map,
    width = 10,
    height = 10,
    units = "in",
    dpi = 300
)
https://github.com/mckeowr1/species_maps.git