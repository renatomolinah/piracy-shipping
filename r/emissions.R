  shipping_data_stars <- aggregate_spatial_shipping_activity %>%
    stars::st_as_stars(dims  = c('lon_bin','lat_bin'))%>%
    sf::st_set_crs(4326) %>%
    sf::st_as_sf() %>%
    sf::st_transform(global_map_projection)
  
    # Load world land
  world_land <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>%
    sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
    sf::st_transform(global_map_projection)
  
  # Create bounding box of world, to use as outline in the projected maps
  # vectors of latitudes and longitudes that go once around the 
  # globe in 1-degree steps
  lats <- c(90:-90, -90:90, 90)
  longs <- c(rep(c(180, -180), each = 181), 180)
  
  world_bbox_sf <- 
    list(cbind(longs, lats)) %>%
    sf::st_polygon() %>%
    sf::st_sfc( # create sf geometry list column
      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    ) %>% 
    sf::st_sf() %>%
    sf::st_transform(global_map_projection)
  
 ggplot2::ggplot() +
    ggplot2::geom_sf(data = shipping_data_stars,
                      aes(fill = emissions_co2_mt,
                          color = emissions_co2_mt)) +
    ggplot2::geom_sf(data = world_land,
                     color = "darkgrey",
                     fill = "darkgrey") +
    ggplot2::geom_sf(data = world_bbox_sf,
                     fill = NA,
                     color = "black") +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    scale_fill_gradient2(bquote(atop("Emissions of "~CO[2],"(MT)")),
                         trans = "log10",
                         labels = scales::comma,
                         breaks = c(100,1000,10000,100000,1e6),
                         low = "white",
                         high = "dodgerblue4")+
    scale_color_gradient2(bquote(atop("Emissions of "~CO[2],"(MT)")),
                         trans = "log10",
                         labels = scales::comma,
                         breaks = c(100,1000,10000,100000,1e6),
                         low = "white",
                         high = "dodgerblue4")+
    theme(legend.key.size = unit(1, 'cm'))
  
  ggplot2::ggsave(filename = "figures/map_emissions.png",
                  plot,
                  height = 3,
                  width = 7,
                  dpi = 300)
  
  return(plot)