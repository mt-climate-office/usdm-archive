get_usdm_outlook <-
  function(){
    
    latest <-
      list(
        sf::read_sf("/vsizip//vsicurl/https://ftp.cpc.ncep.noaa.gov/GIS/droughtlook/mdo_polygons_latest.zip/DO_Merge_Clip.shp") %>%
          dplyr::mutate(scale = "monthly"),
        sf::read_sf("/vsizip//vsicurl/https://ftp.cpc.ncep.noaa.gov/GIS/droughtlook/sdo_polygons_latest.zip/DO_Merge_Clip.shp") %>%
          dplyr::mutate(scale = "seasonal")
      ) %>%
      dplyr::bind_rows() %>%
      sf::st_make_valid() %>%
      dplyr::filter(Outlook != "No_Drought") %>%
      dplyr::transmute(outlook = factor(Outlook,
                                        levels = c("Removal",
                                                   "Improvement",
                                                   "Persistence",
                                                   "Development"),
                                        labels = c("Removal",
                                                   "Improvement",
                                                   "Persistence",
                                                   "Development"),
                                        ordered = TRUE),
                       date = lubridate::mdy(Fcst_Date),
                       target = Target,
                       scale) %>%
      sf::st_cast("MULTIPOLYGON") %>%
      dplyr::arrange(dplyr::desc(date), scale, outlook) %>%
      sf::st_transform("EPSG:5070") %>%
      sf::st_intersection(
        sf::st_union(
          sf::st_geometry(
            sf::read_sf("conus.parquet")
          )
        )
      ) %>%
      sf::st_cast("MULTIPOLYGON")
    
    outfile <-
      file.path("outlook", "parquet", paste0(latest$date[[1]],".parquet"))
    
    sf::write_sf(latest,
                 outfile,
                 layer_options = c("COMPRESSION=BROTLI",
                                   "GEOMETRY_ENCODING=GEOARROW",
                                   "WRITE_COVERING_BBOX=NO"),
                 driver = "Parquet")
    
    return(sf::read_sf(outfile))
    
  }

update_usdm_outlook <- 
  function(){
    outlook <- 
      get_usdm_outlook() %>%
      sf::st_cast("POLYGON") %>%
      tigris::shift_geometry() %>%
      sf::st_make_valid() %>%
      dplyr::group_by(outlook, date, scale, target) %>%
      dplyr::summarise(.groups = "drop") %>%
      sf::st_cast("MULTIPOLYGON") %>%
      dplyr::mutate(outlook = factor(outlook,
                                     levels = c("Removal",
                                                "Improvement",
                                                "Persistence",
                                                "Development"),
                                     labels = c("Removal",
                                                "Improvement",
                                                "Persistence",
                                                "Development"),
                                     ordered = TRUE)) %>%
      dplyr::group_by(scale) %>% 
      {
        magrittr::set_names(dplyr::group_split(.),
                            dplyr::group_keys(.)[[1]]
        )
      }
    
    purrr::iwalk(
      .x = outlook,
      .f = 
        \(x,y){
          date <- x$date[[1]]
          
          outfile <- file.path("outlook", "png", paste0(date,"-",y,".png"))
          
          conus <- 
            sf::read_sf("conus.parquet") %>%
            tigris::shift_geometry()
          
          p <-
            ggplot(x) +
            geom_sf(data = 
                      dplyr::summarise(
                        conus
                      ),
                    fill = "gray80",
                    color = NA,
                    show.legend = FALSE) +
            geom_sf(aes(fill = outlook),
                    color = "white",
                    size = 0.1,
                    show.legend = T) +
            geom_sf(data = conus,
                    color = "white",
                    alpha = 0,
                    show.legend = FALSE,
                    size = 0.2) +
            scale_fill_manual(values = c("#A4A056",
                                         "#D6C8AF",
                                         "#884F39",
                                         "#FED750"),
                              drop = FALSE,
                              name = paste0("US Drought Outlook\nthrough ",x$target[[1]]),
                              guide = guide_legend(direction = "vertical",
                                                   title.position = "top") ) +
            usdm_layout(attribution = "The U.S. Drought Outlook is produced by the National Oceanic and Atmospheric\nAdministration's Climate Prediction Center based on data from the\nNational Drought Mitigation Center at the University of Nebraska-Lincoln.\nMap data courtesy of NDMC and CPC. Map courtesy of the Montana Climate Office.",
                        footnote = paste0("Data released ", format(lubridate::as_date(date), "%B %e, %Y"))%>% stringr::str_squish())
          
          gt <- ggplot_gtable(ggplot_build(p))
          gt$layout$clip[gt$layout$name == "panel"] <- "off"
          
          grid::grid.draw(gt) %>%
            ggsave(plot = .,
                   filename = outfile,
                   device = ragg::agg_png,
                   width = 10,
                   height = 5.14,
                   # height = 6.86,
                   bg = "white",
                   dpi = 600)
          
          grid::grid.draw(gt) %>%
            ggsave(plot = .,
                   filename = file.path("outlook", "png", paste0("latest-",y,".png")),
                   device = ragg::agg_png,
                   width = 10,
                   height = 5.14,
                   # height = 6.86,
                   bg = "white",
                   dpi = 600)
          
        })
  }
