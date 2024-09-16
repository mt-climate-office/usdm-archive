update_usdm_change <- 
  function(){
    
    format_date <-
      function(x){
        lubridate::stamp("Jan 4, 2000", orders = "%b %d, %Y")(x) %>%
          stringr::str_replace(" 0", " ")
      }
    
    tifs <- list.files("tif",
                       full.names = TRUE,
                       pattern = ".tif$")
    
    tifs <- tifs[
      tifs %>%
        sort() %>%
        magrittr::is_weakly_greater_than("tif/2024-01-01.tif")
    ] %>%
      {.[c(1, length(.))]} %>%
      terra::rast()
    
    
    
    change <- 
      terra::diff(tifs) 
    
    names(change) <- "usdm_class"
    
    # change[tifs[[1]] == 0 & tifs[[2]] == 0] <- NA
    
    # levels(change) <- data.frame(value = -5:5, 
    #                              usdm_class = 
    #                                c(
    #                                  paste0(5:1, " Class Improvement"),
    #                                  "No Change",
    #                                  paste0(1:5, " Class Degradation")
    #                                ))
    
    change %<>%
      terra::as.polygons() %>% 
      sf::st_as_sf() %>%
      tibble::as_tibble() %>%
      sf::st_as_sf() %>%
      dplyr::transmute(usdm_class = 
                         factor(usdm_class,
                                levels = -5:5,
                                labels = c(
                                  paste0(5:1, " Class Improved"),
                                  "No Change",
                                  paste0(1:5, " Class Degraded")
                                ),
                                ordered = TRUE))
    
    date <- lubridate::as_date(names(tifs))
    
    outfile <- file.path("png", "latest-change.png")
    
    conus <- 
      sf::read_sf("conus.parquet") %>%
      tigris::shift_geometry()
    
    p <-
      ggplot() + 
      geom_sf(data = dplyr::summarise(conus),
              fill = "gray80",
              color = NA,
              show.legend = FALSE) +
      geom_sf(data = change,
              aes(fill = usdm_class),
              color = "white",
              size = 0.1,
              show.legend = T) +
      geom_sf(data = conus,
              color = "white",
              alpha = 0,
              show.legend = FALSE,
              size = 0.2) +
      scale_fill_manual(values = rev(cols4all::c4a("tableau.classic_orange_blue", n = 11)),
                        drop = FALSE,
                        name = "US Drought Monitor\nClass Change\nStart of Calendar Year",
                        guide = guide_legend(direction = "vertical",
                                             title.position = "top",
                                             ncol = 2) ) +
      usdm_layout(footnote = paste0("Data released ", format_date(date[[2]]))) +
      theme(legend.text = element_text(size = 10),
            legend.key.height = unit(0.17, "inches"),
            legend.key.width = unit(0.17, "inches"),
            legend.position.inside = c(0.15,0.9))
    
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
    
    tifs <- 
      list.files("tif",
                 full.names = TRUE,
                 pattern = ".tif$") %>%
      terra::rast()
    
    usdm_history <-
      list(
        `D0–D4` = mean(tifs > 0),
        `D1–D4` = mean(tifs > 1),
        `D2–D4` = mean(tifs > 2),
        `D3–D4` = mean(tifs > 3),
        `D4` = mean(tifs > 4)
      )
    
    plot_history <- 
      function(x, y){
        outfile <- file.path("png", paste0("latest-history-", y, ".png"))
        
        p <-
          ggplot() + 
          geom_sf(data = dplyr::summarise(conus),
                  fill = "gray80",
                  color = NA,
                  show.legend = FALSE) +
          geom_raster(data = as.data.frame(x, xy = TRUE),
                      mapping = aes(x = x,
                                    y = y,
                                    fill = mean * 100)) +
          
          geom_sf(data = conus,
                  color = "white",
                  alpha = 0,
                  show.legend = FALSE) +
          cols4all::scale_fill_binned_c4a_seq(palette = "brewer.reds",
                                              breaks = seq(0,100,10),
                                              limits = c(0,100),
                                              # drop = FALSE,
                                              name = paste0("US Drought Monitor\n% of Weeks in Class ",y,"\n",format_date(lubridate::as_date(min(names(tifs))))," – ",
                                                            format_date(lubridate::as_date(max(names(tifs))))),
          ) +
          guides(
            fill = 
              guide_colorsteps(direction = "horizontal",
                               title.position = "top",
                               ncol = 2,
                               show.limits = TRUE)
          ) +
          usdm_layout(footnote = paste0("Data released ", format_date(date[[2]]))) +
          theme(legend.text = element_text(size = 12),
                legend.text.position = "bottom",
                legend.key.height = unit(0.3, "inches"),
                legend.key.width = unit(0.6, "inches"),
                legend.position.inside = c(0.15,0.9),
                legend.title = element_text(size = 16, 
                                            face = "bold", 
                                            hjust = 1)
          )
        
        
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
        
        return(outfile)
      }
    
    usdm_history %>%
      purrr::iwalk(~plot_history(x = terra::trim(.x), y = .y))
    
  }
