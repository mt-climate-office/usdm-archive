noaa <-
  "https://droughtmonitor.unl.edu/webfiles/logos/NOAA/png/NOAA-logo-color.png" %>%
  httr::GET() %>%
  httr::content(type = "image/png") %>%
  grid::rasterGrob(interpolate=TRUE,
                   width = unit(0.75, "in"),
                   height = unit(0.75, "in"),
                   x = unit(-0.2, "npc"),
                   y = unit(0.28, "npc"),
                   just = "left")

nidis <-
  "https://upload.wikimedia.org/wikipedia/commons/e/ef/NOAA-NIDIS-logo.png" %>%
  httr::GET() %>%
  httr::content(type = "image/png") %>%
  grid::rasterGrob(interpolate=TRUE,
                   # width = unit(0.75, "in"),
                   height = unit(0.7, "in"),
                   x = unit(-0.04, "npc"),
                   y = unit(0.28, "npc"),
                   just = "center")

ndmc <- 
  "https://droughtmonitor.unl.edu/webfiles/logos/NDMC/png/NDMC-logo-color.png" %>%
  httr::GET() %>%
  httr::content(type = "image/png") %>%
  grid::rasterGrob(interpolate=TRUE,
                   width = unit(0.75, "in"),
                   height = unit(0.75, "in"),
                   x = unit(0.12, "npc"),
                   y = unit(0.28, "npc"),
                   just = "right")



mco <-
  "MCO_logo.png" %>%
  png::readPNG() %>%
  grid::rasterGrob(interpolate=TRUE,
                   width = unit(0.6 * 3600/1325, "in"),
                   height = unit(0.6, "in"),
                   x = unit(-0.04, "npc"),
                   y = unit(0.11, "npc"),
                   just = "center")

usdm_layout <-
  function(attribution = "The U.S. Drought Monitor is jointly produced by the National Drought\nMitigation Center at the University of Nebraska-Lincoln, the United States\nDepartment of Agriculture, and the National Oceanic and Atmospheric Administration.\nMap data courtesy of NDMC. Map courtesy of the Montana Climate Office.",
           footnote = ""){
    return(
      list(
        theme_void(base_size = 24),
        theme(legend.position = "inside",
              legend.position.inside = c(0.11,0.9),
              legend.justification = c(1,1),
              plot.margin = unit(c(0,0,0,0.2), "npc"),
              # legend.justification = c(0.1,0.5),
              # legend.key.width = unit(0.1, "npc"),
              legend.text.position = "left",
              legend.title = element_text(size = 18, 
                                          face = "bold", 
                                          hjust = 1),
              legend.text = element_text(
                size = 14),
              strip.text.x = element_text(margin = margin(b = 5))),
        annotation_custom(noaa, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
        annotation_custom(nidis, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
        annotation_custom(ndmc, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
        annotation_custom(mco, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
        annotation_custom(      grid::textGrob(
          label = 
            attribution,
          just = "left",
          x = unit(-0.21, "npc"),
          y = unit(0.43, "npc"),
          gp = grid::gpar(fontface = "italic",
                          fontsize = 6)
        ), 
        xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf),
        annotation_custom(
          grid::textGrob(
            label = 
              footnote,
            just = "right",
            x = unit(0.95, "npc"),
            y = unit(0.05, "npc"),
            gp = grid::gpar(fontface = "italic",
                            fontsize = 6)
          ), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
      )
    )
  }
