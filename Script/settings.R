########################################
## Ställer in kommunnamn, färger etc####
########################################

get_settings <- function() {
  kommunkod <- c("0330", "0331", "0360", "0380", "0381", "0382", "0305", "0319")
  kommuner <- sort(c("Knivsta", "Heby", "Tierp", "Uppsala", "Enköping", "Östhammar", "Håbo", "Älvkarleby"))
  
  lan <- 'Uppsala län'
  lanskod <- "03"
  
  # Fonts
  try(showtext::font_add_google("Source Sans Pro", "sourcesanspro"), silent = TRUE)
  font_add("Arial", "path/to/arial.ttf")
  showtext::showtext_auto()
  
  # Theme
  region_theme_pdf <- ggplot2::theme(
    text = element_text(family = "sourcesanspro", size = 18),
    plot.title = element_text(family = "Arial", face = "bold", size = 24, hjust = 0.5),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(family = "Arial", face = "bold", size = 18),
    strip.text = element_text(family = "Arial", face = "bold", size = 16),
    axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0.5),
    plot.margin = margin(15, 30, 15, 15)
  )
  ggplot2::theme_set(ggplot2::theme_minimal() + region_theme_pdf)
  
  kommun_colors <- c(
    "Enköping" = "#D57667",
    "Heby" = "#F9B000", 
    "Håbo" = "#019CD7",
    "Knivsta" = "#D0342C",
    "Tierp" = "#4AA271",
    "Uppsala" = "#6F787E",
    "Älvkarleby" = "#8B4A9C",
    "Östhammar" = "#E67E22"
  )
  
  
  riket_narliggande <- c('00','03',"04", "05", "18", "19" )
  
  upplat_colors <- c(
    "hyresrätt"     = "#D57667",
    "bostadsrätt"   = "#F9B000",
    "äganderätt"    = "#019CD7"
    # add more if you have more upplåtelseformer
  )
  # Returnera allt som lista
  list(
    kommunkod = kommunkod,
    kommuner = kommuner,
    kommun_colors = kommun_colors,
    riket_narliggande=riket_narliggande,
    upplat_colors=upplat_colors,
    lan = lan,
    lanskod = lanskod
  )
}
