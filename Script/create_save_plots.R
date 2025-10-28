#########################
#### Sparar diagram #####
#########################
{
  source("Script/install_load_packages.R")
  source("Script/settings.R")
  install_and_load()
  settings <- get_settings()
  
  kommunkod <- settings$kommunkod
  kommuner <- settings$kommuner
  kommun_colors <- settings$kommun_colors
  upplat_colors <- settings$upplat_colors
  
}


##### Storleksfördelning########

flerbostadsarea <- function(){
  
  df_bostadsarea <- read.csv('Data/df_bostadsarea.csv')
  # vektor med lägenhetsstorlekar
  ordning <- c("< 31 kvm", "31-40 kvm", "41-50 kvm", "51-60 kvm", "61-70 kvm",
               "71-80 kvm", "81-90 kvm", "91-100 kvm", "101-110 kvm",
               "111-120 kvm", "121-130 kvm", "131-140 kvm", "141-150 kvm",
               "191-200 kvm", "> 200 kvm")
  
  
  # Plockar ut senaste årets data på flerbostadshus
  flerbostadsarea <- df_bostadsarea %>%  filter(hustyp=='flerbostadshus', år== max(as.numeric(år)), Antal > 0) %>% 
    group_by(region) %>%  mutate(Andel = round((Antal/ sum(Antal))*100),
                                 bostadsarea = factor(bostadsarea, levels = ordning))%>%
    filter(!is.na(bostadsarea)) # tar ej med övrig 
  
  
  p <- ggplot(flerbostadsarea, aes(x = bostadsarea, y = Andel, fill = region)) + 
    geom_col() +
    facet_wrap(vars(region), nrow = 4) +
    scale_fill_manual(values = kommun_colors) +
    ggtitle("Fördelningen av bostadsarea i flerbostadshus") +
    xlab("Area") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90),
          legend.position="none",
          text = element_text(family = "sourcesanspro", size = 14),
          axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0.5))
  
  
  ggsave('Figurer/flerbostadsarea.svg',plot = p,device = "svg", width = 7, height = 5)
  
}



###### Utvekling upplåtelseformer #######
# region
region_utv_upp <- function(){
  df_supplatelse <- read.csv('Data/df_supplatelse.csv')
  # Liknande kod som ovan men summering över hela regionen
  tid_bostad_befolk <- df_supplatelse %>% 
    filter(
      hustyp != "specialbostäder",
      upplåtelseform != "uppgift saknas",
      as.numeric(år) > 2012
    ) %>% 
    group_by(upplåtelseform, år) %>%   # <-- no region grouping
    summarise(Total = sum(Antal), .groups = "drop")
  
  tid_bostad_befolk_totals <- tid_bostad_befolk %>%
    group_by(år) %>%
    summarise(Total_all = sum(Total), .groups = "drop")
  
  tid_bostad_befolk <- tid_bostad_befolk_totals %>%
    left_join(tid_bostad_befolk, by = "år")
  


  # Se till att år är numeriskt
  tid_bostad_befolk <- tid_bostad_befolk %>%
    mutate(
      år = as.integer(år),
      upplåtelseform = factor(upplåtelseform,
                              levels = c("hyresrätt","bostadsrätt","äganderätt"))
    )
  
  # Gör long-df för linjerna
  lines_df <- tid_bostad_befolk %>%
    select(år, Total_all) %>%
    distinct() %>%        # undvik dubbletter från flera upplåtelseformer
    tidyr::pivot_longer(
      cols = c(Total_all),
      names_to = "Typ",
      values_to = "Total"
    )
  
  # Plot
  p <- ggplot() +
    # staplar
    geom_col(data = tid_bostad_befolk,
             aes(x = år, y = Total, fill = upplåtelseform),
             position = position_dodge(width = 0.9), width = 0.8) +
    
    # linje för total
    geom_line(data = lines_df,
              aes(x = år, y = Total, color = Typ, group = Typ),
              linewidth = 1.2) +
    
    # x-axel varannat år
    scale_x_continuous(
      breaks = seq(min(tid_bostad_befolk$år, na.rm = TRUE),
                   max(tid_bostad_befolk$år, na.rm = TRUE),
                   by = 2)
    ) +
    
    # färger för staplar (upplåtelseformer)
    scale_fill_manual(
      values = upplat_colors,
      labels = tools::toTitleCase(names(upplat_colors))
    ) +
    
    # färger för linjen (total)
    scale_color_manual(
      values = c("Total_all" = "#B81867"),
      labels = c("Total_all" = "Totalt")
    ) +
    
    labs(
      title = "Utveckling av upplåtelseformer",
      x = "År", y = "Antal",
      fill = "Upplåtelseform",
      color = ""   # rubrik för linjen
    ) +theme_minimal() +theme(
      text = element_text(family = "sourcesanspro", size = 14),
      axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0.5))
  
  
  
  ggsave('Figurer/region_utv_upp.svg',plot = p,device = "svg", width = 7, height = 5)
  
}


# kommun 

kommun_utv_upp <- function(){
  df_supplatelse <- read.csv('Data/df_supplatelse.csv')
  # plockar ut alla ägande småhus, hyresbostäder i flerbostadshus och småhus från 2012(då knivsta blev egen kommun)
  tid_bostad_befolk <- df_supplatelse %>% filter(hustyp != 'specialbostäder',upplåtelseform != 'uppgift saknas', as.numeric(år) > 2012) %>% 
    group_by(region, upplåtelseform, år)  %>%
    summarise(Total = sum(Antal), .groups = "drop")
  
  tid_bostad_befolk_totals <- tid_bostad_befolk %>%
    group_by(region, år) %>%
    summarise(Total_all = sum(Total), .groups = "drop")
  
  
  
  tid_bostad_befolk <- tid_bostad_befolk_totals %>% left_join(tid_bostad_befolk, by = c('region', 'år'))
  #ggplot(tid_bostad_befolk, aes(x = år, y = Total, fill=upplåtelseform)) + geom_col()
  
  tid_bostad_befolk$upplåtelseform <- factor(
    tid_bostad_befolk$upplåtelseform,
    levels = c("hyresrätt", "bostadsrätt", "äganderätt")  # ordning som du vill ha
  )
  
  upplatelse_plot_func <- function(kommun_val){
    
    # filtrera för vald kommun
    df <- tid_bostad_befolk %>%
      filter(region == kommun_val) %>%
      mutate(år = as.integer(år))
    
    # egen df för linjen (så vi inte får dubletter)
    lines_df <- df %>%
      select(år, Total_all) %>%
      distinct() %>%
      tidyr::pivot_longer(
        cols = c(Total_all),
        names_to = "Typ",
        values_to = "Total"
      )
    
    p <- ggplot() +
      # staplar per upplåtelseform
      geom_col(data = df,
               aes(x = år, y = Total, fill = upplåtelseform),
               position = position_dodge(width = 0.9), width = 0.8) +
      
      # linje för Totalt
      geom_line(data = lines_df,
                aes(x = år, y = Total, color = Typ, group = Typ),
                linewidth = 1.2) +
      
      scale_x_continuous(
        breaks = seq(min(df$år, na.rm = TRUE),
                     max(df$år, na.rm = TRUE),
                     by = 2)
      ) +
      
      # färger för staplar (upplåtelseformer)
      scale_fill_manual(
        values = upplat_colors,
        labels = tools::toTitleCase(names(upplat_colors))
      ) +
      
      # färger för linjen
      scale_color_manual(
        values = c("Total_all" = "black"),
        labels = c("Total_all" = "Totalt")
      ) +
      
      labs(
        title = kommun_val,
        x = "År", y = "Antal",
        fill = "Upplåtelseform",
        color = ""  # rubrik för linjen i legend
      ) +
      theme_get()
    
    p
  }
  
  
  for (r in unique(tid_bostad_befolk$region)) {
    p <- upplatelse_plot_func(r)
    
    
    # spara plot som SVG
    file_name <- paste0("Figurer/plot_upplatelseform_", r, ".svg")
    ggsave(filename = file_name, plot = p, width = 14, height = 8, device = "svg")
    
    
  }
  
  
}


######### Befolkningsförändring i relation till färdigställda bostäder ##########

# region

nybygg_region <- function(){
  
  nybyggda <- read.csv('Data/df_nybyggda.csv')
  df_befolkf <- read.csv('Data/df_befolkf.csv')
  tid_df_nybyggda <- df_nybyggda %>% 
    filter( as.numeric(år) > 2012
    ) %>% 
    group_by(upplåtelseform, år) %>%   # <-- no region grouping
    summarise(nybygg = sum(Antal), .groups = "drop")
  tid_df_nybyggda$år <- as.integer(tid_df_nybyggda$år)
  nybyggda_total <- tid_df_nybyggda %>% 
    group_by(år) %>%   # <-- no region grouping
    summarise(Total_nybygg = sum(nybygg))
  
  tid_df_befolkf <- df_befolkf %>% rename(Antal_personer = Antal.personer) %>%  filter(as.numeric(år) > 2012) %>% 
    group_by(år) %>%  summarise(Total_personer = sum(Antal_personer), .groups = "drop")
  
  tid_nybygg_befolk <- tid_df_nybyggda %>%
    left_join(tid_df_befolkf, by = "år")%>%
    left_join(nybyggda_total, by = "år")
  
  # gör long-df för linjerna
  lines_df <- tid_nybygg_befolk %>%
    select(år, Total_nybygg, Total_personer) %>%
    tidyr::pivot_longer(
      cols = c(Total_nybygg, Total_personer),
      names_to = "Typ",
      values_to = "Total"
    )
  
  tid_nybygg_befolk$upplåtelseform <- factor(
    tid_nybygg_befolk$upplåtelseform,
    levels = c("hyresrätt", "bostadsrätt", "äganderätt")  # ordning som du vill ha
  )
  
  tid_nybygg_befolk <- tid_nybygg_befolk %>%
    mutate(år = as.integer(år))
  
  lines_df <- lines_df %>%
    mutate(år = as.integer(år))
  
  # plott
  p <- ggplot() +
    # staplar
    geom_col(data = tid_nybygg_befolk,
             aes(x = år, y = nybygg, fill = upplåtelseform),
             position = position_dodge(width = 0.9), width = 0.8) +
    
    # linjer (nu kommer färgerna från Typ)
    geom_line(data = lines_df,
              aes(x = år, y = Total, color = Typ, group = Typ),
              linewidth = 1.2) +
    geom_text(data = lines_df,
              aes(x = år, y = Total, label = Total),
              vjust = -1, size = 4,show.legend = FALSE) +
    scale_x_continuous(
      breaks = seq(min(tid_nybygg_befolk$år), max(tid_nybygg_befolk$år), by = 2)
    ) +
    
    scale_fill_manual(
      values = upplat_colors,
      labels = tools::toTitleCase(names(upplat_colors))
    ) +
    scale_color_manual(
      values = c("Total_nybygg" = "#B81867",
                 "Total_personer" = "#4AA271"),
      labels = c("Total_nybygg" = "Nybyggda bostäder",
                 "Total_personer" = "Befolkningsförändring")
    ) +
    labs(
      title = "Uppsala län",
      x = "År", y = "Antal",
      fill = "",
      color = ""   # rubrik för linjer
    ) +
    theme_minimal() +theme(
      text = element_text(family = "sourcesanspro", size = 14),
      axis.title.y = element_text(angle = 0, vjust = 1, hjust = 0.5))
  
  
  ggsave(filename = 'Figurer/nybygg_region.svg', plot = p, width = 7, height = 6, device = "svg")
}


# Kommun

nybygg_kommun <- function(){
  
  nybyggda <- read.csv('Data/df_nybyggda.csv')
  df_befolkf <- read.csv('Data/df_befolkf.csv')
  
  # Liknande kod som ovan men splittar per kommun
  tid_df_nybyggda <- df_nybyggda %>% 
    filter( as.numeric(år) > 2012
    ) %>% 
    group_by(region, upplåtelseform, år) %>%   
    summarise(nybygg = sum(Antal), .groups = "drop")
  
  tid_df_nybyggda$år <- as.integer(tid_df_nybyggda$år)
  
  nybyggda_total <- tid_df_nybyggda %>% 
    group_by(år, region) %>%   
    summarise(Total_nybygg = sum(nybygg), .groups = "drop")
  
  
  tid_df_befolkf <- df_befolkf %>% rename(Antal_personer = Antal.personer) %>%  filter(as.numeric(år) > 2012) %>% 
    group_by(region, år) %>%  summarise(Total_personer = sum(Antal_personer), .groups = "drop")
  
  tid_nybygg_befolk <- tid_df_nybyggda %>%
    left_join(tid_df_befolkf, by = c('region',"år"))%>%
    left_join(nybyggda_total, by = c('region',"år"))
  
  # gör long-df för linjerna
  lines_df <- tid_nybygg_befolk %>%
    select(region, år, Total_nybygg, Total_personer) %>%
    tidyr::pivot_longer(
      cols = c(Total_nybygg, Total_personer),
      names_to = "Typ",
      values_to = "Total"
    )
  
  tid_nybygg_befolk$upplåtelseform <- factor(
    tid_nybygg_befolk$upplåtelseform,
    levels = c("hyresrätt", "bostadsrätt", "äganderätt")  # ordning som du vill ha
  )
  
  tid_nybygg_befolk <- tid_nybygg_befolk %>%
    mutate(år = as.integer(år))
  
  lines_df <- lines_df %>%
    mutate(år = as.integer(år))
  
  plot_tid_nybygg_befolk <- function(kommun_val){
    p <- tid_nybygg_befolk %>%
      filter(region == kommun_val) %>%
      ggplot(aes(x = år)) +
      
      # bars per upplåtelseform
      geom_col(aes(y = nybygg, fill = upplåtelseform),
               position = position_dodge(width = 0.9), width = 0.8) +
      
      # linjer (nu kommer färgerna från Typ)
      geom_line(data = lines_df %>% filter(region == kommun_val),
                aes(x = år, y = Total, color = Typ, group = Typ),
                linewidth = 1.2) +
      geom_text(data = lines_df%>% filter(region == kommun_val),
                aes(x = år, y = Total, label = Total),
                vjust = -1, size = 5,show.legend = FALSE) +
      scale_x_continuous(
        breaks = seq(min(tid_nybygg_befolk$år), max(tid_nybygg_befolk$år), by = 2)
      ) +
      scale_fill_manual(
        values = upplat_colors,
        labels = tools::toTitleCase(names(upplat_colors))
      ) +
      scale_color_manual(
        values = c("Total_nybygg" = "#6F787E",
                   "Total_personer" = "#4AA271"),
        labels = c("Total_nybygg" = "Nybyggda bostäder",
                   "Total_personer" = "Befolkningsförändring")
      ) +
      labs(title = kommun_val,
           x = "År", y = "Antal",
           fill = "",
           color = "" ) +
      theme_get()
    
    p
  }
  
  for (r in unique(tid_nybygg_befolk$region)) {
    p <- plot_tid_nybygg_befolk(r)
    
    
    # spara plot som SVG
    file_name <- paste0("Figurer/plot_tid_nybygg_befolk_", r, ".svg")
    ggsave(filename = file_name, plot = p, width = 14, height = 8, device = "svg")
    
    
  }
  
  
}


######## Uppskatat behov av bostäder ###############

uppskatt_behov <- function(){
  # Läser in data och skapar set för basår
  folkmangd <- read.csv('Data/df_folkmangd.csv')
  folkmangd_tot <- folkmangd %>% filter(år == 2006) %>%  group_by(region) %>% 
    summarise(Total_folkmängd = sum(Folkmängd), .groups = 'drop') 
  
  df_supplatelse <- read.csv('Data/df_supplatelse.csv')
  
  # Summerar
  df_supplatelse_2006 <- df_supplatelse %>% filter(år =='2006') %>% 
    group_by(region) %>% summarise(Total_bostader = sum(Antal), .groups='drop')
  
  df_kvot <- left_join(df_supplatelse_2006,folkmangd_tot, by='region' )
  # Beräknar kvoten
  df_kvot$Kvot <- df_kvot$Total_folkmängd / df_kvot$Total_bostader # beräknar kvoten
  
  colnames(df_kvot) <- c('Region', 'Antal bostäder', 'Antal vuxna', 'Kvot')
  df_nybyggda <- read.csv('Data/df_nybyggda.csv')
  df_befolkf <- read.csv('Data/df_befolkf.csv')
  
  df_nybyggda$år <- as.integer(df_nybyggda$år)
  # Summera nybygg utan upplåtelseform
  tid_df_nybyggda_total <- df_nybyggda %>% 
    filter(as.numeric(år) > 2012) %>% 
    group_by(region, år) %>%   
    summarise(Total_nybygg = sum(Antal), .groups = "drop")
  
  # Befolkning
  tid_df_befolkf <- df_befolkf %>% 
    rename(Antal_personer = Antal.personer) %>%  
    filter(as.numeric(år) > 2012) %>% 
    group_by(region, år) %>%  
    summarise(Total_personer = sum(Antal_personer), .groups = "drop")
  
  # Lägg på kvot och uppskattat bostadsbehov
  tid_df <- tid_df_befolkf %>%
    left_join(tid_df_nybyggda_total, by = c("region", "år")) %>%
    left_join(df_kvot %>% select(Region, Kvot), by = c("region" = "Region")) %>%
    mutate(Uppskattat_behov = ceiling(Total_personer / Kvot))
  
  
  # Long-format för linjer
  lines_df <- tid_df %>%
    select(region, år, Total_personer, Uppskattat_behov) %>%
    pivot_longer(
      cols = c(Total_personer, Uppskattat_behov),
      names_to = "Typ",
      values_to = "Total"
    )
  tid_df <- tid_df %>% mutate(år = as.integer(år))
  # Plotfunktion
  plot_tid_nybygg_befolk_tot <- function(kommun_val){
    df_plot <- tid_df %>% filter(region == kommun_val)
    lines_plot <- lines_df %>% 
      filter(region == kommun_val)  # Bostadsbrist är nu med
    
    p<- ggplot(df_plot, aes(x = år)) +
      # Totala nybyggda som kolumner
      geom_col(aes(y = Total_nybygg, fill = "Nybyggda bostäder"), width = 0.8) +
      # Linjer för befolkning, uppskattat behov och bostadsbrist
      geom_line(data = lines_plot,
                aes(x = år, y = Total, color = Typ, group = Typ),
                linewidth = 2) +
      geom_text(data = lines_plot,
                aes(x = år, y = Total, label = Total),
                vjust = -1, size = 6, show.legend = FALSE) +
      scale_x_continuous(breaks = seq(min(df_plot$år), max(df_plot$år), by = 2)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
      scale_color_manual(
        values = c(
          "Total_personer" = "#4AA271",
          "Uppskattat_behov" = "#F9B000"
        ),
        labels = c(
          "Total_personer" = "Befolkningsförändring",
          "Uppskattat_behov" = "Uppskattat bostadsbehov"
        )
      ) +scale_fill_manual(
        values = c("Nybyggda bostäder" = "#6F787E"),
        labels = c("Nybyggda bostäder" = "Nybyggda bostäder")
      ) +
      labs(title = kommun_val, x = "År", y = "Antal", color = "", fill = "") +
      theme_get()
    
    p
  }
  
  for (r in sort(kommuner)){
    p <- plot_tid_nybygg_befolk_tot(r)
    
    
    # spara plot som SVG
    file_name <- paste0("Figurer/plot_bostadsbrist_", r, ".svg")
    ggsave(filename = file_name, plot = p, width = 14, height = 8, device = "svg")
    
    
  }
  
  
}
######### Deso upplåtelseform#######

deso_upplat <- function(){
  df_deso <- read.csv('Data/df_deso.csv')
  suppressMessages({
    suppressWarnings({
      st_layers("DeSO_2025.gpkg")
      deso_sf <- st_read("DeSO_2025.gpkg", layer = "DeSO_2025", quiet = TRUE) %>%
        filter(lanskod == "03") # we keep only Uppsala län
    })
  })
  
  df_deso <- df_deso %>%
    rename(desokod = region)
  
  deso_sf <- left_join(deso_sf, df_deso, by = "desokod")
  
  
  
  ## plockar ut vanligaste upplåtelseformen och lägger till andelar när man klickar på regionen.
  
  #  Mest populär upplåtelseform per DeSO 
  mest_popular_upplat <- df_deso %>%
    group_by(desokod) %>%
    slice_max(order_by = Antal, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(Popularaste_upplåtelseform = upplåtelseform) %>%
    select(desokod, Popularaste_upplåtelseform)
  
  #  Räkna andelar per DeSO 
  andelar_deso <- df_deso %>% filter(upplåtelseform != 'uppgift saknas') %>% 
    group_by(desokod) %>%
    mutate(
      Total = sum(Antal, na.rm = TRUE),
      Andel = round(100 * Antal / Total, 1)
    ) %>%
    ungroup()
  
  #  Bygg popup-texten med andelar
  popup_text <- andelar_deso %>%
    group_by(desokod) %>%
    summarise(
      popup = paste0(
        "DeSO: ", unique(desokod), "<br>",
        paste0(
          str_to_title(upplåtelseform), ": ", Andel, "% (", Antal, " st)",
          collapse = "<br>"
        )
      ),
      .groups = "drop"
    )
  
  #  Slå ihop geometri, populäraste form & popup 
  deso_sf_pop <- deso_sf %>%
    left_join(mest_popular_upplat, by = "desokod") %>%
    left_join(popup_text, by = "desokod")
  
  # Säkerställ samma faktorordning + titelfall
  deso_sf_pop$Popularaste_upplåtelseform <- factor(
    deso_sf_pop$Popularaste_upplåtelseform,
    levels = names(upplat_colors),
    labels = tools::toTitleCase(names(upplat_colors))
  )
  
  #  Rita kartan 
  map <- mapview(
    deso_sf_pop,
    zcol = "Popularaste_upplåtelseform",
    legend = TRUE,
    layer.name = "Vanligaste upplåtelseform",
    col.regions = upplat_colors,
    popup = deso_sf_pop$popup
  )
  
  map@map <- map@map %>%
    htmlwidgets::prependContent(
      htmltools::tags$style(
        ".info.legend { text-align: left !important; }"
      )
    )
  
  
  suppressMessages({
    suppressWarnings({
      st_write(deso_sf, "Data/deso_sf.gpkg", delete_dsn = TRUE, quiet = TRUE)
    })
  })
  return(map)
  
  
}


####### Byggnadsperiod #########

# region
byggnadsperiod_region <- function(){
  
  df_byggnadsperiod <- read.csv('Data/df_byggnadsperiod.csv')
  
df_byggnadsperiod$år <- as.integer(df_byggnadsperiod$år)
df_byggnadsperiod_plot <- df_byggnadsperiod%>%filter(byggnadsperiod != 'uppgift saknas', år== max(as.numeric(år))) %>%  
  group_by(hustyp, byggnadsperiod) %>% 
  summarise(Total = sum(Antal),.groups = "drop")


df_byggnadsperiod_plot <- df_byggnadsperiod_plot %>%
  mutate(hustyp_label = tools::toTitleCase(hustyp))
# Egna färger för hustyp
my_colors <- c(
  "Småhus" = "#019CD7",
  "Flerbostadshus" = "#D57667",
  "Övriga Hus" = "#6F787E"
)


fig <- plot_ly(
  data = df_byggnadsperiod_plot,
  x = ~byggnadsperiod,
  y = ~Total,
  color = ~hustyp_label,
  colors = my_colors,
  type = "bar"
) %>%
  layout(margin = list(t = 100),
         barmode = "group",   # staplar bredvid varandra (alt. "stack")
         title = "Byggnadsperioder per hustyp år 2024",
         xaxis = list(title = "Byggnadsperiod"),
         yaxis = list(title = "Antal")
  )
fig <- config(
  fig,
  modeBarButtonsToRemove = c(
    'zoom2d',     # zoom button
    'pan2d',      # pan button
    'select2d',   # box select
    'lasso2d',    # lasso select
    'zoomIn2d',   # zoom in
    'zoomOut2d'   # zoom out
  ),toImageButtonOptions = list(
    format = "svg",
    filename = "byggnadsperiod_region"),
  displaylogo = FALSE)   # remove plotly logo/link
fig
}

# kommun
byggnadsperiod_kommun <- function(){
  
  df_byggnadsperiod <- read.csv('Data/df_byggnadsperiod.csv')
  
  
  
  df_byggnadsperiod$år <- as.integer(df_byggnadsperiod$år)
  
  df_byggnadsperiod_plot <- df_byggnadsperiod%>%filter(byggnadsperiod != 'uppgift saknas', år== max(as.numeric(år))) %>%  
    group_by(hustyp, byggnadsperiod, region) %>% 
    summarise(Total = sum(Antal),.groups = "drop")%>%
    mutate(hustyp_label = tools::toTitleCase(hustyp),
           hustyp_label = factor(hustyp_label, levels = c("Flerbostadshus","Småhus","Övriga Hus")))
  
  
  
  # Egna färger för hustyp
  my_colors <- c(
    "Småhus" = "#019CD7",
    "Flerbostadshus" = "#D57667",
    "Övriga Hus" = "#6F787E"
  )
  
  
  fig <- plot_ly()
  
  kommuner <- unique(df_byggnadsperiod_plot$region)
  
  # Lista som håller spårindex per kommun
  spår_per_kommun <- list()
  idx <- 1
  fig <- plot_ly()
  ar_max <- max(as.integer(df_byggnadsperiod$år))
  
  for (k in kommuner) {
    filtered <- df_byggnadsperiod_plot %>% filter(region == k)
    n_spår <- n_distinct(filtered$hustyp_label)
    
    fig <- fig %>%
      add_bars(
        data = filtered,
        x = ~byggnadsperiod,
        y = ~Total,
        color = ~hustyp_label,
        colors = my_colors,
        visible = ifelse(k == kommuner[1], TRUE, FALSE)
      )
    
    spår_per_kommun[[k]] <- idx:(idx + n_spår - 1)
    idx <- idx + n_spår
  }
  
  fig <- fig %>%
    layout(
      margin = list(t = 100),
      title = paste("Byggnadsperioder per hustyp och kommun år ",ar_max ),
      xaxis = list(title = "Byggnadsperiod"),
      yaxis = list(title = "Antal"),
      barmode = "group",
      legend = list(title = list(text = "Hustyp")),
      
      # Dropdown
      updatemenus = list(
        list(
          buttons = lapply(kommuner, function(k) {
            vis <- rep(FALSE, length(unlist(spår_per_kommun)))
            vis[spår_per_kommun[[k]]] <- TRUE
            list(
              method = "update",
              args = list(list(visible = vis)),
              label = k
            )
          }),
          direction = "down",
          x = -0.1, y = 1,
          pad = list(r = 10, t = 10),
          showactive = TRUE
        )
      ),
      
      # Label till dropdown
      annotations = list(
        list(
          text = "Kommun",
          x = -0.2, y = 1.032,
          xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 16)
        )
      )
    )
  fig <- config(
    fig,
    modeBarButtonsToRemove = c(
      'zoom2d',     # zoom button
      'pan2d',      # pan button
      'select2d',   # box select
      'lasso2d',    # lasso select
      'zoomIn2d',   # zoom in
      'zoomOut2d'   # zoom out
    ),toImageButtonOptions = list(
      format = "svg",
      filename = "byggnadsperiod_kommun"),
    displaylogo = FALSE)   # remove plotly logo/link
  
  
  fig
  
}



###### Fritidshus #########


fritidshus_reg <- function(){
  df_fritidshus <- read.csv('Data/df_fritidshus.csv')
  
  df_fritidshus_reg <- df_fritidshus %>% group_by(år) %>% 
    summarize(Antal = sum(Antal), .groups = "drop")
  ar_max <- max(df_fritidshus_reg$år)
  
  p <- ggplot(df_fritidshus_reg, aes(x=as.integer(år), y=Antal))+
    geom_line(color = '#B81867', linewidth = 2) + xlab('År') + ggtitle(paste('Totalt antal fritidshus i Uppsala län (1998-',ar_max, ')' ,sep=""))+
    theme_minimal()+ theme(
      text = element_text(family = "sourcesanspro"))
  
  ggsave(filename = 'Figurer/fritidshus_region.svg', plot = p, width =  7, height = 5, device = "svg")
}

fritidshus_kommun <- function(){
  df_fritidshus <- read.csv('Data/df_fritidshus.csv')
  ar_max <- max(df_fritidshus$år)
  
  p <- ggplot(df_fritidshus, aes(x=as.integer(år), y=Antal,  color = region))+
    geom_line( linewidth = 2) +facet_wrap(vars(region), scales='free_y')+ 
    scale_color_manual(values = kommun_colors) +
    xlab('År') + ggtitle(paste('Antal fritidshus per kommun (1998 och ', ar_max,')', sep=""))+
    scale_x_continuous(
      breaks = seq(min(df_fritidshus$år), max(df_fritidshus$år), by = 7)
    ) +theme_minimal() + theme(legend.position="none",
                               text = element_text(family = "sourcesanspro"))
  
  ggsave(filename = 'Figurer/fritidshus_kommun.svg', plot = p, width =  7, height = 5, device = "svg")
}



####### Hyresutveckling #########

hyres_utveck <- function(){
  df_hyra <- read.csv('Data/df_hyra.csv')
  
  ar_max <- max(df_hyra$år)
  # Förbered data
  df_hyra_clean <- df_hyra %>%
    mutate(
      år = as.integer(år),
      medianhyra = Medianhyra.i.hyreslägenhet
    ) %>%
    filter(!is.na(medianhyra), !is.na(år)) %>%
    arrange(region, år)
  
  # Skapa plotly-figuren
  fig <- plot_ly()
  
  # Lägg till en linje för varje kommun
  for(kommun in unique(df_hyra_clean$region)) {
    kommun_data <- df_hyra_clean %>% filter(region == kommun)
    
    fig <- fig %>%
      add_trace(
        data = kommun_data,
        x = ~år,
        y = ~medianhyra,
        type = 'scatter',
        mode = 'lines+markers',
        name = kommun,
        line = list(
          color = kommun_colors[[kommun]], 
          width = 3
        ),
        marker = list(
          color = kommun_colors[[kommun]],
          size = 6
        )
        )
      
  }
  
  # Konfigurera layout
  fig <- fig %>%
    layout(
      title = list(
        text = paste("Medianhyresutveckling per kommun, 2016 -", ar_max),
        font = list(size = 18, family = "Arial", color = "black")
      ),
      xaxis = list(
        title = "År",
        titlefont = list(size = 14, family = "Arial"),
        tickfont = list(size = 12),
        gridcolor = 'rgba(211,211,211,0.5)',
        showgrid = TRUE
      ),
      yaxis = list(
        title = "Medianhyra (kr/kvm/månad)",
        titlefont = list(size = 14, family = "Arial"),
        tickfont = list(size = 12),
        gridcolor = 'rgba(211,211,211,0.5)',
        showgrid = TRUE
      ),
      legend = list(
        orientation = "v",
        x = 1.05,
        y = 1,
        font = list(size = 12)
      ),
      hovermode = "x unified",
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      margin = list(l = 60, r = 150, t = 80, b = 60)
    )
  
  # En mer interaktiv version där du kan klicka på legendan för att highlighta en specifik kommun
  fig <- fig %>%
    layout(
      legend = list(
        orientation = "v",
        x = 1.05,
        y = 1,
        font = list(size = 12)
      )
    )
  
  fig <- config(
    fig,
    modeBarButtonsToRemove = c(
      'zoom2d',     # zoom button
      'pan2d',      # pan button
      'select2d',   # box select
      'lasso2d',    # lasso select
      'zoomIn2d',   # zoom in
      'zoomOut2d'   # zoom out
    ),toImageButtonOptions = list(
      format = "svg",
      filename = "hyres_utveckling"),
    displaylogo = FALSE)   # remove plotly logo/link
  
  # Visa den interaktiva versionen
  fig
}




####### Prognos bostadsbehov #########

prognos_behov <- function(){
  # Läser in data och skapar set för basår
  folkmangd <- read.csv('Data/df_folkmangd.csv')
  folkmangd_tot <- folkmangd %>% filter(år == 2006) %>%  group_by(region) %>% 
    summarise(Total_folkmängd = sum(Folkmängd), .groups = 'drop') 
  
  df_supplatelse <- read.csv('Data/df_supplatelse.csv')
  
  # Summerar antal
  df_supplatelse_2006 <- df_supplatelse %>% filter(år =='2006') %>% 
    group_by(region) %>% summarise(Total_bostader = sum(Antal), .groups='drop')
  
  # Slår ihop set
  df_kvot <- left_join(df_supplatelse_2006,folkmangd_tot, by='region' )
  
  df_kvot$Kvot <- df_kvot$Total_folkmängd / df_kvot$Total_bostader # beräknar kvoten
  
  colnames(df_kvot) <- c('Region', 'Antal bostäder', 'Antal vuxna', 'Kvot')
  
  df_befolkf <- read.csv('Data/df_folkmangdfram.csv') %>% filter(år > min(år))

  # Befolkning
  tid_df_befolkf <- df_befolkf %>% 
    filter(ålder >= 20, as.numeric(år) < (min(år)+ 20)) %>%  # 20 år framåt och alla över 20
    group_by(region, år) %>%  
    summarise(Totalt = sum(Antal), .groups = "drop")
  
  # folkmängden 2024
  max_ar <- max(folkmangd$år)
  folkokning <- folkmangd %>% filter(år == max_ar) %>%  group_by(region) %>% 
    summarise(Totalt = sum(Folkmängd), .groups = 'drop') %>% 
    mutate(år = max_ar) %>% select(region, år, Totalt)
  
  # Lägger till faktiskt antal i framskrivningen
  tid_df_befolkf <- rbind(tid_df_befolkf,folkokning)
  
  data_diff <- tid_df_befolkf %>%
    group_by(region) %>%               # Gruppera efter region
    arrange(år, .by_group = TRUE) %>%  # Se till att åren ligger i rätt ordning
    mutate(
      diff = ceiling(Totalt - lag(Totalt))     # Beräkna skillnaden mot föregående år
    ) %>%
    filter(!is.na(diff)) %>% # tar bort första året
    ungroup()
  
  # Lägg på kvot och uppskattat bostadsbehov
  tid_df <- data_diff %>%
    left_join(df_kvot %>% select(Region, Kvot), by = c("region" = "Region")) %>%
    mutate(Uppskattat_behov = ceiling(diff / Kvot))
  
  
  # Long-format för linjer
  lines_df <- tid_df %>%
    select(region, år, diff, Uppskattat_behov) %>%
    pivot_longer(
      cols = c(diff, Uppskattat_behov),
      names_to = "Typ",
      values_to = "Total"
    )
  # Plotfunktion
  plot_tid_nybygg_befolk_tot <- function(kommun_val){
    df_plot <- tid_df %>% filter(region == kommun_val)
    lines_plot <- lines_df %>% 
      filter(region == kommun_val)  # Bostadsbrist är nu med
    
    p<- ggplot(lines_plot, aes(x = år, y = Total, color = Typ, group = Typ)) +
      # Linjer för befolkning, uppskattat behov och bostadsbrist
      geom_line(data = lines_plot,
                aes(x = år, y = Total, color = Typ, group = Typ),
                linewidth = 4) +
      geom_text(data = lines_plot,
                aes(x = år, y = Total, label = Total),
                vjust = -1, size = 6, show.legend = FALSE, color='black') +
      scale_x_continuous(breaks = seq(min(df_plot$år), max(df_plot$år), by = 2)) +
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))+
      scale_color_manual(
        values = c(
          "diff" = "#4AA271",
          "Uppskattat_behov" = "#F9B000"
        ),labels = c(
          "diff" = "Befolkningsförändring",
          "Uppskattat_behov" = "Uppskattat behov av nya bostäder" )) +
      labs(title = kommun_val, x = "År", y = "Antal", color = "") +
      theme_get() + theme(axis.text.x = element_text(angle=45, vjust = 0.9),
                          legend.position = "bottom")
    
    p
  }
  
  for (r in sort(kommuner)){
    p <- plot_tid_nybygg_befolk_tot(r)
    
    
    # spara plot som SVG
    file_name <- paste0("Figurer/plot_bostadsprognos_", r, ".svg")
    ggsave(filename = file_name, plot = p, width = 14, height = 8, device = "svg")
    #print(p)
    
  }
  
  
}
