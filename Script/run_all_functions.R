####### Kör alla andra scripts 
####### Laddar ner data
####### Sparar plots 

{
  source("Script/install_load_packages.R")
  source("Script/settings.R")
  install_and_load()
  settings <- get_settings()
  
  kommunkod <- settings$kommunkod
  kommuner <- settings$kommuner
  kommun_colors <- settings$kommun_colors
  upplat_colors <- settings$upplat_colors
  source("Script/load_save_data.R")
  source("Script/create_save_plots.R")
}

######### Funktioner som sparar bilder till folder: "Figurer"
# Funktion för Storleksfördelning
flerbostadsarea()

# Utvekling upplåtelseformer
region_utv_upp()
kommun_utv_upp()

# Befolkningsförändring i relation till färdigställda bostäder

nybygg_region()
nybygg_kommun()

# Uppskattat behov av bostäder

uppskatt_behov()

# Fritidshus

fritidshus_reg()
fritidshus_kommun()


# Prognos bostadsbehov

prognos_behov()

# Prisutveckling

prisfastighet()

# Trångboddhet

trangbodd()



