############################
# Laddar och sparar data####
############################
{
  source("Script/install_load_packages.R")
  source("Script/settings.R")
  install_and_load()
  settings <- get_settings()
  
  kommunkod <- settings$kommunkod
  kommuner <- settings$kommuner
  kommun_colors <- settings$kommun_colors
  riket_narliggande <- settings$riket_narliggande
}


##############Laddar in data om Antal lägenheter efter region, hustyp och byggnadsperiod. År 2013 - 2024 #######
{
  # Laddar in data om Antal lägenheter efter region, hustyp och byggnadsperiod. År 2013 - 2024
  url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104D/BO0104T02'
  # Skapa en referenstabell med kommunkoder och namn
  
  kommunkod <- c("0330", "0331", "0360", "0380", "0381", "0382", "0305", "0319")
  kommuner <- c("Knivsta", "Heby", "Tierp", "Uppsala", "Enköping", "Östhammar", "Håbo", "Älvkarleby")
  
  
  pxweb_query_list <-
    list('Region' = kommunkod,
         'Hustyp' = '*',
         'Byggnadsperiod' = '*',
         'Tid' = '*',
         'ContentsCode' = '*'
    )
  
  
  # Download data 
  px_data <- pxweb_get(url = url,pxweb_query_list )
  
  # Convert to data.frame 
  df_byggnadsperiod <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  
  # sparar data med variabler: region, unrikes/utrikes född, kön, ålder, tid , antal
  write.csv(df_byggnadsperiod, "Data/df_byggnadsperiod.csv", row.names = F)
  
}



#######laddar in data om Antal lägenheter efter region, hustyp, ägarkategori och år #######
{
# laddar in data om Antal lägenheter efter region, hustyp, ägarkategori och år

url <-'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104D/BO0104T03'

pxweb_query_list <-
  list('Region' = kommunkod,
       'Hustyp' = '*',
       'Agarkategori' = '*',
       'Tid' = '*',
       'ContentsCode' = '*'
  )


# Download data 
px_data <- pxweb_get(url = url,pxweb_query_list)

# Convert to data.frame 
df_agarkategori <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

write.csv(df_agarkategori, "Data/df_agarkategori.csv", row.names = F)
}


#######laddar in data om Antal lägenheter efter region, hustyp, bostadsarea och år #######

{
  
  # laddar in data om Antal lägenheter efter region, hustyp, bostadsarea och år
  
  url <-'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104D/BO0104T5'
  
  pxweb_query_list <-
    list('Region' = kommunkod,
         'Hustyp' = '*',
         'Bostadsarea' = '*',
         'Tid' = '*',
         'ContentsCode' = '*'
    )
  
  
  # Download data 
  px_data <- pxweb_get(url = url,pxweb_query_list)
  
  # Convert to data.frame 
  df_bostadsarea <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  write.csv(df_bostadsarea, "Data/df_bostadsarea.csv", row.names = F) 
  
}


######### laddar in data om Antal lägenheter efter region, hustyp, lägenhetstyp och år #######

{
  
  
  # laddar in data om Antal lägenheter efter region, hustyp, lägenhetstyp och år
  
  url <-'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104D/BO0104T09'
  
  pxweb_query_list <-
    list('Region' = kommunkod,
         'Hustyp' = '*',
         'Lagenhetstyp' = '*',
         'Tid' = '*',
         'ContentsCode' = '*'
    )
  
  
  # Download data 
  px_data <- pxweb_get(url = url,pxweb_query_list)
  
  # Convert to data.frame 
  df_lagenhetstyp <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  write.csv(df_lagenhetstyp, "Data/df_lagenhetstyp.csv", row.names = F) 
  
  
}




########## laddar in data om Antal lägenheter efter region, typ av specialbostad, bostadsarea och år (äldre/funktionshindrade, student, övrigt) #######
{
url <-'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104D/BO0104T7'

pxweb_query_list <-
  list('Region' = kommunkod,
       'TypAvSpecialbostad' = '*',
       'Bostadsarea' = '*',
       'Tid' = '*',
       'ContentsCode' = '*'
  )


# Download data 
px_data <- pxweb_get(url = url,pxweb_query_list)

# Convert to data.frame 
df_specialbostad <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

write.csv(df_specialbostad, "Data/df_specialbostad.csv", row.names = F) 
}


####### laddar in data om Antal lägenheter efter region, hustyp, upplåtelseform och år #######

{
url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104D/BO0104T04'

pxweb_query_list <-
  list('Region' = kommunkod,
       'Upplatelseform' = '*',
       'Hustyp' = '*',
       'Tid' = '*',
       'ContentsCode' = '*'
  )


# Download data 
px_data <- pxweb_get(url = url,pxweb_query_list)

# Convert to data.frame 
df_supplatelse <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

write.csv(df_supplatelse, "Data/df_supplatelse.csv", row.names = F) 
}


####### Laddar hem exl-fil från boverket###########


url <- 'https://www.boverket.se/contentassets/fe1716843d2147edb3c38cd4ea7df7b9/laget-pa-bostadsmarknaden-och-bostadsbyggande---bme-2025.xlsx'

# Download the file
download.file(url, destfile = 'Data/boverket.xlsx', mode = "wb")








######## # Läser in data om Andel lediga lägenheter i flerbostadshus, allmännyttiga efter region, lägenhetstyp och år #######
{
url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0303/BO0303A/OuthAllmLghTypKom0'


pxweb_query_list <-
  list('Region' = kommunkod,
       'Lagenhetstyp'="*",
       'Tid' = '*',
       'ContentsCode' = '*'
  )


# Download data 
px_data <- pxweb_get(url = url,pxweb_query_list)

# Convert to data.frame 
df_lediga <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")


write.csv(df_lediga, "Data/df_lediga.csv", row.names = F) 


}
##########Laddar in data om Färdigställda lägenheter i nybyggda hus, antal efter region, hustyp, upplåtelseform och år #####################
{
url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0101/BO0101A/LghReHtypUfAr'
# Skapa en referenstabell med kommunkoder och namn


pxweb_query_list <-
  list('Region' = kommunkod,
       'Upplatelseform' = '*',
       'Hustyp' = '*',
       'Tid' = '*',
       'ContentsCode' = '*'
  )



# Download data 
px_data <- pxweb_get(url = url ,pxweb_query_list)

# Convert to data.frame 
df_nybyggda <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

write.csv(df_nybyggda, "Data/df_nybyggda.csv", row.names = F) 

############## laddar in data för befolkningsförändring#################
url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101G/BefforandrKvRLK'
# Skapa en referenstabell med kommunkoder och namn


pxweb_query_list <-
  list('Region' = kommunkod,
       'Forandringar' = '110',
       'Period' = 'hel',
       'Tid' = '*',
       'Kon' = '1+2',
       'ContentsCode' = '*'
  )

# Download data 
px_data <- pxweb_get(url = url,pxweb_query_list )

# Convert to data.frame 
df_befolkf <-  as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

write.csv(df_befolkf, "Data/df_befolkf.csv", row.names = F) 
}

######## Laddar in data Folkmängden efter region,  ålder  År 2006 ##########
{
url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy'
# Skapa en referenstabell med kommunkoder och namn


pxweb_query_list <-
  list('Region' = kommunkod,
       'Alder' = c(as.character(20:99), "100+"), # 20 år och uppåt
       'Tid' = c("2006","2024") , # basåret
       'ContentsCode' = 'BE0101N1' # folkmängd
  )



# Download data 
px_data <- pxweb_get(url = url,pxweb_query_list)

# Convert to data.frame 
folkmangd <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
write.csv(folkmangd, "Data/df_folkmangd.csv", row.names = F)

}



############## DESO nivå upplåtelseform############

{
  
  url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104X/BO0104T01N2'
  
  # Hämta metadata för Region
  meta <- pxweb_get(url)
  
  # Visa tillgängliga regionkoder
  regioner <- meta$variables[[1]]$values
  
  # Välj endast regioner som börjar med "03"
  uppsala_koder <- regioner[startsWith(regioner, "03")]
  
  pxweb_query_list <-
    list('Region' = uppsala_koder,
         'Upplatelseform'="*",
         'Tid' = '*',
         'ContentsCode' = '*'
    )
  
  
  # Download data 
  px_data <- pxweb_get(url = url, pxweb_query_list)
  
  # Convert to data.frame 
  df_deso <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  write.csv(df_deso, "Data/df_deso.csv", row.names = F)
  
}


######## Fritidshus #########
{
# data för Antal fritidshus efter region och år
url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0104/BO0104H/BO0104T08'


pxweb_query_list <-
  list('Region' = kommunkod,
       'Tid' = '*',
       'ContentsCode' = '*'
  )


# Download data 
px_data <- pxweb_get(url = url,pxweb_query_list)

# Convert to data.frame 
df_fritidshus <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
write.csv(df_fritidshus, "Data/df_fritidshus.csv", row.names = F)
}



######## Hyresutveckling ###########
{
# Läser in data
url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BO/BO0406/BO0406E/BO0406Tab01'



pxweb_query_list <-
  list('Region' = kommunkod,
       'Hyresuppg'='Mh_kvm',
       'Tid' = '*',
       'ContentsCode' = '000000J4'
  )


# Download data 
px_data <- pxweb_get(url = url,pxweb_query_list)

# Convert to data.frame 
df_hyra <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# imputerar ett saknande värde till samma som året innan och efter
df_hyra$`Medianhyra i hyreslägenhet`[df_hyra$region=='Tierp' & df_hyra$år=='2017'] <- 86

write.csv(df_hyra, "Data/df_hyra.csv", row.names = F)

}







######### KOLLLAAA INN !!!!!!!!!!!!

## DESO 
# Antal personer efter region och upplåtelseform. År 2012 - 2024
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0111__HE0111YDeSo/HushallT33Deso/


# Antal personer efter region och hustyp. År 2012 - 2024
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0111__HE0111YDeSo/HushallT32Deso/

########## Befolkningsprognoser ###########

{
  url <- 'https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0401/BE0401A/BefProgRegFakN'
  
  px_get_list <- list(Region = kommunkod,
                      InrikesUtrikes = '*',
                      Kon = '*',
                      Alder = '*',
                      ContentsCode = '*',
                      Tid = '*')
  
  
  px_get <- pxweb_get(url,px_get_list)
  
  # laddar data och gör till rätt format
  df_folkmangdfram <- as.data.frame(px_get, column.name.type = "text", variable.value.type = "text")
  df_folkmangdfram$ålder <- gsub("\\+", "", df_folkmangdfram$ålder)
  df_folkmangdfram$ålder <- as.integer(gsub(" år", "", df_folkmangdfram$ålder))
  df_folkmangdfram$år = as.integer(df_folkmangdfram$år)
  df_folkmangdfram <- df_folkmangdfram %>% filter(`inrikes/utrikes född` == "inrikes och utrikes födda")
  
  # sparar data med variabler: region, unrikes/utrikes född, kön, ålder, tid , antal
  write.csv(df_folkmangdfram, "Data/df_folkmangdfram.csv", row.names = F)
}






####### Kolada fastighetspris ##########
source("Script/search_kolada.R")

### Fastighetspriser per kommun och år
{
  df <- search_and_fetch_kolada("Fastighetspris",kommunkod=kommunkod)
  
  df <- df %>% filter(year > 2004, title !="Fastighetspris småhus, tkr")

  write.csv(df, "Data/fastighetspris.csv", row.names = F)
}

####### Kolada trångboddhet #########
{
  df <- search_and_fetch_kolada("Trångboddhet",kommunkod=kommunkod)
  
  df <- df %>% filter(gender != 'T')
  
  write.csv(df, "Data/trandboddhet.csv", row.names = F)
}



######### Boverket prognos ########

############ Byt ut året i länken för ny data
# https://www.boverket.se/sv/om-boverket/oppna-data/byggbehovsberakning/
{
year <-2025
url <- paste0('https://www.boverket.se/contentassets/8cac305f717845d39c4e471d761f176f/beraknat-bostadsbyggnadsbehov-',year,'-05-27.xlsx')

download.file(url, destfile = 'Data/boverket_prognos.xlsx', mode = "wb")
}
