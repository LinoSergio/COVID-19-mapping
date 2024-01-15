# Loading required libraries

pacman::p_load(leaflet, sf, tidyverse, geobr, plotly, flextable)

# Loadng the dataframe

data <- read_csv2("PAINEL_COVIDBR_2022_Parte2_13out2023.csv")

tab <- flextable(head(data[,1:8])) 

tab %>% 
  bg(bg= "black", part = "all") %>% 
  color(color = "white", part = "all") %>% 
  theme_box()

# data cleaning and filtering

data1 <- data %>% 
  filter(estado == "SE",
         !is.na(municipio),
         casosNovos > 0,
         obitosNovos > 0)

tab1 <- flextable(head(data1[,1:10]))

tab1 %>% 
  bg(bg= "black", part = "all") %>% 
  color(color = "white", part = "all") %>% 
  align_text_col(align = "center") %>% 
  align_nottext_col(align = "center") %>% 
  theme_box()

# Data visualization

img <- ggplot(data1) +
  geom_smooth(aes(x = semanaEpi, y = casosAcumulado)) +
  ggtitle("<b>Cumulative COVID-19 cases (July-December of 2022)<b>") +
  ylab("Cumulative Cases") +
  xlab("Epidemiological week") +
  theme_bw()

ggplotly(img)

img2 <- ggplot(data1) +
  geom_smooth(aes(x = semanaEpi, y = casosNovos)) +
  ggtitle("<b>New cases of COVID-19 (July-December of 2022)<b>") +
  ylab("New Cases") +
  xlab("Epidemiological week") +
  theme_bw()

ggplotly(img2)

img3 <- ggplot(data1) +
  geom_point(aes(x = obitosNovos, y = semanaEpi, color = municipio)) +
  ggtitle("<b>New deaths from COVID-19 (July-December of 2022)<b>") +
  ylab("Epidemiological Week") +
  xlab("Number of deaths") +
  theme_bw()

ggplotly(img3)

# Geolocation from Brazilian municipalities

cities_lat_long <- read_csv("municipios.csv")

tab3 <- flextable(head(cities_lat_long[,1:8]))

tab3 %>% 
  bg(bg= "black", part = "all") %>% 
  color(color = "white", part = "all") %>% 
  align_text_col(align = "center") %>% 
  align_nottext_col(align = "center") %>% 
  theme_box()

# Filtering Municipalities in the state of Sergipe

SE_cities <- cities_lat_long %>% 
  filter(codigo_uf == 28)

tab4 <- flextable(head(SE_cities[,1:8]))

tab4 %>% 
  bg(bg= "black", part = "all") %>% 
  color(color = "white", part = "all") %>% 
  align_text_col(align = "center") %>% 
  align_nottext_col(align = "center") %>% 
  theme_box()

# Creating the final dataframe with geolocation information and epidemiological data from COVID-19 in Brazil

data_covid19 <- left_join(data1, SE_cities, by = c("municipio" = "nome"))

tab5 <- flextable(head(data_covid19[,1:10]))

tab5 %>% 
  bg(bg= "black", part = "all") %>% 
  color(color = "white", part = "all") %>% 
  align_text_col(align = "center") %>% 
  align_nottext_col(align = "center") %>% 
  theme_box()

# Creating the interactive map

map_total_cases <- leaflet(data_covid19) %>% addTiles() %>% 
  addCircleMarkers(
    radius = ~sqrt(data_covid19$casosNovos),
    fillOpacity = 0.5, stroke = F, 
    popup = paste0("<b>City: </b>", data_covid19$municipio,"<br>",
                   "<b>Confirmed Cases: </b>", data_covid19$casosNovos),
    label = ~municipio)


map_total_cases