library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(shiny)



warszawa = read.csv("data.csv", stringsAsFactors = F) %>% mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H"))


#LAYOUT
ui <- navbarPage(
  
  title = "Analiza klimatu Warszawy",
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css")
  ),
  tabPanel("Roczna Analiza 1980-2023",
           sliderInput("year2", "Wybierz rok:", min = 1980, max = 2023, value = 2000, step = 1, sep = "", ticks = F),
           plotOutput("zestawienie", height = "1400px", width = "1700px")
  ),
  tabPanel("Podsumowanie danych 1980-2020",
           plotOutput("podsumowanie", height = "1400px", width = "1700px")
  )
)

#DATA ANALYSIS
server <- function(input, output) {
  
 #TABULAR SUMMARY
 
  output$wybrane <- renderTable({
   
   if (input$zmienne == "temperatura"){
    temperatura = warszawa %>% group_by(data = as.Date(time)) %>% summarise(temp = mean(temperature_2m, na.rm = T))
    temperatura_srednia = temperatura %>% group_by(rok = year(data)) %>% summarise(temp = mean(temp, na.rm = T)) %>% na.omit(temperatura_srednia)
    selected_year = input$year
    selected_data = filter(temperatura_srednia, rok == selected_year) %>%  mutate(across(where(is.numeric), ~format(.x, big.mark = "", scientific = FALSE)))
   
   }
   
   if (input$zmienne == "temperatura sierpnia"){
     temperatura = warszawa %>% filter(month(time) ==8) %>% group_by(data = as.Date(time)) %>% summarise(temp = mean(temperature_2m, na.rm = T))
     temperatura_sierp = temperatura %>% group_by(rok = year(data)) %>% summarise(temp = mean(temp, na.rm = T)) %>% na.omit(temperatura_sierp)
     selected_year = input$year
     selected_data = filter(temperatura_sierp, rok == selected_year) %>%  mutate(across(where(is.numeric), ~format(.x, big.mark = "", scientific = FALSE)))
     
   }
   
   
   if (input$zmienne == "temperatura stycznia"){
     temperatura = warszawa %>% filter(month(time) ==1) %>% group_by(data = as.Date(time)) %>% summarise(temp = mean(temperature_2m, na.rm = T))
     temperatura_stycznia = temperatura %>% group_by(rok = year(data)) %>% summarise(temp = mean(temp, na.rm = T)) %>% na.omit(temperatura_stycznia)
     selected_year = input$year
     selected_data = filter(temperatura_stycznia, rok == selected_year) %>%  mutate(across(where(is.numeric), ~format(.x, big.mark = "", scientific = FALSE)))
     
   }
  
   if (input$zmienne == "promieniowanie krótkofalowe") {
     promie = warszawa %>% group_by(data = as.Date(time)) %>% summarise(prom = mean(shortwave_radiation, na.rm = T))
     prom_srednia = promie %>% group_by(rok = year(data)) %>% summarise(prom = mean(prom, na.rm = T)) %>% na.omit(prom_srednia)
     selected_year = input$year
     selected_data = filter(prom_srednia, rok == selected_year) %>%  mutate(across(where(is.numeric), ~format(.x, big.mark = "", scientific = FALSE)))
    
   }
   
   if (input$zmienne == "wilgotność") {
     wilgotnosc = warszawa %>% group_by(data = as.Date(time)) %>% summarise(wilg = mean(relative_humidity_2m, na.rm = T))
     wilgotnosc_srednia = wilgotnosc %>% group_by(rok = year(data)) %>% summarise(wilg = mean(wilg, na.rm = T)) %>% na.omit(wilgotnosc_srednia)
     selected_year = input$year
     selected_data = filter(wilgotnosc_srednia, rok == selected_year) %>%  mutate(across(where(is.numeric), ~format(.x, big.mark = "", scientific = FALSE)))
   
   }  
          
   if (input$zmienne == "opady") {
     opady = warszawa %>% group_by(data = as.Date(time)) %>% summarise(opad = sum(precipitation, na.rm = T))
     opady_suma = opady %>% group_by(rok = year(data)) %>% summarise(opad = sum(opad, na.rm = T)) %>% na.omit(opady_suma)
     selected_year = input$year
     selected_data = filter(opady_suma, rok == selected_year) %>%  mutate(across(where(is.numeric), ~format(.x, big.mark = "", scientific = FALSE)))
   }
    
   selected_data
    
 })
  
 
 
 
 
  #SUMMARY 1980-2024
  
  filtered_data = reactive({
    warszawa %>% filter(year(time) == input$year2)
  })
  
  output$zestawienie =  renderPlot({
    
    data2 = filtered_data() %>% group_by(data = month(time)) %>% summarise(temp = mean(temperature_2m, na.rm = TRUE))
    przerywana = warszawa %>% filter(year(time) >= 1980) %>% group_by(miesiac = month(time)) %>% summarise(srednia = mean(temperature_2m, na.rm = T))
   
    a = ggplot() +
      geom_line(data = data2, aes(x = data, y = temp, color = "średnia miesięczna temperatura w danym roku"), linewidth = 1.25) +
      geom_line(data = przerywana, aes(x = miesiac, y = srednia, color = "średnia miesięczna temperatura z 1980-2020"), linetype = "dashed") +
      labs(title = "Średnia miesięczna temperatura",
           x = "Okres",
           y = "Średnia temperatura (°C)",
           color = "Legenda") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 1:12,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
      scale_color_manual(values = c("średnia miesięczna temperatura z 1980-2020" = "blue", "średnia miesięczna temperatura w danym roku" = "red"))

    
    
    dan_b = filtered_data() %>%  group_by(miesiac = month(time)) %>% summarise(wilg = mean(relative_humidity_2m, na.rm = T), wia = mean(wind_speed_10m, na.rm = T), temp = mean(temperature_2m - apparent_temperature, na.rm = T))
    
    b = ggplot(dan_b, aes(x = miesiac)) +
      geom_line(aes(y = temp, color = wia), linewidth = 1.25) +
      scale_color_gradient( low = "yellow", high = "red") + 
      geom_line(aes(y = (wilg - 50) / 5), color = "blue", linetype = "dashed") +  
      scale_y_continuous(
        name = "Średnia temperatura (°C)",
        sec.axis = sec_axis(~ . * 5 + 50, name = "Wilgotność (%)")  
      ) +
      labs(title = "różnica między temp. rzeczywistą, a odczuwalną w zależności od wilgotności i prędkosci wiatru",
           x = "okres",
           color = "prędkość wiatru (km/h)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 1:12,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11),
        axis.title.y.right = element_text(size = 16),
        axis.text.y.right = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) 
    
    
    
   dan_c <- filtered_data() %>% filter(month(time) %in% 6:7 ) %>% group_by(dzien = yday(time)) %>% summarise(temp = mean(temperature_2m - apparent_temperature), wilg = mean(relative_humidity_2m), wia = mean(wind_speed_10m))
   
   c = ggplot(dan_c, aes(x = dzien)) +
     geom_line(aes(y = temp, color = wia), linewidth = 1.25) +
     scale_color_gradient( low = "yellow", high = "red") + 
     geom_line(aes(y = (wilg - 50) / 5), color = "blue", linetype = "dashed") +  
     scale_y_continuous(
       name = "Średnia temperatura (°C)",
       sec.axis = sec_axis(~ . * 5 + 50, name = "Wilgotność (%)")  
     ) +
     labs(title = "różnica między temp. rzeczywistą, a odczuwalną dla czerwca i lipca",
          x = "okres",
          color = "prędkość wiatru (km/h)") +
     theme_minimal() +
     scale_x_continuous(
       breaks = c(168, 199),
       labels = c("Cze", "Lip")) +
     theme(
       plot.title = element_text(hjust = 0.5, size = 20),
       axis.title.x = element_text(size = 16),
       axis.text.x = element_text(size = 11),
       axis.title.y = element_text(size = 16),
       axis.text.y = element_text(size = 11),
       axis.title.y.right = element_text(size = 16),
       axis.text.y.right = element_text(size = 11),
       legend.text = element_text(size = 12),
       legend.title = element_text(size = 12)) 
  
    
    data_ma  = filtered_data() %>% group_by(time = month(time)) %>% summarise(temp = max(temperature_2m, na.rm = TRUE))
    data_mi = filtered_data() %>% group_by(time = month(time)) %>% summarise(temp = min(temperature_2m, na.rm = TRUE))
    
    d = ggplot() +
      geom_line(data = data_ma, aes(x = time, y = temp, color = "maksymalna miesięczna temperatura"), linewidth = 1.1) +
      geom_line(data = data_mi, aes(x = time, y = temp, color = "minimalna miesięczna temperatura"), linewidth = 1.1) +
      labs(title = "maksymalne i minimalne miesięczne temperatury",
           x = "Okres",
           y = "temperatura (°C)",
           color = "Legenda") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 1:12,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
      scale_color_manual(values = c("maksymalna miesięczna temperatura" = "red", "minimalna miesięczna temperatura" = "blue"))
    
    
    data3 = filtered_data() %>% group_by(data = month(time)) %>% summarise(opad = sum(precipitation, na.rm = TRUE), slon = sum(sunshine_duration, na.rm = TRUE))
    
    e = ggplot(data3, aes(x = data, y = opad)) +
      geom_bar(stat = "identity", fill = "blue", color = "black") +
      labs(title = "miesięczna suma opadów",
           x = "okres",
           y = "suma opadów (mm)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 1:12,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11))
    
    f = ggplot(data3, aes(x = data, y = slon / 3600)) +
      geom_bar(stat = "identity", fill = "gold", color = "black") +
      labs(title = "Miesięczny czas nasłonecznienia",
           x = "Okres",
           y = "Czas nasłonecznienia (h)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 1:12,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11))
    
    
    
    data = filtered_data() %>% group_by(godzina = hour(time), dzien = yday(time)) %>% summarise(srednia_temp = mean(temperature_2m, na.rm = TRUE), srednia_wilg = mean(relative_humidity_2m, na.rm = TRUE), sredni_wiatr = mean(wind_speed_10m, na.rm = TRUE))
    
    g = ggplot(data, aes(x = dzien - 1, y = godzina, fill = srednia_temp)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("blue", "cyan", "yellow", "orange", "red")) +
      labs(title = "Temperatura w zależności od miesiąca i godziny",
           x = "Miesiąc",
           y = "Godzina",
           fill = "Temperatura (°C)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349),
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      scale_y_continuous(breaks = 0:23) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
    
    
    
    h = ggplot(data, aes(x = dzien - 1, y = godzina, fill = srednia_wilg)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("lightyellow", "lightgreen", "green", "darkgreen")) +
      labs(title = "Wilgotność w zależności od miesiąca i godziny",
           x = "Miesiąc",
           y = "Godzina",
           fill = "Wilgotność (%)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349),
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      scale_y_continuous(breaks = 0:23) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
  
     i = ggplot(data, aes(x = dzien - 1, y = godzina, fill = sredni_wiatr)) +
       geom_tile() +
       scale_fill_gradientn(colors = c("lightblue", "blue", "darkblue")) +
       labs(title = "Prędkość wiatru w zależności od miesiąca i godziny",
            x = "Miesiąc",
            y = "Godzina",
            fill = "Prędkość wiatru (km/h)") +
       theme_minimal() +
       scale_x_continuous(
         breaks = c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349),
         labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
       scale_y_continuous(breaks = 0:23) +
       theme(
         plot.title = element_text(hjust = 0.5, size = 20),
         axis.title.x = element_text(size = 16),
         axis.text.x = element_text(size = 11),
         axis.title.y = element_text(size = 16),
         axis.text.y = element_text(size = 11),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 12))
    
    (a+b) /(c+d) / (e + f) / (g + h) / i #Combining Plots
})
 
  
 #SUMMARY
  filtered_data2 = reactive({
    warszawa %>% filter(year(time) >=1980) 
     
  })
  
  output$podsumowanie =  renderPlot({
    
    pom1 = filtered_data2() %>% group_by(godzina = hour(time), miesiac = month(time)) %>% summarise(temp = mean(temperature_2m, na.rm = T), wilg = mean(relative_humidity_2m, na.rm = T), wiatr = mean(wind_speed_10m, na.rm = T))
    
    a = ggplot(pom1, aes(x = miesiac - 1, y = godzina, fill = temp)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("blue", "cyan", "yellow", "orange", "red")) +
      labs(title = "Temperatura w zależności od miesiąca i godziny",
           x = "Miesiąc",
           y = "Godzina",
           fill = "Temperatura (°C)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 0:11,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      scale_y_continuous(breaks = 0:23) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
    
    
    b = ggplot(pom1, aes(x = miesiac - 1, y = godzina, fill = wilg)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("lightyellow", "lightgreen", "green", "darkgreen")) +
      labs(title = "Wilgotność w zależności od miesiąca i godziny",
           x = "Miesiąc",
           y = "Godzina",
           fill = "Wilgotność (%)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 0:11,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      scale_y_continuous(breaks = 0:23) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
    
    c = ggplot(pom1, aes(x = miesiac - 1, y = godzina, fill = wiatr)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("lightblue", "blue", "darkblue")) +
      labs(title = "Prędkość wiatru w zależności od miesiąca i godziny",
           x = "Miesiąc",
           y = "Godzina",
           fill = "Prędkość wiatru (km/h)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 0:11,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      scale_y_continuous(breaks = 0:23) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
    
    pom2 = filtered_data2() %>% group_by(rok = year(time), miesiac = month(time)) %>% summarise(opad = sum(precipitation, na.rm = TRUE), slon = sum(sunshine_duration, na.rm = TRUE)) %>% group_by(miesiac) %>% summarise(opad = mean(opad, na.rm = T), slon = mean(slon, na.rm = T))
   
    d = ggplot(pom2, aes(x = miesiac, y = opad)) +
      geom_bar(stat = "identity", fill = "blue", color = "black") +
      labs(title = "miesięczna suma opadów",
           x = "okres",
           y = "suma opadów (mm)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 1:12,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11))
    
    e = ggplot(pom2, aes(x = miesiac, y = slon / 3600)) +
      geom_bar(stat = "identity", fill = "gold", color = "black") +
      labs(title = "Miesięczny czas nasłonecznienia",
           x = "Okres",
           y = "Czas nasłonecznienia (h)") +
      theme_minimal() +
      scale_x_continuous(
        breaks = 1:12,
        labels = c("Sty", "Lut", "Mar", "Kwi", "Maj", "Cze", "Lip", "Sie", "Wrz", "Paź", "Lis", "Gru")) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 11))
    
    
    (a + b)/ (c + d) / e
    
    })

}
  
# Run the application 
shinyApp(ui = ui, server = server)

