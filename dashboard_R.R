## Dashboard lonomismo en Misiones - Argentina
##  - Script:
##    Marília Melo Favalesso (mariliabioufpr@gmail.com)

## Paquetes --------------------------------------------------------------------
library(shinydashboard)
library(shiny)
library(png)
library(DT)
library(readr)
library(tidyverse)
library(raster)
library(leaflet)
library(slickR)
library(RColorBrewer)
library(kableExtra)
library(readxl)
library(formattable)
library(rgdal)
library(zoo)
library(htmlwidgets)

## Datos -----------------------------------------------------------------------
tabepid <- read_excel("www/tabepid.xlsx", sheet = "Plan1")
tabspecie <- read_excel("www/tabepid.xlsx", sheet = "Planilha1")
mes <- read_excel("www/tabepid.xlsx", sheet = "Planilha2")

misiones <- readOGR("www/misiones.shp")
rasterr <- raster("www/result_raster.tif")

## Gráficos --------------------------------------------------------------------
# Use "," en lugar de "." en los gráficos
options(OutDec = ",")

## Sexo --
sex <- tabepid[1:2, c(2, 4)] # Seleccionar datos relacionados con el sexo
sex$ymax <- cumsum(sex$perc) # Calcule los porcentajes acumulativos
sex$ymin <- c(0, head(sex$perc, n = -1)) # Calcular la parte inferior de cada rectángulo
sex$labelPosition <- (sex$ymax + sex$ymin) / 2 # Calcular la posición de la etiqueta
sex$label <- paste0(sex$Category, "\n", sex$perc, "%") # Texto de la etiqueta

graphsex <- ggplot(
  sex, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Category)
) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 6) +
  scale_fill_manual(values = c("#fbc4ab", "#f08080")) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

## Edad --
edad <- tabepid[3:10, c(2, 4)] # Seleccionar datos relacionados con edad
edad$label <- paste0(edad$perc, "%") # Texto de la etiqueta

graphedad <- ggplot(edad, aes(x = Category, y = perc, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(rep("#f08080", 2), rep("#fbc4ab", 5), "grey80")) +
  theme_bw(base_size = 16) +
  ylim(c(0, 40)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(vjust = -0.6)
  ) +
  labs(x = "Grupo de edad", y = "") +
  geom_text(aes(label = label),
    size = 5,
    position = position_dodge(width = 0.9), vjust = -0.80
  )

## Area --
area <- tabepid[11:14, c(2, 4)] # Seleccionar datos relacionados con área
area$label <- paste0(area$perc, "%") # Texto de la etiqueta

grapharea <- ggplot(area, aes(x = reorder(Category, desc(perc)), y = perc, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("grey80", "#fbc4ab", "#fbc4ab", "#fbc4ab")) +
  theme_bw(base_size = 16) +
  ylim(c(0, 50)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(vjust = -0.6)
  ) +
  labs(x = "Área", y = "") +
  geom_text(aes(label = label),
    size = 5,
    position = position_dodge(width = 0.9), vjust = -0.80
  )

## Hora --
hora <- tabepid[26:28, c(2, 4)] # Seleccionar datos relacionados con hora
hora$label <- paste0(hora$perc, "%") # Texto de la etiqueta

graphhora <- ggplot(hora, aes(x = Category, y = perc, fill = Category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#fbc4ab", "#fbc4ab", "grey80")) +
  theme_bw(base_size = 16) +
  ylim(c(0, 60)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(vjust = -0.6)
  ) +
  labs(x = "Hora del accidente", y = "") +
  geom_text(aes(label = label),
    size = 5,
    position = position_dodge(width = 0.9), vjust = -0.80
  )

## Circunstancias --
circunstancias <- tabepid[23:25, c(2, 4)] # Seleccionar datos relacionados con circunstancias
circunstancias$label <- paste0(circunstancias$perc, "%") # Texto de la etiqueta

graphcircunstancias <- ggplot(
  circunstancias,
  aes(
    x = reorder(Category, desc(perc)),
    y = perc, fill = Category
  )
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("grey80", "#fbc4ab", "#fbc4ab")) +
  theme_bw(base_size = 16) +
  ylim(c(0, 60)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(vjust = -0.6)
  ) +
  labs(x = "Circunstancias del accidente", y = "") +
  geom_text(aes(label = label),
    size = 5,
    position = position_dodge(width = 0.9), vjust = -0.80
  )

## Cuerpo --
cuerpo <- tabepid[29:31, c(2, 4)] # Seleccionar datos relacionados con parte del cuerpo
cuerpo$label <- paste0(cuerpo$perc, "%") # Texto de la etiqueta

graphcuerpo <- ggplot(cuerpo, aes(
  x = reorder(Category, desc(perc)),
  y = perc, fill = Category
)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#fbc4ab", "#f08080", "#fbc4ab")) +
  theme_bw(base_size = 16) +
  ylim(c(0, 80)) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(vjust = -0.6)
  ) +
  labs(x = "Parte del cuerpo afectada", y = "") +
  geom_text(aes(label = label),
    size = 5,
    position = position_dodge(width = 0.9), vjust = -0.80
  )

## Mapa de riesgos -------------------------------------------------------------
## El mapa de riesgo --
## Rellene las zonas de Misiones que faltan datos
a <- raster(
  xmn = -56.05971, xmx = -53.59184,
  ymn = -28.16423, ymx = -25.50155,
  res = res(rasterr),
  crs = crs(rasterr)
)

origin(a) <- origin(rasterr)
values(a) <- 0.01

## Crear el ráster del mapa de riesgo final
b <- merge(rasterr, a, overlap = TRUE)
c <- mask(crop(b, misiones), misiones)

## El mapa
# Paleta de colores para valores
pal <- colorBin(brewer.pal(name = "RdBu", n = 10), values(c),
  na.color = "transparent",
  bins = 23,
  reverse = TRUE
)
# Paleta de colores para leyenda
pal2 <- colorRampPalette(c(
  "#0571B0", "#92C5DE", "#F7F7F7",
  "#F4A582", "#CA0020"
), interpolate = "linear")(7)

# Mapa
rm <- leaflet() %>%
  addTiles() %>%
  addRasterImage(c, colors = pal, opacity = 0.65) %>%
  addLegend(
    colors = pal2,
    labels = c("Bajo", rep("", 5), "Alto"),
    position = c("topright"),
    title = "Riesgo de accidentes"
  )

## Tabla número de accidentes por provincia --
options(OutDec = ".") # usemos el separador decimal = "."

depart <- tabepid[15:22, c(2:4)] # datos de acc por departamento
depart$perc <- round(depart$perc, 0) # datos porcentuales redondeados

depart$perc <- color_bar("#fbc4ab")(depart$perc) # Gráfico de barras para el % en la tabla

names(depart) <- c("Departamento", "Número de accidentes", "Porcentaje (%)")

tabdep <- kbl(depart, escape = F, align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "4cm", italic = F) %>%
  column_spec(2, width = "2cm") %>%
  column_spec(3, width = "1cm", color = "black", bold = T)

## Tabla con especies de plantas -----------------------------------------------
sp <- read_excel("www/tabepid.xlsx", sheet = "Planilha1") # datos de sps

sp <- sp %>% mutate(`Porcentaje (%)` = round(`Porcentaje (%)` * 100, 0)) # % redondeado

sp$`Porcentaje (%)` <- color_bar("#fbc4ab")(sp$`Porcentaje (%)`) # Gráfico de barras para el % en la tabla

tabsp <- kbl(sp, escape = F, align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  column_spec(1, width = "5cm", italic = T) %>%
  column_spec(2, width = "5cm") %>%
  column_spec(3, width = "5cm") %>%
  column_spec(4, width = "5cm", color = "black", bold = T) %>%
  column_spec(5, width = "5cm", color = "black", bold = T) %>%
  footnote(c("*Al menos un caso de lonomismo registrado"),
    escape = F,
    symbol_manual = NULL, footnote_as_chunk = F,
    fixed_small_size = T, general_title = ""
  )

## Estacionalidad --------------------------------------------------------------
options(OutDec = ",") # Use "," en lugar de "." en los gráficos

mes <- read_excel("www/tabepid.xlsx", sheet = "Planilha2")

# Girar la tabla para graficar datos
mes <- mes %>%
  pivot_longer(2:4,
    names_to = "Estagio de vida",
    values_to = "Numero"
  )

# Cambiar los nombres de las columnas
names(mes) <- c("Mes", "Etapa de vida:", "Cantidad")

# Mes como un factor
mes$Mes <- as_factor(mes$Mes)

# Agrupa la cantidad de lonomias por mes
smes <- mes %>%
  group_by(Mes) %>%
  summarise(num = sum(Cantidad))

smes$perc <- round((smes$num / sum(smes$num)) * 100, 2)
names(smes) <- c("Mes", "n", "Perc")

# Etiqueta
labmes <- paste0(smes$Perc, "%")

grames <- ggplot(mes, aes(x = Mes, y = Cantidad, fill = `Etapa de vida:`)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#b29457", "#436a36", "#2b4625")) +
  theme_bw(base_size = 16) +
  annotate(
    geom = "text", x = 1, y = 8.5, label = labmes[1],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 2, y = 3.5, label = labmes[2],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 3, y = 2.5, label = labmes[3],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 4, y = 12.5, label = labmes[4],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 5, y = 8.5, label = labmes[5],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 6, y = 0.5, label = labmes[6],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 7, y = 0.5, label = labmes[7],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 8, y = 0.5, label = labmes[8],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 9, y = 4.5, label = labmes[9],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 10, y = 10.5, label = labmes[10],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 11, y = 8.5, label = labmes[11],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 12, y = 15.5, label = labmes[12],
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", x = 13, y = 3.5, label = labmes[13],
    color = "black", size = 6
  ) +
  theme(
    panel.background = element_rect(fill = "#ECF0F5"), # bg of the panel
    plot.background = element_rect(fill = "#ECF0F5", color = NA), # bg of the plot
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "#ECF0F5") # get rid of legend bg
  )

## APP -------------------------------------------------------------------------

header <- dashboardHeader(title = "Lonomismo AR")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Aspectos epidemiológicos", tabName = "epidemio", icon = icon("users")),
    menuItem("Plantas hospedantes", tabName = "plantas", icon = icon("pagelines")),
    menuItem("Estacionalidad", tabName = "estacionalidad", icon = icon("sun")),
    menuItem("Mapa de riesgo", tabName = "mapa", icon = icon("map-marked")),
    menuItem("Autoras", tabName = "autoras", icon = icon("bookmark"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "home",
      includeMarkdown("home.Rmd")
    ),
    tabItem(
      tabName = "epidemio",
      h2("Aspectos epidemiológicos"),
      fluidRow(
        box(h3("Sexo"),
          p("La mayoría de los accidentes reportados fueron con hombres."),
          plotOutput("graphsex"),
          height = 500
        ),
        box(h3("Edad"),
          p("La mayoría de los accidentes fueron con jóvenes (menor o igual a 20 años)."),
          plotOutput("graphedad"),
          height = 500
        ),
        box(h3("Zona donde ocurrió el accidente"),
          p("Los accidentes reportados ocurrieron en zonas periurbanas, rurales y selváticas."),
          plotOutput("grapharea"),
          height = 500
        ),
        box(h3("Hora del día"),
          p("Los accidentes reportados ocurrieron durante el día."),
          plotOutput("graphhora"),
          height = 500
        ),
        box(h3("Circunstancias del accidente"),
          p("Los accidentes reportados ocurrieron durante actividades recreativas o trabajo en áreas abiertas."),
          plotOutput("graphcircunstancias"),
          height = 500
        ),
        box(h3("Parte del cuerpo afectada"),
          p("El contacto con las orugas ocurrió principalmente en los dedos, manos y brazos."),
          plotOutput("graphcuerpo"),
          height = 500
        )
      )
    ),
    tabItem(
      tabName = "plantas",
      h2("Plantas hospedantes"),
      h3("Plantas hospederas para larvas de Lonomia en Misiones, Argentina."),
      h4(a("Haga clic aquí para acceder a las fotos de las plantas hospedantes.",
        href = "https://drive.google.com/drive/folders/1pWLC8R1U0T6Qo0Wiu-R14m85mRL83kwQ?usp=sharing"
      )),
      fluidRow(
        box(tableOutput("tabsp"), width = "300px", height = "100%")
      )
    ),
    tabItem(
      tabName = "estacionalidad",
      h2("Estacionalidad"),
      p("Las ocurrencias acumuladas en los meses del periodo de estudio (enero 2014 a mayo 2020)."),
      fluidRow(
        plotOutput("grames", width = "95%", height = "500px")
      )
    ),
    tabItem(
      tabName = "mapa",
      h2("Mapa de riesgo"),
      p("Mapa de riesgo de lonomismo para Misiones, Argentina (26 de enero de 2014 - 8 de mayo de 2020) usando estimación de densidad kernel."),
      fluidRow(
        box(leafletOutput("rm"), height = "435px", width = "200px")
      ),
      h2("Número de accidentes por departamento de Misiones"),
      fluidRow(
        box(tableOutput("tabdep"), height = "350px", width = "100px")
      )
    ),
    tabItem(
      tabName = "autoras",
      includeMarkdown("autoras.Rmd")
    )
  )
)

server <- function(input, output) {
  output$graphsex <- renderPlot({
    graphsex
  })
  output$graphedad <- renderPlot({
    graphedad
  })
  output$grapharea <- renderPlot({
    grapharea
  })
  output$graphhora <- renderPlot({
    graphhora
  })
  output$graphcircunstancias <- renderPlot({
    graphcircunstancias
  })
  output$graphcuerpo <- renderPlot({
    graphcuerpo
  })
  output$rm <- renderLeaflet({
    rm
  })
  output$tabsp <- renderTable({
    tabsp
  })
  output$grames <- renderPlot({
    grames
  })
  output$tabdep <- renderTable({
    tabdep
  })
}

ui <- dashboardPage(
  header,
  sidebar,
  body,
  skin = "green"
)

shinyApp(ui, server)
