#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(flexdashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(chilemapas)



# Lectura bases
cod <- readxl::read_xlsx("codigo_comunas.xlsx") %>% 
    janitor::clean_names() %>% 
    rename('codigo_comuna' = cut_codigo_unico_territorial) %>% 
    mutate(comuna = chartr('áéíóú','aeiouu',tolower(nombre)))

df_elec <- readRDS("base_elec.rds") %>% 
    janitor::clean_names() %>% 
    mutate(comuna = chartr('áéíóú','aeiouu',tolower(comuna)),
           comuna = case_when((comuna == 'aisen') ~ 'aysen',
                              (comuna == 'calera') ~ 'la calera',
                              (comuna == 'marchigue') ~ 'marchihue',
                              (comuna == 'tiltil') ~ 'til til',
                              TRUE ~ comuna))

df_elec2 <- left_join(df_elec,
                      cod,
                      by = 'comuna') %>% 
    select(-provincia.y)

# Incorporar nuevas regiones
df_elec2 <- df_elec2 %>% 
    mutate(id_region = (ifelse((comuna %in% c('corral', 'futrono', 'la union', 'lago ranco', 'lanco', 'los lagos', 'mafil', 'mariquina', 'paillaco', 'panguipulli', 'rio bueno', 'valdivia')),14,id_region)),
           id_region = (ifelse((comuna %in% c('arica','camarones','general lagos','putre')),15,id_region)))

# Diferenciar entre elecciones (primera y segunda vuelta no siempre fueron en años diferentes)
df_elec2 <- df_elec2 %>% 
    mutate(vuelta = case_when((votacion_presidencial == 'UNICA VOTACIÓN')~ '1',
                              (votacion_presidencial == 'PRIMERA VOTACIÓN')~ '1',
                              (votacion_presidencial == 'SEGUNDA VOTACIÓN')~ '2',
                              TRUE ~ NA_character_),
           ano_de_eleccion = case_when((ano_de_eleccion == 2000)~1999,
                                       (ano_de_eleccion == 2006)~2005,
                                       (ano_de_eleccion == 2010)~2009,
                                       TRUE ~ ano_de_eleccion),
           eleccion = paste0(vuelta,'-',ano_de_eleccion))


df_elec2$eleccion <- factor(df_elec2$eleccion, levels = c('1-1989',
                                                                '1-1993',
                                                                '1-1999',
                                                                '2-1999',
                                                                '1-2005',
                                                                '2-2005',
                                                                '1-2009',
                                                                '2-2009',
                                                                '1-2013',
                                                                '2-2013',
                                                                '1-2017',
                                                                '2-2017'))

# Extracción de nulos y blancos
df_elec2 <- df_elec2 %>% 
    filter(candidato_a != 'VOTOS NULOS',
           candidato_a != 'VOTOS EN BLANCO')

# Cálculo de total de votos por candidato por comuna y región
df_elec_calc <- df_elec2 %>% 
    group_by(codigo_comuna,eleccion) %>% 
    mutate(tot_com = sum(votos_totales),
           por_com = (votos_totales/tot_com)) %>% 
    group_by(candidato_a,codigo_comuna,eleccion) %>% 
    mutate(tot_cand_com = round(sum(por_com),2)) %>% 
    ungroup() %>% 
    group_by(eleccion) %>% 
    mutate(tot_nac = sum(votos_totales)) %>% 
    group_by(candidato_a,eleccion) %>% 
    mutate(votos_cand_nac = sum(votos_totales),
           tot_cand_nac = round(votos_cand_nac/tot_nac,2)) %>% 
    ungroup()

# Agrupación por coaliciones
df_elec_calc <- df_elec_calc %>% 
    mutate(coalicion =  case_when((candidato_a %in% c('HERNAN BUCHI', 'ARTURO ALESSANDRI', 'JOAQUIN LAVIN', 'SEBASTIAN PIÑERA', 'EVELYN MATTHEI FORNET', 'SEBASTIAN PIÑERA ECHENIQUE'))~'Chile Vamos y anteriores',
                                  (candidato_a %in% c('PATRICIO AYLWIN', 'EDUARDO FREI', 'RICARDO LAGOS', 'MICHELLE BACHELET', 'MICHELLE BACHELET JERIA', 'CAROLINA GOIC BOROEVIC', 'ALEJANDRO  GUILLIER ALVAREZ')~ 'Nueva Mayoría y anteriores'),
                                  TRUE ~ 'Otros'))

# Cálculo de porcentajes por coalición por comuna
df_elec_com <- df_elec_calc %>%
    count(eleccion,codigo_comuna,comuna,candidato_a,tot_cand_com,coalicion,tot_nac) %>% 
    group_by(codigo_comuna,coalicion,eleccion) %>% 
    mutate(tot_coal_com = sum(tot_cand_com)) %>% 
    select(-n)

# Cálculo de porcentajes por coalición nacional
df_elec_nac <- df_elec_calc %>%
    count(eleccion,candidato_a,tot_cand_nac,coalicion) %>% 
    group_by(coalicion,eleccion) %>% 
    mutate(tot_coal_nac = sum(tot_cand_nac)) %>% 
    count(eleccion,coalicion,tot_coal_nac) %>% 
    select(-n)

# Unión con chilemapas
mapa <- mapa_comunas %>% 
    left_join(df_elec_com, by = 'codigo_comuna') %>% 
    mutate(region = as.numeric(codigo_region))

# Para mapas finales
df_map <- mapa %>%
    group_by(eleccion,comuna,coalicion) %>% 
    sjmisc::add_id() %>% 
    filter(ID == 1) %>% 
    select(coalicion,geometry,tot_coal_com,eleccion,comuna,tot_nac) %>% 
    ungroup() %>% 
    group_by(eleccion,comuna) %>% 
    mutate(ganador = (max(tot_coal_com)),
           aux = (ganador == tot_coal_com),
           gan_f = ifelse((aux == TRUE),coalicion,0)) %>% 
    filter(gan_f != 0)

# Inicio shiny
ui <- navbarPage(
    "Elecciones presidenciales Chile 1989-2017",
    
    theme = shinythemes::shinytheme('yeti'),
    
    tabPanel("Mapa de elecciones",

    sidebarPanel(
            selectInput('region', label = 'Región:',
                        choices = c(15,1:9,14,10,11:13), 
                        selected = '13'),
            selectInput('coalicion', label = 'coalición:',
                        choices = c('Chile Vamos y anteriores',
                                    'Nueva Mayoría y anteriores', 
                                    'Otros'), 
                        selected = 'Otros'),
            selectInput('eleccion', label = 'Elección (1- es primera vuelta, 2- es segunda vuelta):',
                        choices = c('1-1989',
                                    '1-1993',
                                    '1-1999',
                                    '2-1999',
                                    '1-2005',
                                    '2-2005',
                                    '1-2009',
                                    '2-2009',
                                    '1-2013',
                                    '2-2013',
                                    '1-2017',
                                    '2-2017'), 
                        selected = '1-1989'),
            width = 2
        ),

        mainPanel(
            h3('Mapa seleccionado'),
            h6('Utilice los comandos del mapa para navegar sobre él. Haga doble clic para volver el mapa a su posición inicial'),
            plotlyOutput("mapa_chile"),
            h6('Número de candidatos en Elección seleccionada'),
            valueBoxOutput("candidatos"),
            h3('Evolución de la coalición seleccionada'),
            plotOutput('evol_coal')
        )),
    tabPanel("Comparación entre elecciones",
    
    sidebarPanel(selectInput('eleccion_2', label = 'Elección (1- es primera vuelta, 2- es segunda vuelta):',
                             choices = c('1-1989',
                                         '1-1993',
                                         '1-1999',
                                         '2-1999',
                                         '1-2005',
                                         '2-2005',
                                         '1-2009',
                                         '2-2009',
                                         '1-2013',
                                         '2-2013',
                                         '1-2017',
                                         '2-2017'), 
                             selected = '1-1989'),
                 selectInput('eleccion_3', label = 'Elección (1- es primera vuelta, 2- es segunda vuelta):',
                             choices = c('1-1989',
                                         '1-1993',
                                         '1-1999',
                                         '2-1999',
                                         '1-2005',
                                         '2-2005',
                                         '1-2009',
                                         '2-2009',
                                         '1-2013',
                                         '2-2013',
                                         '1-2017',
                                         '2-2017'), 
                             selected = '1-1993'),
                 width = 2),
    mainPanel(
        splitLayout(
        plotOutput('mapacom_1',width = '80%'),
        plotOutput('mapacom_2',width = '80%')),
        mainPanel(
            h3('Evolución del total de votantes en elecciones seleccionadas'),
            plotOutput('ev_vot')
        )
    )))

# Background de shiny
server <- function(input, output) {

    output$mapa_chile <- renderPlotly({
        uno <- mapa %>% 
            filter(region == input$region,
                   eleccion == input$eleccion,
                   coalicion == input$coalicion) %>% 
            mutate(total = tot_coal_com*100) %>% 
            ggplot() +
            geom_sf(aes(fill = total, geometry = geometry),size = 0.01) +
            geom_sf_text(aes(geometry = geometry,label = comuna), size = 0.1, colour = 'black',alpha = 0.2)+
            theme_classic()+
            theme(axis.line.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.line.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.title.y = element_blank(),
                  plot.background = element_blank())+
            labs(fill = '%')+
            scale_fill_continuous(low = 'white',high = case_when((input$coalicion == 'Otros')~'orange',
                                                                 (input$coalicion == 'Chile Vamos y anteriores')~'blue',
                                                                 TRUE ~ 'red'))
        
        ggplotly(uno)
        
    })
   output$candidatos <- renderValueBox({
       valueBox(formatC((mapa %>%
                               filter(eleccion == input$eleccion) %>% 
                               count(candidato_a,codigo_region) %>% 
                               tidyr::spread(candidato_a,n) %>% 
                               ncol()-1)))
   }) 

   output$evol_coal <- renderPlot({
       df_elec_nac %>% 
           filter(coalicion == input$coalicion) %>% 
           ggplot(aes(x = eleccion, y = tot_coal_nac, group = 1))+
           geom_line() +
           geom_text(aes(label = scales::percent(tot_coal_nac,accuracy = 1)),size = 3, nudge_y = 0.02) +
           theme_bw()+
           theme(axis.line.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.title.y = element_blank())+
           labs(x = 'Elección')
   })

   output$mapacom_1 <- renderPlot({
       df_map %>% 
           filter(eleccion == input$eleccion_2) %>% 
           ggplot()+
           stat_sf_coordinates(aes(geometry = geometry, color = gan_f))+
           theme_classic()+
           theme(axis.line.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 axis.line.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.title.y = element_blank())+
           coord_sf(xlim = c(-80, -60),expand = TRUE)+
           labs(color = '')+
           scale_color_manual(values = c('blue','red','orange'), breaks = c('Chile Vamos y anteriores', 'Nueva Mayoría y anteriores','Otros'))
   })
   output$mapacom_2 <- renderPlot({
       df_map %>% 
           filter(eleccion == input$eleccion_3) %>% 
           ggplot()+
           stat_sf_coordinates(aes(geometry = geometry, color = gan_f))+
           theme_classic()+
           theme(axis.line.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank(),
                 axis.line.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.title.y = element_blank())+
           coord_sf(xlim = c(-80, -60),expand = TRUE)+
           labs(color = '')+
           scale_color_manual(values = c('blue','red','orange'), breaks = c('Chile Vamos y anteriores', 'Nueva Mayoría y anteriores','Otros'))
   })
   
  output$ev_vot <- renderPlot({ df_map %>% 
          filter(eleccion %in% c(input$eleccion_2,input$eleccion_3),
                 comuna == 'santiago') %>% 
      mutate(tot_nac = round(tot_nac/100000,1)) %>% 
          ggplot(aes(x=eleccion,y=tot_nac))+
          geom_text(aes(label = tot_nac),size=3.5)+
          theme_bw()+
          theme(axis.line.x = element_blank(),
                axis.line.y = element_blank()) +
          labs(x = 'Elección',
               y = 'Votos x 100000')
    }) 

   } 


# Corriendo el shiny
shinyApp(ui = ui, server = server)