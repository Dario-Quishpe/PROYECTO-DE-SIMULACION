
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
options(dplyr.summarise.inform = FALSE)


# UI
shinyUI(fluidRow(column(width=2,align="center",style="background:#DEE9F9",img(src="https://us.123rf.com/450wm/daryatigrph/daryatigrph1909/daryatigrph190900143/130409028-ilustraci%C3%B3n-de-vector-de-plantilla-de-logotipo-de-b%C3%BAho.jpg", width="150px", height="125px")), # Logo página principal
                           column(width=8,style="background:#FC8585", h1("TRABAJO GRUPAL DE SIMULACION-2023A ", 
                                         style = "background:#F9EDE9 ;text-align:center;align-items:center;color:'black';padding:30px;font-size:2.2em")),
                           column(width=2,align="center",style="background:#DEE9F9",img(src="https://us.123rf.com/450wm/daryatigrph/daryatigrph1909/daryatigrph190900143/130409028-ilustraci%C3%B3n-de-vector-de-plantilla-de-logotipo-de-b%C3%BAho.jpg", width="150px", height="125px"))
),
navbarPage("MODULOS", # Menú principal
           tabPanel("NO ME TOCA ", 
                    h4("Una máquina produce tiras de goma de longitud aleatoria con distribución exp(lambda) (en metros). Después de fabricadas, 
                               las tiras de goma pasan a otra máquina que las estira hasta que rompen en dos  (para comprobar su elasticidad). Suponiendo
                               que el lugar por donde rompe cada tira es aleatorio y con distribución uniforme a lo largo de toda su longitud,
                               describir detalladamente un algoritmo que simule las longitudes de los dos trozos en los que se rompe cada goma."),
                    numericInput("lambda", "Parámetro de la exponencial:", value = 2, min=0.5, max = 4),
                    numericInput("nsim", "Número de gomas a simular:", value = 100, min=10, max = 100000),
                    
                    h3("Gráfico de cajas y bigotes"),
                    fluidRow(column(2, ""),
                             column(8, box(highchartOutput("sim01"), width = 12)),
                             column(2, "")
                    ),
                    p()
                    
           ),
           navbarMenu("Simulación de Variables Aleatorias Continuas", 
                      tabPanel("Transformada Inversa (TRIANGULAR)",style = "background: #D7F4FC",
                               h2("Método de simulación de la transformada inversa"),
                               h3("Distribución Triangular"),
                               numericInput("a_val", "Ingrese el valor de a. Recuerde que el intervalo es (0,a]:", 
                                            value = 3, min = 0, max = 1000),
                               numericInput("nsimtriangular", "Ingrese el número de variables a simular:", 
                                            value = 1000, min = 1, max = 10000),
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("triangular_tabla"))),
        
                                        column(8, box(highchartOutput("triangular_hc",height = 400), width = 12)),
                                        
                               ),
                               
            
                      ),
                      tabPanel("Aceptación y rechazo (TRIANGULAR)",style = "background: #D7F4FC",
                               h2("Método de simulación Aceptación y rechazo "),
                               h3("Distribución Triangular"),
                               numericInput("a_val_AR", "Ingrese el valor de a. Recuerde que el intervalo es (0,a]:", 
                                            value = 3, min = 0, max = 1000),
                               numericInput("nsimtriangular_AR", "Ingrese el número de variables a simular:", 
                                            value = 1000, min = 1, max = 10000),
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("triangular_tabla_AR"))),
                                        
                                        column(8, box(highchartOutput("triangular_hc_AR",height = 400), width = 12)),
                                        
                               ),
                               
                               
                      ),
                      
                      # Sidebar
                      tabPanel("Función de Probabilidad Discreta digitada por el usuario",style = "background: #D7F4FC",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("num", "Ingrese el numero de variables a simular: ", 
                                       value = 10000, min = 0, max = 10000),
                          
                          DTOutput("tabla_usuario"),
                          actionButton("go",label = "Imprimir_FA")
                        ),
                        
                        # Show plot
                        mainPanel(h4("INDICACIONES GENERALES: La 1era tabla a la izquierda representa una pequeña funcion de probabilidades para una variable aleatoria discreta X
                                     la cual puede tomar valores del 0 al 10 con la probabilidad correspondiente.Dichas probabilidades pueden ser modificadas por el usuario mediante 
                                     un doble click en la celda correspondiente. Cabe recalcar que se debe cumplir que la probabilidades colocadas para cada valor x que toma X deben 
                                     sumar 1, y tener solo valores positivos"),
                          plotOutput("F_distribucion")
                        ),
                      ),
                      fluidRow(column(width=4, "RESULTADOS OBTENIDOS DEL METODO",
                                  div(tableOutput("sim_usuario_tabla"))),
                               column(8, box(highchartOutput("usuario_resultado",height = 400), width = 10)),
                               
                      ),
                      ),
                      
           ),
)
)

