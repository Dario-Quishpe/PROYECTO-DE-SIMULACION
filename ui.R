
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shinythemes))
options(dplyr.summarise.inform = FALSE)


# UI
shinyUI(fluidPage(fluidRow(column(width=2,align="center",style="background:#DEE9F9",img(src="https://cem.epn.edu.ec/imagenes/logos_institucionales/big_png/BUHO_EPN_big.png", width="150px", height="125px")), # Logo página principal
                           column(width=8,style="background:#FC8585", h1("TRABAJO GRUPAL DE SIMULACIÓN-2023A ", 
                                         style = "background:#F9EDE9 ;text-align:center;align-items:center;color:'black';padding:30px;font-size:2.2em")),
                           column(width=2,align="center",style="background:#DEE9F9",img(src="https://cem.epn.edu.ec/imagenes/logos_institucionales/big_png/BUHO_EPN_big.png", width="150px", height="125px"))
),
navbarPage("MÓDULOS", theme=shinytheme("cosmo"), 
           navbarMenu("Números pseudoaleatorios",
                      tabPanel("Cuadrados Medios",
                               h2("Método de los cuadrados medios"),
                               p(),
                               numericInput("xocm", "Ingrese una semilla:", value = 123, min = 100, max = 999),
                               numericInput("ncm", "Seleccione el número de dígitos:", value = 4, min = 1, max = 6),
                               numericInput("numcm", "Cantidad de números a generar:", value = 10, min = 1, max = 1000),
                               column(12, "Resultados",
                                      div(tableOutput("cm")))
                      ),
                      tabPanel("Lehmer",
                               h2("Método de Lehmer"),
                               p(),
                               numericInput("xolhm","Ingrese una semilla:", value = 12345, min = 100, max = 999999),
                               numericInput("clhm","Ingrese c (menor cantidad de cifras que la semilla):", value = 76, min = 100, max = 99999),
                               numericInput("numlhm", "Cantidad de números a generar:", value = 10, min = 1, max = 1000),
                               column(12,"Resultados",
                                      div(tableOutput("lhm")))
                      ),
                      tabPanel("Congruencial simple",
                               h2("Método Congruencial simple"), 
                               p(),
                               numericInput("xocs","Ingrese una semilla:", value = 123456, min = 100, max = 999999),
                               numericInput("mcs","Ingrese el valor de m:", value = 10, min = 1, max = 100),
                               numericInput("acs","Ingrese el valor de a:", value = 1, min = 1, max = 99),
                               numericInput("ccs","Ingrese el valor de c:", value = 1, min = 1, max = 99),
                               numericInput("numcs", "Cantidad de números a generar:", value = 10, min = 1, max = 1000),
                               column(12,"Resultados",
                                      div(tableOutput("cs")))
                      ),
                      tabPanel("Congruencial Multiplicativo",
                               h2("Método Congruencial lineal (minimal standar)"), 
                               p(),
                               numericInput("xocmult","Ingrese una semilla:", value = 123456, min = 100, max = 999999),
                               numericInput("mcmult","Ingrese el valor de m:", value = 10, min = 1, max = 100),
                               numericInput("acmult","Ingrese el valor de a:", value = 2, min = 1, max = 99),
                               numericInput("numcmult", "Cantidad de números a generar:", value = 10, min = 1, max = 1000),
                               column(12,"Resultados",
                                      div(tableOutput("cmult")))
                      )),
           
           navbarMenu("Simulación de Variables Aleatorias Continuas", 
                      tabPanel("Transformada Inversa. Distribución Exponencial",style = "background: #D7F4FC",
                               h2("Método de simulación de la transformada inversa"),
                               h3("Distribución Exponencial"),
                               p(),
                               fluidRow(
                                 column(width=3,
                                        numericInput("lambdaexp", "Ingrese el valor de lambda:", 
                                                     value = 4, min = 0.01, max = 10, step = 0.01),
                                        numericInput("numexp", "Ingrese el número de variables a simular:", 
                                                     value = 200, min = 1, max = 1000),
                                        column(12, "Resultados",
                                               div(tableOutput("exponencial")))
                                 ),
                                 column(width=9,
                                         box(highchartOutput("exponencial_hc",height = 400), width = 12)))
                               
                               
                      ),
                      tabPanel("Transformada Inversa. Distribución de Cauchy",style = "background: #D7F4FC",
                               h2("Método de simulación de la transformada inversa"),
                               h3("Distribución Cauchy"),
                        
                               numericInput("nsim_1", "Ingrese el número de simulaciones a realizar:", 
                                            value = 3, min = 2, max = 1000),
                               numericInput("mu_1", "Ingrese el valor de mu para simular cauchy:", 
                                            value = 3, min = 0, max = 100),
                               numericInput("gamma_1", "Ingrese el valor de gamma para simular cauchy:", 
                                            value = 1, min = 1, max = 10),
                               fluidRow(column(width=4, "Resultados"
                               ),
                               
                               column(8, box(highchartOutput("histograma_cauchy_inversa",height = 400), width = 12)),
                               
                               ),
                               
                      ),
                      tabPanel("Transformada Inversa. Distribución Triangular",style = "background: #D7F4FC",
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
                      tabPanel("Transformada Inversa. Distribución Pareto",style = "background: #D7F4FC",
                               h2("Método de simulación de la transformada inversa"),
                               h3("Distribución de Pareto"),
                      
                               
                               
                      ),
                      tabPanel("Transformada Inversa. Distribución Weibull",style = "background: #D7F4FC",
                               h2("Método de simulación de la transformada inversa"),
                               h3("Distribución Weibull"),
                          
                               
            
                      ),
                     
                      tabPanel("Aceptación y rechazo. Distribución de Cauchy",style = "background: #D7F4FC",
                               h2("Método de simulación Aceptación y rechazo "),
                               h3("Distribución de Cauchy"),
                               numericInput("nsim", "Ingrese el número de simulaciones a realizar:", 
                                            value = 3, min = 2, max = 1000),
                               numericInput("mu", "Ingrese el valor de mu para simular cauchy:", 
                                            value = 3, min = 0, max = 100),
                               numericInput("gamma", "Ingrese el valor de gamma para simular cauchy:", 
                                            value = 1, min = 1, max = 10),
                               fluidRow(column(width=4, "Resultados"
                                               ),
                                        
                                        column(8, box(highchartOutput("histograma_cauchy",height = 400), width = 12)),
                                        
                               ),
                               
                               
                      
                    
                               
                               
                      ),
                      tabPanel("Aceptación y rechazo. Distribución Triangular",style = "background: #D7F4FC",
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
                      tabPanel("Aceptación y rechazo. Distribución Pareto",style = "background: #D7F4FC",
                               h2("Método de simulación Aceptación y rechazo "),
                               h3("Distribución de Pareto"),
                     
                               
                               
                      ),
                      tabPanel("Aceptación y rechazo. Distribución Weibull",style = "background: #D7F4FC",
                               h2("Método de simulación Aceptación y rechazo "),
                               h3("Distribución Weibull"),
           
                               
                               
                      )),
           
           navbarMenu("Simulación de Variables Aleatorias discretas", 
                      tabPanel("Método de Transformación Cuantil", style = "background: #D7F4FC",
                               h2("Simulación de una Distribución Binomial"),
                               numericInput("nsim", "Ingrese el número de simulaciones: ", value = 3, min= 2, max = 1000 ),
                               numericInput("prob", "Ingrese el valor de la probabilida a simular: ", value = 0.5, min= 0.01, max = 1 ),
                               
                               fluidRow(column(4, " "),
                                        #nombre de la tabla a plotar
                                        column(7, box(highchartOutput("histograma")) # nombre de lo q tiene q devolver
                                               
                                        ),
                                        
                                        
                                        
                                        
                               ),
                      ),
                      
          
           
                                 
                              
                        
                       
           
           
                        tabPanel("Tabla Guía. Distribución Binomial",style = "background: #D7F4FC",
                                 h2("Método de simulación Tabla Guía"),
                                 h3("Distribución Binomial"),
                    
                        ),
                        tabPanel("Transformación Cuantil. Función de masa de probabilidad definida por el usuario",style = "background: #D7F4FC",
                                 h2("Método de simulación Transformación Cuantil"),
                                 h3("Función de masa definida por el usuario"),
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
                        tabPanel("Tabla Guía. Función de masa de probabilidad definida por el usuario",style = "background: #D7F4FC",
                                 h2("Método de simulación Tabla Guía"),
                                 h3("Función de masa definida por el usuario"),
                                 
                        ),
                        tabPanel("Distribución Poisson",style = "background: #D7F4FC",
                                 h2("Simulación variable discreta con dominio infinito"),
                                 h3("Distribución de Poisson"),
                                 p(),
                                 fluidRow(
                                   column(width=3,
                                          numericInput("lambdap", "Ingrese el valor de lambda:", 
                                                       value = 4, min = 0.01, max = 10, step = 0.01),
                                          numericInput("numpois", "Ingrese el número de variables a simular:", 
                                                       value = 200, min = 1, max = 1000),
                                          column(12, "Resultados",
                                                 div(tableOutput("poisson")))
                                   ),
                                   column(width=9,
                                          box(highchartOutput("poisson_hc",height = 400), width = 12)))
                                 
                        ),
                        ),
)
)
)
