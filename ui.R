suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shinythemes))
options(dplyr.summarise.inform = FALSE)


# UI
shinyUI(fluidPage(fluidRow(column(width=2,align="center",style="background:#DEE9F9",img(src="https://cem.epn.edu.ec/imagenes/logos_institucionales/big_png/BUHO_EPN_big.png", width="110px", height="125px")), # Logo página principal
                           column(width=8,style="background:black", h1("TRABAJO GRUPAL DE SIMULACIÓN-2023A ", 
                                                                       style = "background:#F9EDE9 ;text-align:center;align-items:center;color:'black';padding:30px;font-size:2.2em")),
                           column(width=2,align="center",style="background:#DEE9F9",img(src="https://cem.epn.edu.ec/imagenes/logos_institucionales/big_png/BUHO_EPN_big.png", width="110px", height="125px"))
),
navbarPage("MÓDULOS", theme=shinytheme("cosmo"), 
           navbarMenu("Números pseudoaleatorios",
                      tabPanel("Cuadrados Medios",style = "background: #EEFFE3 ",
                               h2("Método de los cuadrados medios"),
                               p(),fluidRow(
                                 column(width=5,numericInput("xocm", "Ingrese una semilla:", value = 123, min = 100, max = 999),
                                        numericInput("ncm", "Seleccione el número de dígitos:", value = 4, min = 1, max = 6),
                                        numericInput("numcm", "Cantidad de números a generar:", value = 10, min = 1, max = 1000)),
                                 column(7, "Resultados Obtenidos del método",
                                        div(tableOutput("cm")))
                               ),
                      ),
                      tabPanel("Lehmer",style = "background: #EEFFE3 ",
                               h2("Método de Lehmer"),
                               p(),
                               fluidRow(
                                 column(width=5,numericInput("xolhm","Ingrese una semilla:", value = 12345, min = 100, max = 999999),
                                        numericInput("clhm","Ingrese c (menor cantidad de cifras que la semilla):", value = 76, min = 100, max = 99999),
                                        numericInput("numlhm", "Cantidad de números a generar:", value = 10, min = 1, max = 1000)),
                                 column(7,"Resultados Obtenidos del método",
                                        div(tableOutput("lhm")))
                               ),
                      ),
                      tabPanel("Congruencial simple",style = "background: #EEFFE3 ",
                               h2("Método Congruencial simple"), 
                               p(),fluidRow(
                                 column(width=5,numericInput("xocs","Ingrese una semilla:", value = 123456, min = 100, max = 999999),
                                        numericInput("mcs","Ingrese el valor de m:", value = 10, min = 1, max = 100),
                                        numericInput("acs","Ingrese el valor de a:", value = 1, min = 1, max = 99),
                                        numericInput("ccs","Ingrese el valor de c:", value = 1, min = 1, max = 99),
                                        numericInput("numcs", "Cantidad de números a generar:", value = 10, min = 1, max = 1000)),
                                 column(7,"Resultados Obtenidos del método",
                                        div(tableOutput("cs")))
                               ),
                      ),
                      tabPanel("Congruencial Multiplicativo",style = "background: #EEFFE3 ",
                               h2("Método Congruencial Multiplicativo"), 
                               p(),fluidRow(
                                 column(width=5,numericInput("xocmult","Ingrese una semilla:", value = 123456, min = 100, max = 999999),
                                        numericInput("mcmult","Ingrese el valor de m:", value = 10, min = 1, max = 100),
                                        numericInput("acmult","Ingrese el valor de a:", value = 2, min = 1, max = 99),
                                        numericInput("numcmult", "Cantidad de números a generar:", value = 10, min = 1, max = 1000)),
                                 column(7,"Resultados Obtenidos del método",
                                        div(tableOutput("cmult")))
                               ),
                      )),
           
           navbarMenu("Simulación de Variables Aleatorias Continuas", 
                      tabPanel("Transformada Inversa: Distribución Exponencial",style = "background: #D7F4FC",
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
                      tabPanel("Transformada Inversa: Distribución de Cauchy",style = "background: #D7F4FC",
                               h2("Método de simulación de la transformada inversa"),
                               h3("Distribución Cauchy"),
                               
                               numericInput("nsim_1", "Ingrese el número de simulaciones a realizar:", 
                                            value = 27, min = 2, max = 1000),
                               numericInput("mu_1", "Ingrese el valor de mu para simular cauchy:", 
                                            value = 3, min = 0, max = 100),
                               numericInput("gamma_1", "Ingrese el valor de gamma para simular cauchy:", 
                                            value = 1, min = 1, max = 10),
                               fluidRow(column(width=4, "Resultados", div(tableOutput("inv_Cauchy"))
                               ),
                               
                               column(8, box(highchartOutput("histograma_cauchy_inversa",height = 400), width = 12)),
                               
                               ),
                               
                      ),
                      tabPanel("Transformada Inversa: Distribución Triangular",style = "background: #D7F4FC",
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
                      tabPanel("Transformada Inversa: Pareto",
                               h2("Método de simulación de la transformada inversa"),
                               h3("Distribución Pareto"),
                               numericInput("a", "Ingrese el valor de a, el cual debe de ser mayor a 0:", 
                                            value = 4, min = 0.01, max = 10, step = 0.01),
                               numericInput("b", "Ingrese el valor de b, el cual debe de ser mayor a 0:", 
                                            value = 4, min = 0.01, max = 10, step = 0.01),
                               numericInput("numpare", "Ingrese el número de variables a simular:", 
                                            value = 10, min = 1, max = 1000),
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("Tabla_Pareto"))),
                                        column(8, box(highchartOutput("Pareto_hc",height = 400), width = 12))),
                               
                      ),
                      
                      tabPanel("Transformada inversa: Weibull",style = "background: #D7F4FC",
                               h2("Método de transformación inversa"),
                               h3("Distribución Weibull"),
                               numericInput("n_1", "Ingrese el número de simulaciones", 
                                            value = 150, min = 0, max = 5000),
                               numericInput("lambda_1", "Ingrese el valor de lambda", 
                                            value = 2, min = 1, max = 10),
                               numericInput("alpha_1", "Ingrese el valor de alpha", 
                                            value = 4, min = 1, max = 10),
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("inv_Weibull"))),
                                        
                                        column(8, box(highchartOutput("Weibull_histograma1",height = 400), width = 12)),
                                        
                               ),
                      ),
                      
                      tabPanel("Aceptación y rechazo: Distribución de Cauchy",style = "background: #D7F4FC",
                               h2("Método de simulación Aceptación y rechazo "),
                               h3("Distribución de Cauchy"),
                               numericInput("nsim_01", "Ingrese el número de simulaciones a realizar:", 
                                            value = 600, min = 2, max = 1000),
                               numericInput("mu_01", "Ingrese el valor de mu para simular cauchy:", 
                                            value = 3, min = 0, max = 100),
                               numericInput("gamma_01", "Ingrese el valor de gamma para simular cauchy:", 
                                            value = 1, min = 1, max = 10),
                               fluidRow(column(width=4, "Resultados",div(tableOutput("AR_Cauchy"))
                               ),
                               
                               column(8, box(highchartOutput("histograma_cauchy",height = 400), width = 12)),
                               
                               ),
                               
                               
                               
                               
                               
                               
                      ),
                      tabPanel("Aceptación y rechazo: Distribución Triangular",style = "background: #D7F4FC",
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
                      tabPanel("Aceptación y rechazo: Distribución Beta",style = "background: #D7F4FC",
                               h2("Método de simulación Aceptación y rechazo "),
                               h3("Distribución Beta"),
                               numericInput("alfa", "Ingrese el valor de alfa:", 
                                            value = 2, min = 2, max = 1000),
                               numericInput("nsimbeta", "Ingrese el número de variables a simular:", 
                                            value = 10, min = 2, max = 10000),
                               numericInput("beta", "Ingrese el valor de beta:", 
                                            value = 3, min = 0, max = 1000),
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("Betatabla_AR"))),
                                        
                                        column(8, box(highchartOutput("Beta_hc",height = 400), width = 12)),
                                        
                               ),
                               
                               
                      ),
                    
                      
                      tabPanel("Aceptación y Rechazo: Weibull",style = "background: #D7F4FC",
                               h2("Método de Aceptación-Rechazo"),
                               h3("Distribución Weibull"),
                               numericInput("n_2", "Ingrese el número de simulaciones", 
                                            value = 150, min = 0, max = 5000),
                               numericInput("lambda_2", "Ingrese el valor de lambda", 
                                            value = 2, min = 1, max = 10),
                               numericInput("alpha_2", "Ingrese el valor de alpha", 
                                            value = 4, min = 1, max = 10),
                               numericInput("a_2", "Ingrese el valor de a (límite inferior del intervalo)", 
                                            value = 2, min = 2, max = 1000),
                               numericInput("b_2", "Ingrese el valor de b (límite superior del intervalo)", 
                                            value = 10, min = 4, max = 10000),
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("AcRc_Weibull"))),
                                        
                                        column(8, box(highchartOutput("Weibull_histograma2",height = 400), width = 12)),
                                        
                               ),
                      ),
           ),
           
           navbarMenu("Simulación de Variables Aleatorias discretas", 
                      tabPanel("Transformación Cuantil: Distribución Binomial", style = "background: #CFFFD8",
                               h2("Método de Transformación Cuantil"),
                               h3("Distribución Binomial"),
                               numericInput("nsim", "Ingrese el número de simulaciones: ", value = 3, min= 2, max = 1000 ),
                               numericInput("n", "Ingrese el número de observaciones para la variable: ", value = 3, min= 1, max = 100 ),
                               numericInput("prob", "Ingrese el valor de la probabilida a simular: ", value = 0.5, min= 0.01, max = 1 ),
                               
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("tabla_binom"))),
                                        #nombre de la tabla a plotar
                                        column(7, box(highchartOutput("histograma")) # nombre de lo q tiene q devolver
                                               
                                        ),
                                        
                                        
                                        
                                        
                               ),
                      ),
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      tabPanel("Tabla guía: Distribución Binomial",style = "background: #CFFFD8",
                               h2("Método de simulación Método de tabla guía"),
                               h3("Distribución Binomial"),
                               numericInput("n1","Ingrese el valor de n:",
                                            value=100,min=1,max=100),
                               numericInput("p1","Ingrese el valor de p:",
                                            value=0.1,min=0.001,max=1),
                               numericInput("nbtg","Ingrese el número  de variables a simular:",
                                            value=100,min=1,max=1000),
                               
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("BinomTG"))),
                                        
                                        column(8, box(highchartOutput("BinoTG_hc",height = 400), width = 12)),
                                        
                               ),
                               
                               
                      ),
                      tabPanel("Transformación Cuantil: Función de masa de probabilidad definida por el usuario",style = "background: #CFFFD8",
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
                                       la cual puede tomar valores del 1 al 10 con la probabilidad correspondiente.Dichas probabilidades pueden ser modificadas por el usuario mediante 
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
                      
                      tabPanel("Tabla Guía: Función de masa de probabilidad definida por el usuario",style = "background: #CFFFD8",
                               h2("Método de simulación Tabla Guía"),
                               h3("Función de masa definida por el usuario"),
                               sidebarLayout(
                                 sidebarPanel(
                                   numericInput("m_3", "Escriba el número de intervalos: ",
                                                value = 100, min = 0, max = 10000),
                                   numericInput("nsim_3", "Ingrese el numero de variables a simular: ",
                                                value = 1000, min = 0, max = 10000),
                                   
                                   DTOutput("tabla_usuario_1"),
                                   actionButton("go",label = "Imprimir_F2")
                                 ),
                                 
                                 # Show plot
                                 mainPanel(h4("INDICACIONES GENERALES: La 1era tabla a la izquierda representa una pequeña funcion de probabilidades para una variable aleatoria discreta X
                                     la cual puede tomar valores del 1 al 10 con la probabilidad correspondiente.Dichas probabilidades pueden ser modificadas por el usuario mediante
                                     un doble click en la celda correspondiente. Cabe recalcar que se debe cumplir que la probabilidades colocadas para cada valor x que toma X deben
                                     sumar 1, y tener solo valores positivos"),
                                           plotOutput("F_distribucion_1")
                                 ),
                               ),
                               fluidRow(column(width=4, "Resultados",
                                               div(tableOutput("sim_usuario_tabla_1"))),
                                        column(8, box(highchartOutput("usuario_resultado_1",height = 400), width = 10)),
                                        
                               ),
                      ),
                      tabPanel("Distribución Poisson",style = "background: #CFFFD8",
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
