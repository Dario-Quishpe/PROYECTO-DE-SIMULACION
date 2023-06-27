suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(sparkline))

#getDependency('sparkline')
options(dplyr.summarise.inform = FALSE)



options(scipen = 999)
dir.p <- getwd() # Directorio principal





### DISTRIBUCION TRIANGULAR CON INVERSA
sim_triangular <- function(a,nsim){
  U <- runif(nsim)
  X <- a*(1-U^(1/2))
  res <- data.table(N = 1:nsim, X =X)
  return(res)
}


sim_pan <- function(nsim, a, b, mu, sigma){
  res <- data.table(N = 1:nsim, Produccion = rpar(nsim, a, b), Demanda = rnorm(nsim, mean = mu, sd = sqrt(sigma)))
  res[, Marca := ifelse(Produccion > Demanda, 1, 0)]
  return(res)
}

##Distribucion triangular con metodo de aceptacion y rechazo
sim_triangular_AR<-function(a,nsim){
  fx<-function(a,x){
    return((2/a)*(1-(x/a)))
  }
  gx<-function(a,x){
    return(1/a)
  }
  maxi<-max(fx(a,seq(0,a,a/(nsim-1))))
  c<-maxi*(a)
  ejer1<-function(a,nsim){
    resp<-numeric(nsim)
    x<-seq(0,a,a/nsim)
    for (i in 1:nsim){
      while(TRUE){
        U<-runif(1,0,1)
        V<-runif(1,0,1)
        T <- a*V
        if(c*U*gx(a,x[i])<=fx(a,T)){
          X<-T
          break
        }
      }
      resp[i]<-X
    }
    return(resp)
  }
  return(data.table(N = 1:nsim, X=ejer1(a,nsim)))
}

##Metodo  del cuantil para la funcion de probabilidades
sim_usuario<-function(valores,probs,num){
  Tr_cuantil_unidad<-function(valores,probs){
    p<-probs; X<-valores
    U<-runif(1)
    I<-1
    S<-p[I]
    while(U>S){
      I<-I+1
      S<-S+p[I]
    }
    return(X[I])
  }
  res_<-numeric(num)
  for(i in 1:num){
    res_[i]<-TrCuantil(valores,probs)
  }
  return(data.table(N = 1:num, X=res_))
}
shinyServer(function(input, output, session){
  
  #### Tabla Simulacion Distribucion Triangular (INVERSION)
  
  output$triangular_tabla <- function(){
    res <- data.frame(sim_triangular(input$a_val,input$nsimtriangular))
    
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#5ED7EF") %>% 
      scroll_box(width = "300px", height = "400px")
  }
  #### Tabla Simulacion Distribucion Triangular (INVERSION)
  output$triangular_hc <- renderHighchart({
    
    tb01 <- sim_triangular(input$a_val, input$nsimtriangular)
    
    hchart(tb01$X,name="histograma de los Nros Aleatorios obtenidos por el metodo aplicado  a una Distribucion Triangular",color = "#8ACBA9") %>% 
      hc_title(text = 'HISTOGRAMA',align="center",width="25") |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_add_theme(hc_theme_handdrawn())
  })
  #### Tabla Simulacion Distribucion Triangular (AR)
  output$triangular_tabla_AR <- function(){
    res <- data.frame(sim_triangular_AR(input$a_val_AR,input$nsimtriangular_AR))
    
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#5ED7EF") %>% 
      scroll_box(width = "300px", height = "400px")
  }
  #### Tabla Simulacion Distribucion Triangular(AR)
  output$triangular_hc_AR <- renderHighchart({
    
    tb01 <- sim_triangular_AR(input$a_val_AR, input$nsimtriangular_AR)
    
    hchart(tb01$X,name="histograma de los Nros Aleatorios obtenidos por el metodo aplicado  a una Distribucion Triangular",color = "#8ACBA9") %>% 
      hc_title(text = 'HISTOGRAMA',align="center",width="25") |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_add_theme(hc_theme_handdrawn())
  })
  
  ### INGRESO DE DATOS A LA TABLA EDITABLE Y GENERACION DE SU CORRESPONDIENTE FUNCION DE DISTRIBUCION
  v <- reactiveValues(data = { 
    data.frame(x_i = numeric(0),p_i = numeric(0)) %>% 
      add_row(x_i = seq(1,10), p_i= round(dbinom(0:10,size=9,p=0.4)[0:10],digits = 5))
  })
  
  output$tabla_usuario <- renderDT({
    DT::datatable(v$data, editable = TRUE)
  })
  
  observeEvent(input$tabla_usuario_cell_edit, {
    info = input$tabla_usuario_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    v$data[i,j] <- k
  })
  ### Funcion de distribucion de los datos inidiales creados
  output$F_distribucion <- renderPlot({
    req(input$go)
    v$data|>mutate(res=cumsum(p_i)) |>
      filter(p_i!=0) |> 
      ggplot(aes(x_i,res)) +
      geom_point(size = 3, shape = 5, colour = "blue")+
      geom_line(size = 1.2, linetype = "dashed",color="blue")+labs(title='FUNCION DE DISTRIBUCION DE LOS DATOS INGRESADOS', 
                                                   
                                                      caption="Funcion de distribucion-Si para el nro final de registros ingresados la funcion de distribucion no es 1 vuelva a ingresar los datos")
  })
  output$usuario_resultado <- renderHighchart({
    datos<-v$data|>
      filter(p_i!=0) 
    tb01 <- sim_usuario(datos$x_i,datos$p_i,input$num)
    hchart(tb01$X,breaks=10,name="histograma de los Nros Aleatorios obtenidos por el metodo aplicado a una variable discreta",color = "#8ACBA9") %>% 
      hc_title(text = 'HISTOGRAMA',align="center",width="10") |> 
      hc_plotOptions(series = list(animation =TRUE)) |> 
      hc_add_theme(hc_theme_handdrawn())
  })
  output$sim_usuario_tabla <- function(){
    datos<-v$data|>
      filter(p_i!=0) 
    res <- data.frame(sim_usuario(datos$x_i,datos$p_i,input$num))
    
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#5ED7EF") %>% 
      scroll_box(width = "300px", height = "400px")
  }
  
  
  
  
 
  
 
  
  
})



