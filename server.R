suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(sparkline))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(stringr))

#getDependency('sparkline')
options(dplyr.summarise.inform = FALSE)



options(scipen = 999)
dir.p <- getwd() # Directorio principal

###Numeros Pseudoaleatorios
##Cuadrados medios
nummedio <- function(xo, n){
  x1 <- xo*xo
  res <- 0
  if(str_length(x1) == 2*n){
    res <- str_sub(x1, start = (0.5*n+1), end = (1.5*n))
  } else {
    x1 <- paste0(rep(0, (2*n-str_length(x1))), x1)
    res <- str_sub(x1, start = (0.5*n+1), end = (1.5*n))
  }
  return(res)
}
cuadradosmedios <- function(xo, n, num){
  vector <- numeric(num + 1)
  vector[1] <- xo
  for(i in 1:num){
    vector[i+1] <- as.numeric(nummedio(vector[i], n))
  }
  vector<-vector/(10^n)
  return(vector[-1])
}
##Lehmer
kcifras<-function(xo,n,c,k){
  x1 <- xo*c
  res <- 0
  if(str_length(x1) == n+k){
    aux1<-str_sub(x1, start = 1, end = k)
    aux2<-str_sub(x1, start = k+1, end = n+k)
    res<-as.numeric(aux2)-as.numeric(aux1)
  } else {
    x1 <- paste0(rep(0, (n+k-str_length(x1))), x1)
    aux1<-str_sub(x1, start = 1, end = k)
    aux2<-str_sub(x1, start = k+1, end = n+k)
    res<-as.numeric(aux2)-as.numeric(aux1)
  }
  return(res)
}  

lehmer<-function(xo,n,c,k,num){
  vector<-numeric(num+1)
  vector[1]<-xo
  for (i in 1:num) {
    vector[i+1]<-kcifras(vector[i],n,c,k)
  }
  vector<-vector/(10^n)
  return(vector[-1])
}

##Congruencial Simple
congruencialsimple<-function(xo,a,c,m,n){
  x<-numeric(n+1)
  x[1]<-xo
  for (i in 1:n) {
    x[i+1]<-(a*x[i]+c)%%m
  }
  u=x/m
  return(u[-1])
}   
##Congruencial Multiplicativo
congruencialmult<-function(xo,a,m,n){
  x<-numeric(n+1)
  x[1]<-xo
  for (i in 1:n) {
    x[i+1]<-(a*x[i])%%m
  }
  u=x/m
  return(u[-1])
}   

### DISTRIBUCION TRIANGULAR CON INVERSA
sim_triangular <- function(a,nsim){
  U <- runif(nsim)
  X <- a*(1-U^(1/2))
  res <- data.table(N = 1:nsim, X =X)
  return(res)
}

# DISTRIBUCION PARETO TRANSFORMADA INVERSA
sim_Pareto<-function(nsim,a,b){
  U<-runif(nsim)
  X<-b/((U)^{1/2})
  res<-data.table(N=1:nsim,X=X)
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
    res_[i]<-Tr_cuantil_unidad(valores,probs)
  }
  return(data.table(N = 1:num, X=res_))
}



sim_cauchy <- function(nsim, mu, gamma){
  cauchy <- numeric(nsim)
  i<-1
  while (i <= nsim) {
    U <- runif(1)
    V <- runif(1)
    
    #encontremos la constante c que es una cota superior de la funcion de densidad de cauchy
    f_max <- 1/pi*(1+mu^2)
    c <- f_max*1.1 #cota superior
    
    #encontremos T q sera una distribucion de cauchy pues es la q mas se asemeja a la funcion de densida f de cauchy
    
    laplace <- mu+gamma*tan(pi*V) #forma simplificada de cauchy
    
    #calculemos f(T) q es la fucnion de densidad de cauchy en T
    fx <- 1/(pi*(1+laplace^{2}))
    
    #aceptar o rechazar
    if(U* c <= fx){
      cauchy[i]<- laplace #entra mi punto T
      i<- i+1
    }
    
  }
  return(data.table(N = 1:nsim, X=cauchy))
}


#distribucion de cauchy # METODO DE TRANSFORMADA INVERSA

sim_cauchy_inversa <- function(nsim_1, mu_1, gamma_1){
  cauchy_inversa<- numeric(nsim_1)
  X <- numeric(nsim_1)
  
  for (i in 1:nsim_1) {
    U<- runif(1)
    cauchy_inversa[i] <- mu_1+gamma_1*tan(pi*U)
  }
  
  
  return(data.table(N = 1:nsim_1, X=cauchy_inversa))
}

##Método Aceptación-Rechazo Beta
ARBeta<-function(alf,bet){
  c<-dbeta((alf-1)/(alf+bet-2),alf,bet)
  while(TRUE){
    U<-runif(1,0,1)
    X<-runif(1,0,1)
    if(c*U<=dbeta(X,alf,bet))
      return(X)
  }
  
}

ARBeta2<-function(nsim,alf,bet){
  X<-numeric(nsim)
  for(i in 1:nsim){
    X[i]<-ARBeta(alf,bet)
  }
  res<-data.table(N=1:nsim,X=X)
  return(res)
}


#VARIABLES DISCRETAS
#TRANSFORMACION CUANTIL # DISTRIBUCION BINOMIAL


sim_bino <- function(nsim, n, prob){ #n es el numero de observaciones
  
  binom <- numeric(nsim)
  for (i in 1:nsim) {
    U <- runif(1)  # Generar valores uniformes U
    binom[i] <- qbinom(U, size = n, prob = prob) # funcion de distribucion binomial
  }
  
  return(data.table(N = 1:nsim, X=binom))
}

##Método tabla guía Binomial

TGB<- function(n,p,nsim) {
  m<-n-1
  x<-0:n
  fmp<-dbinom(x,n,p)
  Fx <- cumsum(fmp)
  g <- rep(1,m)
  i <- 1
  for(j in 2:m) {
    while (Fx[i] < (j-1)/(m)) i <- i+1
    g[j] <- i
  }
  X <- numeric(nsim)
  U <- runif(nsim)
  for(j in 1:nsim) {
    i <- g[floor(U[j]*(m))+1]
    while (Fx[i] < U[j]) i <- i + 1
    X[j] <- x[i]
  }
  res<-data.table(N=1:nsim,X=X)
  return(res)
}

#inversa Weibull
WEIBULL<-function(n,lambda, alpha){
  set.seed(1)
  U<-runif(n)
  X <- (-log(U))^(1/alpha)/lambda
  res <- data.table(N = 1:n, X =X)
  return(res)
}

#tabla guía
rfmp.tabla <- function(x,p, m, nsim) {
  
  #x<-0:n
  #p<-dbinom(x,10,pr)
  Fx <- cumsum(p)
  g <- rep(1, m)
  
  
  i <- 1
  for(j in 2:m) {
    while (Fx[i] < (j-1)/m) {
      i <- i + 1
    }
    g[j] <- i
  }
  
  
  X <- numeric(nsim)
  U <- runif(nsim)
  
  for(j in 1:nsim) {
    i <- g[floor(U[j]*m)+1]
    while (Fx[i] < U[j]) {
      i <- i + 1
    }
    X[j] <- x[i]
  }
  res<-data.table(N=1:nsim,X=X)
  return(res)
}

den_Weibull<-function(x,lambda,alpha){
  return(alpha*lambda^alpha*x^(alpha-1)*exp((-lambda*x)^alpha))
}

##Aceptación-Rechazo Weibull
AR_Weibull<-function(n,lambda,alpha,a,b){
  x<-seq(a,b,l=n)
  U<-runif(n,0,1)
  V<-runif(n,0,1)
  M=max(den_Weibull(x,lambda,alpha))
  c=M*(b-a)
  T=a+(b-a)*V
  X<-T[M*U<=den_Weibull(T,lambda,alpha)]
  res<-data.table(N=1:n,X=X)
  return(res)
}

#________________________________________________________



shinyServer(function(input, output, session){
  
  ####Numero pseudoaletorios
  ###Cuadrados Medios
  output$cm <- function(){
    res <- data.frame(i=seq(1:input$numcm), 
                      X=cuadradosmedios(xo=input$xocm, n = input$ncm, num = input$numcm))
    
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#F57B9A") %>% 
      scroll_box(width = "600px", height = "400px")
  }
  ###Lehmer
  output$lhm <- function(){
    res<-data.frame(n=seq(1:input$numlhm),
                    X=lehmer(input$xolhm,str_length(input$xolhm),input$clhm,str_length(input$clhm),input$numlhm))
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#F57B9A") %>% 
      scroll_box(width = "600px", height = "400px")
  }
  
  ###Congruencial simple
  output$cs<-function(){
    res<-data.frame(n=seq(1:input$numcs),
                    X=congruencialsimple(input$xocs,input$acs,input$ccs,input$mcs,input$numcs))
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#F57B9A") %>% 
      scroll_box(width = "600px", height = "400px")
  }
  ###Congruencial Multiplicativo
  output$cmult<-function(){
    res<-data.frame(n=seq(1:input$numcmult),
                    X=congruencialmult(input$xocmult,input$acmult,input$mcmult,input$numcmult))
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#F57B9A") %>% 
      scroll_box(width = "600px", height = "400px")
  }
  
  ###Simulación Distribucion exponencial. Transformada inversa
  simexponencial <- function(lambda, num){
    U <- runif(num)
    X <- -log(U)/lambda
    return(X)
  }
  
  output$exponencial <- function(){
    res <- data.frame(n=seq(1:input$numexp),
                      X=simexponencial(input$lambdaexp, input$numexp))
    
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "skyblue") %>% 
      scroll_box(width = "300px", height = "400px")
  } 
  
  output$exponencial_hc <- renderHighchart({
    
    X<-simexponencial(input$lambdaexp, input$numexp)
    hchart(X,name="",color = "skyblue") %>% 
      hc_title(text = 'HISTOGRAMA',align="center",width="25") |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_add_theme(hc_theme_economist())
  })
  
  
  #### Tabla Simulacion Distribucion Triangular (INVERSION)
  
  output$triangular_tabla <- function(){
    res <- data.frame(sim_triangular(input$a_val,input$nsimtriangular))
    
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#5ED7EF") %>% 
      scroll_box(width = "300px", height = "400px")
  }
  
  ####Tabla simulación Distrbución Pareto por Transformación Inversa
  
  output$Tabla_Pareto<-function(){
    respareto<-data.frame(sim_Pareto(input$numpare,input$a,input$b))
    
    kbl(respareto) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#B88F96") %>% 
      scroll_box(width = "300px", height = "300px")
    
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
  
  ##Tabla simulación Binomial, método tabla  guía
  
  
  output$BinomTG<-function(){
    tg<-data.frame(TGB(input$n1,input$p1,input$nbtg))
    kbl(tg) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#B88F96") %>% 
      scroll_box(width = "300px", height = "300px")
  }
  ##Tabla simulación Beta Aceptación-Rechazo
  
  output$Betatabla_AR<-function(){
    Beta<-data.frame(ARBeta2(input$nsimbeta,input$alfa,input$beta))
    kbl(Beta) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#B88F96") %>% 
      scroll_box(width = "300px", height = "300px")
    
  }
  
  ##Gráfico Binomial, Tabla guía
  output$BinoTG_hc<-renderHighchart({
    TG<-TGB(input$n1,input$p1,input$nbtg)
    hchart(TG, "column", hcaes(x = X, y = N))|> 
      hc_title(text = 'Gráfico Barras Método Tabla guía Binomial',width="25")|>
      hc_add_theme(hc_theme_darkunica())
    
    
  })
  
  
  ##Gráfico Pareto, Transformación Inversa
  
  output$Pareto_hc<-renderHighchart({
    tbpareto<-sim_Pareto(input$numpare,input$a,input$b)
    hchart(tbpareto$X,name="Densidad",type="area",color="#B7858E")|> 
      hc_add_theme(hc_theme_monokai())
  })
  ##Gráfico Beta, aceptación-rechazo
  
  output$Beta_hc<-renderHighchart({
    Beta<-ARBeta2(input$nsimbeta,input$alfa,input$beta)
    hchart(Beta$X,color="#F23F5E",name="Hitograma Beta")|>
      hc_title(text = 'Histograma Simulación Beta',align="center",width="25") |>
      hc_add_theme(hc_theme_bloom())
  })
  
  ### Tabla inversa Weibull
  output$inv_Weibull <- function(){
    res_1 <- data.frame(WEIBULL(input$n_1,input$lambda_1,input$alpha_1))
    kbl(res_1) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "blue") %>% 
      scroll_box(width = "300px", height = "400px")
  }
  ### Tabla Aceptación-rechazo Weibull
  output$AcRc_Weibull <- function(){
    res_1 <- data.frame(AR_Weibull(input$n_2,input$lambda_2,input$alpha_2,
                                   input$a_2,input$b_2))
    kbl(res_1) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "blue") %>% 
      scroll_box(width = "300px", height = "400px")
  }
  
  ### Gráfico de Trans. inversa Weibull
  output$Weibull_histograma1 <- renderHighchart({
    his1<-WEIBULL(input$n_1,input$lambda_1,input$alpha_1)
    hchart(his1$X,name="histograma de los Nros Aleatorios obtenidos por el metodo de Transformación inversa a una Weibull",color = "#8ACBA9") %>% 
      hc_title(text = 'HISTOGRAMA',align="center",width="25") |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_add_theme(hc_theme_handdrawn())
  })
  ### Gráfico A-R Weibull
  output$Weibull_histograma2 <- renderHighchart({
    his2<-AR_Weibull(input$n_2,input$lambda_2,input$alpha_2,
                     input$a_2,input$b_2)
    hchart(his2$X,name="histograma de los Nros Aleatorios obtenidos por el metodo Aceptación-Rechazo a una Weibull",color = "#8ACBA9") %>% 
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
      geom_line(size = 1.2, linetype = "dashed",color="blue")+labs(title='FUNCION DE DISTRIBUCION ACUMULADA DE LOS DATOS INGRESADOS', 
                                                                   
                                                                   caption="Funcion de distribucion Acumulada-Si para el nro final de registros ingresados la funcion de distribucion no es 1 vuelva a ingresar los datos")
  })
  output$usuario_resultado <- renderHighchart({
    datos<-v$data|>
      filter(p_i!=0) 
    tb01 <- sim_usuario(datos$x_i,datos$p_i,input$num)
    hchart(tb01$X,breaks=10,name="Histograma de los Nros Aleatorios obtenidos por el metodo aplicado a una variable discreta",color = "#8ACBA9") %>% 
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
  
  ##Distribucion de Poisson
  simPoisson<- function(lambda, nsim = 1000) {
    X <- numeric(nsim)
    U <- runif(nsim)
    for(j in 1:nsim) {
      i <- 0
      p <- exp(-lambda)
      Fx <- p
      while (Fx < U[j]){
        i <- i + 1
        p <-lambda/i*p
        Fx<-Fx+p
      } 
      X[j] <- i
    }
    return(X)
  }
  
  output$poisson <- function(){
    res <- data.frame(n=seq(1:input$numpois),
                      X=simPoisson(input$lambdap, input$numpois))
    
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "skyblue") %>% 
      scroll_box(width = "300px", height = "400px")
  } 
  
  output$poisson_hc <- renderHighchart({
    
    X<-simPoisson(input$lambdap, input$numpois)
    hchart(X,name="",color = "skyblue") %>% 
      hc_title(text = 'HISTOGRAMA',align="center",width="25") |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_add_theme(hc_theme_economist())
  })
  
  output$inv_Cauchy <- function(){
    res_1 <- data.frame(sim_cauchy_inversa(input$nsim_0,input$mu_0,input$gamma_0))
    kbl(res_1) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "blue") %>% 
      scroll_box(width = "300px", height = "400px")
  }
  
  output$histograma_cauchy_inversa <- renderHighchart({
    his2<-sim_cauchy_inversa(input$nsim_0,input$mu_0,input$gamma_0)
    hchart(his2$X,name="histograma de la Distribución de Cauchy por la transformación inversa",color = "#8ACBA9") %>% 
      hc_title(text = 'HISTOGRAMA',align="center",width="25") |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_add_theme(hc_theme_handdrawn())
  })
  
  output$histograma_cauchy <- renderHighchart({
    his2<-sim_cauchy(input$nsim_01,input$mu_01,input$gamma_01)
    hchart(his2$X,name="histograma de la Distribución de Cauchy por Aceptación y Rechazo",color = "#8ACBA9") %>% 
      hc_title(text = 'HISTOGRAMA',align="center",width="25") |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_add_theme(hc_theme_handdrawn())
  })
  output$AR_Cauchy <- function(){
    res_1 <- data.frame(sim_cauchy(input$nsim_01,input$mu_01,input$gamma_01))
    kbl(res_1) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "green") %>% 
      scroll_box(width = "300px", height = "400px")
  }
  
 
  #------------------- VARIABLES DISCRETAS 
  
  #trabla
  output$tabla_binom <- function(){
    res <- data.frame(
      X=sim_bino(input$nsim, input$n, input$prob))
    
    kbl(res) %>% 
      kable_styling(position = "center") %>% 
      row_spec(0, bold = TRUE, background = "#EA08F3") %>% 
      scroll_box(width = "300px", height = "400px")
  } 
  
  # Generar el histograma con renderHighchart
  output$histograma <- renderHighchart({
    binomial <- sim_bino(input$nsim, input$n, input$prob)
    
    # Crear el histograma
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_add_series(data = binomial$X, name = "Valor de la binomial", color = "skyblue") %>%
      hc_title(text = "Histograma de la Distribución Binomial (Transformación cuantil)") %>%
      hc_xAxis( title = list(text = "Número de Simulación")) %>%
      hc_yAxis(title = list(text = "Valor de la binomial")) %>%
      hc_colors(c("#EA08F3")) %>%
      hc_plotOptions(column = list(colorByPoint = TRUE))
    
    hc
    
    
    
  })
  
  ### INGRESO DE DATOS A LA TABLA EDITABLE PARA MÉTODO DE LA TABLA GUÍA
  
  v1 <- reactiveValues(data = { 
    data.frame(x_2i = numeric(0),p_2i = numeric(0)) %>% 
      add_row(x_2i = seq(1,10), p_2i= round(dbinom(0:10,size=9,p=0.4)[0:10],digits = 5))
  })
  
  output$tabla_usuario_1 <- renderDT({
    DT::datatable(v1$data, editable = TRUE)
  })
  
  observeEvent(input$tabla_usuario_cell_edit_1, {
    info = input$tabla_usuario_cell_edit_1
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    v1$data[i,j] <- k
  })
  ### Funcion de distribucion de los datos inidiales creados
  output$F_distribucion_1 <- renderPlot({
    req(input$go)
    v1$data|>mutate(res=cumsum(p_2i)) |>
      filter(p_2i!=0) |> 
      ggplot(aes(x_2i,res)) +
      geom_point(size = 3, shape = 5, colour = "blue")+
      geom_line(size = 1.2, linetype = "dashed",color="blue")+labs(title='FUNCION DE DISTRIBUCION DE LOS DATOS INGRESADOS', 
                                                                   
                                                                   caption="Funcion de distribucion-Si para el nro final de registros ingresados la funcion de distribucion no es 1 vuelva a ingresar los datos")
  })
  output$usuario_resultado_1 <- renderHighchart({
    datos<-v1$data|>
      filter(p_2i!=0)
    tb01 <- rfmp.tabla(datos$x_2i,datos$p_2i,input$m_3,input$nsim_3)
    hchart(tb01$X,breaks=10,name="histograma de los Nros Aleatorios obtenidos por el metodo de la tabla guia aplicado a una variable discreta",color = "#8ACBA9") %>%
      hc_title(text = 'HISTOGRAMA',align="center",width="10") |>
      hc_plotOptions(series = list(animation =TRUE)) |>
      hc_add_theme(hc_theme_handdrawn())
  })
  
  output$sim_usuario_tabla_1 <- function(){
    datos<-v1$data|>
      filter(p_2i!=0)
    res <- data.frame(rfmp.tabla(datos$x_2i,datos$p_2i,input$m_3,input$nsim_3))
    
    kbl(res) %>%
      kable_styling(position = "center") %>%
      row_spec(0, bold = TRUE, background = "#5ED7EF") %>%
      scroll_box(width = "300px", height = "400px")
  }
  
  
  
})





