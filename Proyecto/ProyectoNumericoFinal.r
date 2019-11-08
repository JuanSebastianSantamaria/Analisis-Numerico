
#install.packages("DT")
#install.packages("data.table")
#install.packages("shinydashboard")
#install.packages("deSolve")
#install.packages("shinyjs")
#install.packages("phaseR")

library (shiny) 
library (shinydashboard) 
library (deSolve)
library (phaseR)
library (pracma)

ui <- dashboardPage ( 
  dashboardHeader (), 
  dashboardSidebar (), 
  dashboardBody () 
) 
r=5
t=0.25*3.1416*(r)**4
L=10

header <- dashboardHeader(title = "Worm Simulator",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Autores",
                                         message = "Natalia Navas"
                                       ),
                                       messageItem(
                                         from = "Autores",
                                         message = "Sebastian Santamaria"
                                       ),
                                       messageItem(
                                         from = "Autores",
                                         message = "Jorge Salgado"
                                       ),
                                       messageItem(
                                         from = "Referencia",
                                         message = "https://www.hindawi.com/journals/scn/2018/9756982/"
                                       )
                          )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Informacion", tabName = "Informacion", icon = icon("bell")),
    menuItem("S.I.", tabName = "SI", icon = icon("circle")    ),
    menuItem("S.I.R.", tabName = "SIR", icon = icon("circle") )
  )
  #,h5(textOutput(" currentTime"))
)

frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

TabSI <- fluidRow( 
  
  box(
    title = "Datos iniciales S.I.", 
    width = 4,
    shinyjs::useShinyjs(),
    shinyjs::disabled(numericInput("poblacionInicialSI", "Host iniciales:", 20, min = 1, max = 100000)),
    sliderInput("infectadosInicialesSI", "Infectados iniciales:", 1, 5000, 1),
    sliderInput("suceptiblesInicialesSI", "Host suceptibles iniciales:", 300, 10000, 999),
    sliderInput("tiempoLimiteSI", "Tiempo limite:", 5, 20, 10),
    sliderInput("bethaSI", "Betha:", 0.1, 1, 0.2),
    sliderInput("gammaSI", "Gamma:", 1, 10, 5),
    radioButtons("metodoSeleccionadoSI", "Seleccione el metodo deseado",
                 c("E.D. Ordinarias" = "EDOSI",
                   "E.D. Finitas" = "DFSI") ),
    actionButton("botonCalcularSI", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficas", height = "300px",
    tabPanel("Modelo 1",
             width= 200,
             plotOutput("plotM1SI", height = 200)
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM1PendSI", height = 250, width = 400)
    ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM1ErrorSI", height = 250, width = 500)
    )
  ),
  
  tabBox(
    
    title = "Metodo 2",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM1SI", height = "300px",
    tabPanel("Modelo",
             width= 200,
             plotOutput("plotM2SI", height = 200)
             
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM2PendSI", height = 250, width = 400)
                 ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM2ErrorSI", height = 250, width = 500)
                 )
  )
  
)


TabSIR <- fluidRow( 
  box(
    title = "Datos iniciales S.I.R.", 
    width = 4,
    shinyjs::useShinyjs(),
    shinyjs::disabled(numericInput("poblacionInicialSIR", "Host iniciales:", 20, min = 1, max = 100000)),
    sliderInput("infectadosInicialesSIR", "Infectados iniciales:", 1e-6, min = 0, max = 1,step = 1e-6),
    sliderInput("suceptiblesInicialesSIR", "Host suceptibles iniciales:", 1-1e-6, min = 0, max = 1, step = 1e-6 ),
    sliderInput("tiempoLimiteSIR", "Tiempo limite:", 40, 70, 56),
    sliderInput("bethaSIR", "Betha:", 0.6, 1.5, 1.4247,step = 0.1),
    sliderInput("gammaSIR", "Gamma:", 0, 0.2, 0.14286, step = 0.01),
    radioButtons("metodoSeleccionadoSIR", "Seleccione el metodo deseado",
                 c("E.D. Ordinarias" = "EDOSIR",
                   "E.D. Finitas" = "DFSIR") ),
    actionButton("botonCalcularSIR", "Calcular")
  ),
  tabBox(
    title = "Metodo 1",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM1SI", height = "300px",
    tabPanel("Modelo",
             width= 200,
             plotOutput("plotM1SIR", height = 200)
                 ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM1PendSIR", height = 250, width = 400)
                 ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM1ErrorSIR", height = 250, width = 500)
            )
  ),
  
  tabBox(
    
    title = "Metodo 2",
    width = 8,
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabGraficasM2SI", height = "300px",
    tabPanel("Modelo",
             width= 200,
             plotOutput("plotM2SIR", height = 200)
    ),
    tabPanel("Pendientes",
             width = 200,
             plotOutput("plotM2PendSIR", height = 250, width = 400)
                 ),
    tabPanel("Error",
             width = 200,
             plotOutput("plotM2ErrorSIR", height = 250, width = 500)
                 )
  )
)


information <- fluidRow( 
  box(
    title = "Informacion",
    width = "600px",
    h3("Informacion importante"),
    box(
      title = "Glosario",
      h5(" * Malware: consiste en todo programa o codigo malicioso que deteriora los sistemas digitales. El objetivo del malware es invadir, destruir o deshabilitar equipos, sistemas informaticos, dispositivos IoT (Inthernet of things), dispositivos moviles, entre otros. [2]"),
      h5(" * Host: tambien conocido como anfitrion, consiste en una unidad infromatica, o un sistema, conectada a una red que proveen y utilizan servivicios de ella. [3]"),
      h5(" * Worm (gusano): consiste en un malware o sub clase de virus construidos con distintos fines destructivos. El mayor peligro de los gusanos consiste en su capacidad de replicarse y propagarse sin la ayuda de una persona, es decir, no quiere una interaccion directa. Son considerados debastadores en un entorno informatico. [4]")
      ),
    box(
      title = "Descripcion",
      h5("Gracias al creciente desarrollo del internet, los problemas de malware, y mas especificamente gusanos, en redes se han vuelto cada vez mas graves. En contramedida, los desarrolladores y proveedores han generado una serie de parches a sus productos para evitar la propagacion de distintos tipos de malware. No obstante, los equipos se contaminan o son suceptibles debido a distintas fallas en protocolos de seguridad. Los malware se caracterizan por infectar dispositivos en procesos no lineales a menos que exista una contramedida o salvaguardas. Asi mismo, las unidades infectadas pueden ser recuperadas o depuradas de estos gusanos. Teniendo en cuenta lo anterior, se establece un rango de una tasa de infeccion que describe el proceso de propagacion de los gusanos, dicha tasa de denomina Betha. Por otra parte, Gamma se define como la probabilidad de contagio entre distintas unidades o host. El siguiente aplicativo tiene como finalidad plantear un modelo de propagacion de gusanos, dicho modelo permite describir su comportamiento cuando se propagan via internet. [1]")
    ),
    box(
      title = "Referencias",
      h5("Principal: https://www.hindawi.com/journals/scn/2018/9756982/ , obtenido de:"),
      h5("[1] S. Staniford, V. Paxson, and N. Weaver, How to Own the Internet in Your Spare Time, pp. 149-169, 2002."),
      h5("[2] Niels Provos. «The Ghost In The Browser, Analysis of Web-based Malware» "),
      h5("[3] Suppl. Meth. (Moench) 221 [2 de mayo de 1802] (IK)"),
      h5("[4] Zakon, Robert H. (noviembre de 1997). «RFC 2235 - Hobbes' Internet Timeline». Network Working Group (en ingles). Consultado el 5 de noviembre de 2019")
    )
  )
  
)

body <- dashboardBody(
  tabItems(
    tabItem("SI",frow1, TabSI),
    tabItem("SIR",TabSIR),
    tabItem("Informacion",information)
  )
  
)

ui <- dashboardPage(title = 'Analisis Numerico', header, sidebar, body, skin='yellow')

server <- function (input, output,session) 
{
  
  CalcularEDOSI <- function(){
    output$plotM1SI <- renderPlot({
      #tamanio poblacional
      N = input$poblacionInicialSI
      #N = 20
      
      #Tiempo limite
      tf = input$tiempoLimiteSI
      #tf = 70
      
      #estado inicial de los compartimentos
      init <- c(S = input$suceptiblesInicialesSI , # Suceptibles ->>  input$suceptiblesInicialesSI
                I = input$infectadosInicialesSI)  # Infectados ->>  input$infectadosInicialesSI
      
      #parametros del modelo (coeficientes de las variables)
      param <- c( beta = input$bethaSI , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                  gamma = input$gammaSI ) # Probabilidad de transmitir la infeccion  input$gammaSI
      
      #crear la funcion con las ODE
      si <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- - gamma*beta*S*I/(S+I)
               dI <- + gamma*beta*S*I/(S+I)
               #resultados de las tasas de cambio    
               return(list(c(dS, dI)))
             })
      }
      
      #intervalo de tiempo y resolucion
      times <- seq(0, tf, by = 0.1)
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM1SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
      #simulacionM1SI.si
      #eliminar la variable 'time' en out
      #out$time <- NULL
      #Esta orden nos permite hacer referencia de manera directa a #las columnas de los resultados obtenidos
      attach(simulacionM1SI.si)
      #Calculamos el tamanio de la poblacion
      N <- sum(init)
      updateTextInput(session, "poblacionInicialSI", value =N)
      
      
      
      #Representamos graficamente los resultados obtenidos
      plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de host",main = "Metodo 1: Runge Kutta 4")
      lines(times, I, type="l", col="red")
      #title("Modelo SI")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2)) 
      
    })
    
    output$plotM2SI <- renderPlot({
      #tamanio poblacional
      N = input$poblacionInicialSI
      #N = 20
      
      #Tiempo limite
      tf = input$tiempoLimiteSI
      #tf = 70
      
      #estado inicial de los compartimentos
      init <- c(S = input$suceptiblesInicialesSI , # Suceptibles ->>  input$suceptiblesInicialesSI
                I = input$infectadosInicialesSI)  # Infectados ->>  input$infectadosInicialesSI
      
      #parametros del modelo (coeficientes de las variables)
      param <- c( beta = input$bethaSI , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                  gamma = input$gammaSI ) # Probabilidad de transmitir la infeccion  input$gammaSI
      
      #crear la funcion con las ODE
      si <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- - gamma*beta*S*I/(S+I)
               dI <- + gamma*beta*S*I/(S+I)
               #resultados de las tasas de cambio    
               return(list(c(dS, dI)))
             })
      }
      
      #intervalo de tiempo y resolucion
      times <- seq(0, tf, by = 0.1)
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM2SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "euler"))
      #simulacionM2SI.si
      #eliminar la variable 'time' en out
      #out$time <- NULL
      #Esta orden nos permite hacer referencia de manera directa a #las columnas de los resultados obtenidos
      attach(simulacionM2SI.si)
      #Calculamos el tamanio de la poblacion
      N <- sum(init)
      updateTextInput(session, "poblacionInicialSI", value =N)
      
      #Representamos graficamente los resultados obtenidos
      plot(times, S, type="l", col="blue", ylim=c(0,sum(init)), xlab="Tiempo (en horas)", ylab="Numero de host", main = "Metodo 2: Euler")
      lines(times, I, type="l", col="red")
      #title("Modelo SI")
      legend(x = "topright", legend=c("Susceptibles", "Infectados"), col=c("blue", "red"), lty=rep(1, 2)) 
      
    })
      output$plotM1PendSI <- renderPlot({
        scopeField <- function(t, p, parameters){
          k <- parameters[1]
          n <- parameters[2]
          dp <- k*(p*(n-p))
          list(dp)
        }
        scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSI),
                                          ylim = c(0,input$suceptiblesInicialesSI), parameters = c(0.0005,1000),
                                          system = "one.dim",
                                          add = FALSE, xlab = "Tiempo (en horas)", ylab = "Numero de host", 
                                          main = "Campo de pendientes")
      })
    output$plotM2PendSI <- renderPlot({ 
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSI),
                                        ylim = c(0,input$suceptiblesInicialesSI), parameters = c(0.0005,1000),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en horas)", ylab = "Numero de host", 
                                        main = "Campo de pendientes")
    })
    
    output$plotM1ErrorSI <- renderPlot({
      
      #tamanio poblacional
      N = input$poblacionInicialSI
      #N = 20
      
      #Tiempo limite
      tf = input$tiempoLimiteSI
      #tf = 70
      
      #estado inicial de los compartimentos
      init <- c(S = input$suceptiblesInicialesSI , # Suceptibles ->>  input$suceptiblesInicialesSI
                I = input$infectadosInicialesSI)  # Infectados ->>  input$infectadosInicialesSI
      
      #parametros del modelo (coeficientes de las variables)
      param <- c( beta = input$bethaSI , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                  gamma = input$gammaSI ) # Probabilidad de transmitir la infeccion  input$gammaSI
      
      #crear la funcion con las ODE
      si <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- - gamma*beta*S*I/(S+I)
               dI <- + gamma*beta*S*I/(S+I)
               #resultados de las tasas de cambio    
               return(list(c(dS, dI)))
             })
      }
      
      #intervalo de tiempo y resolucion
      times <- seq(0, tf, by = 0.1)
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM1SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
      #simulacionM1SI.si
      
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM2SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "euler"))
      #simulacionM2SI.si
      
      # SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      i =1
      erroresM1S <- c()
      for (x in simulacionM1SI.si$S) {
        #En M1, el teorico es M2
        erroresM1S <- c(erroresM1S, ( (abs(simulacionM2SI.si$S[i]-x) )/simulacionM2SI.si$S[i] ) *100)
        i = i +1;
      }
      x <- seq(1,70)
      plot(simulacionM1SI.si$time , erroresM1S, col="blue", type="l", xlab="Tiempo (en horas)", ylab="Error relativo", main = "Error", ylim = c(0,100))
      
    })
    
    output$plotM2ErrorSI <- renderPlot({
      
      #tamanio poblacional
      N = input$poblacionInicialSI
      #N = 20
      
      #Tiempo limite
      tf = input$tiempoLimiteSI
      #tf = 70
      
      #estado inicial de los compartimentos
      init <- c(S = input$suceptiblesInicialesSI , # Suceptibles ->>  input$suceptiblesInicialesSI
                I = input$infectadosInicialesSI)  # Infectados ->>  input$infectadosInicialesSI
      
      #parametros del modelo (coeficientes de las variables)
      param <- c( beta = input$bethaSI , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                  gamma = input$gammaSI ) # Probabilidad de transmitir la infección  input$gammaSI
      
      #crear la funcion con las ODE
      si <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- - gamma*beta*S*I/(S+I)
               dI <- + gamma*beta*S*I/(S+I)
               #resultados de las tasas de cambio    
               return(list(c(dS, dI)))
             })
      }
      
      #intervalo de tiempo y resolucion
      times <- seq(0, tf, by = 0.1)
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM1SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
      #simulacionM1SI.si
      
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM2SI.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "euler"))
      #simulacionM2SI.si
      
      i =1
      erroresM1I <- c()
      for (x in simulacionM1SI.si$I) {
        #En M1, el teorico es M2
        erroresM1I <- c(erroresM1I, ( (abs(simulacionM2SI.si$I[i]-x) )/simulacionM2SI.si$I[i] ) *100 )
        i = i +1;
      }
      
      x <- seq(1,701)
      plot(simulacionM1SI.si$time , erroresM1I, col="red", type="l", xlab="Tiempo (en horas)", ylab="Error relativo", main = "Error", ylim = c(0,100))
      
    })
    
  }
  
  
  CalcularEDOSIR <- function(){
    output$plotM1SIR <- renderPlot({ 
      ## Creacion de la funcion SIR function
      sir <- function(time, state, parameters) {
        
        with(as.list(c(state, parameters)), {
          
          dS <- -beta * S * I
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          
          return(list(c(dS, dI, dR)))
        })
      }
      
      ### Conjunto de parametros
      ## Propocion compartimiento: 
      #Susceptible 0.999999, Infected 0.000001, Recovered 0
      init       <- c(S = (input$suceptiblesInicialesSIR), I = input$infectadosInicialesSIR, R = 0.0)
      ## beta: tasa de infeccion; gamma: tasa de recuperacion
      parameters <- c(beta = input$bethaSIR, gamma = input$gammaSIR)
      ## periodo de tiempo
      times      <- seq(0, input$tiempoLimiteSIR, by = 1)
      
      ## Solucion usando libreria ode (General Solver for Ordinary Differential Equations)
      out <- ode(y = init, times = times, func = sir,
                 parms = parameters, method = "euler")
      ## cambio en data frame
      out <- as.data.frame(out)
      
      N <- sum(init)
      updateTextInput(session, "poblacionInicialSIR", value =N)
      
      plot(out$time,out$S,col="blue", type="l", xlab="Tiempo (en horas)", ylab="Numero de host",ylim =c(0,1.5), main = "Metodo 1: Euler")
      par(new=TRUE)
      plot(out$time,out$I ,col="red", type="l", xlab="Tiempo (en horas)", ylab="Numero de host", ylim = c(0,1.5), main = "Metodo 1: Euler")
      par(new=T)
      plot(out$time,out$R, col="green", type="l", xlab="Tiempo (en horas)", ylab="Numero de host" , ylim = c(0,1.5), main = "Metodo 1: Euler")
      #title("Modelo SIR")
      legend(x = "topright", legend=c("Susceptibles", "Infectados", "Recuperados"), col=c("blue", "red","green"), lty=rep(1, 1))
      
      ## Delete time variable
      out$time <- NULL
      })
  
    
    output$plotM2SIR <- renderPlot({ 
      ## Creacion de la funcion SIR function
      sir <- function(time, state, parameters) {
        
        with(as.list(c(state, parameters)), {
          
          dS <- -beta * S * I
          dI <-  beta * S * I - gamma * I
          dR <-                 gamma * I
          
          return(list(c(dS, dI, dR)))
        })
      }
      
      ### Conjunto de parametros
      ## Propocion compartimiento: 
      #Susceptible 0.999999, Infected 0.000001, Recovered 0
      init       <- c(S = (input$suceptiblesInicialesSIR), I = input$infectadosInicialesSIR, R = 0.0)
      ## beta: tasa de infeccion; gamma: tasa de recuperacion
      parameters <- c(beta = input$bethaSIR, gamma = input$gammaSIR)
      ## periodo de tiempo
      times      <- seq(0, input$tiempoLimiteSIR, by = 1)
      
      ## Solucion usando libreria ode (General Solver for Ordinary Differential Equations)
      out <- ode(y = init, times = times, func = sir,
                 parms = parameters, method = "rk4")
      ## cambio en data frame
      out <- as.data.frame(out)
      
      N <- sum(init)
      updateTextInput(session, "poblacionInicialSIR", value =N)
      
      plot(out$time,out$S,col="blue", type="l", xlab="Tiempo (en horas)", ylab="Numero de host",ylim =c(0,1.5), main = "Metodo 2: Runge Kutta 4")
      par(new=TRUE)
      plot(out$time,out$I ,col="red", type="l", xlab="Tiempo (en horas)", ylab="Numero de host", ylim = c(0,1.5), main = "Metodo 2: Runge Kutta 4")
      par(new=T)
      plot(out$time,out$R, col="green", type="l", xlab="Tiempo (en horas)", ylab="Numero de host" , ylim = c(0,1.5), main = "Metodo 2: Runge Kutta 4")
      #title("Modelo SIR")
      legend(x = "topright", legend=c("Susceptibles", "Infectados", "Recuperados"), col=c("blue", "red","green"), lty=rep(1, 1))
      
      ## Delete time variable
      out$time <- NULL
      })
    
    # --- Campos de pendientes ---
    
    output$plotM1PendSIR <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSIR),
                                        ylim = c(0,input$suceptiblesInicialesSIR), parameters = c(0.3,1),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en horas)", ylab = "Numero de host", 
                                        main = "Campo de pendientes")
    })
    output$plotM2PendSIR <- renderPlot({
      scopeField <- function(t, p, parameters){
        k <- parameters[1]
        n <- parameters[2]
        dp <- k*(p*(n-p))
        list(dp)
      }
      scopeField.flowField <- flowField(scopeField, xlim = c(0,input$tiempoLimiteSIR),
                                        ylim = c(0,input$suceptiblesInicialesSIR), parameters = c(0.1,1),
                                        system = "one.dim",
                                        add = FALSE, xlab = "Tiempo (en horas)", ylab = "Numero de host", 
                                        main = "Campo de pendientes")
    })
    
    output$plotM1ErrorSIR <- renderPlot({ 
      #tamanio poblacional
      N = input$poblacionInicialSIR
      #N = 20
      
      #Tiempo limite
      tf = input$tiempoLimiteSIR
      #tf = 70
      
      #estado inicial de los compartimentos
      init <- c(S = input$suceptiblesInicialesSIR , # Suceptibles ->>  input$suceptiblesInicialesSI
                I = input$infectadosInicialesSIR,
                R = 0.0)  # Infectados ->>  input$infectadosInicialesSI
      
      #parametros del modelo (coeficientes de las variables)
      param <- c( beta = input$bethaSIR , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                  gamma = input$gammaSIR ) # Probabilidad de transmitir la infeccion  input$gammaSI
      #crear la funcion con las ODE
      sir <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- -beta * S * I
               dI <-  beta * S * I - gamma * I
               dR <-                 gamma * I
               
               return(list(c(dS, dI, dR)))
             })
      }
      
      #intervalo de tiempo y resolucion
      times <- seq(0, tf, by = 0.1)
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM1SIR.sir <- as.data.frame(ode(y=init, times=times, func=sir,parms=param,method = "rk4"))
      #simulacionM1SI.si
      
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM2SIR.sir <- as.data.frame(ode(y=init, times=times, func=sir,parms=param,method = "euler"))
      #simulacionM2SI.si
      
      i =1
      erroresM1I <- c()
      for (x in simulacionM1SIR.sir$I) {
        #En M1, el teorico es M2
        erroresM1I <- c(erroresM1I, ( (abs(simulacionM2SIR.sir$I[i]-x) )/simulacionM2SIR.sir$I[i] ) *100 )
        i = i +1;
      }
      
      plot(simulacionM1SIR.sir$time , erroresM1I, col="red", type="l", xlab="Tiempo (en horas)", ylab="Error relativo", main = "Error", ylim = c(0,100))
      
      })
    
    
    output$plotM2ErrorSIR <- renderPlot({ 
      #tamanio poblacional
      N = input$poblacionInicialSIR
      #N = 20
      
      #Tiempo limite
      tf = input$tiempoLimiteSIR
      #tf = 70
      
      #estado inicial de los compartimentos
      init <- c(S = input$suceptiblesInicialesSIR , # Suceptibles ->>  input$suceptiblesInicialesSI
                I = input$infectadosInicialesSIR,
                R = 0.0)  # Infectados ->>  input$infectadosInicialesSI
      
      #parametros del modelo (coeficientes de las variables)
      param <- c( beta = input$bethaSIR , # Tasa de infeccion ( contagiados por unidad de tiempo) input$bethaSI
                  gamma = input$gammaSIR ) # Probabilidad de transmitir la infeccion  input$gammaSI
      #crear la funcion con las ODE
      sir <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- -beta * S * I
               dI <-  beta * S * I - gamma * I
               dR <-                 gamma * I
               
               return(list(c(dS, dI, dR)))
             })
      }
      
      #intervalo de tiempo y resolucion
      times <- seq(0, tf, by = 0.1)
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM1SIR.sir <- as.data.frame(ode(y=init, times=times, func=sir,parms=param,method = "rk4"))
      #simulacionM1SI.si
      
      #Mediante la funcion "ode" resolvemos el sistema de ecuaciones diferenciales y generamos un data frame 
      simulacionM2SIR.sir <- as.data.frame(ode(y=init, times=times, func=sir,parms=param,method = "euler"))
      #simulacionM2SI.si
      
      i =1
      erroresM1S <- c()
      for (x in simulacionM1SIR.sir$S) {
        #En M1, el teorico es M2
        erroresM1S <- c(erroresM1S, ( (abs(simulacionM2SIR.sir$S[i]-x) )/simulacionM2SIR.sir$S[i] ) *100 )
        i = i +1;
      }
      
      plot(simulacionM1SIR.sir$time , erroresM1S, col="blue", type="l", xlab="Tiempo (en horas)", ylab="Error relativo", main = "Error", ylim = c(0,100))
      
      })
  }
  
  
  
  CalcularDFSI <- function(){
    output$plotM1SI <- renderPlot ({
      xs <- seq(0, 10, length.out = 100)
      
      
      #crear la función con las ODE
      si <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- - gamma*beta*S*I/(S+I)
               dI <- + gamma*beta*S*I/(S+I)
               #resultados de las tasas de cambio    
               return(list(c(dS, dI)))
             })
      }
      
      #intervalo de tiempo y resolución
      
      times <- seq(0, 100, by = 0.1)
      init = c(S=999,I=1)
      param= c(gamma=5,beta=0.2)
      simulacion.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
      simulacion.si
      
      #susceptibles
      STk<- function( beta,t) {
        ##1000 es el numero inicial de hosts
        return (1000-2*1*beta*simulacion.si$I[t-1]*simulacion.si$S[t-1]-simulacion.si$I[t-2])
        
      }
      ds<-function(beta,t){
        return(STk(beta,t)-STk(beta,t+1))/2
      }
      ts <- seq(0, 50, by = 1)
      
      resds <- as.data.frame(ds(param[2],ts[ts>2]))
      resds
      plot(ts[ts > 2],resds$`ds(param[2], ts[ts > 2])`,col="blue", type="l", xlab="Tiempo (en horas)", ylab="Numero de host",ylim =c(0,1000), main = "Metodo Diferencias Finitas SI")
      
    })
  }
  
  CalcularDFSIR <- function(){
    output$plotM1SIR <- renderPlot ({
      xs <- seq(0, 10, length.out = 100)
      
      
      #crear la función con las ODE
      si <- function(times, init, param) 
      {
        with(as.list(c(init, param)), 
             {
               #ecuaciones diferenciales   
               dS <- - gamma*beta*S*I/(S+I)
               dI <- + gamma*beta*S*I/(S+I)
               #resultados de las tasas de cambio    
               return(list(c(dS, dI)))
             })
      }
      
      #intervalo de tiempo y resolución
      
      times <- seq(0, 100, by = 0.1)
      init = c(S=999,I=1)
      param= c(gamma=5,beta=0.2)
      simulacion.si <- as.data.frame(ode(y=init, times=times, func=si,parms=param,method = "rk4"))
      simulacion.si
      
      #susceptibles
      STk<- function( beta,t) {
        ##1000 es el numero inicial de hosts
        return (1000-2*1*beta*simulacion.si$I[t-1]*simulacion.si$S[t-1]-simulacion.si$I[t-2])
        
      }
      ds<-function(beta,t){
        return(STk(beta,t)-STk(beta,t+1))/2
      }
      ts <- seq(0, 50, by = 1)
      
      resds <- as.data.frame(ds(param[2],ts[ts>2]))
      resds
      plot(ts[ts > 2],resds$`ds(param[2], ts[ts > 2])`,col="blue", type="l", xlab="Tiempo (en horas)", ylab="Numero de host",ylim =c(0,1000), main = "Metodo Diferencias Finitas SIR")
      
    })
  }
  
  observeEvent(input$botonCalcularSI, {
    if(input$metodoSeleccionadoSI == "DFSI"){
      CalcularDFSI()
      output$plotM2SI <- ({})
      output$plotM2PendSI <- ({})
      output$plotM2ErrorSI <- ({})
      output$plotM1PendSI <- ({})
      output$plotM1ErrorSI <- ({})
    }
    else if(input$metodoSeleccionadoSI == "EDOSI")
    {
      CalcularEDOSI() 
    }
      
    })
  
  observeEvent(input$botonCalcularSIR, {
    if(input$metodoSeleccionadoSIR == "DFSIR"){
      CalcularDFSIR()
      output$plotM2SIR <- ({})
      output$plotM2PendSIR <- ({})
      output$plotM2ErrorSIR <- ({})
      output$plotM1PendSIR <- ({})
      output$plotM1ErrorSIR <- ({})
    }
    else if(input$metodoSeleccionadoSIR == "EDOSIR")
    {
      CalcularEDOSIR()
    }
    
  })
  
}#Fin del codigo del servidor

shinyApp (ui, server)
