###################################################################
########## Data Analysis CO2 Constant Rate #############################
###################################################################

         #   rm(list=ls(all=TRUE))

      set.seed(19)

       library(compiler)
     enableJIT(3)

       library(readODS)
     
       library(gdata)
       library(readxl)
     
###################################################################
##### Leitura dos dados ###########################################
###################################################################

  #   D <- read_ods("/Users/erlandison/Documents/Erlandson/ZConsultoria/2020/03_PauloMuller/DadosPaulo.ods", sheet=7) 
     
     D <- as.matrix(read_excel("C:\\file path.xlsx", sheet=1))
     
#############
     
               
            x <- as.numeric(D[,1])
            y <- as.numeric(D[,2])
            n <- length(y)
           id <- seq(1, n)

          plot(x, y, pch=19)

#######################          
##### Modelo Linear
#######################          

      mod.lin <- lm(y ~ x)
     coef.lin <- as.numeric(coef(mod.lin))
     pred.lin <- predict(mod.lin)
      EQM.lin <- mean((y - pred.lin)^2) 
      EQM.lin
      
#######################          
##### Modelo por partes 
#######################        

##### Funções
      
         y.pq <- function(xa, cf2)
              {
              cf2[1] + cf2[2]*xa + cf2[3]*(xa^2) 
              }
      
         y.pl <- function(xb, cf1)
              {
              cf1[1] + cf1[2]*xb
              }
      
##### Quantís
      
           co <- 5
        
           n4 <- 0      
           qi <- 0.10           
 while(n4 < co){
           qi <- qi + 0.01
           xi <- quantile(x, qi)
           n4 <- length(x[x<xi])
              }

           n4 <- 0      
           qf <- 0.90           
 while(n4 < co){
           qf <- qf - 0.01 
           xf <- quantile(x, qf)
           n4 <- length(x[x>=xf])
              }
          
           ep <- 0.001

           Gr <- seq(xi,xf,ep)
           nr <- length(Gr)

          dif <- numeric()
for(i in 1:nr){
           
          ir1 <- id[x<Gr[i]]
          ir2 <- id[x>=Gr[i]]
          
           x1 <- x[ir1]
           x2 <- x[ir2]
           y1 <- y[ir1]
           y2 <- y[ir2]
    
       mod.pq <- lm(y1 ~ x1 + I(x1^2))
      coef.pq <- as.numeric(coef(mod.pq))
         
       mod.pl <- lm(y2 ~ x2)
      coef.pl <- as.numeric(coef(mod.pl))
                                    
       dif[i] <- abs(y.pq(Gr[i], coef.pq) - y.pl(Gr[i], coef.pl))
           
            if(dif[i]==min(dif[1:i]))
              {
           xe <- Gr[i]  
   coef.pquad <- coef.pq 
    coef.plin <- coef.pl  
           xq <- x1
           xl <- x2
           yq <- y1
           yl <- y2  
              }
              }

       EQM.MP <- mean(c(((yq - y.pq(xq, coef.pquad))^2), ((yl - y.pl(xl,coef.plin))^2)))
        
###############        
##### Gráfico
###############        
        
           Res <- round(coef.plin, 6)
            di <- abs(EQM.lin - EQM.MP)
          
             if(di < 0.0001)
               {
           Res <- round(coef.lin, 6) 
      coef.est <- coef.lin   
         points(x, y.pl(x, coef.est), lwd=2, type="l", col="blue")
               }
            
             if(di > 0.0001)
               {
             if(EQM.MP < EQM.lin)
               {
         points(sort(c(xq)), y.pq(sort(c(xq)), coef.pquad), lwd=2, type="l", col="blue")
         points(sort(c(xl)), y.pl(sort(c(xl)), coef.plin), lwd=2, type="l", col="green")
            xa <- c(max(xq),min(xl))    
            ya <- c(y.pq(xa[1], coef.pquad), y.pl(xa[2], coef.plin))    
            m <- (ya[1]-ya[2])/(xa[1]-xa[2])
            b <- -m*xa[1] + ya[1]
       coef.mg <- c(b, m)
            xb <- seq(xa[1], xa[2], 0.0001)
         points(xb, y.pl(xb,coef.mg), lwd=2, type="l", col="red")    
               }
       
             if(EQM.lin < EQM.MP)
               {
           Res <- round(coef.lin, 6) 
      coef.est <- coef.lin   
         points(x, y.pl(x, coef.est), lwd=2, type="l", col="blue")
               }
               }
            
            
###############       
##### Results
###############
       
           Res

       EQM.lin
       EQM.MP       
              
abs(EQM.lin - EQM.MP)

                                           