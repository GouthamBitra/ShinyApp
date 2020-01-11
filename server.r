library(shiny)
data(mtcars)
shinyServer(
  
  function(input, output) {
    
    #To print the summary of selected dataset
    output$sum <- renderPrint({
      if (input$dataset =='iris') { dispdata <- data.frame(iris) } 
      if(input$dataset=='mtcars'){dispdata <- data.frame(mtcars)}
      if (input$dataset =='USArrests') { dispdata <- data.frame(USArrests) }
      summary(dispdata)
      
    })
    #To print the structure of the dataset
    output$str <- renderPrint({
      if (input$dataset =='iris') { dispdata <- data.frame(iris) } 
      if(input$dataset=='mtcars'){dispdata <- data.frame(mtcars)}
      if (input$dataset =='USArrests') { dispdata <- data.frame(USArrests) }
      str(dispdata)
      
    })
    #To display the data of dataset
    output$data <- renderTable({
      if (input$dataset =='iris') {dispdata <- data.frame(iris)} 
      if(input$dataset=='mtcars'){dispdata <- data.frame(mtcars)}
      if (input$dataset =='USArrests') { dispdata <- data.frame(USArrests) }
      print(dispdata)
      
    })
    #To display the histogram
    output$myhist <- renderPlot({
      if (input$dataset =='iris') { 
        colm <- as.numeric(input$column1) #Retrieving the assigned numbers in UI to use them to plot histogram
        hist(iris[,colm], breaks=seq(0, max(iris[,colm]), l=input$bins+1), col=input$color, main="Histogram of iris dataset", xlab=names(iris[colm]), xlim=c(0,max(iris[,colm])))}
      if (input$dataset =='USArrests') {
        colm <- as.numeric(input$column2)
        hist(USArrests[,colm], breaks=seq(0, max(USArrests[,colm]), l=input$bins+1), col=input$color, main="Histogram of USArrests dataset", xlab=names(USArrests[colm]), xlim=c(0,max(USArrests[,colm])))}
      if (input$dataset =='mtcars') {
        colm <- as.numeric(input$column3)
        hist(mtcars[,colm], breaks=seq(0, max(mtcars[,colm]), l=input$bins+1), col=input$color, main="Histogram of mtcars dataset", xlab=names(mtcars[colm]), xlim=c(0,max(mtcars[,colm])))}
      
    })
    #To display the boxplot
    output$mybox <- renderPlot({
      if (input$dataset =='iris') { 
        colm <- as.numeric(input$column1) #Retrieving the assigned numbers in UI to use them to plot Box plot
        boxplot(iris[,colm], breaks=seq(0, max(iris[,colm]), l=input$bins+1), col=input$color, main="Box Plot of iris dataset", xlab=names(iris[colm]), xlim=c(0,max(iris[,colm])))}
      if (input$dataset =='USArrests') {
        colm <- as.numeric(input$column2)
        boxplot(USArrests[,colm], breaks=seq(0, max(USArrests[,colm]), l=input$bins+1), col=input$color, main="Box Plot of USArrests dataset", xlab=names(USArrests[colm]), xlim=c(0,max(USArrests[,colm])))}
      if (input$dataset =='mtcars') {
        colm <- as.numeric(input$column3)
        boxplot(mtcars[,colm], breaks=seq(0, max(mtcars[,colm]), l=input$bins+1), col=input$color, main="Box Plot of mtcars dataset", xlab=names(mtcars[colm]), xlim=c(0,max(mtcars[,colm])))}
      
    })

        
    
        dist<- reactive({
          return(input$dist)
        })
        sig<- reactive({
          return(input$sig)
        })
        tails<- reactive({
          return(input$tails)
        })
        tailcol<- reactive({
          return(input$tail)
        })
        fencecol<- reactive({
          return(input$fence)
        })
        freedom<- reactive({
          return(input$freedom)
        })
        
        dplot<- function(){
          x<- seq(-5,5,0.01)
          if(tails()=="two"){
            alpha<- sig()/2
            tails<- 2
          } else {
            alpha<- sig()
            if(tails()=="left"){
              tails<- -1
            } else {
              tails<- 1
            }
          }
          
          
          if(input$dist=="T"){
            pdf<- dt(x, input$freedom)
            critical<- qt(alpha, input$freedom)
            plottitle<- paste("Rejection region based on",dist(), " distribution and alpha=", formatC(alpha,3), sep = "")
          } else if (input$dist=="Z") {
            pdf<- dnorm(x)
            critical<- qnorm(alpha)
            plottitle<- paste("Rejection region based on",dist(), " distribution and alpha=", formatC(alpha,3), sep = "")
          } else {
            pdf<- dnorm(x)
            critical<- qnorm(aplha)
            plottitle<- paste("Rejection region based on",dist(), " distribution and alpha=", formatC(alpha,3), sep = "")
          }
          
          dd<- data.frame(x,pdf)
          library(ggplot2)
          finalplot<- ggplot(dd, aes(x,pdf))
          finalplot<- finalplot + geom_line() + ggtitle(plottitle)
          
          if(tails==-1||tails==2){
            finalplot<- finalplot + geom_ribbon(data = subset(dd, x<= critical),aes(ymax=pdf),ymin=0,fill=tailcol(), alpha=0.5) + 
              geom_vline(xintercept = critical, colour= fencecol(), linetype="dashed", size=1 )
            plotlabel<- paste("Reject H0 if","\n", "test stat < ",formatC(critical,4),".")
            finalplot<- finalplot + annotate("text", x= -3.75, y=0.10, label= plotlabel)
          }
          
          if(tails==1||tails==2){
            finalplot<- finalplot + geom_ribbon(data = subset(dd, x >= -critical),aes(ymax=pdf),ymin=0,fill=tailcol(), alpha=0.5) + 
              geom_vline(xintercept = -critical, colour= fencecol(), linetype="dashed", size=1 )
            plotlabel<- paste("Reject H0 if","\n", "test stat < ",formatC(-critical,4),".")
            finalplot<- finalplot + annotate("text", x= 3.75, y=0.10, label= plotlabel)
          }
          
          return(finalplot)
        }
        output$plot<- renderPlot({
          p<- dplot()
          print(p)
        })
        output$debugger<- renderText({
          paste("Dist:",dist(),"Sig Level:",sig(),"Num Tails:",tails())
        })
        
        output$download<- downloadHandler(
          filename = function(){paste(input$dist,'_',input$tails,'_',sig(),'.png',sep = '')},
          content = function(file){
            ggsave(file, plot=doPlot(), device=png, width=800, height=800, limitsize=FALSE)
          }
        )  
      
    
        
        data <- reactive({ 
          req(input$file1) ## ?req #  require that the input is available
          
          inFile <- input$file1
        })
        output$contents <- renderTable({
          
          # input$file1 will be NULL initially. After the user selects
          # and uploads a file, head of that data file by default,
          # or all rows if selected, will be shown.
          req(input$file1)
          
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          
          if(input$disp == "head") {
            return(head(df))
          }
          else {
            return(df)
          }
          
        })
        
        output$summary <- renderPrint({
          req(input$file1)
          
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          print(summary(df))
          
        })
        
        output$corrplot<- renderPlot({
          req(input$file1)
          
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          library(PerformanceAnalytics)
          chart.Correlation(df, histogram=TRUE, pch=19)
        })
        
        output$Hist1<- renderPlot({
          req(input$file1)
          
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          hist(df$YearsExperience,
               main="Histogram for Years of Experience",
               xlab = "YearsExperience",
               border = "blue")
        })
        
        output$Hist2<- renderPlot({
          req(input$file1)
          
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          hist(df$Salary,
               main = "Histogram for Salary",
               xlab = "Salary",
               border = "blue")
        })
        
        
        
        
        output$modelSummary <- renderPrint({
          req(input$file1)
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          model <- load(file = "SimpleLinearRegression.rda")
          YE <- data.frame(YearsExperience = df$YearsExperience)
          #y_pred = predict(get(model), newdata = YE)
          #print(y_pred)
          dat2 <- data.frame(YE, Predicted = predict(get(model), YE))
          print(merge(df, dat2, by = 'YearsExperience', all.y = TRUE))
        })
        
        output$Viz<- renderPlot({
          req(input$file1)
          df <- read.csv(input$file1$datapath,
                         header = input$header,
                         sep = input$sep,
                         quote = input$quote)
          library(ggplot2)
          ggplot() +
            geom_point(aes(x = df$YearsExperience, y = df$Salary),
                       colour = 'red') +
            geom_line(aes(x = df$YearsExperience, y = predict(regressor, newdata = df)),
                      colour = 'blue') +
            ggtitle('Salary vs Experience') +
            xlab('Years of experience') +
            ylab('Salary')
        })
        
        output$histogram <- renderPlot({ 
          
          if (input$conmodel == 'normal') { 
            
            par(mfrow=c(1,2))  
            
            x=seq(-input$i,input$i,0.01)  
            
            plot(x,dnorm(x,input$mu,input$sigma),type='l', col='red') 
          } 
          
          if (input$conmodel == 'exponential') { 
            
            # exponential  
            
            par(mfrow=c(1,2)) 
            
            x=seq(0,input$i,0.01)  
            
            plot(x,dexp(x,input$lam),type='l',col='green') 
            
          } 
          if (input$conmodel == 'uniform') { 
            
            a <- input$a 
            
            b <- input$b 
            
            n1 <- input$s 
            rand.unif <- runif(n1, min = a, max = b) 
            hist(rand.unif,  
                 
                 freq = FALSE,  
                 
                 xlab = 'x',   
                 
                 ylim = c(0, 0.4), 
                 
                 xlim = c(-3,3), 
                 
                 density = 20, 
                 
                 main = "Uniform distribution") 
            
            
            curve(dunif(x, min = a, max = b),  
                  
                  from = -3, to = 3,  
                  
                  n = n1,  
                  
                  col = "darkblue",  
                  
                  lwd = 2,  
                  
                  add = TRUE,  
                  
                  yaxt = "n", 
                  
                  ylab = 'probability') 
            
          } 
          
        })    
        
        output$tab <- renderTable({  
          Normal=rnorm(input$s,input$mu, input$sigma) 
          Exp=rexp(input$s,input$lam)  
          if (input$conmodel == 'exponential') { 
            d2=data.frame(Exp)  
          }
          else 
          { 
            d1=data.frame(Normal)  
          } 
        })  
        output$prob <- renderPrint({  
          p1=pnorm(input$j1,input$mu, input$sigma)  
          p2=pexp(input$j2,input$lam) 
          if (input$conmodel == 'exponential') { 
            c(p2)
          } 
          if (input$conmodel == 'normal') { 
            c(p1)  
          }
        })
        
        
        })


