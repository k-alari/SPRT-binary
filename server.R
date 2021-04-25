#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This applet was designed by Krissi Alari as apart of her senior capstone at
# Cal State Monterey Bay. This applet was designed specifically for E. coli
# testing in lettuce for a particular food testing company. The re-paramterization
# option of risk levels is only applicable for their testing purposes. The rest of
# the applet (using "Average (μ)" as the parameter of interest) is suitable for 
# general use of the SPRT.
#
# This version of the applet is designed for binary testing (Bernoulli outcomes)
# where X = 1 indicates a defective observation and X = 0 otherwise
#
# The sprt_graph_horizontal() function contains the buildPoly() function by
# Joran Elias, @joran https://stackoverflow.com/a/6809174/1870254
# and modified by @jan-glx https://stackoverflow.com/users/1870254/jan-glx 
# to shade rejection and acceptance areas in the SPRT graph.

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# Define server logic 
shinyServer(function(input, output) {
  
  output$barplot<-renderPlot({
    
    prop =c(input$p0, input$p1)
    type =c("Null Value (p0)", "Alternative Value (p1)")
    type = ordered(type, levels = c("Null Value (p0)", "Alternative Value (p1)"))
    temp.data = data.frame(type, prop)
    
    ggplot(temp.data, aes(x=type, y=prop)) + ylab("Proportion") + xlab("") +
      geom_bar(stat = "identity", fill = "lightgreen") + theme_bw() + 
      geom_text(aes(label = paste0(prop)), position = position_stack(vjust = 0.5)) + 
      ggtitle("Null and Alternative Values") + 
      theme(plot.title = element_text(size=20), text = element_text(size=15))
    })
  
    output$sprtPlot<-renderPlot({
      
      ## Convert inputted data into a new variable to use in the server file
      data = input$data.receive
      data = as.character(input$data.receive)  #Make sure it is a character
      temp = gsub( " ", "", data )  #Remove spaces
      data = as.numeric( unlist( strsplit( data, split="," ) ) )  #Split by comma and list as numeric vector
      
      sprt_graph <- function(data, alpha, beta, p0, p1){ 
        
        ## Function to shade areas in a ggplot by creating a polygon
        buildPoly <- function(slope, intercept, above, xr, yr){
          # By Joran Elias, @joran https://stackoverflow.com/a/6809174/1870254
          # and modified by @jan-glx https://stackoverflow.com/users/1870254/jan-glx 
          # Find where the line crosses the plot edges
          yCross <- (yr - intercept) / slope
          xCross <- (slope * xr) + intercept
          
          #Build polygon by cases
          if (above & (slope >= 0)){
            rs <- data.frame(x=-Inf,y=Inf)
            if (xCross[1] < yr[1]){
              rs <- rbind(rs,c(-Inf,-Inf),c(yCross[1],-Inf))
            }
            else{
              rs <- rbind(rs,c(-Inf,xCross[1]))
            }
            if (xCross[2] < yr[2]){
              rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,Inf))
            }
            else{
              rs <- rbind(rs,c(yCross[2],Inf))
            }
          }
          if (!above & (slope >= 0)){
            rs <- data.frame(x= Inf,y= -Inf)
            if (xCross[1] > yr[1]){
              rs <- rbind(rs,c(-Inf,-Inf),c(-Inf,xCross[1]))
            }
            else{
              rs <- rbind(rs,c(yCross[1],-Inf))
            }
            if (xCross[2] > yr[2]){
              rs <- rbind(rs,c(yCross[2],Inf),c(Inf,Inf))
            }
            else{
              rs <- rbind(rs,c(Inf,xCross[2]))
            }
          }
          if (above & (slope < 0)){
            rs <- data.frame(x=Inf,y=Inf)
            if (xCross[1] < yr[2]){
              rs <- rbind(rs,c(-Inf,Inf),c(-Inf,xCross[1]))
            }
            else{
              rs <- rbind(rs,c(yCross[2],Inf))
            }
            if (xCross[2] < yr[1]){
              rs <- rbind(rs,c(yCross[1],-Inf),c(Inf,-Inf))
            }
            else{
              rs <- rbind(rs,c(Inf,xCross[2]))
            }
          }
          if (!above & (slope < 0)){
            rs <- data.frame(x= -Inf,y= -Inf)
            if (xCross[1] > yr[2]){
              rs <- rbind(rs,c(-Inf,Inf),c(yCross[2],Inf))
            }
            else{
              rs <- rbind(rs,c(-Inf,xCross[1]))
            }
            if (xCross[2] > yr[1]){
              rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,-Inf))
            }
            else{
              rs <- rbind(rs,c(yCross[1],-Inf))
            }
          }
          return(rs)
        }
        GeomSection <- ggproto("GeomSection", GeomPolygon, 
                               default_aes = list(fill="blue", size=0, alpha=0.2, colour=NA, linetype="dashed"), 
                               required_aes = c("slope", "intercept", "above"),
                               draw_panel = function(data, panel_params, coord) {
                                 ranges <- coord$backtransform_range(panel_params)
                                 data$group <- seq_len(nrow(data))
                                 data <- data %>% group_by_all %>% do(buildPoly(.$slope, .$intercept, .$above, ranges$x, ranges$y)) %>% unnest
                                 GeomPolygon$draw_panel(data, panel_params, coord)
                               }
        )
        
        geom_section <- function (mapping = NULL, data = NULL, ..., slope, intercept, above, 
                                  na.rm = FALSE, show.legend = NA) {
          if (missing(mapping) && missing(slope) && missing(intercept) && missing(above)) {
            slope <- 1
            intercept <- 0
            above <- TRUE
          }
          if (!missing(slope) || !missing(intercept)|| !missing(above)) {
            if (missing(slope)) 
              slope <- 1
            if (missing(intercept)) 
              intercept <- 0
            if (missing(above)) 
              above <- TRUE
            data <- data.frame(intercept = intercept, slope = slope, above=above)
            mapping <- aes(intercept = intercept, slope = slope, above=above)
            show.legend <- FALSE
          }
          layer(data = data, mapping = mapping, stat = StatIdentity, 
                geom = GeomSection, position = PositionIdentity, show.legend = show.legend, 
                inherit.aes = FALSE, params = list(na.rm = na.rm, ...))
        }
        
        ## Note: Data needs to be imported as a single vector
        
        n = length(data)    ## Get sample size
        obs.num = 1:n       ## Store observation numbers (from 1 to n)
        csum = rep(NA, n)   ## Storage for cummulative sum
        ## Get cumulative sums of observations.
        for (i in 1:n){   
          csum[i] = sum(data[1:i]) 
        }
        
        ## Calculate Wald's 'acceptance number' (page 173 in his text)
        ## If data points fall above this line, we conclude H0
        Am = log(beta/(1-alpha)) / (log(p1/p0) - log((1-p1)/(1-p0))) + obs.num*log((1-p0)/(1-p1)) / (log(p1/p0) - log((1-p1)/(1-p0)))
        
        ## Calculate Wald's 'rejection number' (page 173 in his text)
        ## If data points fall below this line, we conclude H1
        Rm = log((1-beta)/alpha) / (log(p1/p0) - log((1-p1)/(1-p0))) + obs.num*log((1-p0)/(1-p1)) / (log(p1/p0) - log((1-p1)/(1-p0)))
        
        ## Store intercepts of acceptance and rejection lines separately (to use in buildPoly and geom_section for shading)
        intercept1 =  log(beta/(1-alpha)) / (log(p1/p0) - log((1-p1)/(1-p0)))
        intercept2 = log((1-beta)/alpha) / (log(p1/p0) - log((1-p1)/(1-p0)))
        slope1 = log((1-p0)/(1-p1)) / (log(p1/p0) - log((1-p1)/(1-p0)))
        slope2 = log((1-p0)/(1-p1)) / (log(p1/p0) - log((1-p1)/(1-p0)))
        
        ## Convert data into a data frame structure for easier use in ggplot
        obs.data = data.frame("obs_num" = obs.num, "APC" = data, "c_sum" = csum, "accept"=Am, "reject"=Rm)
        
        ggplot(data=obs.data, aes(x=obs.num, y=csum)) +  geom_point(size=3, col="navy") + 
          geom_line(aes(x=obs.num, y=Am), size=1.5, color="deeppink", alpha=0.5) + 
          geom_line(aes(x=obs.num, y=Rm), color="darkcyan", size=1.5, alpha=0.5) +
          labs(x="Observation Number", y="Cumulative Sum of Observations") + theme_bw() +
          geom_section(data=data.frame(slope=c(slope1,slope2), above=c(TRUE, FALSE), selected=c("selected","selected 2")), 
                       aes(slope=slope, above=above, intercept=c(intercept1, intercept2), fill=selected), size=1) +
          ggtitle("SPRT") + scale_x_continuous(n.breaks = 3) + theme(plot.title = element_text(size=20), text = element_text(size=15)) + 
          scale_fill_discrete(name = "SPRT Conclusion", labels = c("Conclude H0", "Conclude H1"))
      }
      
      sprt_graph(data=data, alpha=input$alpha, beta=input$beta, p0=input$p0, p1=input$p1)
    })

  
    output$sprtRslt <- renderText({

        ## Convert inputted data into a new variable to use in the server file
        data = input$data.receive
        data = as.character(input$data.receive)  #Make sure it is a character
        temp = gsub( " ", "", data )  #Remove spaces
        data = as.numeric( unlist( strsplit( data, split="," ) ) )  #Split by comma and list as numeric vector
    
        ## Take in other inputted values
        alpha = input$alpha
        beta = input$beta
        p0 = input$p0
        p1 = input$p1
        
        ## Function to run the SPRT
        sprt.func.binary = function(alpha, beta, p0, p1, data){
            
            ## Calculate boundaries
            lower = beta / (1 - alpha)
            upper = (1 - beta) / alpha
            
            ## Get the sample size
            n = length(data)
            
            ## Get sum of observations
            s = sum(data)
            
            ## Likelihood under the alternative assumption for X ~ Bern(p1)
            alt.like = (p1 ^ s)*(1-p1)^(n-s) 
            
            ## Likelihood under the null assumption for X ~ Bern(p0)
            null.like = (p0 ^ s)*(1-p0)^(n-s) 
            
            ## Test statistic 
            test.stat = alt.like / null.like
            
            ## Determine the conclusion
            if( test.stat >= upper ){
                text.concl="Given your data, the SPRT has concluded H1."
            } else if (test.stat <= lower ){
                text.concl="Given your data, the SPRT has concluded H0."
            } else {
                text.concl="Given your data, the SPRT is not able to make a conclusion. Make another observation and run the applet again."
            }
        } 
        
        conclusion = sprt.func.binary(alpha=alpha, beta=beta, p0=p0, p1=p1, data=data)

    })
    
    ## Heading text format
    output$headingtext <- renderText({
      paste("<B>SPRT Conclusion:</B>")
    })

})
