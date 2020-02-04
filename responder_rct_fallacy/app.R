#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(shiny) 
library(nlme)
library(VCA)
library(MASS)
require(tidyverse)
require(ggplot2)
library(shinyWidgets)

options(max.print=1000000)
fig.width <- 1200
fig.height <- 550
fig.height2 <- 450
library(shinythemes)        # more funky looking apps
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=100)
#set.seed(12345) #reproducible

pop=1e6
# function to create longitudinal data

is.even <- function(x){ x %% 2 == 0 }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("paper"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                
                setBackgroundColor(
                    color = c("#d7d7ce", "#d3ced7"),  
                    gradient = "radial",
                    direction = c("bottom", "left")
                ),
                
                h3("Responders non responders fallacy - lets power this...."),
                shinyUI(pageWithSidebar(
                    headerPanel(" "),
                    
                    sidebarPanel( 
                        div(p("  xxxxxxxxxxxxxx")),
                        
                        div(
                            
                            actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/biochemistry-and-haematology/master/heam_biochem/app.R', '_blank')"),   
                            actionButton("resample", "Simulate a new sample"),
                            br(), br(),
                            
                            div(strong("Select the parameters using the sliders below"),p(" ")),
                            
                            div(("  
                           xxxxxxxxxxxxxx  ")),
                            br(),
                            # selectInput("Plot",
                            #             strong("1. Select which biochemistry test to present"),
                            #             choices=biochemistry),
                            # 
                            # selectInput("Plot1",
                            #             strong("2. Select plot"),
                            #             choices=c("Overall","Individual","Individual all tests")),
                           
                            sliderInput("power", 
                                        strong("Power"),
                                        min=.50, max=1, step=.01, value=.9, 
                                        ticks=FALSE),
                            
                            sliderInput("alpha", 
                                        strong("alpha"),
                                        min=.01, max=.2, step=.01, value=.05, 
                                        ticks=FALSE),
                            
                            # sliderInput("n", 
                            #             strong("Sample size"),
                            #                           min=50, max=5000, step=5, value=1000, 
                            #             ticks=FALSE),
                                        
                            sliderInput("trt",
                                        strong("treatment effect"),
                                        min=-10, max=10, step=.2, value=2, ticks=FALSE),
                            
                            sliderInput("pop_mu",
                                        strong("population mean"),
                                        min=1, max=10, step=1, value=7, ticks=FALSE),
                            
                            sliderInput("pop_sd",
                                        strong("population sd"),
                                        min=1, max=10, step=1, value=10, ticks=FALSE),
                            
                            sliderInput("noise",
                                        strong("random noise"),
                                        min=0, max=4, step=.2, value=1, ticks=FALSE),
                            
                            
                            sliderInput("eligible",
                                        strong("eligible"),
                                        min=0, max=100, step=1, value=5, ticks=FALSE),
                            
                            
                            # textInput('vec1', 
                            #           strong("3. Select patient. If '2 select plot' 'Individual' is selected, enter sample ID(s) (comma delimited); 
                            #           enter 999 to show all profiles; If 'Individual all tests' is selected, all test results for the first ID only are presented"), "1,2,3,4"),
                            # 
                            # 
                            # sliderInput("V",
                            #             strong("4. Maximum visit number in data simulation including baseline"),
                            #             min=3, max=10, step=1, value=8, ticks=FALSE),
                            # 
                            # sliderInput("VV",
                            #             strong( "5. Estimate treatment effect at this visit"),
                            #             min=1, max=10, step=1, value=4, ticks=FALSE),
                            
                            
                            div(p( strong("References:"))),  
                            
                            
                            
                            tags$a(href = "https://en.wikipedia.org/wiki/Anscombe%27s_quartet", "[1] Anscombe's quartet"),
                            div(p(" ")),
                            tags$a(href = "https://en.wikipedia.org/wiki/Comprehensive_metabolic_panel", "[2] Comprehensive metabolic panel"),
                            div(p(" ")),
                            tags$a(href = "https://ggplot2.tidyverse.org/reference/geom_boxplot.html", "[3] Boxplots using ggplot2"),
                            div(p(" ")),
                            tags$a(href = "https://en.wikipedia.org/wiki/Statistical_process_control", "[4] Statistical process control"),
                            div(p(" ")),
                            tags$a(href = "https://twitter.com/f2harrell/status/1220700181496320001", "[5] Purpose of RCT"),
                            div(p(" ")),
                        )
                        
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(
                        
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        #    tabsetPanel(type = "tabs", 
                        navbarPage(       
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                            tags$style(HTML(" 
                            .navbar-default .navbar-brand {color: cyan;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
     
                   ")), 
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
                            tabPanel("Plotting the data", 
                                     #    h2("Plotting the data"),
                                     div(plotOutput("reg.plot3", width=fig.width, height=fig.height)),  
                                     
                                     h3(" "),
                                     
                                     p(strong("xxxxx")) ,
                                     
                                     #                     
                                     p(strong("Total sample size:")),
                                    # div(class="span7", verbatimTextOutput("samplesize")),
                                     
                                   #  DT::dataTableOutput("table2"),
                                    
                                   # tableOutput("table2")
                                   verbatimTextOutput("summaryx3")
                            ) ,
                            
                            
                            # tabPanel("Model assumption check", value=3, h3("Assess normality of the residuals"),
                            #          p('Look left at the distribution of the residuals and assess normality assumption.'),
                            #          br(), plotOutput("plot3"),  
                            #          h5("The model residual estimate is printed below (on the log transformed scale)..... 
                            #             useful to power a follow up study."),
                            #          # ),
                            #          tableOutput("table")),
                            # 
                            
                            
                            
                            
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Summary statistics", value=3, 
                                     #  div( verbatimTextOutput("table2")),
                                     h4("xxxxxxxxxxxxxxxxxx"),#
                                     h6("xxxxxxxxxxxxxxxxxx."),
                                    # DT::dataTableOutput("table2"),
                                   #  #h6("This is superior to a plain rtf output in that this can be sorted and filtered on the fly."),
                                     # tags$head(tags$style("#dummy table {background-color: red; }", media="screen", type="text/css")),
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Statistical modelling", value=6, 
                                     h4("Modelling"),
                                     p(strong("xxxxxxxxxxxxxxxxxxxxxx.")),
                                    # div(class="span7", verbatimTextOutput("reg.summaryx")),
                                     #div(class="span7", verbatimTextOutput("table4")),
                                     #div(class="span7", verbatimTextOutput("reg.summary2")),
                            ) ,
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Plot of the treatment effect estimates", 
                                     #  h4("Plot of the treatment effect estimates"),
                                     #div(plotOutput("reg.plote", width=fig.width, height=fig.height2)),  
                                     #div(DT::dataTableOutput("reg.summary4"), style = "font-size: 110%; width: 75%")
                                     
                                     
                            ) ,
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("xxxxxxxxxxx",
                                     h4("Fxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                     div(plotOutput("res.plot", width=fig.width, height=fig.height)),       
                                     p(strong("xxxxxxxxxxxxxxxxxxxxxxxxxxxx
                                              ")),
                            ),
                            
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                            tabPanel("Data listing", value=3, 
                                     #  h4("Data listing"),
                                     h6("xxxxxxxxxxxxxxxxxxxxxxxxxxx"),
                                     DT::dataTableOutput("table1"),
                                     
                                     
                            ) 
                            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        )
                        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                    
                )
                )
)

server <- shinyServer(function(input, output   ) {
    
    
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated 
    random.sample <- reactive({
        
        # Dummy line to trigger off button-press
        power <- input$power
        alpha <- input$alpha
        foo <-    input$resample
        trt<-     input$trt
        mu <-     input$pop_mu
        sd <-     input$pop_sd
        n <-      pop
        noise <-  input$noise     
        eligible <- input$eligible  
        
        return(list( n=n ,  trt=trt , mu=mu, sd=sd, noise=noise, eligible=eligible, power=power, alpha=alpha )) 
        
    })

    n <- 10000
    power <- .9
    alpha <- .05
    beta.treatment <-     -2
    pop_mu <-     7
    pop_sd <-     2
 
    noise <-  3   
    ur.eligible <- 1 
    
    
    
    
    
    make.data <- reactive({
        
        sample <- random.sample()
         
        n <-  sample$n
        noise <-  sample$noise        # add noise (within person var & meas. error) to the baseline & foll. up
        beta.treatment <-  sample$trt #  all trt'd subjects exp same trt effect, so no resp - non responders!!
        alpha <- sample$alpha
        power <-   sample$power
        pop_mu <-  sample$mu    # population mean 
        pop_sd <-  sample$sd   # between person SD
        ur.eligible <- sample$eligible #89
        
        
        N <- round(power.t.test( delta = beta.treatment, sd= pop_sd , 
                                 sig.level= alpha, power= power,
                                 type="two.sample", alternative=c("two.sided"))$n*2)
        
        # eligibility criteria for trial
        
        y.0true <- rnorm(n, pop_mu, pop_sd)                  # true baseline
        y.0observed <- y.0true + rnorm(n, 0, 1*noise)        # observed baseline 
        
        eligible <- ifelse(y.0observed > ur.eligible, 1, 0)  # 1sd above norm eligible for trial
        treat <- 1*(runif(n)<.5)                             # random treatment allocation
        y.1true <- y.0true + (treat*beta.treatment)          # true follow up, treated only respond
        y.1observed <- y.1true + rnorm(n, 0, 1*noise)        # observed follow up, noise added 
        delta.observed <- y.1observed - y.0observed
        
        d <- data.frame(y.0true, y.0observed, eligible, treat , beta.treatment,
                        y.1true, y.1observed, delta.observed)
        
        # prob that a member of pop observed baseline is eligible
        # pnorm(ur.eligible, mean= pop_mu, sd=sqrt(pop_sd^2 + noise^2))
        # 1- pnorm( (pop_mu - ur.eligible) / sqrt(pop_sd^2+noise^2) )  # z score calc.
        
        
        
        trial  <- d[d$eligible==1,]    # select the trial subjects
        
        d <- trial <- trial[1:N,]  # selcet out sample size from the population
        
        
        return(list(trial=trial,  d=d, N=N)) 
        
    })
    
    

    
    
  
    
    output$summaryx3 <- renderPrint({
      print(make.data()$N)
    }) 

    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    fit.regression <- reactive({
        

    })     
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # treatment effect estimate
    output$reg.summary4 = DT::renderDataTable({
        
    
    })     
    
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    # Plot the estimated trt effect  
    
    output$reg.plote <- renderPlot({         
        

        
    }) 
    
    # --------------------------------------------------------------------------
    # -----------------------------------------------OVERALL PLOT
    # ---------------------------------------------------------------------------
    
    output$reg.plot3 <- renderPlot({         
        
        trial <- make.data()$trial
        
        N <- make.data()$N
        
         diff <- trial$y.1observed - trial$y.0observed
        mi <-  min( diff)*1.2
        ma <-  max(diff)*1.2
        
        
        par(mfrow=c(1,2))
        
        # mi <-  min(trial$treat)*.5
        # ma <-  max(trial$treat)*1.1
        
        
        trt <- trial[trial$treat==1,]
        trt$diff <- trt$y.1observed - trt$y.0observed
        
        # mi <-  min(trt$diff)*1.1
        # ma <-  max(trt$diff)*1.1
        # mi <-  min(trt[,"diff"])
        # ma <-  max(trt[,"diff"])
        
        foo <- sort(trt[,"diff"])
        
    
        
        plot(foo, main="Individual changes in response in treated arm
           Suggested individual differences due entirely to regression to the mean
           and random error (within subject and measurement error)",
             ylab= "Observed response", xlab="Individual subjects order by observed response", xlim=c(0,1.05*N/2), ylim=c(mi,ma), #length(trt[,"diff"])
             col=ifelse(foo > input$trt, 'red', 'blue') ) #, asp=4)
        abline(h=0, lty=2)
        abline(h=input$trt)
        # this many were not observed to have reduced response by more than 5
        # wrongly labelled as 'non responders'
        mean(foo > input$trt)*length(foo)   # shown in red
        
        
        
        
        trt <- trial[trial$treat==0,]
        trt$diff <- trt$y.1observed - trt$y.0observed
        
        
     #   mi <-  min(trt[,"diff"])
     #   ma <-  max(trt[,"diff"])
        
        foo <- sort(trt[,"diff"])
        plot(foo, main="Individual changes in response in control arm
           Suggested individual differences due entirely to regression to the mean
           and random error (within subject and measurement error)",
             ylab= "Observed response", xlab="Individual subjects order by observed treatment response", xlim=c(0,1.05*N/2),ylim=c(mi,ma), #length(trt[,"diff"])
             col=ifelse(foo > input$trt, 'red', 'blue') )#, asp=4)
        abline(h=0, lty=2)
        abline(h=input$trt)
        # this many were not observed to have reduced response by more than 5
        # wrongly labelled as 'non responders'
        mean(foo > input$trt)*length(foo)   # shown in red
        
        
        
        
        
        par(mfrow=c(1,1))
        
        
        
        
        
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # diagnostics
    
    output$res.plot  <- renderPlot({       
        
      
      sample <- random.sample()
      
        trial <- make.data()$trial
        
        diff <- trial$y.1observed - trial$y.0observed
        mi <-  min( diff)*1.2
        ma <-  max(diff)*1.2
        
        x <- trial$y.0observed
        mix <-  min( x) 
        max <-  max(x) 
        
        
        trt <- trial[trial$treat==1,]
        trt$diff <- trt$y.1observed - trt$y.0observed
        
        
        par(mfrow=c(1,2))
        with(trt, plot(diff ~  y.0observed, col=ifelse(diff < sample$trt, 'blue', 'black'), pch=16
                       , xlab="observed baseline",  ylab="follow up - baseline"  ,
                       main="Treatment arm: Individual changes against baseline, observed responders in blue", cex.main =1,
                       ylim=c(mi,ma), xlim=c(mix,max) ))
        with(trt, abline(lm(diff ~  y.0observed)))
        with(trt, abline(h=mean(beta.treatment), lty=2))
        with(trt, abline(h=0, col="red", lty="dashed"))
        
        
        
        
        
        ctr <- trial[trial$treat==0,]
        ctr$diff <- ctr$y.1observed - ctr$y.0observed
        
        with(trt, cor.test( diff,   y.0observed, method="pearson"))
        
        
        
        with(ctr, plot(diff ~  y.0observed, col=ifelse(diff <  sample$trt, 'blue', 'black'), pch=16
                       , xlab="observed baseline",  ylab="follow up - baseline"  ,
                       main="Control arm:  Individual changes against baseline, observed responders in blue", cex.main =1,
                       ylim=c(mi,ma), xlim=c(mix,max) ))
        with(ctr, abline(lm(diff ~  y.0observed)))
        with(ctr, abline(h=mean(beta.treatment), lty=2))
        with(trt, abline(h=0, col="red", lty="dashed"))
        
        
        with(ctr, cor.test( diff,   y.0observed, method="pearson"))
        par(mfrow=c(1,1))
      
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # listing of simulated data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$table1 <- DT::renderDataTable({
        
        foo<- make.data()$d
        # 
        # target <- input$Plot
        # 
        # foo <- foo[foo$test %in% target,]
        # 
        # foo$eij <- NULL 
       # c("y.0true", "y.0observed", "eligible", "treat", "beta.treatment", 
        #  "y.1true", "y.1observed", "delta.observed")
        # 
        namez <- c("true baseline","observed baseline","eligible","treatment group","true treatment effect\n in treated only","
                  true response","observed response","delta observed")
        names(foo) <- namez
         rownames(foo) <- NULL
        library(DT)
        # 
        datatable(foo,   
        #           
                   rownames = TRUE,
        #           
                   options = list(
                       searching = TRUE,
                       pageLength = 15,
                       paging=TRUE,
                       lengthMenu = FALSE ,
                       lengthChange = FALSE,
                       autoWidth = FALSE,
                    #  colReorder = TRUE,
                     #deferRender = TRUE,
                       # scrollY = 200,
                      scroller = T
                   ))  %>%
          
             formatRound(
                 columns= namez,   
                            digits=c(2,2,0,0,1,2,2,2)  )
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # summary stats
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    output$table2 = DT::renderDataTable({
        
        # foo<- make.data()$d1
        # 
        # target <- input$Plot
        # 
        # foo <- foo[foo$test %in% target,]
        # 
        # f<-plyr::ddply(foo, c("test", "memorypar","tailindex"), summarise,
        #                min=min(hillest),mean = mean(hillest), sd = sd(hillest, na.rm=TRUE),
        #                sem = sd(hillest)/sqrt(length(hillest)),  Q1=quantile(hillest, 0.25)    , 
        #                median=median(hillest),   Q3=quantile(hillest, 0.75)  , max=max(hillest)  )
        # 
        # names(f) <- c("Biochemistry test",  "Visit", "Treatment","Minimum", "Mean" , "SD", "SE", "Q1","Median","Q3", "Maximum")
        # 
        # rownames(f) <- NULL
        # 
        # 
        # library(DT)
        # datatable(f,   
        #           rownames = TRUE,
        #           options = list(
        #               searching = TRUE,
        #               pageLength = input$V-1,
        #               paging=FALSE,
        #               lengthMenu = FALSE ,
        #               lengthChange = FALSE,
        #               autoWidth = FALSE
        #               # colReorder = TRUE,
        #               # deferRender = TRUE,
        #               # scrollY = 200,
        #               # scroller = T
        #           ))  %>%
        #     formatRound(
        #         columns= c("Minimum", "Mean" , "SD", "SE", "Q1","Median","Q3", "Maximum"), digits=c(2)  )
        
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # model output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$reg.summary2 <- renderPrint({
        
        # summary <- fit.regression()$fit.res
        # 
        # return(list(summary))
        
    })  
    
    output$reg.summaryx <- renderPrint({
        
        # summary <- input$Plot
        # 
        # return(list(summary))
        
    })  
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
})

# Run the application 
shinyApp(ui = ui, server = server)