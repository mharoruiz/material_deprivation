library(shiny)
library(bslib)
library(cluster)
library(tidyverse)
library(reshape2)
library(plotly)
library(viridis)
library(prodlim)
library(ltm)

## Import and select data
d_raw <- read.csv("SOEP_depriv_17 copy.csv", sep = ";")
d_raw <- d_raw %>%
  dplyr::select ("meat", "heating","clothes", "friends",  
                 "car",  "furniture", "vacation", "emergency")

## MONA Function 
cluster.analysis = function(cluster.obj, k){
  indices = c()
  results.vector = c()
  
  for(i in 1:k){
    indices = append(indices, which(cluster.obj$step == i))
  }
  sorted.indices = sort(indices)
  
  ## Add the total number of households to the vector
  sorted.indices = append(sorted.indices, length(cluster.obj$order))
  
  n = 1
  clust.size = NULL
  n_new = NULL
  for (i in 1:k){
    n_new =  n - length(which(cluster.obj$step == i)) + length(which(cluster.obj$step == i))*2
    n = n_new
  }
  
  ## Start with 1
  low = 1
  ## Make a vector of length n, in which each cluster is specified.
  for(j in 1:n){
    high = sorted.indices[j]
    cluster.size = length(cluster.obj$order[low:high])
    results.vector = append(results.vector, rep(j, cluster.size))
    
    low = high +1 
  }
  df = data.frame(order = cluster.obj$order, results.vector = results.vector)
  df = df[order(df$order),]
  return(df$results.vector)
}


##  *****  UI  *****

ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "united", 
                                        primary ="#0a7e8c"),
                title = "Appendix: MONA Clustering",
                fluidRow(
                column(width = 10, offset = 1,
                       tags$h1('Appendix: MONA Clustering'),
                       tags$p('Miguel Haro Ruiz',
                              style = 'text-align:justify;
                              margin-bottom: 40px;
                              margin-top: 25px;
                              line-height: 27px;
                              font-size: 20px;'),
                       #tags$br(),
                       column(width = 10, offset = 1,
                              tags$p('This is an online appendix to the paper',
                                     tags$em('“Understanding Deprivation at the Household Level:
                                  An IRT-MONA joint approach.”'), 
                                     'The app allows you to explore the mechanics 
                                     of the MONA clustering algorithm in an interactive
                                     manner. To use it, follow the steps bellow:',
                                     style = 'text-align:justify;
                                  margin-bottom: 20px;
                                  line-height: 27px;
                                  font-size: 18px;'
                              ),
                              #tags$br(),
                              tags$ol(
                                tags$li('Select the combination of variables that 
                                 you are interested in exploring. The items 
                                 analysed in the study are preselected.',
                                        style = 'text-align:justify;
                                 margin-bottom: 15px;
                                 line-height: 27px;
                                 font-size: 18px;'),
                                
                                tags$li('The MONA algorithm yields different clusters 
                                 at each partition step. For example, at the 
                                 first step, the sample is divided into two 
                                 clusters, which are divided into four clusters 
                                 in the second step, and so on. You can adjust 
                                 the number of partition steps using the first slider.',
                                        style = 'text-align:justify;
                                 margin-bottom: 15px;
                                 line-height: 27px;
                                 font-size: 18px;'),
                                
                                tags$li('As the number of partition steps increases, 
                                 so does the presence of small clusters. 
                                 This may distort the results by including 
                                 clusters of households that are nor representative 
                                 to the sample as a whole. In order to address 
                                 this issue, the number of clusters shown in the 
                                 visualisations can be adjusted using the second 
                                 slider. This removes the smallest clusters one 
                                 at a time. Additionally, the text under the slider 
                                 shows the percentage of the sample that is plotted 
                                 after smaller clusters have been removed.',
                                        style = 'text-align:justify;
                                 margin-bottom: 15px;
                                 line-height: 27px;
                                 font-size: 18px;'),
                                
                                tags$li('Once the three parameters have been adjusted, 
                                 the results will be plotted. For the first and 
                                 second partition steps, a line plot conveniently 
                                 shows the cluster means by item. However, as the 
                                 number of partition steps increases, the line plot 
                                 becomes harder to interpret. In this case, the 
                                 tab ‘Heatmap’ displays the same information in 
                                 heatmap format.',
                                        style = 'text-align:justify;
                                 margin-bottom: 15px;
                                 line-height: 27px;
                                 font-size: 18px;'),
                                
                                tags$li('Additional information about each cluster 
                                 and item can be retrieved by hovering the mouse 
                                 over a particular point in the graphs. In 
                                 particular, information about specific items 
                                 and cluster means can be obtained, as well as 
                                 general information about clusters, such as 
                                 mean and within-cluster variance of purchasing 
                                 ability, as well as the size of each cluster 
                                 as a fraction of the whole sample. ',
                                        style = 'text-align:justify;
                                 margin-bottom: 40px;
                                 line-height: 27px;
                                 font-size: 18px;'),
                                )
                              ), ## Inner Column
                       #tags$br()
                       ) ## Outer Column
                ), ## Fluid row 
                column(width = 10, offset = 1,
                       fluidRow(
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput(inputId = "CheckVars",
                                                label = tags$div(
                                                  tags$b("1. Select Items"),
                                                  style = "margin-bottom: 15px;"
                                                  ),
                                                choices = colnames(d_raw),
                                                selected = colnames(d_raw)),
                             uiOutput("SliderParts"),
                             uiOutput("SliderClusts"),
                             verbatimTextOutput("TextHH")
                             ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Line Plot",
                                         tags$br(),
                                         plotlyOutput("lineplot", 
                                                      width = 800, height = 500
                                                      ))
                                ,
                                tabPanel("Heatmap",
                                         tags$br(),
                                         plotlyOutput("heatmap",
                                                      width = 700, height = 700
                                                      ))
                               ) ## Tab set panel 
                             ) ## Main panel 
                           ) ## Sidebar panel 
                         ) ## Fluid row
                       ), ## Column
                style =  'margin-top: 2.5%;
                margin-bottom: 7.5%;'
                ) ## Fluid page 

  

##  *****  SERVER  *****

server <- function (input, output){
  
  d <- reactive({
    req(input$CheckVars)
    d <- d_raw %>%
      dplyr::select (input$CheckVars) %>% 
      drop_na()
    
    return(d)
  }) 
  
  res.mona <- reactive({
    req(d())
    validate(need( dim(d())[2] >= 2, "Select at least 2 items"))
    
    res.mona <- mona(x = d())
    
    return(res.mona)
  })
  
  output$SliderParts <- renderUI({
    req(input$CheckVars)
    
    sliderInput(inputId = "SliderParts",
                label = tags$div(
                  tags$b("2. Adjust partition step"),
                  style = "margin-bottom: 15px;
                  margin-top: 20px"
                ),
                min = 1, max = dim(d())[2],
                value = 1,
                step = 1,
                ticks = FALSE)
  })
  
  ## Obtain number of clusters after each partition
  n_clust <- reactive({
    req(input$SliderParts)
    
    step <- input$SliderParts
    
    n = 1
    clust.size = NULL
    n_new = NULL
    n_clust = c()
    
    for (i in 1:step){
      n_new =  n - length(which(res.mona()$step == i)) + length(which(res.mona()$step == i))*2
      n = n_new
      n_clust[i] <- n
    }
    
    return(n_clust[input$SliderParts])
  })
  
  ## Render "Clusters Shown" slider 
  output$SliderClusts <- renderUI({
    req(n_clust())
    
    sliderInput(inputId = "SliderClusts",
                label = tags$div(
                  tags$b("3. Adjust clusters shown"),
                  style = "margin-bottom: 15px;
                  margin-top: 20px;"
                ),
                min = 1, max = n_clust(),
                value = n_clust(),
                step = 1,
                ticks = FALSE)
    })
  
  ## Create data frame for line plot 
  ## columns: clusters, rows: item means, ability, s.d. ability, size 
  data1 <- reactive({
    req(input$SliderClusts)
    
    validate(need(input$SliderClusts <= n_clust(), "Awaiting data..."))
    
    irm <- rasch(d(), constraint = cbind(dim(d())[2] + 1, 1))
    factor_scores <- factor.scores(irm)$score.dat
    
    i_scores <- row.match(d()[, 1:dim(d())[2]], factor_scores[, 1:dim(d())[2]])
    
    scores <- c()
    for (i in 1:length(i_scores)){
      scores[i] <- factor_scores$z1[i_scores[i]]
    }
    
    clust <- cluster.analysis(res.mona(), input$SliderParts)
    clust.mona <- cbind(d(), scores, clust)
    
    clust.mean <- data.frame(matrix(ncol = n_clust(), nrow = (dim(d())[2]+3)))
    
    clust_names <- c()
    for (c in 1:ncol(clust.mean)){
      name <- paste("Cluster", c)
      clust_names <- c(clust_names, name)
    }
    
    colnames(clust.mean) <- clust_names
    
    for (c in 1:n_clust()) {
      for (v in 1:(dim(d())[2]+1)) {
        clust.mean[v, c]<- round(mean((clust.mona %>% dplyr::filter(clust == c))[ ,v]), 3)
        if (v == (dim(d())[2]+1)) {
          clust.mean[(dim(d())[2]+2), c] <- round(var((clust.mona %>% dplyr::filter(clust == c))[ ,v]), 3)
          clust.mean[(dim(d())[2]+3), c] <- count((clust.mona%>% dplyr::filter(clust == c)))
        }
      }
    }
    
    ## Order by size and select largest clusters 
    ord <- order(-as.numeric(clust.mean[dim(d())[2]+3, ]))
    clust.mean.ord <- clust.mean[ , ord] 
    clust.mean.ord <- clust.mean.ord[, 1:input$SliderClusts]
    
    ## Order by ability
    ord <- order(-as.numeric(clust.mean.ord[dim(d())[2]+1, ]))
    clust.mean.ord <- clust.mean.ord[ , ord]
    
    return(clust.mean.ord)
  })
  
  ## Render text with share of households in selected clusters
  output$TextHH <- renderText({
    req(input$SliderClusts)
    
    validate(need(input$SliderClusts <= n_clust(), "Awaiting data..."))
    
    clust <- cluster.analysis(res.mona(), input$SliderParts)
    clust.mona <- cbind(d(), clust)
    
    clust.mean <- data.frame(matrix(ncol = n_clust(), nrow = (dim(d())[2]+3)))
    
    for (c in 1:n_clust()) {
      for (v in 1:(dim(d())[2]+1)) {
        clust.mean[v, c]<- round(mean((clust.mona %>% dplyr::filter(clust == c))[ ,v]), 3)
        if (v == (dim(d())[2]+1)) {
          clust.mean[(dim(d())[2]+2), c] <- round(var((clust.mona %>% dplyr::filter(clust == c))[ ,v]), 3)
          clust.mean[(dim(d())[2]+3), c] <- count((clust.mona%>% dplyr::filter(clust == c)))
        }
      }
    }
    
    ## Order by size and select largest clusters 
    ord <- order(-as.numeric(clust.mean[dim(d())[2]+3, ]))
    clust.mean.ord <- clust.mean[ , ord]
    
    clust.size <- clust.mean.ord[c(dim(clust.mean.ord)[1]), ]
    pct_hh <- round(sum(clust.size[1:input$SliderClusts])/sum(clust.size)*100, 2)

    if (input$SliderClusts > 1) {
      txt <- paste("Share of HHs in ", input$SliderClusts, " clusters: ", pct_hh, "%", sep ="")
    } else {
      txt <- paste("Share of HHs in ", input$SliderClusts, " cluster: ", pct_hh, "%", sep ="")
    }
    
    return(txt)
  })
  
  ## Create title for plots
  title <- reactive({
    
    if (input$SliderParts == 1){
      title <- paste(" Cluster means by item after ", input$SliderParts,
                     " partition step", sep = "")
    } else {
      title <- paste(" Cluster means by item after ", input$SliderParts,
                     " partition steps", sep = "")
    }
    
    return(title)
  })

  ## Render line plot 
  output$lineplot <- renderPlotly({
    req(data1())
    req(title())
    
    clust.mean_ability  <- data1()[c(dim(d())[2]+1), ]
    clust.var_ability    <- data1()[c(dim(d())[2]+2), ]
    clust.size          <- data1()[c(dim(d())[2]+3), ]
    clust.mean.ord      <- data1()[-c((dim(d())[2]+1),
                                             (dim(d())[2]+2), 
                                             (dim(d())[2]+3)), ]
    
    color_list <- viridis(input$SliderClusts)
    
    vars <- list()
    ticks <- list()
    for (item in 1: length(colnames(d()))){
      vars <- append(vars, colnames(d())[item] )
      ticks <- append(ticks, item-1)
    }
    
    l_plot <- plot_ly() %>%
      layout(autosize = TRUE,
             hovermode = "x", 
             title = list(text = title(),
                          font = list(size = 20),
                          x = 0.06,
                          y = 0.99),
             margin = list(l = 50, r = 50,
                           b = 0, t = 50),
             xaxis = list(range = c(ticks[[1]]-0.1, ticks[[length(ticks)]]+0.1),
                          ticktext = vars, 
                          tickvals = ticks,
                          tickmode = "array",
                          tickfont = list(size = 15)),
             yaxis = list(title = "Cluster mean",
                          range = c(-0.05, 1.05),
                          titlefont = list(size = 17),
                          tickfont = list(size = 12))
      )
    for (c in 1:input$SliderClusts){
      l_plot <- l_plot %>% 
        add_trace(y = clust.mean.ord[, c], 
                  name = colnames(clust.mean.ord)[c], 
                  type = "scatter", 
                  mode = "lines+markers",
                  line = list(color = color_list[c], width = 4),
                  marker = list(color = color_list[c], size = 8),
                  hoverinfo = "text",
                  text = paste("<b>", colnames(clust.mean.ord)[c], "</b>",
                               "<br>Item: ", vars, 
                               "<br>Mean: ", round(clust.mean.ord[, c], 3),
                               "<br>     Mean ability: ", clust.mean_ability[[c]],
                               "<br>     Var ability: ", clust.var_ability[[c]],
                               "<br>     Size: ", round((clust.size[[c]]/dim(d())[1])*100,2), '%',
                               sep = "" )
                  )
    }
    
    return(l_plot)
  })
  
  

  
  ## Create data frame for heatmap
  ## long table, columns: cluster, item, mean, size 
  data2 <- reactive ({
    req(input$SliderParts)
    
    validate(need(input$SliderClusts <= n_clust(), "Awaiting data..."))
    
    irm <- rasch(d(), constraint = cbind(dim(d())[2] + 1, 1))
    factor_scores <- factor.scores(irm)$score.dat
    
    i_scores <- row.match(d()[, 1:dim(d())[2]], factor_scores[, 1:dim(d())[2]])
    
    scores <- c()
    for (i in 1:length(i_scores)){
      scores[i] <- factor_scores$z1[i_scores[i]]
    }
    
    clust <- cluster.analysis(res.mona(), input$SliderParts)
    clust.mona <- cbind(d(), scores, clust)
    
    clust.mean <- data.frame(matrix(nrow = n_clust() , ncol = (dim(d())[2]+3)))
    colnames(clust.mean) <- c(colnames(d()), "mean_ability", "var_ability", "size")
    
    for (c in 1:n_clust()) {
      for (v in 1:(dim(d())[2]+1)) {
        clust.mean[c, v]<- round(mean((clust.mona %>% dplyr::filter(clust == c))[ ,v]), 3)
        if (v == (dim(d())[2]+1)) {
          clust.mean[c, (dim(d())[2]+2)] <- round(var((clust.mona %>% dplyr::filter(clust == c))[ ,v]), 3)
          clust.mean[c, (dim(d())[2]+3)] <- count((clust.mona%>% dplyr::filter(clust == c)))
        }
      }
    }
    
    init <- n_clust() - input$SliderClusts + 1
    
    clust.mean.ord <- tibble::rowid_to_column(clust.mean, "Cluster")
    
    ## Order by size and select largest clusters
    clust.mean.ord <- clust.mean.ord[order(clust.mean.ord$size), ]
    clust.mean.ord <- clust.mean.ord[init:n_clust(), ]
    
    ## Order by ability
    clust.mean.ord <- clust.mean.ord[order(clust.mean.ord$mean_ability), ]
    
    mean_abilities <- rep(clust.mean.ord$mean_ability, dim(d())[2])
    var_abilities   <- rep(clust.mean.ord$var_ability, dim(d())[2])
    sizes          <- rep(clust.mean.ord$size, dim(d())[2])
    
    melt_clust <- melt(tail(clust.mean.ord[, 1:(dim(d())[2]+1)], n_clust()), id.vars  = "Cluster")
    colnames(melt_clust) <- c("Cluster", "Item", "Mean")
    melt_clust$M.Ability <- mean_abilities 
    melt_clust$Var.Ability <- var_abilities 
    melt_clust$Size <- sizes 
    
    for (i in 1:dim(melt_clust)[1]){
      melt_clust$Cluster[i] <- paste("Cluster",  melt_clust$Cluster[i])
    } 
    
    return(melt_clust)
  })
  

  ## Render heatmap 
  output$heatmap <- renderPlotly({
    req(data2())
    req(title())
    
    h_map <- plot_ly(colors = "GnBu") %>%
      layout(autosize = TRUE,
             title = list(text = title(),
                          font = list(size = 20),
                          x = 0.06,
                          y = 0.99),
             margin = list(l = 50, r = 50,
                           b = 0, t = 50),
             xaxis=list(title = "",
                        tickfont = list(size = 15)),
             yaxis=list(title = "",
                        ticktext = list(NULL),
                        tickvals = list(NULL))) %>%
      add_trace(data = data2(), x = ~Item, y = ~Cluster, z = ~Mean, 
                type = "heatmap",
                hoverinfo = "text",
                text = ~paste( "<b>", data2()$Cluster, "</b>", 
                               "<br>Item: ", data2()$Item,
                               "<br>Mean: ", data2()$Mean,
                               "<br>     Mean ability: ", data2()$M.Ability,
                               "<br>     Var ability: ", data2()$Var.Ability,
                               "<br>     Size: ", round((data2()$Size/dim(d())[1])*100,2), '%',
                                sep = "")
                )
    
    return(h_map)
    })
  
}

shinyApp(ui = ui,  server = server)
