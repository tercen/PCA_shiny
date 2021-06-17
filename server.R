library(shiny)
library(tercen)
library(dplyr)
library(rgl)
library(reshape2)
library(shinyjs)
library(shinydashboard)
library(htmlwidgets)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
  output$body <- renderUI({
    data     <- dataInput()
    mx_comp  <- dim(data$pca$rotation)[2]
    no_comp  <- min(mx_comp, data$no_components)
    px       <- paste("PC", 1:no_comp, sep = "")
    py       <- px[c(2, 1, 3:no_comp)]
    
    dashboardPage(
      # Application title
      dashboardHeader(title = "Shiny PCA scores results"),
      
      # Sidebar with controls
      dashboardSidebar(
        shinyjs::useShinyjs(),
        tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);')),
        tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label = "hidden"))),
        tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
        tags$head(tags$style(HTML('.content-wrapper, .right-side { background-color: white; }'))),
        selectizeInput("labsOrSpheres", getSelectLabel("What to show"), choices = c("Text labels", "Spheres")),
        selectizeInput("colorAnnotation", getSelectLabel("Select factor for coloring"), choices = colnames(data$color_annotation)),
        selectizeInput("textAnnotation", getSelectLabel("Select factor for text labels"), choices = colnames(data$sample_annotation)),
        tags$hr(),
        plotOutput("legend", width = "200px"),
        width = "20%"
      ),
      
      # mainpanel
      dashboardBody( 
        tabsetPanel(tabPanel("Scores 3D", rglwidgetOutput("scores3D", height = 700, width = 700)),
                    tabPanel("Scores Plot matrix",tags$hr(),
                             sliderInput("ncomp", "Number of components to show in the matrix.", min = 2, max = mx_comp, step = 1, value = no_comp),
                             tags$hr(),
                             plotOutput("mat", height = 700)),
                    tabPanel("Biplot", plotOutput("biplot",height = 600, width = 600), 
                             sliderInput("showvars", "% largest variable loadings to show",min = 0, max = 100, value = 10 ),
                             sliderInput("zl","Zoom variable loadings (%)", min = 1, max = 400, value = 100), 
                             selectInput("px", "X-axis", choices = px),
                             selectInput("py", "Y-axis", choices = py)),
                    tabPanel("Variation", plotOutput("scree"))
        ),
        textOutput("done")
      ))
  })
  
  dataInput <- reactive({
    getValues(session)
  })
  
  cCol = reactive({
    data <- dataInput()
    cHdr = as.character(input$colorAnnotation)
    cGrp = as.factor(data$color_annotation[[cHdr]])
    return(1+as.numeric(cGrp))
  })
  
  sLab = reactive({
    data <- dataInput()
    tHdr <- as.character(input$textAnnotation)
    as.character(data$sample_annotation[[tHdr]])
  })
  
  output$mat <- renderPlot({
    data <- dataInput()
    clrs <- cCol()
    pairs(data$pca$x[,1:input$ncomp], col = clrs, bg = clrs, pch = 21)
  }, height = 650, width = 850)
  
  
  output$scores3D <- renderRglwidget({
    data <- dataInput()
    pSym <- switch(input$labsOrSpheres, 
                   "Text labels" = "p",
                   "Spheres" = "s"
    )
    clrs  <- cCol()
    tGrp  <- sLab()
    open3d()
    plot3d(x = data$pca$x[,1],y = data$pca$x[,2], z = data$pca$x[,3],  col = clrs, box = FALSE, type = pSym, size = 2, xlab = "PC1", ylab = "PC2", zlab = "PC3")
    if(pSym == "p"){
      text3d(x = data$pca$x[,1], y = data$pca$x[,2], z= data$pca$x[,3], texts = tGrp, adj = 0, col = clrs, box = FALSE)
    }
    scene1 <- scene3d()
    rgl.close()
    rglwidget(scene1)
  })
  
  output$scree = renderPlot({
    data <- dataInput()
    screeplot(data$pca, main = "variance explained per component", col = "lightblue")
  })
  
  output$legend = renderPlot({
    data         <- dataInput()
    color        <- as.character(input$colorAnnotation)
    color_values <- as.factor(data$color_annotation[[color]])
    colors       <- unique(cCol())
    
    plot(1,1, xaxt = "n", yaxt ="n", xlab = "", ylab = "", col = "white", bty = "n")
    legend(x = "top", legend = unique(color_values), fill = colors, cex = 1, bty = "n")
  })
  
  output$biplot = renderPlot({
    data    <- dataInput()
    mx_comp <- dim(data$pca$rotation)[2]
    no_comp <- min(mx_comp, data$no_components)
    px      <- paste("PC", 1:no_comp, sep = "")
    py      <- px[c(2, 1, 3:no_comp)]
    xIdx    <- c(1:no_comp)[input$px == px]
    yIdx    <- c(1:no_comp)[input$py == px]
    qntVars <- 1-0.01*input$showvars
    zmVars  <- 0.01 * input$zl;
    oLabs   <- sLab()
    clrs    <- cCol()
    idx     <- c(xIdx, yIdx)
    obs     <- data$pca$x[,idx]
    vars    <- data$pca$rotation[,idx]
    sc      <- data$pca$sdev[idx]^2
    lvars   <- apply(vars^2, 1, sum)
    bVar    <- lvars > quantile(lvars, qntVars)
    vars    <- vars[bVar,]
    xp      <- c(sc[1]*vars[,1], obs[,1])
    yp      <- c(sc[2]*vars[,2], obs[,2])
    zm      <- 1.1
    scxVars <- zmVars*sc[1]*vars[,1]
    scyVars <- zmVars*sc[2]*vars[,2]
    
    plot(x = scxVars, y = scyVars, pch = ".", xlim = zm * c(min(xp), max(xp)), ylim = zm * c(min(yp), max(yp)), xlab = input$px, ylab = input$py)
    arrows(x0 = 0, y0 = 0, x = scxVars, y = scyVars, col = "grey")
    text(x = obs[,1], y = obs[,2], oLabs, col = clrs, cex = 2)
    text(x = scxVars, y = scyVars, rownames(vars), col = "black")
  })
  
})

getSelectLabel <- function(label) {
  shiny::HTML(paste0("<p><span style='color: black'>", label, "</span></p>"))
}

getValues <- function(session){
  ctx          <- getCtx(session)
  data         <- ctx %>% select(.ri, .ci, .y)
  group_labels <- ctx$colors
  array_labels <- names(ctx$cnames)
  spot_labels  <- names(ctx$rnames)
  
  if (length(array_labels) > 0){
    obs <- droplevels(interaction(ctx$cselect(array_labels)))
  } else {
    stop("PCA requires multiple columns (observations) in he BN cross-tab view")
  }
  
  if (length(spot_labels) > 0){
    vars <- droplevels(interaction(ctx$rselect(spot_labels)))
  } else {
    stop("PCA requires multiple rows (variables) in the BN cross-tab view")
  }
  
  sample_annotation <- ctx$cselect(array_labels)
  color_annotation  <- ctx$select(ctx$colors)
  
  X <- t(acast(data, .ri~.ci, value.var = ".y"))
  if (any(is.na(X))) {
    stop("Missing values are not allowed")
  }
  obsNames    <- obs[data$.ri == 1]
  varNames    <- vars[data$.ci == 1]
  rownames(X) <- obsNames
  colnames(X) <- varNames
  b_scale     <- ifelse(is.null(ctx$op.value('Scale Spots')), 'No', ctx$op.value('Scale Spots')) == "Yes"
  no_comp     <- ifelse(is.null(ctx$op.value('Number of Components')), 5, as.double(ctx$op.value('Number of Components')))
  rm_comp     <- ifelse(is.null(ctx$op.value('Subtract component')), 0, as.double(ctx$op.value('Subtract component')))
  pca_data    <- prcomp(X, scale. = b_scale)
  sVal        <- NULL
  
  if (rm_comp > 0) {
    N         <- scale(X, scale = b_scale) - (pca_data$x[, rm_comp]) %*% (t(pca_data$rotation[, rm_comp]))
    pca_data  <- prcomp(N, scale. = b_scale)
    sVal      <- melt(N)
  }
  
  list(pca_data          = pca_data, 
       sample_annotation = sample_annotation, 
       color_annotation  = sample_annotation, 
       var_names         = varNames,
       no_components     = no_comp)
}