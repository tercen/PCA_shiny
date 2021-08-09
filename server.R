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

server <- shinyServer(function(input, output, session) {
  
  output$body <- renderUI({
    data     <- dataInput()
    mx_comp  <- dim(data$pca$rotation)[2]
    no_comp  <- min(mx_comp, data$no_components)
    px       <- paste("PC", 1:no_comp, sep = "")
    py       <- px[c(2, 1, 3:no_comp)]
    colors   <- unique(c(colnames(data$color_annotation), colnames(data$sample_annotation)))
    texts    <- unique(c(colnames(data$sample_annotation), colnames(data$color_annotation)))
    
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
        selectizeInput("colorAnnotation", getSelectLabel("Select factor for coloring"), choices = colors),
        selectizeInput("textAnnotation", getSelectLabel("Select factor for text labels"), choices = texts),
        tags$hr(),
        plotOutput("legend", width = "200px"),
        HTML(paste("<center><h5 style='color: black'>Click below to send data back to Tercen</h5>", actionButton("button", "Transform data"),"</center>")),
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
  
  returnData <- reactive({
    getReturnData(session, dataInput())
  })
  
  dataInput <- reactive({
    getValues(session)
  })
  
  get_color_values <- reactive({
    data  <- dataInput()
    color <- as.character(input$colorAnnotation)
    if (color %in% colnames(data$sample_annotation)) {
      group <- as.factor(data$sample_annotation[[color]])
    } else {
      group <- as.factor(acast(cbind(data$data, data$color_annotation), .ri~.ci, value.var = color)[1,])
    }
    group
  })
  
  get_colors <- reactive({
    1 + as.numeric(get_color_values())
  })
  
  sLab <- reactive({
    data <- dataInput()
    text <- as.character(input$textAnnotation)
    if (text %in% colnames(data$sample_annotation)) {
      result <- as.character(data$sample_annotation[[text]])
    } else {
      result <- as.character(acast(cbind(data$data, data$color_annotation), .ri~.ci, value.var = text)[1,])
    }
    result
  })
  
  output$mat <- renderPlot({
    data <- dataInput()
    clrs <- get_colors()
    pairs(data$pca$x[,1:input$ncomp], col = clrs, bg = clrs, pch = 21)
  }, height = 650, width = 850)
  
  
  output$scores3D <- renderRglwidget({
    data <- dataInput()
    pSym <- switch(input$labsOrSpheres, 
                   "Text labels" = "p",
                   "Spheres" = "s"
    )
    clrs   <- get_colors()
    labels <- sLab()
    open3d()
    plot3d(x = data$pca$x[,1],y = data$pca$x[,2], z = data$pca$x[,3],  col = clrs, box = FALSE, type = pSym, size = 2, xlab = "PC1", ylab = "PC2", zlab = "PC3")
    if (pSym == "p") {
      text3d(x = data$pca$x[,1], y = data$pca$x[,2], z= data$pca$x[,3], texts = labels, adj = 0, col = clrs, box = FALSE)
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
    color_values <- get_color_values()
    colors       <- unique(get_colors())
    
    plot(1,1, xaxt = "n", yaxt ="n", xlab = "", ylab = "", col = "white", bty = "n")
    legend(x = "top", legend = unique(color_values), fill = colors, cex = 1, bty = "n")
  })
  
  output$biplot = renderPlot({
    data    <- dataInput()
    mx_comp <- dim(data$pca$rotation)[2]
    no_comp <- min(mx_comp, data$no_components)
    px      <- paste("PC", 1:no_comp, sep = "")
    xIdx    <- c(1:no_comp)[input$px == px]
    yIdx    <- c(1:no_comp)[input$py == px]
    qntVars <- 1 - 0.01 * input$showvars
    zmVars  <- 0.01 * input$zl
    oLabs   <- sLab()
    clrs    <- get_colors()
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
  
  observeEvent(input$button, {
    shinyjs::disable("button")
    
    ctx <- getCtx(session)
    returnData() %>% 
      ctx$addNamespace() %>% 
      ctx$save()
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
  
  if (length(array_labels) > 0) {
    obs_names <- droplevels(interaction(ctx$cselect(array_labels)))
  } else {
    stop("PCA requires multiple columns (observations) in he BN cross-tab view")
  }
  
  if (length(spot_labels) > 0){
    var_names <- droplevels(interaction(ctx$rselect(spot_labels)))
  } else {
    stop("PCA requires multiple rows (variables) in the BN cross-tab view")
  }
  
  sample_annotation <- ctx$cselect(array_labels)
  color_annotation  <- NULL
  if (length(ctx$colors) > 0) {
    color_annotation  <- ctx$select(ctx$colors)
  }
  
  X <- t(acast(data, .ri~.ci, value.var = ".y"))
  if (any(is.na(X))) {
    stop("Missing values are not allowed")
  }
  
  rownames(X) <- obs_names
  colnames(X) <- var_names
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
  
  list(data              = data,
       matrix_data       = X,
       pca_data          = pca_data, 
       sample_annotation = sample_annotation, 
       color_annotation  = color_annotation, 
       var_names         = var_names,
       no_components     = no_comp,
       s_value           = sVal)
}

getReturnData <- function(session, input_data) {
  data     <- input_data$data
  X        <- input_data$matrix_data
  pca_data <- input_data$pca_data
  no_comp  <- input_data$no_components
  sVal     <- input_data$s_value
  
  # reformat and return the required number of loadings and scores
  flat_rotation <- matrix(nrow = dim(data)[1], ncol = no_comp)
  flat_score    <- matrix(nrow = dim(data)[1], ncol = no_comp)
  for (j in 1:no_comp) {
    for (i in 1:dim(X)[2]) {
      flat_rotation[data$.ri == (i-1), j] = pca_data$rotation[i, j]
    }
    for (i in 1:dim(X)[1]){
      flat_score[data$.ci == (i-1), j] = pca_data$x[i, j]
    }
  }
  result           <- data.frame(flat_rotation)  
  colnames(result) <- paste('PC', 1:no_comp, sep = '');
  result           <- data.frame(result, flat_score)
  result           <- data.frame(result, .ri = data$.ri, .ci = data$.ci)     
  result           <- result[order(result$.ri, result$.ci), ]
  if (!is.null(sVal)) {
    result <- data.frame(result, sVal = sVal$value)
  }
  result
}
