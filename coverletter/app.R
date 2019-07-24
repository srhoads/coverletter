library(shiny)
library(shinydashboard)
library(tidyverse)
library(pdftools)
library(magick)
library(shinyWidgets)
# file.copy("/Users/srhoads/GitHub/portfolio/pdfsubsetter/pdfsubsetter/samnetji.png", "samnetji.png")
# writelogo=F # writelogo=T
# magick::image_read('samnetji.png') %>%
#   image_trim() %>%
#   image_crop(., "470x321+165+26") %>% # right (dim) x bottom (dim) + left (decrease whitespace) top (increase whitespace)
#   if(writelogo) image_write(., 'logo.png') else .


try_image_read_pdf <- function(path, pages = NULL, density = 72, password = ""){
  if(pdf_length(path) > 50) {maxpgs <- 1:50; pdftooigtxt <- paste0("Full PDF too large to display (", pdf_length(path), " pgs)\nOnly 1st 50 pgs displayed")} else {maxpgs <- NULL; pdftooigtxt <- " "}
  bigpdf1t10 <- magick::image_read_pdf(bigpdfn, pages = maxpgs, density=density)
  bigpdf1t10 %>% image_annotate(., text=pdftooigtxt, location = geometry_point(10, 50), size = 30, color="red")
}


now_hms <- function() {
  gsub("/","",format(Sys.time(), "%D_%H_%M_%S"))
}

pdfsplitter_fxn <- function(path=NULL, namewanted=NULL, split_before_pgs=NULL, write=T){
  `%>%` <- magrittr::`%>%`
  if(is.null(split_before_pgs)) split_before_pgs <- 1
  if(is.null(path)) path <- list.files(pattern="\\.pdf", recursive=T, full.names = T)[1]
  (s2 <- sort(unique(c(0, split_before_pgs))))
  # (fnpathnoext <- tools::file_path_sans_ext(path))
  (fnpathnoext <- tools::file_path_sans_ext(namewanted))
  (npgtotal <- pdftools::pdf_length(path))
  pdfs <- list()
  for(i in 1:length(s2)){
    firstpage <- s2[i]
    lastpage <- s2[i+1]-1
    pdfs[i] <- paste0(c(firstpage, lastpage), collapse=", ")
  }
  pdfs
  (newlastrange <- unlist(pdfs[length(pdfs)]) %>% gsub("NA", npgtotal, .))
  (newfirstrange <- unlist(pdfs[1]) %>% gsub("0,", "1,", .))
  pdfs[length(pdfs)] <- list(newlastrange)
  pdfs[1] <- list(newfirstrange)
  pdfs
  (pdfs <- lapply(pdfs, function(x){
    (newfilepgs <- unlist(x) %>% gsub(" ", "", .) %>% strsplit(., ",") %>% unlist() %>% as.numeric())
    if(newfilepgs[[2]] > npgtotal) newfilepgs[[2]] <- npgtotal
    if(newfilepgs[[1]] > npgtotal) newfilepgs <- NULL
    newfilepgs
  }) %>% plyr::compact())
  laststep <- lapply(pdfs, function(x){
    (newfilepgs <- unlist(x) %>% gsub(" ", "", .) %>% strsplit(., ",") %>% unlist() %>% as.numeric())
    (outputname <- paste0(fnpathnoext, "_", newfilepgs[1], "-", newfilepgs[2], ".pdf"))
    if(newfilepgs[2] !=0 & newfilepgs[1] <= newfilepgs[2] & newfilepgs[2] <= npgtotal) if(write) return(pdftools::pdf_subset(path, pages=newfilepgs[1]:newfilepgs[2], output=outputname)) else return(outputname)
  })
  cat(paste0("outputnames <- ", "c(", paste0("'", unlist(laststep), "'", collapse=", "), ")\n\n"))
  return(plyr::compact(laststep))
}


# magick::image_read('http://ilgconference.com/2018/wp-content/uploads/2017/01/Jackson_Lewis.png') %>%
#   image_trim() %>%
#   image_write(., 'logojl.png')

ui <- shinyUI(
  navbarPage(" ",
             tabPanel("App",
                      fluidPage(
                        fluidPage(
                          titlePanel(
                            fluidRow(
                              br(),
                              column(4, HTML("<i>Simple Coverletter</i></br>"), offset=8),
                              br()#,
                              # hr()#,
                              # column(2, img(src=' ', height=32))
                            )
                          ),
                          # titlePanel(title=HTML(paste0("PDF Subsetter","<font color='white'>by Sam Rhoads!</font>", div(img(src='logo.png', height=125))))),
                          # titlePanel("PDF Subsetter"),
                          # fluidPage(
                          #   fluidPage(
                          #     titlePanel(img(src='https://www.fawl.org/assets/LOGOS/jackson%20lewis%20no%20tag%20color%20png.png', height=35)),
                          #     titlePanel("PDF Splitter"),
                          sidebarLayout(
                            sidebarPanel(
                              # fileInput('file1', 'Choose PDF File',
                              #           accept=c('pdf', 'pdf','jpeg',
                              #                    'png', 'jpg',
                              #                    'pdf')
                              # ),
                              # textInput("yourname", "Your name", value="lalala"),# placeholder="Sami Rhoads"),
                              # textInput("company", "Company you want a job from", value="ooooo"),# , placeholder="Sami Corporation"),
                              # textInput("position", "Job Position", value="iiiiiii"),# , placeholder="Cat Whisperer"),
                              
                              tags$hr(),
                              tags$head(tags$style(
                                type="text/css",
                                "#viewimage img {max-width: 110%; width: 110%; height: auto}"
                              )),
                              HTML("Enter relevant information below. To leave a field blank, just enter a <code>space</code> as a placeholder."),
                              br(),
                              hr(),
                              textInput("yourname",
                                        HTML("Your name"),
                                        value="SAM I. AM"),
                              textInput("company",
                                        HTML("Company of application"),
                                        value=HTML("SRHOADSIAN LLC")),
                              textInput("position",
                                        HTML("Application job position"),
                                        value="CHIEF PROCRASTINATION OFFICER"),
                              textInput("youremail",
                                        HTML("Company of application"),
                                        value="GHOST@PSEUDOEMAIL.COM"),
                              textInput("yourphone",
                                        HTML("Your phone #"),
                                        value="(111) 111-1111"),
                              textInput("yourtitle",
                                        HTML("Your title (current career)"),
                                        value="SR. CAT WHISPERER"),
                              textInput("yourtitle2",
                                        HTML("Your title (education, other)"),
                                        value="BS, MA, MFA, JD, MD, PhD"),
                              hr(),
                              
                              
                              # conditionalPanel(
                              #   condition = "input.au != 'Overall Company'",
                              #   uiOutput('subunit')
                              # ),
                              
                              
                              # textInput("paragraph1",
                              #           HTML("Append a paragraph?"),
                              #           value="I LIKE TO CODE SOMETIMES. THIS PARAGRAPH GOES HERE."),
                              # # tags$textarea(id="paragraph1", rows=5,placeholder =  "Leave a comment...", ""), 
                              # textInput("paragraph2",
                              #           HTML("Append another paragraph?"),
                              #           value="MY EYES HURT FROM STARING AT THE SCREEN TOO LONG."),
                              # hr(),
                              tags$small("Suggestions? Contact app creator"), tags$small(a(href='https://www.linkedin.com/in/samantha-rhoads/', 'Samantha Rhoads')),
                              br(),
                              tags$small(HTML(paste(tags$span(style="color:grey", 
                                                              "Visit my full app gallery on ", a(href="https://srhoadsian.shinyapps.com/gallery/", "Shinyapps"),
                                                              "or", a(href="http://rpubs.com/srhoadsian", "Rpubs"))))),
                              br(),
                              hr(),
                              navlistPanel(widths = c(12, 12, 12),
                                           HTML("<small>Code</small>"),
                                           tabPanel("app.R",
                                                    pre(includeText("app.R"))
                                           ),
                                           tabPanel("simple_coverletter_shiny.R",
                                                    pre(includeText("simple_coverletter_shiny.R"))
                                           ),
                                           tabPanel("report.tex",
                                                    pre(includeText("report.tex"))
                                           ),
                                           selected=NA
                              )),
                            mainPanel(
                              # htmlOutput('pathtext'),
                              # "\n",
                              # br(),
                              # uiOutput("downloadData"),
                              # HTML("<strong>File preview...</strong><br>"),
                              downloadButton('downloadData', 'Download Coverletter PDF'),
                              hr(),
                              # br(),
                              imageOutput('viewpdf')
                            ), fluid=T, position="right"
                          )
                        ), setBackgroundColor("#F0F8FF")
                      )
             ),
             tabPanel("Source Code",
                      # uiOutput('sourcecode')
                      navlistPanel(widths = c(12, 12, 12),
                                   HTML("<small>Code</small>"),
                                   tabPanel("app.R",
                                            pre(includeText("app.R"))
                                   ),
                                   tabPanel("simple_coverletter_shiny.R",
                                            pre(includeText("simple_coverletter_shiny.R"))
                                   ),
                                   tabPanel("report.tex",
                                            pre(includeText("report.tex"))
                                   ),
                                   selected="app.R"
                      )
             )
             
  )
)




server <- shinyServer(function(input, output) {
  
  output$sourcecode <- renderUI({
    pre(includeText("app.R"))
  })
  # getImgs <- reactive({
  #   inFile <- input$file1
  #   path <- inFile$datapath
  #   if (is.null(input$file1)) return(NULL)
  #   if(pdf_length(path) <= 50) magick::image_read_pdf(path, density = 72) else try_image_read_pdf(path, density=70) %>% 
  #     magick::image_join() %>% magick::image_animate(fps=2) %>%
  #     magick::image_write(tempfile(fileext='jpg'), format = 'jpg')
  # })
  # 
  output$viewpdf <- renderImage({
    req(input$yourname)
    source("simple_coverletter_shiny.R")
    simple_coverletter(
      company=input$company,
      yourphone=input$yourphone,
      yourtitle=input$yourtitle,
      yourtitle2=input$yourtitle2,
      yourname=input$yourname,
      position=input$position,
      youremail=input$youremail,
      paragraph1="I like to write code sometimes",
      paragraph2="My eyes hurt",
      filename_override=NULL,
      file="report.Rnw"
    )
    plzwrk <- pdf_subset("report.pdf", pages = -10000000, output='fullinputpdf.pdf')
    if(pdf_length('fullinputpdf.pdf') <= 50) pdfz <- magick::image_read_pdf('fullinputpdf.pdf', density = 100) else pdfz <- try_image_read_pdf('fullinputpdf.pdf', density = 100)
    
    tmpfile <- pdfz %>%
      # {
      #   x <- .
      #   for(i in 1:length(x)) x[i] <- image_annotate(x[i], paste0("p. ", i), location = geometry_point(10, 10), size = 24)
      #   x
      # } %>%
      image_append(., stack=T) %>%
      image_write(tempfile(fileext='jpg'), format = 'jpg')
    tryCatch(file.remove('fullinputpdf.pdf'), error = function(e) NULL)
    list(src = tmpfile, contentType = "image/jpeg")
    
  })
  
  # getFilename <- reactive({
  #   if(!is.null(input$file1$datapath)) return(tools::file_path_sans_ext(basename(input$file1$name))) else return(NULL)
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste0("report", ".pdf")},
    content = function(file) { 
      simple_coverletter(
        company=input$company,
        yourphone=input$yourphone,
        yourtitle=input$yourtitle,
        yourtitle2=input$yourtitle2,
        yourname=input$yourname,
        position=input$position,
        youremail=input$youremail,
        paragraph1=input$paragraph1,
        paragraph2=input$paragraph2,
        filename_override=NULL,
        file="report.Rnw"
      )
      file.copy('report.pdf', file, overwrite = TRUE)
      
    }
  )
  
  
  # output$pathtext <- renderText({
  #   # req(input$file1)
  #   if(!is.null(input$file1$datapath)) return(paste0("<b>Input file: </b>", code(basename(input$file1$name)))) else return(NULL)
  # })
  
  # getFilename <- reactive({
  #   if(!is.null(input$file1$datapath)) return(tools::file_path_sans_ext(basename(input$file1$name))) else return(NULL)
  # })
  # 
  # getfilenames <- reactive({
  #   # pdfsplitter_fxn(path=input$file1$datapath, split_before_pgs=input$split_before_pgs, write=F)
  #   req(input$split_before_pgs)
  #   req(input$file1)
  #   split_before_pgs <- input$split_before_pgs %>% gsub(" ", "", .) %>% strsplit(., ",") %>% unlist() %>% as.numeric()
  #   (newfnames <- unlist(pdfsplitter_fxn(path=input$file1$datapath, namewanted=input$file1$name, split_before_pgs=split_before_pgs, write=F)))
  # })
  # 
  # output$getfilenames <- renderUI({
  #   req(input$split_before_pgs)
  #   req(input$file1)
  #   split_before_pgs <- input$split_before_pgs %>% gsub(" ", "", .) %>% strsplit(., ",") %>% unlist() %>% as.numeric()
  #   newfnames <- unlist(pdfsplitter_fxn(path=input$file1$name, split_before_pgs=split_before_pgs, write=F))
  #   paste0(newfnames %>% length())
  # })
  
  # observe({
  #   lapply(1:length(getfilenames()), function(i) {
  #     output[[paste0("downloadData", i)]] <- downloadHandler(
  #       filename = function() getfilenames()[i], #{paste0(getFilename(), "_pp", input$firstpage, "-", input$lastpage, "_", now_hms(), ".pdf")},
  #       content = function(file) {
  #         (pgrange <- unlist(strsplit(getfilenames()[i], "_pp|_")) %>% .[length(.)] %>% gsub("\\.pdf", "", .) %>% strsplit(., "-") %>% unlist() %>% as.numeric())
  #         pdftools::pdf_subset(input$file1$datapath, pages = pgrange[1]:pgrange[2], output = file)
  #       }
  #     )
  #   })
  # })
  # 
  # output$downloadButtons <- renderUI({
  #   lapply(1:length(getfilenames()), function(i) {
  #     downloadButton(paste0("downloadData", i), HTML(paste0("Download ", i, ": ", tags$code(getfilenames()[i]))))
  #   })
  # })
  # 
})

shinyApp(ui = ui, server = server)


# 
# inputfilenamenoext <- basename(inFile$datapath)
# inputfilenamenopathnoext <- tools::file_path_sans_ext(inputfilenamenoext)
# 
