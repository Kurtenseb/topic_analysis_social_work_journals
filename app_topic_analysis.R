###############################################
#####Programming Example Sebastian KURTEN######
#This analysis is part of the publication: Nature and Extent of Quantitative Research in Social Work Journals: A Systematic Review from 2016 to 2020
#It is available under: https://academic.oup.com/bjsw/advance-article/doi/10.1093/bjsw/bcab171/6352221?guestAccessKey=3b4442c3-587f-4fc1-9734-8ae125911c6f 
#Online deployed version of this application at: https://anonymous-for-peer-review.shinyapps.io/Topics/ 
###############################################

###Setup
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library (text2vec)
library (shiny)
library (LDAvis)
library (servr)
library (dplyr)
library (tidytext)
library (tidyr)
library (ggplot2)
library (ldatuning)

###Define UI
ui <- navbarPage(
    title = "NLP app",
    
    tabPanel("Topic Model",icon = icon("group"), 
             fluidPage(
                 
                 headerPanel(""),
                 titlePanel(p(h2("Topics of Quantitative & Mixed Methods Articles in leading Social Work Journals",style = "color:#4d3a7d"))),
                 
                 mainPanel( 
                     tabsetPanel(
                     tabPanel("Topic Visualisation", hr(),helpText(h2("Please wait (up to 3 min.) until the topics are loaded, then select a topic!")),  visOutput('visChart')),
                     tabPanel("Topic Number Validation", plotOutput("plot")) #that tabpanel still does not work perfectly
                     #For some reason, the inclusion of that tabPanel throws an error when uploading the app to the shiny server
             )
             )
    )
)
)

###Define server
server <- function(input, output, session) {
    
    Topic_Subset <- reactive({
        set.seed(2021)
        
        ###loading the pdf documents
        pdfs <- readRDS(file="pdfs.rds")
        docs <- pdfs$text_clean
        
        ###Tokenizing the text data
        tokens = docs %>% 
            tolower %>% 
            word_tokenizer
        
        ###Load stop words
        stop_words = readRDS("stop_words.rds") #commented out to increase runtime
        
        ###Create object for Shiny app
        it = itoken(tokens, progressbar = FALSE) #commented out to increase runtime
        v = create_vocabulary(it,stopwords=stop_words) #commented out to increase runtime
        v = v[order(v$term_count, decreasing = TRUE),] #commented out to increase runtime
        vectorizer = vocab_vectorizer(v) #commented out to increase runtime
        
        ###Create DTM 
        dtm = create_dtm(it, vectorizer, type = "dgTMatrix") #commented out to increase runtime
     
        ###Determine the number of topics
        #commented out because runtime is approx. 45 minutes.
        #system.time({
        #    tunes <- FindTopicsNumber(
        #        dtm,
        #        topics = c(2:100),
        #        metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
        #        method = "Gibbs",
        #        control = list(seed = 77),
        #        mc.cores = 4L,
        #        verbose = TRUE
        #    )
        #})
        tunes = readRDS(file = "tunes.rds") #load in results from previously run analysis
        output$plot <- renderPlot({
            input$newplot
            FindTopicsNumber_plot(tunes)
        }) 
        
        ###set number of topics
        nTopics = 40 #derived from plot
        
        ###create lda model
        lda_model = text2vec::LDA$new(n_topics = nTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
        lda_model$fit_transform(x = dtm, n_iter = 50, 
                                convergence_tol = 0.001, n_check_convergence = 25, 
                                progressbar = TRUE)
        
        return(lda_model) # 
    })
    
    output$visChart <- renderVis({
        set.seed(2021)
        input$GoButton
        
        isolate({
            nterms    <- 20
            lda_model <- Topic_Subset()
        })
        
        lda_model$plot(out.dir = "./results", R = nterms, open.browser = FALSE)
        
        readLines("./results/lda.json")
        
    })

}

###Run the application
shinyApp(ui = ui, server = server)
