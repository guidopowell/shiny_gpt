#https://pubmed.ncbi.nlm.nih.gov/help/#using-search-field-tags

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, stringr, readxl, dplyr, xlsx, shiny, shinythemes, easyPubMed)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("PubMed Search and GPT API Interface"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("data_source", "Data source", choices = c("Upload Excel File", "PubMed Search"), selected = "PubMed Search"),
      
      # Conditional panel for uploading an Excel file
      conditionalPanel(
        condition = "input.data_source == 'Upload Excel File'",
        fileInput("file", "Upload Excel File", accept = c("xlsx"))
      ),
      
      # Conditional panel for PubMed search
      conditionalPanel(
        condition = "input.data_source == 'PubMed Search'",
        tags$div(
          id = "div_1",
          textInput("search_term_1", "Search term"),
          selectInput("search_tag_1", "Search tag", choices = list(
            "All Fields" = "[all]",
            "Author" = "[au]",
            "Editor" = "[ed]",
            "Journal" = "[ta]",
            "Language" = "[la]",
            "MeSH Terms" = "[mh]",
            "Publication Date" = "[dp]",
            "Title" = "[ti]",
            "Title/Abstract" = "[tiab]"
          ))
        ),
        actionButton("add", "Add search term"),
        tags$div(id = "search_terms")
      ),
      
      actionButton("view", "View"),
      textInput("id_var", "Nom de la colonne d'identifiant", value = ""),
      textInput("contenu_var", "Nom(s) de colonne(s) de contenu (séparés par virgule si plusieurs)", value = ""),
      textInput("api_key", "Clé API", value = ""),
      selectInput("model", "Model", choices = c("gpt-4", "gpt-3.5-turbo"), selected = "gpt-4"),
      sliderInput("temperature", "Temperature", value = 0, min = 0, max=1, step = .1, width="50%"),
      textAreaInput("message_system", "Message pour GPT",
                    placeholder = "Tu es un expert de revue de littérature. Tu détecte la pertinence d'un article à partir du sommaire...",value = "",height="150px", resize="vertical"),
      actionButton("submit", "submit"),
      downloadButton("downloadData", "Télécharger")
    ),
    mainPanel(
      tags$head(
        tags$style(HTML("
          #result table {
            border-collapse: collapse;
          }
          #result td, #result th {
            border: 1px solid #ddd;
            padding: 8px;
          }
          #scrollBox {
            width: 100%;
            height: 800px;
            overflow: auto;
            position: relative;
          }
        "))
      ),
      tags$div(id = "scrollBox",
               tableOutput("result"))
    )
  )
)


server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, result_data = NULL)
  num_search_terms <- reactiveVal(1)
  
  observeEvent(input$view, {
    
    if (input$data_source == 'Upload Excel File') {
      req(input$file)
      rv$data <- read.xlsx(input$file$datapath, sheetIndex = 1)
    } else if (input$data_source == 'PubMed Search') {
      
      query_parts <- lapply(seq_len(num_search_terms()), function(i) {
        search_term_id <- paste0("search_term_", i)
        search_tag_id <- paste0("search_tag_", i)
        paste(input[[search_term_id]], input[[search_tag_id]])
      })
      search_query <- paste(query_parts, collapse = " AND ")
      
      my_query <- get_pubmed_ids(search_query)
      my_abstracts_xml <- fetch_pubmed_data(my_query)
      all_xml <- articles_to_list(my_abstracts_xml, simplify = F)
      rv$data <- lapply(all_xml, article_to_df, max_chars = -1, getAuthors = FALSE, getKeywords=T) %>% bind_rows()
    }
    
    output$result <- renderTable({
      req(rv$data)
      rv$data
    }, rownames = FALSE)
  })
  
  observeEvent(input$add, {
    num_search_terms(num_search_terms() + 1)
    new_term_id <- num_search_terms()
    search_term_id <- paste0("search_term_", new_term_id)
    search_tag_id <- paste0("search_tag_", new_term_id)
    tag_list = list("All Fields" = "[all]",
                    "Author" = "[au]",
                    "Editor" = "[ed]",
                    "Journal" = "[ta]",
                    "Language" = "[la]",
                    "MeSH Terms" = "[mh]",
                    "Publication Date" = "[dp]",
                    "Title" = "[ti]",
                    "Title/Abstract" = "[tiab]")
    insertUI(
      selector = "#search_terms",
      ui = tags$div(
        id = paste0("div_", new_term_id),
        textInput(search_term_id, "Search term"),
        selectInput(search_tag_id, "Search tag", choices = tag_list)
      )
    )
  })
  observeEvent(input$submit, {
    req(rv$data)
    
    data <- rv$data
    
    gpt_api<-function(data, id_var, contenu_var  , api_key, model, temperature, 
                      message_system#, message_user_debut
    ){
      contenu_var<-strsplit(contenu_var,",") %>% unlist %>% gsub(" ","",.)
      
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key)), content_type_json(), encode = "json",
        body = list(
          model = model,
          messages = list(list(role = "system",
                               content = message_system),
                          list(role = "user",
                               content = paste0("\n",
                                                apply(data[contenu_var], 1, paste, collapse='\n\n')))
          ),
          temperature = temperature)
      )
      data.frame(ID = data[[id_var]], Contenu = apply(data[contenu_var], 1, paste, collapse='\n\n'),
                 reponse_gpt = ifelse(response$status == 200,
                                      unlist(content(response)$choices[[1]]$message$content), NA))
    }
    
    withProgress(message = 'En cours', value = 0, {
      results <- lapply(1:nrow(data), function(x) {
        incProgress(1/nrow(data))
        gpt_api(data = data[x,],
                id_var = input$id_var,
                contenu_var = input$contenu_var,
                model = input$model,
                temperature = input$temperature,
                api_key = input$api_key,
                message_system = input$message_system)#,
        # message_user_debut = input$message_user_debut)
      }) %>% bind_rows
    })
    
    colnames(results) <- c(input$id_var, input$contenu_var, "output_gpt")
    
    rv$result_data <- results
    
    output$result <- renderTable({
      req(rv$result_data)
      rv$result_data
    }, rownames = FALSE)
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.time(), ".xlsx", sep = "")
    },
    content = function(file) {
      xlsx::write.xlsx(rv$result_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
