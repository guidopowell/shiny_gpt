if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr,stringr,readxl,dplyr,xlsx,shiny,shinythemes)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("GPT API Interface"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = c("xlsx")),
      textInput("id_var", "Nom de la colonne d'identifiant", value = ""),
      textInput("contenu_var", "Nom de la colonne de contenu", value = ""),
      textInput("api_key", "Clé API", value = ""),
      selectInput("model", "Model", choices = c("gpt-4", "gpt-3.5-turbo"), selected = "gpt-4"),
      sliderInput("temperature", "Temperature", value = 0,min = 0, max=1, step = .1,width="50%"),
      textAreaInput("message_system", "Message de système (contexte à assigner)", 
                    placeholder = "Tu es un expert de revue de littérature. Tu détecte la pertinence d'un article à partir du sommaire...",value = "",height="150px", resize="vertical"),
      textAreaInput("message_user_debut", "Messager usager (petit prompt avant le contenu)",
                    placeholder = "Voici un sommaire: ",value = "", height="50px", resize="vertical"),
      actionButton("submit", "Soumettre"),
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

server <- function(input, output) {
  
  result_data <- eventReactive(input$submit, {
    req(input$file)
    
    data <- read.xlsx(input$file$datapath,sheetIndex = 1)
    
    gpt_api<-function(data, id_var, contenu_var, api_key,model,temperature,message_system,message_user_debut){
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions", 
        add_headers(Authorization = paste("Bearer", api_key)),content_type_json(),encode = "json",
        body = list(
          model = model,
          messages = list(list(role = "system", 
                               content = message_system),
                          list(role = "user", 
                               content = paste(message_user_debut,"\n",
                                               data[[contenu_var]]))),
          temperature=temperature))
      data.frame(ID=data[[id_var]],Contenu=data[[contenu_var]],
                 reponse_gpt=ifelse(response$status==200, 
                                    unlist(content(response)$choices[[1]]$message$content),NA))}
    
    withProgress(message = 'En cours', value = 0, {
      results <- lapply(1:nrow(data), function(x) {
        
        incProgress(1/nrow(data))
        
        gpt_api(data = data[x,]
                ,id_var = input$id_var
                ,contenu_var = input$contenu_var
                ,model = input$model
                ,temperature = input$temperature
                ,api_key = input$api_key
                ,message_system = input$message_system
                ,message_user_debut = input$message_user_debut
        )
      }) %>% bind_rows
    })
    
    colnames(results)<-c(input$id_var,input$contenu_var,"output_gpt")
    
    results
    
  }, ignoreNULL = FALSE)
  
  output$result <- renderTable({
    req(result_data())
    result_data()
  }, rownames = TRUE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.time(), ".xlsx", sep="")
    },
    content = function(file) {
      xlsx::write.xlsx(result_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
