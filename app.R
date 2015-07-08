#sociomatrixapp #1
# Example #2
library(shiny)

wom <- read.csv("WOM4.csv", header = T, strip.white = T) # remove extra spaces in the middle with strip.white
ns <- paste(as.character(wom$First.Name),as.character(wom$Last.Name))
names <- list(ns)
Collaborators <- as.character(wom$Collaborators)

# for all researchers, create a vector of all zeros, and change to a 1
# if they have collaborated with a researcher in the corresponding column
l <- list()
for (n in 1:66) {
  if (!is.na(Collaborators[n])) (Collaborators[[n]] <- strsplit(as.character(Collaborators[n]),", "))
  v <- vector("numeric",length = length(ns))
  for (m in 1:66) {
    if (ns[m]%in%Collaborators[[n]][[1]]) { #if the column researcher is a collaborator of the row researcher
      v[m] = 1
    }
  }
  l[[n]] <- v
}

# turn the list into a matrix
sociomatrix <- matrix(unlist(l),nrow = length(ns), ncol = length(ns), dimnames = names)
colnames(sociomatrix) <- ns

#ui
ui <- shinyUI(fluidPage(
    titlePanel("WOM Researchers and Collaborators"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("selection", 
                    label = "Choose a name to display",
                    choices = sort(rownames(sociomatrix)),
                    selected = "Alessandro Peluso")
        
      ),
      mainPanel(
        textOutput("text1"),
        textOutput("text2")
      )
  )
))

#server
server <- shinyServer(
  function(input, output) {
    output$text1 <- renderText({
      paste("The collaborators of", input$selection, "are:")
    })
    
    
    output$text2 <- renderText({
      paste(colnames(sociomatrix)[sociomatrix[input$selection,] == 1], ",")
    })
    
  })
shinyApp(ui = ui, server = server)
