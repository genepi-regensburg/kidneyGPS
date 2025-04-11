print(paste(date(),"entering global.R"))

print(paste(date(),"load libraries"))
source("data/packages.r")

print(paste(date(),"load workspace"))
load("data/workspace.RData")
print(paste(date(),"loaded workspace"))

print(paste(date(),"get the ui"))
source("ui.R")
print(paste(date(),"get the server"))
source("server.R")

# start Shiny App is not necessary (does the shiny envirnoment for you)
# shinyApp(ui = ui, server = server)

print(paste(date(),"Exiting global.R"))
