##############################################################################################################################
### KidneyGPS1.3.1 by Kira Stanzick                                                                                          #
### KidneyGPS 2.4.0 update by mathias Gorski                                                                                 #
###                                                                                                                          #
### This script contains the defintion of the user interface (UI) and the server part of KidneyGPS.                          #
###                                                                                                                          #
##############################################################################################################################

print(paste(date(),"entering app.R"))
source("data/packages.r")

load("data/workspace.RData")
print(paste(date(),"loaded workspace"))

print(paste(date(),"get the ui"))
source("ui.R")
print(paste(date(),"get the server"))
source("server.R")

# ui <- fct.generate.ui()
# print(class(ui))

# print("# setup the server")
# server <- fct.generate.server(input, output, session)
# print(class(server))

# start Shiny App:
shinyApp(ui = ui, server = server)

print("Exiting app.R")


# # delete existing workspace:
# # rm(list = ls(all = TRUE))

# source("data/packages.r")
# load("data/workspace.RData")
# # start Shiny App:
# shinyApp(ui = ui, server = server)
