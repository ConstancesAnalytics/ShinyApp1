library(shiny)

shinyUI(navbarPage(
    "Dashboard-like navbarPage",
    tabPanel("Panel1",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 selectInput("CES", "CES", choices = c(levels(para_num$CES), "All"), selected = "All"),
                 width = 2
               ),
               mainPanel(DT::dataTableOutput('datatable1'))
             ))
    ),
    tabPanel("Panel2",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "variable", choices = all.list_num[all.list_num != "CESantenne"], selected = all.list_num[1]),
                 width = 2
               ),
               mainPanel(DT::dataTableOutput('datatable2'))
             ))
    ),
    tabPanel("Panel3",
             fluidPage(sidebarLayout(
               sidebarPanel(
                 selectInput("variable2", "variable2", choices = all.list_num[all.list_num != "CESantenne"], selected = all.list_num[1]),
                 width = 2
               ),
               mainPanel(DT::dataTableOutput('datatable3'))
             ))
    )
  )
)


# -----------------------------------
# 
# library(shiny)
# shinyUI(fluidPage(
#   titlePanel("test"),
#   sidebarLayout(
#     sidebarPanel(
#       conditionalPanel(
#         selectInput("CES", "CES", choices = c(levels(para_num$CES), "All"), selected = "All"),
#         width = 2
#       ),
#       conditionalPanel(
#         selectInput("variable", "variable", choices = all.list_num, selected = all.list_num[1]),
#         width = 2
#       )
#     ), 
#     mainPanel(
#       tabsetPanel(
#         tabPanel("CES", DT::dataTableOutput('datatable1')),
#         tabPanel("variable",  DT::dataTableOutput('datatable2'))
#       ),
#       width = 10
#     )
#   )
# ))
# 



