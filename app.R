#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(DBI)
library(RMySQL)

user_base <- dplyr::tibble(
    user = c("kanishka", "dakshinya"),
    password = c("kani", "dakshi"),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two")
)
makereactivetrigger <- function() {
    rv <- reactiveValues(a = 0)
    list(
        depend = function() {
            rv$a
            invisible()
        },
        trigger = function() {
            rv$a <- isolate(rv$a + 1)
        }
    )
}
dbtrigger <- makereactivetrigger()
con <- dbConnect(MySQL(), user = 'root', password = 'kanishka2002@@',
                 dbname = 'project', host = 'localhost')


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                shinyauthr::loginUI(id = "login"),div(class = "pull-right", shinyauthr::logoutUI(id = "logout"),
                                                      # Show a plot of the generated distribution
                                                      titlePanel("LIBRARY DATA MANAGEMENT"),
        mainPanel(
           img(src = "header.png", height = 200, width = 1700),
           
           tabsetPanel(type = "tabs",
                       tabPanel("HOME",fluid=TRUE,sidebarPanel(img(src = "e-library.png", height = 300, width = 300),h1("MISSION"),br(),
                                                               p("We develop, organize, provide access to and preserve materials to meet 
                                                                 the needs of present and future generations of students and scholars."),
                                                               p("The Libraries promote intellectual growth and creativity by developing collections through donations.")),    mainPanel(
                                                                   h2("ABOUT LIBRARY"),
                                                                   p("Library Management System allows the user to borrow and donate an e-book.
                                                                     It stores the details of all the books that are available in the library. "),br(),p("The Database Library System is 
                                                                                                                                                         intended to Automate the library 
                                                                                                                                                         activities such as creating a new borrower, 
                                                                                                                                                         giving books to the borrowers, maintaining the 
                                                                                                                                                         details of all the item that were available in the books ."),br(),
                                                                   p("The department of Library of Kanshi Institute of Technology is established in 1980. It is located next to the main building in a 
                                                                     vast area of 12000 sq.ft. (1302 sq.m.). The main objective of our library is to provide information services and access e-resources to support 
                                                                     the scholarly and informational needs of the institute community. The library is well equipped with modern facilities and resources in the form of books, 
                                                                     print and e-journals, CD-ROMs, on-line databases and audio video cassettes etc. Open Access System is being followed in the library."),br(),img(src = "kani.png", height = 250, width = 700)))),
                       tabPanel("BOOK LIST",fluid=TRUE,mainPanel(img(src = "book_list.png",
                                                                     height = 1130, width = 1100))),
                       tabPanel("BORROW BOOKS",fluid = TRUE,titlePanel("ENTER YOUR DETAILS :"),textInput("name", "NAME :"),textInput("id","BOOK ID :"),
                                numericInput("rn", "ROLL NUMBER :",value=0,min = 2000000, max = 3000000),textInput("dept", "DEPARTMENT :"),
                                numericInput("ph", "PHONE NUMBER :",value=0,min=1000000000,max=9999999999)
                                ,dateRangeInput("date", "BORROW DATE - DUE DATE"),
                                textAreaInput("address", "RESIDENTIAL ADDRESS :", rows = 3),fluidRow(
                                    actionButton("submit", "SUBMIT", class = "btn-lg btn-success"))), 
                                tabPanel("DONATE E-BOOKS",fluid=TRUE,titlePanel("E-BOOK DONATION"),textInput("name", "NAME :"),
                                         numericInput("rn", "ROLL NUMBER :",value="e",min = 2000000, max = 3000000),textInput("dept", "DEPARTMENT :"),
                                         numericInput("ph", "PHONE NUMBER :",value="e",min=1000000000,max=9999999999),textInput("bname","BOOK NAME")
                                         ,fileInput("book","UPLOAD THE E-BOOK"),fluidRow(
                                             actionButton("submit", "SUBMIT", class = "btn-lg btn-success"))),
                       tabPanel("LIST OF BOOKS",fluid=TRUE,mainPanel(img(src = "tab1.png",
                                                                     height = 500, width = 1000),img(src = "list2.png",
                                                                                                      height = 350, width = 1000))),
                       tabPanel("GALLERY",fluid=TRUE,mainPanel(img(src = "gallery1.png",
                                                                     height = 300, width = 1100),img(src = "gallery2.png",
                                                                                                      height = 300, width = 1100),img(src = "gallery3.png",
                                                                                                                                       height = 300, width = 1100))),
                       tabPanel("ANALYSIS",fluid=TRUE,mainPanel(img(src = "graph4.png",
                                                                         height = 500, width = 800),br(),img(src = "graph1.png",
                                                                                                         height = 500, width = 800),br(),img(src = "graph3.png",
                                                                                                                                         height = 500, width = 800),br(),img(src = "graph2.png",
                                                                                                                                                                         height = 500, width = 800))),
                       tabPanel("RULES",fluid=TRUE,mainPanel(
                           h1("RULES AND REGULATIONS"),br(),p("* Digital Library is to be used for academic/Research purposes only.",
                                                              br(),p("* Members must not to be sharing their net access ID and Password with other students. 
                                                                     If access other student by using your id and password then you will be responsible for the same."
                                                                     ,br(),p("* Changing the settings and display of the Computers kept in the Digital Library is not permitted."),br(),
                                                                     p("* Login I.D.'s, Passwords of various e-resources or online 
                                                                       resources will be communicated to the students and faculty members 
                                                                       from time to time, an access to those resources will be I.P. based which cannot be accessed beyond the boundaries of the College campus.")
                                                                     )))),
                           tabPanel("CONTACT",fluid=TRUE,mainPanel(img(src = "contactfinal.png",
                                                                         height = 500, width = 1100))))))
           
# Define server logic required to draw a histogram
server <- function(input, output) {
    credentials <- shinyauthr::loginServer(
        id = "login",
        data = user_base,
        user_col = user,
        pwd_col = password,
        log_out = reactive(logout_init()) 
    )
    
    # call the logout module with reactive trigger to hide/show
    logout_init <- shinyauthr::logoutServer(
        id = "logout",
        active = reactive(credentials()$user_auth)
    )
    mytableinshiny <- reactive({
        dbtrigger$depend()
        dbGetQuery(con, 'SELECT rn, ph from mytable')
    })
    observeEvent(input$writetodb, {
        sql1 = "INSERT INTO mytable (rn, ph) VALUES (?rn, ?ph)"
        sql <- sqlInterpolate(con, sql1, col1=input$col1, col2=input$col2)
        dbExecute(con, sql)
        dbtrigger$trigger()
    })
    output$dbtable <- renderTable({
        mytableinshiny()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
