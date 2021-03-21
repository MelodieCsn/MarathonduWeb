library(shinyjs)
library(dplyr)
library(shiny)
library(digest)
library(shinythemes)

fieldsMandatory <- c("name", "favourite_pkg")

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}

fieldsAll <- c("name", "favourite_pkg", "used_shiny", "r_num_years", "os_type")
responsesDir <- file.path("/M1MIASHS/MarathonduWeb/responses/")
epochTime <- function() {
    as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

#loady <- load(file = "test.txt")

appCSS <-
    ".mandatory_star { color: red; }"

shinyApp(
    ui = fluidPage(
        shinyjs::useShinyjs(),
        theme = shinytheme("cerulean"),
        shinyjs::inlineCSS(appCSS),
        titlePanel("Mimicking a Google Form with a Shiny app"),
        
        div(
            id = "form",
            
            textInput("name", labelMandatory("Name"), ""),
            textInput("favourite_pkg", labelMandatory("Favourite R package")),
            checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
            sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
            selectInput("os_type", "Operating system used most frequently",
                        c("",  "Windows", "Mac", "Linux")),
            actionButton("submit", "Submit", class = "btn-primary"),
            shinyjs::hidden(
                span(id = "submit_msg", "Submitting..."),
                div(id = "error",
                    div(br(), tags$b("Error: "), span(id = "error_msg"))
                )
            )
        ),
        div(id = "form"),
        shinyjs::hidden(
            div(
                id = "thankyou_msg",
                h3("Thanks, your response was submitted successfully!"),
                actionLink("submit_another", "Submit another response")
            )
        )
    ),
    server = function(input, output, session) {
        observe({
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        formData <- reactive({
            data <- sapply(fieldsAll, function(x) input[[x]])
            data <- c(data, timestamp = epochTime())
            data <- t(data)
            data
        })
        saveData <- function(data) {
            fileName <- sprintf("%s_%s.csv",
                                humanTime(),
                                digest::digest(data))
            #print(loady)
            
            write.csv(x = data, file = file.path(responsesDir, fileName),
                      row.names = FALSE, quote = TRUE)
        }
        
        # action to take when submit button is pressed
        observeEvent(input$submit, {
            shinyjs::disable("submit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            tryCatch({
                saveData(formData())
                shinyjs::reset("form")
                shinyjs::hide("form")
                shinyjs::show("thankyou_msg")
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("submit")
                shinyjs::hide("submit_msg")
            })
        })
        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
        })
    }
)