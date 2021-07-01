# This shiny app is intended to compare the offer made by a sportsbook to cash out a bet
# I would expect that the kelly estimated expected growth figure is always higher than what the sportsbook would offer, since the sportsbook wants to make a profit

library(tidyverse)
library(shiny)

app = shinyApp(ui = fluidPage(title = "Sportsbook Buyout Comparison",
                              
                              # inputs
                              numericInput("br", "Bankroll", value = 1),
                              numericInput("offer", "Offer from sportsbook", value = 1),
                              numericInput("ib", "Initial Bet", value = 1),
                              numericInput("odds", "American Odds of Initial Bet", value = 1),
                              numericInput("estprob", "Estimated Probability", value = 0.5, min = 0, max = 1),
                              
                              # return
                              h4("Guarateed Bankroll Growth: ", textOutput("sports_book_br_growth")),
                              h4("Kelly Estimated Bankroll Growth: ", textOutput("kelly_br_growth"))),
               server = function(input, output){
                 output$sports_book_br_growth = renderText({
                   round((input$offer - input$ib)/input$br + 1,4)
                 })
                 output$kelly_br_growth = renderText({
                   convert_amer_odds = if_else(input$odds > 0, input$odds / 100 + 1, 1 - 100/input$odds)
                   estimated_loss = 1 - input$estprob
                   br_size = ((convert_amer_odds)*input$estprob - estimated_loss)/(convert_amer_odds) # kelly formula
                   round(exp(estimated_loss*log(1-br_size) + input$estprob*log(1+(convert_amer_odds-1)*br_size)),4)
                 })
               })
runApp(app)                  


