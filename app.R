library(shiny)
library(plotly) 
library(quantmod)

# Stock symbols that will be used 
symbols <- c("AAPL", "MSFT", "AMZN", "NFLX", "GOOG")
symNames<- c("Apple","Microsoft", "Amazon", "Netflix", "Google")

# Get price history from yahoo using quantmod and save. Use previously saved data if available.
if (!file.exists("quantmod_prices.RData")) {
	getSymbols(Symbols = symbols, src="yahoo")
	ds <- data.frame(Date = index(MSFT), AAPL[,6], MSFT[,6], AMZN[,6], NFLX[,6], GOOG[,6])
	names(ds) <- c("Date", symNames)
	save(ds, file="quantmod_prices.RData")
} else {
	load("quantmod_prices.RData")
}


#### SHINY UI ####
ui <- pageWithSidebar(
	headerPanel("Tech Stock Comparator"),
	sidebarPanel(
		selectInput(inputId="sel_symbols", label="Select One or More Stocks", choices=symNames, multiple=TRUE),
		br(),br(),br(),br(),br(),br(),br(),
		radioButtons("stack_type", label="Stack or Overlay?", 
					 choices=c("Stack","Overlay"), selected="Stack", inline=TRUE),
		br(),br(), 
		actionButton("goButton", "Go!"),
		actionButton("help", "Show Help")
	), 
	mainPanel(plotlyOutput("myplot"))
)


####### SHINY SERVER #####
server <- function(input, output, session) {
	plot1 <- eventReactive(input$goButton, {   # Note: eventReactive automatically isolates all code in the block
		sel_symbols <- input$sel_symbols
		stack_type <- input$stack_type
		l <- length(sel_symbols) 
		if (is.null(l) | l=="" | l==0) 	return(NULL)    # return an empty plot if Go! pressed without selecting any stocks
		if (stack_type == "Overlay") {
			# Create a single plot and add_line() all selected price columns to it
			p <- plot_ly(ds, x = ~Date, mode="lines") %>% 
				layout(
					title = "Stock Prices",
					xaxis = list(rangeslider = list(type = "date")),
					yaxis = list(title = "Price")
				)
			for (sym in sel_symbols) {
				p <- add_lines(p, y = as.formula(paste0("~",sym)), name = sym) 
			}
			
		} else {      # stack_type is "Stack"
			# Create separate plots for all selected stocks and them stack them using shiny::subplot()
			sub_plots <- lapply(sel_symbols, function(sym) {
				plot_ly(ds, x = ~Date, y=as.formula(paste0("~",sym))) %>%
					add_lines(name = sym) %>% 
					layout(
						title = "Stock Prices",
						xaxis = list(rangeslider = list(type = "date")),
						yaxis = list(title = "Price (Log)")
					)
			})
			p <- subplot(sub_plots, nrows = length(sub_plots), shareX = TRUE, titleX = FALSE)
		}
		p     # return the plot 
	})

	output$myplot <- renderPlotly({
		p<- plot1() 
		if (is.null(p)) return() else p  # prevent failure if no stock was selected and plot1() is NULL

	})
	
	observeEvent(input$help,{
		showModal(modalDialog(
			title <- "Instructions - ", paste(
				"1. Choose one or more stocks",
				"2. Select the type of graph",
				"3. Hit Go to see the plot",
				"4. Slide the x-axis sliders to change date range",
				sep="     "),
			easyClose=TRUE
		))	
	})
}

shinyApp(ui, server)
