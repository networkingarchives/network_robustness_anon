

library(shiny)
library(tidyverse)
library(igraph)
library(ggraph)
library(plotly)
library(reader)
options(shiny.maxRequestSize = 30*1024^2)


ui <- fluidPage(
    
    # Application title
    titlePanel("Network Robustness Tool"),
    
    
    sidebarLayout(
        sidebarPanel(p("Upload an edge list (TSV or CSV) to calculate robustness scores. The algorithm removes progressively larger random samples of edges and compares the full ranked metric those calculated using the partial network. All calculations are on an unweighted network"),
            fileInput("file1", "Upload edge list"),
            actionButton('generate', 'Calculate Robustness'),
            selectInput('type', 'Select type', choices = c('degree - total', 'betweenness', 'eigen', 'close', 'degree - in', 'degree - out')),
            textInput('sims', 'Number of simulations', value = '100')
        ),
        
        
        mainPanel(
            plotlyOutput("robustnessPlot")
        )
    )
)

server <- function(input, output, session) {
    
    
    network_filtered = reactive({ 
        
        file <- input$file1
        
        likely_delim = get.delim(file$datapath, n = 20, delims = c(',', '\t'))
        a = read_delim(file$datapath, delim = likely_delim, col_names = T) %>% filter(!is.na(.[1])) %>% filter(!is.na(.[2]))
        
        
        a   
        
    })
    

    
    
    network = eventReactive(input$generate,{ 
        
        
        
        graphmatr = graph_from_data_frame(network_filtered() %>%
                                              distinct(.[1], .[2]), directed = T)
        
        if(input$type == 'degree - total') {
            bw.g <- igraph::degree(graphmatr, mode = 'total')}
        
        else if(input$type == 'degree - in'){
            bw.g <- igraph::degree(graphmatr, mode = 'in')
        }
        else if(input$type == 'degree - out'){
            bw.g <- igraph::degree(graphmatr, mode = 'out')
        }
        else if(input$type == 'betweenness'){
            bw.g <- igraph::betweenness(graphmatr)
        }
        else if(input$type == 'eigen'){
            bw.g <- igraph::eigen_centrality(graphmatr)['vector'][[1]]
        }
        else if(input$type == 'close'){
            bw.g <- igraph::closeness(graphmatr, mode = 'total')
        }
        
        nsim <- as.numeric(input$sims)
        
        mat <- matrix(NA, nsim, 99)  # create matrix for output and then name
        colnames(mat) <- paste0("S", 1:99)
        withProgress(message = 'Calculating Scores', value = 0, {
            
            for(j in 1:99){
                
                
                incProgress(1/99, detail = paste("sample", j, " of 99"))
                
                
                
                for(i in 1:nsim){
                    
               
                        
                        sample_edges = network_filtered() %>% 
                            sample_frac((100-j)/100, replace = F) %>%
                            distinct(.[1], .[2])
 
                    
                    
                    sub.graphmatr = graph_from_data_frame(sample_edges)
                    if(input$type == 'degree - total'){
                        temp.stats = igraph::degree(sub.graphmatr, mode = 'total') %>% tibble::enframe()
                    }
                    else if(input$type == 'degree - in'){
                        temp.stats = igraph::degree(sub.graphmatr, mode = 'in') %>% tibble::enframe()
                    }
                    else if(input$type == 'degree - out'){
                        temp.stats = igraph::degree(sub.graphmatr, mode = 'out') %>% tibble::enframe()
                    }
                    else if(input$type == 'betweenness'){
                        temp.stats = igraph::betweenness(sub.graphmatr) %>% tibble::enframe()
                    }
                    else if(input$type == 'eigen'){
                        temp.stats = igraph::eigen_centrality(sub.graphmatr)['vector'][[1]] %>% tibble::enframe()
                    }
                    else if(input$type == 'close'){
                        temp.stats = igraph::closeness(sub.graphmatr, mode = 'total') %>% tibble::enframe()
                    }
                    
                    
                    temp.stats$full = bw.g[temp.stats$name]
                    
                    mat[i, j] = suppressWarnings(cor(temp.stats$value, temp.stats$full,
                                                     method = "spearman"))
                    
                }
                
                
            }
            
            
        })
        mat
        
    })
    
    
    output$robustnessPlot = renderPlotly({
        
        p =  network() %>% 
            as_tibble() %>% 
            mutate(instance = 1:nrow(network())) %>% 
            pivot_longer(names_to = 'percent_out', values_to = 'value', cols = 1:99) %>% 
            mutate(percent_out = str_remove(percent_out, "S")) %>%  
            mutate(percent_out = as.numeric(percent_out)) %>% 
            ggplot() + geom_line(aes(x = percent_out, y = value, group = instance), alpha = .1) + 
            theme_minimal() + labs(x = 'Percentage Removed', y = 'Spearmans Rho') + 
            coord_cartesian(ylim = c(0,1))
        
        plotly::ggplotly(p)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
