## Can we use a Graph to simulate evolving stocks?

## A source can be considered infinite.
## A sink just as well.

## Then a stock in the middle can be modeled as a network node with a Queue or some other "carrying capacity".
## Note: Not the right wording, but this is me understanding myself... Sorry!

library(igraph) ## Might just use this one...
library(visNetwork)
library(ggraph)
library(dplyr) ## For summarize & al.

## But first, let's forget about a Graph and instead consider two queues and each one feedback of the other.
## i.e. Two Companies, each with its personnel count.
## One company "looses" employees faster than the other (say because it has bad culture)
## The other has bad hiring experience, and so people are less inclined to apply/get hired there.
## That's the gist of it. What would happen?

add_to_stock <- function(stock) { ## Yes, unnecessarily complicated, but I'm preparing for an eventual future.
  stock$current_stock <- stock$current_stock + 1
  stock
}

which_stock_needs_processing <- function(stocks) {
  which(sapply(stocks, function(x) ifelse(x$current_stock > 0, TRUE, FALSE)))
}

consume_from_population <- function(stocks, consumer_stock_num) {
  ## Add some level of randomness
  if(runif(1) > 0.4)
    if(stocks[[1]]$current_stock > 0) {
      consumable <- min(stocks[[1]]$current_stock, stocks[[consumer_stock_num]]$inflow)
      stocks[[1]]$current_stock <- stocks[[1]]$current_stock - consumable
      stocks[[consumer_stock_num]]$current_stock <- stocks[[consumer_stock_num]]$current_stock + consumable
    }
  stocks
}

loose_to_population <- function(stocks, consumer_stock_num) {
  if(runif(1) > 0.4)
    if(stocks[[consumer_stock_num]]$current_stock > 0) {
      producible <- min(stocks[[consumer_stock_num]]$current_stock, stocks[[consumer_stock_num]]$outflow)
      stocks[[consumer_stock_num]]$current_stock <- stocks[[consumer_stock_num]]$current_stock - producible
      stocks[[1]]$current_stock <- stocks[[1]]$current_stock + producible
    }
  stocks
}

add_stock <- function(current_stocks, 
                      stock_name = "stock", initial_stock=1, inflow=1, outflow=1) {
  new_stock <- list(stock_name = stock_name, current_stock = initial_stock, 
                    inflow = inflow, outflow = outflow)
  current_stocks[[length(current_stocks)+1]] <- new_stock
  current_stocks
}


layout <- NULL

rm(list=c("results_df"))


n_time_steps_vec <- c(100, 500, 1000, 5000, 10000)
for(n_time_steps in n_time_steps_vec) {
  for(i in 1:50) { ## Let's do this simulation a few times, shall we? MC-like
    
    stocks <- list()
    ## Let's work with two stocks
    ## First will be the unemployed population, with "big" capacity to consume/pour flows:
    stocks <- add_stock(stocks, stock_name = "population", initial_stock=100,
                        inflow=1000, outflow=1000) 
    
    stocks <- add_stock(stocks, stock_name = "company1", initial_stock=20,
                        inflow=3, outflow=2)
    stocks <- add_stock(stocks, stock_name = "company2", initial_stock=20,
                        inflow=7, outflow=3)
    
    # print(stocks)
    
    ## Now we want to model a dynamic between these stocks...
    ## Can we represent this as... A matrix maybe? Feels a bit similar to a graph's
    ## Adjacency matrix, doesn't it?
    ## Each stock is a node. The companies interact with the Population (first stock).
    
    ## So something like so:
    first_row <- c(0, ## Population with itself doesn't interact
                   stocks[[2]]$inflow, ## What population "feeds" as consumed by first company
                   stocks[[3]]$inflow) ## What population "feeds" as consumed by second company
    second_row <- c(stocks[[2]]$outflow,
                    0,
                    0) ## Company 1 doesn't interact "directly" with the other company
    
    third_row <- c(stocks[[3]]$outflow,
                   0, ## Company 2 doesn't interact "directly" with the other company
                   0) 
    
    graph_matrix <- matrix(c(first_row, second_row, third_row), byrow = T, nrow=3)
    graph_matrix
    g <- igraph::graph_from_adjacency_matrix(graph_matrix, 
                                             weighted=T)
    V(g)$name <- sapply(stocks, \(x) x$stock_name)
    V(g)$size <- sapply(stocks, \(x) x$current_stock)
    
    ## Consistent visualization of Graph...
    if(is.null(layout)) layout <- layout.fruchterman.reingold(g)
    # plot(g, layout=layout)
    
    ## OK, we have our current initial graph, now how is the passing of time then?
    
    for(current_time in 1:n_time_steps) {
      for(node_id in sample(2:3, 2, replace = F)) {
        stocks <- consume_from_population(stocks, node_id)
        stocks <- loose_to_population(stocks, node_id)
      }
      V(g)$size <- sapply(stocks, \(x) x$current_stock)
      
      print(paste0("time step: ", current_time, ":",
                   paste(sapply(stocks, \(x) {paste(x$stock_name, x$current_stock)}), collapse="; "))
      )
      plot(g, layout=layout,
           main=paste("Result distribution after", current_time, "time steps, Sim ", i)
      )
      Sys.sleep(0.25)
    }
    # Sys.sleep(0.25)
    # plot(g, layout=layout,
    #      main=paste("Result distribution after", n_time_steps, "time steps, Sim ", i)
    # )
    
    # t_res <- data.frame(n_time_steps = n_time_steps,
    #                     company1_final_size = stocks[[2]]$current_stock,
    #                     company2_final_size = stocks[[3]]$current_stock)
    # print(t_res)
    # print(exists("results_df"))
    # if(!exists("results_df")) {
    #   results_df <- t_res
    # } else {
    #   results_df <- rbind(results_df, t_res)
    # }
    #   
  }
}

dev.off()

par(mfrow=c(4, 1))

plot(1:50, results_df$company1_final_size[results_df$n_time_steps == 100],
     xlab="Simulation #",
     ylab="",
     ylim = c(0, 200),
     main="Observed number of Employees at Company 1 after 100 time-steps")
abline(h=20, col="green")
abline(h=mean(results_df$company1_final_size[results_df$n_time_steps == 100]), col="blue")

plot(results_df$n_time_steps, results_df$company1_final_size,
     main="Number of employees after n time steps with the same dynamic - 50 sims per max_time_steps",
     xlab="Time steps",
     ylab="Company 1 Employees count")

df_means <- results_df |> group_by(n_time_steps) |> summarize(mean=mean(company1_final_size))
lines(df_means$n_time_steps, df_means$mean, col="blue")
abline(h=20, col="green")


plot(1:50, results_df$company2_final_size[results_df$n_time_steps == 100],
     xlab="Simulation #",
     ylab="",
     ylim = c(0, 200),
     main="Observed number of Employees at Company 2 after 100 time-steps")
abline(h=20, col="green")
abline(h=mean(results_df$company2_final_size[results_df$n_time_steps == 100]), col="blue")

plot(results_df$n_time_steps, results_df$company2_final_size,
     main="Number of employees after n time steps with the same dynamic - 50 sims per max_time_steps",
     xlab="Time steps",
     ylab="Company 2 Employees count")
library(dplyr)
df_means <- results_df |> group_by(n_time_steps) |> summarize(mean=mean(company2_final_size))
lines(df_means$n_time_steps, df_means$mean, col="blue")
abline(h=20, col="green")


