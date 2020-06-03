
library(shiny)
library(ggplot2); theme_set(theme_grey(20))
library(plyr)
library(dplyr)


shinyServer(function(session, input, output){
  
  # create and initialize reactive stuff
  bag <- reactiveValues()
  
  hist_bins_rorsch <- reactive(input$hist_bins_rorsch)
  hist_bins_rorsch_both <- reactive(input$hist_bins_rorsch_both)
  hist_bins_lineup <- reactive(input$hist_bins_lineup)
  hist_bins_lineup_both <- reactive(input$hist_bins_lineup_both)
  d_hist_bins_rorsch <- debounce(hist_bins_rorsch, 1000)        # 1000 = 1 second delay
  d_hist_bins_rorsch_both <- debounce(hist_bins_rorsch_both, 1000)
  d_hist_bins_lineup <- debounce(hist_bins_lineup, 1000)
  d_hist_bins_lineup_both <- debounce(hist_bins_lineup_both, 1000)
  
  
  # function for density curves
  make_density_df <- function(df) {
    x_grid <- seq(min(df$Data), max(df$Data), length.out = 1e3)
    
    ddply(df, "samp", function(dframe) {
      data.frame(
        Data = x_grid,
        norm_density = dnorm(x_grid, mean(dframe$Data), sd(dframe$Data))
      )
    })
  }
  
  
  
  ##### rorschach plots
  #######################################################
  
  # generate data
  observeEvent(input$make_rorsch_plots, {

    if(input$pop_dist == "basic"){
      # pop. distribution to randomly sample from
      dist_random <- ifelse(input$pop_shape == "Normal", rnorm, rbeta)
      
      # parameters of pop. distribution
      params <- switch(input$pop_shape,
        "Normal" = c(0, 1),
        "Left Skewed" = c(5, 2),
        "Very Left Skewed" = c(10, 2),
        "Severely Left Skewed" = c(50, 2),
        "Right Skewed" = c(2, 5),
        "Very Right Skewed" = c(2, 10),
        "Severely Right Skewed" = c(2, 50)
      )
    } else {
      # pop. distribution to randomly sample from
      dist_random <- switch(input$pop_fam,
        "Normal" = rnorm, "t" = rt, "F" = rf, "Uniform" = runif, 
        "Chi-square" = rchisq, "Exponential" = rexp, "Gamma" = rgamma, 
        "Beta" = rbeta
      )
      
      # parameters of pop. distribution
      params <- switch(input$pop_fam,
        "Normal" = c(0, 1),
        "t" = input$t_df,
        "F" = c(input$f_df1, input$f_df2),
        "Uniform" = c(0, 1),
        "Chi-square" = input$chisq_df,
        "Exponential" = input$exp_rate,
        "Gamma" = c(input$gamma_shape, input$gamma_rate),
        "Beta" = c(input$beta_shape1, input$beta_shape2)
      )
    }
    
    
    ### randomly sample data
    ##################################
    samples_rorsch <- 1:input$n_plots_rorsch
    
    datasets <- if(input$pop_fam %in% c("t", "Chi-square", "Exponential")){
      lapply(seq_along(samples_rorsch), function(samples_rorsch) {
        data.frame(
          Data = dist_random(input$sample_size, params), 
          samp = rep(samples_rorsch, each = input$sample_size), 
          stringsAsFactors = FALSE)
      })
    } else {
      lapply(seq_along(samples_rorsch), function(samples_rorsch) {
        data.frame(
          Data = dist_random(input$sample_size, params[1], params[2]), 
          samp = rep(samples_rorsch, each = input$sample_size), 
          stringsAsFactors = FALSE)
      })
    }
    
    bag$all_data_rorsch <- do.call(rbind, datasets)

    
    ### make rorschach plots
    ##################################
    if(input$rorsch_plot_type == "rorsch_qq") {
      output$qq_rorsch <- renderPlot(
        ggplot(bag$all_data_rorsch, aes(sample = Data)) +
          stat_qq() +
          stat_qq_line() +
          facet_wrap(~ samp) +
          labs(x = "Theoretical Quantile", y = "Sample Quantile") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
      )
    } else if (input$rorsch_plot_type == "rorsch_hist") {
      output$hist_rorsch <- renderPlot({
        hist_r <- ggplot(bag$all_data_rorsch, aes(x = Data)) +
          geom_histogram(aes(y = stat(density)), bins = d_hist_bins_rorsch()) +
          facet_wrap(~ samp) +
          labs(x = "Data Value", y = "Density") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
        
        if(input$add_density_rorsch) {
          density_df <- make_density_df(bag$all_data_rorsch)
          
          hist_r <- hist_r +
            geom_line(aes(y = norm_density), data = density_df, 
              color = "red", lwd = 1.1
            )
        }
        
        hist_r
      })
    } else {
      output$qq_rorsch_both <- renderPlot(
        ggplot(bag$all_data_rorsch, aes(sample = Data)) +
          stat_qq() +
          stat_qq_line() +
          facet_wrap(~ samp) +
          labs(x = "Theoretical Quantile", y = "Sample Quantile") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
      )
      
      output$hist_rorsch_both <- renderPlot({
        hist_r <- ggplot(bag$all_data_rorsch, aes(x = Data)) +
          geom_histogram(aes(y = stat(density)), bins = d_hist_bins_rorsch_both()) +
          facet_wrap(~ samp) +
          labs(x = "Data Value", y = "Density") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
        
        if(input$add_density_rorsch_both) {
          density_df <- make_density_df(bag$all_data_rorsch)
          
          hist_r <- hist_r +
            geom_line(aes(y = norm_density), data = density_df, 
              color = "red", lwd = 1.1
            )
        }
        
        hist_r
      })
    }
  })
  
  
  observeEvent(input$data_file, {
      
    req(input$data_file)
      
    bag$user_data_df <- read.csv(input$data_file$datapath, 
      header = input$file_header, sep = input$file_separator
    )
    
    # remove non-numeric variables from uploaded dataset
    bag$user_data_df <- select_if(bag$user_data_df, is.numeric)

    updateSelectInput(session, "select_var", "Variable of Interest",
      choices = colnames(bag$user_data_df)
    )
  })
  

  ### print data in tabular form
  #################################################
  
  output$data_table <- renderTable({
    
    # store data either uploaded or manually input by user
    if(input$input_type == "upload") {
      
      # print data frame showing either all or only 10 rows
      if(input$file_display == "first_10") {
        return(head(bag$user_data_df, n = 10))
      } else {
        return(bag$user_data_df)
      }
    } else if(input$input_type == "manually") {
      
      # convert user-input data to numeric vector
      user_data <- input$user_data %>% 
        strsplit(split = "\n|,") %>%  # split if see comma or new line
        unlist() %>%
        gsub("[^0-9\\.]", "", .) %>%  # only keep integers and decimals
        as.numeric() %>%
        na.omit()    # remove any NAs
      
      (bag$user_data_df <- data.frame(Data = user_data))
    }
  })
  
  

  ##### lineup plots
  #######################################################
  
  observeEvent(input$make_lineup_plots, {
  
    if(input$input_type == "upload") {
      if(ncol(bag$user_data_df) > 1) {
        final_user_data <- data.frame(Data = bag$user_data_df[, input$select_var])
      } else {
        final_user_data <- data.frame(Data = bag$user_data_df[, 1])
      }
    } else if(input$input_type == "manually") {
      final_user_data <- bag$user_data_df
    }
    
    # count how many numbers input by user; excludes extra . (for missing data)
    #  and other non-numeric characters
    sample_size_user <- nrow(final_user_data)

    # randomly permute plot indices
    plot_spots <- sample(input$n_plots_lineup)

    # simulate data -- mean and SD are mean and SD of user's data
    datasets_lineup <- lapply(1:(input$n_plots_lineup - 1), function(Data) {
      data.frame(Data = rnorm(sample_size_user, mean(final_user_data$Data), 
        sd(final_user_data$Data)
      ))
    })
    sim_data_lineup <- do.call(rbind, datasets_lineup)
    

    # combine user data and simulated data
    bag$all_data_lineup <- rbind(final_user_data, sim_data_lineup)
    bag$all_data_lineup$samp <- rep(plot_spots, each = sample_size_user)
    
    

    ### make lineup plots
    ##################################
    if(input$lineup_plot_type == "lineup_qq") {
      output$qq_lineup <- renderPlot({
        ggplot(bag$all_data_lineup, aes(sample = Data)) +
          stat_qq() +
          stat_qq_line() +
          facet_wrap(~ samp) +
          labs(x = "Theoretical Quantile", y = "Sample Quantile") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
      })
    } else if (input$lineup_plot_type == "lineup_hist") {
      output$hist_lineup <- renderPlot({
        hist_l <- ggplot(bag$all_data_lineup, aes(x = Data)) +
          geom_histogram(aes(y = stat(density)), bins = d_hist_bins_lineup()) +
          facet_wrap(~ samp) +
          labs(x = "Data Value", y = "Density") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
        
        if(input$add_density_lineup) {
          density_df <- make_density_df(bag$all_data_lineup)
          
          hist_l <- hist_l +
            geom_line(aes(y = norm_density), data = density_df, 
                color = "red", lwd = 1.1
            )
        }
        
        hist_l
      })
    } else {
      output$qq_lineup_both <- renderPlot({
        ggplot(bag$all_data_lineup, aes(sample = Data)) +
          stat_qq() +
          stat_qq_line() +
          facet_wrap(~ samp) +
          labs(x = "Theoretical Quantile", y = "Sample Quantile") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
      })

      output$hist_lineup_both <- renderPlot({
        hist_l <- ggplot(bag$all_data_lineup, aes(x = Data)) +
          geom_histogram(aes(y = stat(density)), bins = d_hist_bins_lineup_both()) +
          facet_wrap(~ samp) +
          labs(x = "Data Value", y = "Density") +
          theme(strip.text.x = element_text(margin = margin(2, 0, 2, 0)))
        
        if(input$add_density_lineup_both) {
          density_df <- make_density_df(bag$all_data_lineup)
          
          hist_l <- hist_l +
            geom_line(aes(y = norm_density), data = density_df, 
              color = "red", lwd = 1.1
            )
        }
        
        hist_l
      })
    }
    
    
    # reset identify plot output
    output$user_plot_number <- renderText("")
  })
  

  # print plot number associated w/ user's data
  observeEvent(input$identify, {
    output$user_plot_number <- renderText(bag$all_data_lineup$samp[1])
  })
})



