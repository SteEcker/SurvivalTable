##########################################################
##
##                  Module UI 
##
##########################################################
tableModuleUI <- function(id){
  
  # set namespace via id
  ns <- NS(id)
  
  tagList(
    
    fluidPage(
      fluidRow(
        bs4Card(collapsible = F,
                width = 10,
                bs4Card(collapsible = F,
                        width = 12,
                        h2('Choose timepoint [months]'),
                        sliderInput(ns('slider'), width = '200%',
                                    label = NULL,
                                    max = 60,
                                    min = 3,
                                    value = 12, 
                                    post = ' months', 
                                    step = 1

                        )
                        ),
                
                reactableOutput(outputId = ns("table"))

                )
              )
    )
    
  )
}

##########################################################
##
##                  Module Server
##
##########################################################
tableModule <- function(input, output, session){
  
  ##
  ## ============================================ Init
  ##
  ns <- session$ns
  survival_df <- prepareData()
  colorII <- "rgba(231, 243, 251, 0.3)" #background color

  
  
  
  # Build data frame for table
  outcome_table <- reactive({
    tryCatch({
      buildSurvivalTable(survival_df, input$slider)
    },
      error = function(cond) {
        return(NA)
      }
    )
  })
  
  ##
  ## ============================================ KM Plots
  ##
  output$plot1 <- renderPlot({ kaplanMeierPlot(survival_df, timeptn = input$slider, endpoint = 'Local Control') })
  output$plot2 <- renderPlot({ kaplanMeierPlot(survival_df, timeptn = input$slider, endpoint = 'Survival') })
  output$plot3 <- renderPlot({ kaplanMeierPlot(survival_df, timeptn = input$slider, endpoint = 'Cancer specific survival') })

  
  ##
  ## ============================================ Start table
  ##
  
  output$table <- renderReactable({
    
    # Check if enough patients are available for selected timepoint
    validate(
      need(!is.na(outcome_table()), "Sorry, I could not find enough data for this timeframe.")
    )
    
    reactable(
      data = outcome_table(),
      
      pagination = F,
      sortable = F,
      highlight = TRUE,
      onClick = "expand",
      
      details = function(index) {
        if (index == 1) {
             plotOutput(ns("plot1"))}else
        if (index == 6) {
              plotOutput(ns("plot2"))}else
        if (index == 9) {
              plotOutput(ns("plot3"))}
      },
      
      defaultColDef = colDef(format = colFormat(digits = 0, percent = T )),
      
      
      columns = list(
        "Number at risk" = colDef(format = colFormat(percent = F), style = list(background = colorII), headerStyle = list(background = colorII)),
        "NWTCO 4" = colDef(
          headerStyle = list(background = colorII),
          style =
            JS("
                function(rowInfo, colInfo, state) {
                   if (rowInfo.row['NWTCO 4'] >= rowInfo.row['Reference Study']){
                      return { backgroundColor: 'rgba(231, 243, 251, 0.3)', fontWeight: 'bold', color: '#008000'  }
                   } else {
                    return{backgroundColor: 'rgba(231, 243, 251, 0.3)' }
                    }
                }
              "
            )

        ),

      Group = colDef(
          style =
            JS(
              "
                function(rowInfo, colInfo, state) {
                   if (rowInfo.row['NWTCO 4'] >= rowInfo.row['Reference Study']){
                      return { fontWeight: 'bold', color: '#008000'  }
                   }
                }
              "
            )

        ),

        "Lower CI" = colDef(style = list(background = colorII), headerStyle = list(background = colorII)),
        "Upper CI" = colDef(style = list(background = colorII), headerStyle = list(background = colorII)),



      Endpoint = colDef(
          style = JS(
                "
            function(rowInfo, colInfo, state) {
              var firstSorted = state.sorted[0]
              if (!firstSorted || firstSorted.id === 'Endpoint') {
                var prevRow = state.pageRows[rowInfo.viewIndex - 1]
                if (prevRow && rowInfo.row['Endpoint'] === prevRow['Endpoint']) {
                  return { visibility: 'hidden' }
                }
              }
            }"
          )
        )
      ),

      borderless = TRUE,
      rowStyle = JS(
                    "
                      function(rowInfo, state) {
                        var firstSorted = state.sorted[0]
                          var nextRow = state.pageRows[rowInfo.viewIndex + 1]
                          if (nextRow && rowInfo.row['Endpoint'] !== nextRow['Endpoint']) {
                            return { boxShadow: 'inset 0 -2px 0 rgba(0, 0, 0, 0.1)' }
                          }
  
                      }
                    "
                    ),


      class = 'reactableTable'
    )



    }

  )
  ##
  ## ============================================ END Table
  ##
  
}







##########################################################
##
##                  Helper functions 
##
##########################################################

# Sorry I did not have time to document the code, but you'll figure it out :)

survTab <- 
  function(.data, timepoint, endpoint, study_name) { 
    .data %>% 
      group_by(Group) %>% 
      nest() %>% 
      mutate(model = map(data, ~summary(survfit(Surv(.$time, .$status) ~ 1), times = !!timepoint ) )  ) %>% 
      
      transmute(!!study_name := map_dbl(model, ~.$surv) ,
                "Lower CI" = map_dbl(model, ~.$lower),
                "Upper CI" = map_dbl(model, ~.$upper),
                "Number at risk" = map_dbl(model, ~.$n.risk)
      ) %>% 
      mutate("Endpoint" = !!endpoint) %>% 
      select(Endpoint, everything()) %>% 
      ungroup()

  }

addSurvivalGroup <- function(survival_data = NA,
                             grp = NA, 
                             timepoint = NA, 
                             endpoint = "status") {
  
  result <- tryCatch(
    {
      df1 <- survival_data$data[[2]] %>% rename("status" = endpoint)
      
      overall <- df1 %>% 
        mutate(Group = "Overall") %>% 
        survTab(timepoint, endpoint, "NWTCO 4")
      
      by_group <- df1 %>% 
        rename(Group = grp) %>%
        survTab(timepoint, endpoint, "NWTCO 4")

      s1 <- bind_rows(
        overall,
        by_group
      )
      
      df2 <- survival_data$data[[1]] %>% rename("status" = endpoint)
      
      overall <- df2 %>% 
        mutate(Group = "Overall") %>% 
        survTab(timepoint, endpoint, "Reference Study")
      
      by_group <- df2 %>% 
        rename(Group = grp) %>%
        survTab(timepoint, endpoint, "Reference Study")
      
      s2 <- bind_rows(
        overall,
        by_group
      )
      
      
      s1 %>% full_join(s2 %>% select(Endpoint, Group, `Reference Study`), by = c("Endpoint", "Group")) %>% select(Endpoint, Group, `Reference Study`, everything())
      
    }, 
      error=function(cond){
        return(NA)
      }
  )
  
  
    
  return(result)
  
}


prepareData <- function(){
  
  return(
    as_tibble(nwtco) %>% 
    filter(edrel < 2000) %>% 
    mutate(age = if_else(age/12 >= 5, "Age > 5 years", "Age < 5 years") ) %>% 
    mutate(stage = factor(.$stage, labels = c("Stage I", "Stage II", "Stage III", "Stage IV"))) %>% 
    mutate(across(c("instit", "histol"), ~factor(., labels = c("non-smoker", "smoker"))) ) %>% 
    mutate(time = edrel/30) %>% 
    mutate("Local Control" = rel) %>% 
      
    # Create fake data for illustration purpose
    rowwise() %>% 
    mutate("Local Control" = if_else(study == 3 & time < 40 & runif(1) > 0.2 & stage %in% c("Stage II", "Stage IV"), 0, as.numeric(rel)) ) %>% 
    mutate("Survival" = if_else(study == 3 & time < 10 & runif(1) > 0.2, 0, as.numeric(rel)) ) %>% 
    mutate("Cancer specific survival" = if_else(study == 3 & time < 20 & runif(1) > 0.3, 0, as.numeric(rel)) ) %>% 
    ungroup() %>% 

    group_by(study) %>% 
    nest()
  )
  
}




buildSurvivalTable <- function(survival_df, timeptn) {
  bind_rows(
    addSurvivalGroup(survival_df, endpoint = "Local Control", grp = "stage", timepoint = timeptn ),
    addSurvivalGroup(survival_df, endpoint = "Survival", grp = "histol", timepoint = timeptn ),
    addSurvivalGroup(survival_df, endpoint = "Cancer specific survival", grp = "age", timepoint = timeptn )
  )
}



kaplanMeierPlot <- function(survival_df, timeptn, endpoint){
  surv_formula <- as.formula(paste0('~survfit( Surv(time, `', {{endpoint}},'`) ~ 1, data = .)' )  )
  
  df <- survival_df %>% mutate(model = map(data, surv_formula))
  fit <- list(NWTCO4 = df$model[[2]], Reference = df$model[[1]])
  p <- ggsurvplot_combine(fit, plot.df, conf.int = T)
  
  return(
    p$plot + geom_vline(
      xintercept = timeptn,
      colour = "red",
      linetype = "longdash",
      alpha = 0.5
    ) + theme_bw() + xlab("Time [months]") + geom_text(
      aes(
        x = timeptn,
        y = 0.05,
        label = paste0(timeptn, " months")
      ),
      color = "#FF9999",
      nudge_x = 2
    )
  )

}

