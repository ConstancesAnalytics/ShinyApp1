## comments utilisateur2

## change comments

library(shiny)

shinyServer(function(input, output) {

  output$datatable1  <- DT::renderDataTable({
    ifelse(input$CES %in% levels(para_num$CESantenne),
      para_num_CESfilt <- para_num %>% filter(CESantenne == input$CES) %>% select(-CESantenne),
      para_num_CESfilt <- para_num %>% select(-CESantenne))
    data_sum <- as.data.frame(t(sapply(para_num_CESfilt, resumer)))
    DT::datatable(
      data_sum , options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  })

  output$datatable2  <- DT::renderDataTable({
    data_sum <- do.call(rbind, tapply(para_num[,input$variable], para_num$CESantenne, resumer))
    DT::datatable(
      data_sum , options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  })
  
  output$datatable3  <- DT::renderDataTable({
    data_sum2 <- do.call(rbind, tapply(para_num[,input$variable2], para_num$CESantenne, resumer_borne, vect = get(input$variable2, dict_para)))
    DT::datatable(
      data_sum2, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  })

})