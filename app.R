library(shiny);library(data.table);library(DT);library(jstable);library(shinycustomloader);library(survival)
#library(epiDisplay)
library(ggplot2);library(GGally);library(jsmodule);library(jskm);library(forestplot)
options(shiny.sanitize.errors = F, shiny.maxRequestSize = 5*1024^2)
nfactor.limit <- 20
library(survC1)
source("global.R");library(shinymanager)

#setwd("~/ShinyApps/sudang_/HT")
#library(showtext)
#showtext_opts(dpi = 90)                                                ## same to rmd chunk
#font_add("NanumGothic", "/usr/share/fonts/truetype/nanum/NanumGothic.ttf")
#showtext_auto()


ui <- navbarPage("KTBC_EpidemiologicalSpectrum",
                 tabPanel("Data", icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("binary_check"),
                              uiOutput("binary_var"),
                              uiOutput("binary_val"),
                              uiOutput("ref_check"),
                              uiOutput("ref_var"),
                              uiOutput("ref_val"),
                              uiOutput("subset_check"),
                              uiOutput("subset_var"),
                              uiOutput("subset_val")
                            ),
                            mainPanel(
                              tabsetPanel(type = "pills",
                                          tabPanel("Data", withLoader(DTOutput("data"), type="html", loader="loader6")),
                                          tabPanel("Label", withLoader(DTOutput("data_label", width = "100%"), type="html", loader="loader6"))
                              )
                            )
                          )
                 ),
                 tabPanel("Table 1", icon = icon("percentage"),
                          sidebarLayout(
                            sidebarPanel(
                              tb1moduleUI("tb1")
                            ),
                            mainPanel(
                              withLoader(DTOutput("table1"), type="html", loader="loader6"),
                              wellPanel(
                                h5("Normal continuous variables  are summarized with Mean (SD) and t-test(2 groups) or ANOVA(> 2 groups)"),
                                h5("Non-normal continuous variables are summarized with median [IQR or min,max] and kruskal-wallis test"),
                                h5("Categorical variables  are summarized with table")
                              )
                            )
                          )
                          
                 ),
                 tabPanel("Cox model",
                          sidebarLayout(
                            sidebarPanel(
                              coxUI("cox")
                            ),
                            mainPanel(
                              withLoader(DTOutput("coxtable"), type="html", loader="loader6")
                            )
                          )
                 ),
                 navbarMenu("Plot", icon = icon("bar-chart-o"),
                            tabPanel("Kaplan-meier plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         kaplanUI("kaplan")
                                       ),
                                       mainPanel(
                                         optionUI("kaplan"),
                                         withLoader(plotOutput("kaplan_plot"), type="html", loader="loader6"),
                                         ggplotdownUI("kaplan")
                                       )
                                     )
                            ),
                            tabPanel("Scatterplot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         scatterUI("scatter")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("scatter_plot"), type="html", loader="loader6"),
                                         ggplotdownUI("scatter")
                                       )
                                     )
                            ),
                            tabPanel("Boxplot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         boxUI("box")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("box_plot"), type="html", loader="loader6"),
                                         ggplotdownUI("box")
                                       )
                                     )
                            ),
                            tabPanel("Barplot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         barUI("bar")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("bar_plot"), type="html", loader="loader6"),
                                         ggplotdownUI("bar")
                                       )
                                     )
                            ),
                            tabPanel("Lineplot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         lineUI("line")
                                       ),
                                       mainPanel(
                                         withLoader(plotOutput("line_plot"), type="html", loader="loader6"),
                                         ggplotdownUI("line")
                                       )
                                     )
                            )
                            
                 )
                 
)


server <- function(input, output, session) {
  

  
  output$binary_check <- renderUI({
    checkboxInput("check_binary", "Make binary variables")
  })
  
  output$ref_check <- renderUI({
    checkboxInput("check_ref", "Change reference of categorical variables")
  })
  
  
  output$subset_check <- renderUI({
    checkboxInput("check_subset", "Subset data")
  })
  
  
  observeEvent(input$check_binary, {
    var.conti <- setdiff(names(out), factor_vars)
    output$binary_var <- renderUI({
      req(input$check_binary == T)
      selectInput("var_binary", "Variables to dichotomize",
                  choices = var.conti, multiple = T,
                  selected = var.conti[1])
    })
    
    output$binary_val <- renderUI({
      req(input$check_binary == T)
      req(length(input$var_binary) > 0)
      outUI <- tagList()
      for (v in seq_along(input$var_binary)){
        med <- stats::quantile(out[[input$var_binary[[v]]]], c(0.05, 0.5, 0.95), na.rm = T)
        outUI[[v]] <- splitLayout(cellWidths = c("25%", "75%"),
                                  selectInput(paste0("con_binary", v), paste0("Define reference:"),
                                              choices = c("\u2264", "\u2265", "\u003c", "\u003e"), selected = "\u2264"
                                  ),
                                  numericInput(paste0("cut_binary", v), input$var_binary[[v]],
                                               value = med[2], min = med[1], max = med[3]
                                  )
        )
        
      }
      outUI
      
    })
  })
  
  
  observeEvent(input$check_ref, {
    var.factor <- factor_vars
    output$ref_var <- renderUI({
      req(input$check_ref == T)
      selectInput("var_ref", "Variables to change reference",
                  choices = var.factor, multiple = T,
                  selected = var.factor[1])
    })
    
    output$ref_val <- renderUI({
      req(input$check_ref == T)
      req(length(input$var_ref) > 0)
      outUI <- tagList()
      for (v in seq_along(input$var_ref)){
        outUI[[v]] <- selectInput(paste0("con_ref", v), paste0("Reference: ", input$var_ref[[v]]),
                                  choices = levels(factor(out[[input$var_ref[[v]]]])), selected = levels(factor(out[[input$var_ref[[v]]]]))[2])
        
      }
      outUI
      
    })
  })
  
  observeEvent(input$check_subset, {
    output$subset_var <- renderUI({
      req(input$check_subset == T)
      #factor_subset <- c(data.list$factor_original, input$factor_vname)
      
      #validate(
      #  need(length(factor_subset) > 0 , "No factor variable for subsetting")
      #)
      
      tagList(
        selectInput("var_subset", "Subset variables",
                    choices = names(out), multiple = T,
                    selected = names(out)[1])
      )
    })
    
    output$subset_val <- renderUI({
      req(input$check_subset == T)
      req(length(input$var_subset) > 0)
      var.factor <- factor_vars
      
      outUI <- tagList()
      
      for (v in seq_along(input$var_subset)){
        if (input$var_subset[[v]] %in% var.factor){
          varlevel <- levels(as.factor(out[[input$var_subset[[v]]]]))
          outUI[[v]] <- selectInput(paste0("val_subset", v), paste0("Subset value: ", input$var_subset[[v]]),
                                    choices = varlevel, multiple = T,
                                    selected = varlevel[1])
        } else{
          val <- stats::quantile(out[[input$var_subset[[v]]]], na.rm = T)
          outUI[[v]] <- sliderInput(paste0("val_subset", v), paste0("Subset range: ", input$var_subset[[v]]),
                                    min = val[1], max = val[5],
                                    value = c(val[2], val[4]))
        }
        
      }
      outUI
    })
  })
  
  data.info <- reactive({
    out1 <- out[, .SD]
    out1[, (conti_vars) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = conti_vars]
    
    out.label1 <- out.label[, .SD]
    #out.label[, var_label := ref[out.label$variable, name.old]]
    
    req(!is.null(input$check_binary))
    if (input$check_binary == T){
      validate(
        need(length(input$var_binary) > 0 , "No variables to dichotomize")
      )
      sym.ineq <- c("\u2264", "\u2265", "\u003c", "\u003e")
      names(sym.ineq) <- sym.ineq[4:1]
      sym.ineq2 <- c("le", "ge", "l", "g")
      names(sym.ineq2) <- sym.ineq
      for (v in seq_along(input$var_binary)){
        req(input[[paste0("con_binary", v)]])
        req(input[[paste0("cut_binary", v)]])
        if (input[[paste0("con_binary", v)]] == "\u2264"){
          out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) <= input[[paste0("cut_binary", v)]]))]
        } else if (input[[paste0("con_binary", v)]] == "\u2265"){
          out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) >= input[[paste0("cut_binary", v)]]))]
        } else if (input[[paste0("con_binary", v)]] == "\u003c"){
          out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) < input[[paste0("cut_binary", v)]]))]
        } else{
          out1[, BinaryGroupRandom := factor(1 - as.integer(get(input$var_binary[[v]]) > input[[paste0("cut_binary", v)]]))]
        }
        
        cn.new <- paste0(input$var_binary[[v]], "_group_", sym.ineq2[input[[paste0("con_binary", v)]]], input[[paste0("cut_binary", v)]])
        data.table::setnames(out1, "BinaryGroupRandom", cn.new)
        
        label.binary <- mk.lev(out1[, .SD, .SDcols = cn.new])
        label.binary[, var_label := paste0(input$var_binary[[v]], " _group")]
        label.binary[, val_label := paste0(c(input[[paste0("con_binary", v)]], sym.ineq[input[[paste0("con_binary", v)]]]), " ", input[[paste0("cut_binary", v)]])]
        out.label1 <- rbind(out.label1, label.binary)
      }
      
    }
    
    if (!is.null(input$check_ref)){
      if (input$check_ref){
        validate(
          need(length(input$var_ref) > 0 , "No variables to change reference")
        )
        for (v in seq_along(input$var_ref)){
          req(input[[paste0("con_ref", v)]])
          out1[[input$var_ref[[v]]]] <- stats::relevel(out1[[input$var_ref[[v]]]], ref = input[[paste0("con_ref", v)]])
          out.label1[variable == input$var_ref[[v]], ':='(level = levels(out1[[input$var_ref[[v]]]]), val_label = levels(out1[[input$var_ref[[v]]]]))]
        }
        
      }
    }
    
    
    if (!is.null(input$check_subset)){
      if (input$check_subset){
        validate(
          need(length(input$var_subset) > 0 , "No variables for subsetting"),
          need(all(sapply(1:length(input$var_subset), function(x){length(input[[paste0("val_subset", x)]])})), "No value for subsetting")
        )
        var.factor <- factor_vars
        #var.conti <- setdiff(data()$conti_original, input$factor_vname)
        
        for (v in seq_along(input$var_subset)){
          if (input$var_subset[[v]] %in% var.factor){
            out1 <- out1[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
            out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]
            data.table::setkey(out.label1, "variable", "class", "level")
            data.table::setkey(out.label2, "variable", "class", "level")
            out.label1 <- out.label1[out.label2]
          } else{
            out1 <- out1[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]
            #var.factor <- c(data()$factor_original, input$factor_vname)
            out1[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
            out.label2 <- mk.lev(out1)[, c("variable", "class", "level")]
            data.table::setkey(out.label1, "variable", "class", "level")
            data.table::setkey(out.label2, "variable", "class", "level")
            out.label1 <- out.label1[out.label2]
          }
        }
        
      }
    }
    #for (vn in ref[["name.new"]]){
    #  w <- which(ref[["name.new"]] == vn)
    #  out.label1[variable == vn, var_label := ref[["name.old"]][w]]
    #}
    
    return(list(data = out1, label = out.label1))
  })
  
  
  data <- reactive({
    data.info()$data
  })
  
  data.label <- reactive(data.info()$label)
  
  output$data <- renderDT({
    datatable(data(), rownames=F, editable = F, extensions= "Buttons", caption = "Data",
              options = c(jstable::opt.data("data"), list(scrollX = TRUE))
    )
  })
  
  
  output$data_label <- renderDT({
    datatable(data.label(), rownames=F, editable = F, extensions= "Buttons", caption = "Label of data",
              options = c(jstable::opt.data("label"), list(scrollX = TRUE))
    )
  })
  

  
  
  out_tb1 <- callModule(tb1module2, "tb1", data = data, data_label = data.label, data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit, showAllLevels = T)
  
  output$table1 <- renderDT({
    tb <- out_tb1()$table
    cap <- out_tb1()$caption
    out.tb1 <- datatable(tb, rownames = T, extensions = "Buttons", caption = cap,
                         options = c(jstable::opt.tb1("tb1"),
                                     list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                     ),
                                     list(scrollX = TRUE)
                         )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  
  out_cox <- callModule(coxModule, "cox", data = data, data_label = data.label, data_varStruct = reactive(varlist[c(4, 5, 1:3)]), default.unires = T, nfactor.limit = nfactor.limit)
  
  output$coxtable <- renderDT({
    hide <- which(colnames(out_cox()$table) == c("sig"))
    datatable(out_cox()$table, rownames=T, extensions= "Buttons", caption = out_cox()$caption,
              options = c(opt.tbreg(out_cox()$caption),
                          list(columnDefs = list(list(visible=FALSE, targets= hide))
                          )
              )
    )  %>% formatStyle("sig", target = 'row',backgroundColor = styleEqual("**", 'yellow'))
  })
  
  out_kaplan <- callModule(kaplanModule, "kaplan", data = data, data_label = data.label, data_varStruct = reactive(varlist[c(4, 5, 1:3)]), nfactor.limit = nfactor.limit)
  
  output$kaplan_plot <- renderPlot({
    print(out_kaplan())
  })

  
  out_scatter <- scatterServer("scatter", data = data, data_label = data.label, data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit)
  
  output$scatter_plot <- renderPlot({
    print(out_scatter())
  })
  
  out_box <- boxServer("box", data = data, data_label = data.label, data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit)
  
  output$box_plot <- renderPlot({
    print(out_box())
  })
  
  out_bar <- barServer("bar", data = data, data_label = data.label, data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit)
  
  output$bar_plot <- renderPlot({
    print(out_bar())
  })
  
  out_line <- lineServer("line", data = data, data_label = data.label, data_varStruct = reactive(varlist), nfactor.limit = nfactor.limit)
  
  output$line_plot <- renderPlot({
    print(out_line())
  })
  
 
  session$onSessionEnded(function() {
    session$close()
  })


  
}


shinyApp(ui, server)