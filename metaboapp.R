

library(shiny)
library(shinyFiles)
library(shinymanager)
library(readr)
library(tidyverse)
library(DT)
library(purrr)
library(magick)

search_settings = list()

file_list = list()

rdata_set_cumulate = data.frame()

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials = data.frame(
  user = c("1"),
  password = c("1"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)

ui = #secure_app(head_auth = tags$script(inactivity),
                fluidPage(
                  fluidRow(
                    headerPanel(h3("MetaboView: metabolite results viewer", style="color: #02169B; font-weight: bold;")),
                    div(style = "height:72px; background-color: #F1F1F1;") 
                  ),
                  br(), br(), br(), 
                  sidebarPanel(
                    fileInput("data_set", "Choose CSV data set",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    actionButton('view_options', 'View options'),
                    shinyDirButton('dir', label='File select', title='Select Metabolite image directory'),
                    verbatimTextOutput("dir", placeholder = TRUE),
                    checkboxGroupInput('select_vars', label = 'Select variables on which to explore metabolites',
                                       choices = c()),
                    actionButton('update_select', 'Update selected variable'),
                    br(),
                    br(),
                    uiOutput('sidebar_to_explore'),
                    br(),
                    hr(),
                    actionButton('run', 'Run'),
                    br(),
                    hr(),
                    actionButton('data_preview', 'Preview attributes data')
                    #selectInput('browse_metabo', 'Select individual metabolite', rdata_set$data$Metabolite)
                    # uiOutput('metabo1')
                  ),
                  mainPanel(
                    uiOutput('imageUI'),
                    dataTableOutput('contents')
                  )
                  
                )#)

server = function(input, output, session) {
  
  result_auth = secure_server(check_credentials = check_credentials(credentials))
  
  volumes <- getVolumes()
  
  shinyDirChoose(input, 'dir', roots = volumes, session = session, 
                 filetypes = c('', 'jpg'))
  
  mdat_path <- reactive({
    #print(parseDirPath(volumes, input$directory))
    return(parseDirPath(volumes, input$dir))
  })
  
  # observe({
  #   if(!is.null(mdat_path())) {
  #     path_to_images$data = mdat_path()
  #     all_files = list.files(path_to_images$data)
  #     jpg_files$data = all_files[all_files %>% grepl(pattern = "*.jpg")]
  #   }
  #   #path_to_images = parseDirPath(volumes, input$directory)
  #   #renderImage()
  #   #renderImage#######################################################################
  # })
  
  # path_to_images = 
  #   reactiveValues(
  #     'data' = c()
  #   )
  
  # store data set
  rdata_set = 
    reactiveValues(
      'data' = data.frame()
    )
  
  rdata_set_select = 
    reactiveValues(
      'data' = data.frame()
    )
  
  rdata_select_prepared = 
    reactiveValues(
      'data' = data.frame()
    )
  
  # jpg_files = 
  #   reactiveValues(
  #     'data' = data.frame()
  #   )
  
  
  # rdata_set_cumul = 
  #   reactiveValues(
  #     'data' = data.frame()
  #   )
  
  # search_settings = 
  #   reactiveValues(
  #     'data' = list()
  #   )
  
  # read in data set
  observeEvent(input$data_set, {
    rdata_set$data = read_csv(input$data_set$datapath)
  })
  
  output$contents = 
    renderDataTable({
      req(rdata_set)
      return(rdata_set$data)
    })
  
  # observeEvent(input$update_select, {
  #   rdata_set_select$data = rdata_set$data %>% select(eval(input$update_select))
  # })
  
  observeEvent(input$view_options, {
    req(nrow(rdata_set$data) > 0)
    req(input$data_set)
    #print(rdata_set$data)
    x = colnames(rdata_set$data)
    #print(x)
    updateCheckboxGroupInput(session, 'select_vars',
                             label = 'Select variables on which to explore metabolites',
                             choices = x,
                             selected = c()
    )
  })
  
  # observeEvent(input$update_select, {
  #   vars = input$select_vars
  #   sel_data = rdata_set$data %>% select(!!vars)
  #   var_type = 
  #     (sel_data %>% 
  #        dplyr::summarise_all(class) %>% 
  #        tidyr::gather(variable, class))
  #   rdata_set_select$data = var_type
  # })
  
  update_select_helper = reactive({
    req(input$select_vars)
    vars = input$select_vars
    sel_data = isolate(rdata_set$data) %>% select(!!vars)
    var_type = 
      (sel_data %>% 
         dplyr::summarise_all(class) %>% 
         tidyr::gather(variable, class))
    rdata_set_select$data = var_type
  })
  
  selected_to_tibble = reactive({
    req('tbl_df' %in% class(rdata_set_select$data)) 
    rdata_select_prepared$data = as_tibble(t(rdata_set_select$data))
  })
  # observeEvent(rdata_set_select$data, {
  #   req('tbl_df' %in% class(rdata_set_select$data)) 
  #   rdata_select_prepared$data = as_tibble(t(rdata_set_select$data))
  # })
  
  # observeEvent(jpg_files$data, {
  #   req(length(jpg_files$data > 1))
  #   # apply read across all files that we need to load in, 
  #   # only load the ones we need to use (based on settings)
  #   
  # })
  
  
  
  sidebar_to_explore = 
    function(x) {
      #print(x)
      renderUI({
        req(length(rdata_select_prepared$data) > 0)
        #req(input$update_select)
        #req(input$update_select)
        create_UI_component = function(x) {
          colname = x[1]
          type = x[2]
          ID = paste(colname, type, 'search', sep = '_')
          
          if(type == "character") {
            # UI box with selectInput
            UI_component = renderUI({
              tagList(
                #renderPrint({print(colname)}),
                selectInput(ID, paste('Search', colname, sep = ' '),
                            choices = unique(rdata_set$data %>% select(!!colname))),
                br(),
                hr()
              )
            })
            
            # store search setting data
            search_settings <<-  c(search_settings, ID)
            #print(search_settings %>% list_merge(ID))
            #print(ID)
          } else if(type == "numeric") {
            # UI box with filter (two sliders) and sort (low high, high low)
            IDs = paste(colname, type, 'search', sep = '_')
            IDfgt = paste(colname, type, 'f_gt', sep = '_')
            IDflt = paste(colname, type, 'f_lt', sep = '_')
            IDslh = paste(colname, type, 's_lh', sep = '_')
            IDshl = paste(colname, type, 's_hl', sep = '_')
            
            UI_component = renderUI({
              tagList(
                #renderPrint({print(colname)}),
                selectInput(IDs, paste('Search', colname, sep = ' '),
                            choices = unique(rdata_set$data %>% select(!!colname))),
                sliderInput(IDfgt, paste('Fliter greater than', colname, sep = ' '),
                            min = min(rdata_set$data %>% select(!!colname)), 
                            max = max(rdata_set$data %>% select(!!colname)), 
                            value = min(rdata_set$data %>% select(!!colname))),
                sliderInput(IDflt, paste('Fliter less than', colname, sep = ' '),
                            min = min(rdata_set$data %>% select(!!colname)), 
                            max = max(rdata_set$data %>% select(!!colname)), 
                            value = max(rdata_set$data %>% select(!!colname))),
                actionButton(IDslh, paste('Sort low to high', colname, sep = ' ')),
                actionButton(IDshl, paste('Sort high to low', colname, sep = ' ')),
                br(),
                hr()
              )
            })
            search_settings <<- c(search_settings, IDs, IDfgt, IDflt, IDslh, IDshl)
            #print(search_settings %>% list_merge(IDs, IDfgt, IDflt, IDslh, IDshl))
            # store search setting data
            #print(search_settings %>% 
            #  list_merge(list(IDs, IDfgt, IDflt, IDslh, IDshl)))
            #print(search_settings)
          }
          return(UI_component)
        }
        
        return(
          lapply(rdata_select_prepared$data, create_UI_component)
        )
        
      })
    }
  
   
  observeEvent(input$update_select, {
    search_settings <<- list()
    output$sidebar_to_explore = sidebar_to_explore(reactive(input$update_select))
    update_select_helper()
    selected_to_tibble()
  })
    
  
  #output$sidebar_to_explore = sidebar_to_explore
  
  # eventReactive(sidebar_to_explore, {
  #   rdata_set$data 
  # }) ################################################################################## note this chunk
  
  get_data_on_click = 
    eventReactive(input$data_preview, {
      return(rdata_set$data)
    })
  
  output$contents = 
    renderDataTable({
      return(get_data_on_click())
    }, options = list(pageLength = 20))
  
  
  #results = function() {
    # observeEvent(input$run, {
    #   # on run click, extract access inputs and based on name pattern
    #   # do the right data processing to the data set
    #   # when to go from rdata_set to rdata_set_select
    #   rdata_set_select$data = rdata_set$data
    #   rdata_set$data # original
    #   data_names = colnames(rdata_set$data)
    #   search_settings_ = search_settings
    #   rdata_set_select$data # selected and whatever else, filter
    #   for (ipv in 1:length(search_settings_)) {
    #     current_ = search_settings_[[ipv]]
    #     column_ = str_extract(current_, '[^_]+')
    #     input_ = input[[ search_settings_[[ipv]] ]]
    #     if(grep('search', current_)) {
    #       rdata_set_select$data = rdata_set_select$data %>% filter(sym(!!column_) == !!current_)
    #     } else if (grep('f_gt', current_)) {
    #       rdata_set_select$data = rdata_set_select$data %>% filter(sym(!!column_) >= !!current_)
    #     } else if (grep('f_lt', current_)) {
    #       rdata_set_select$data = rdata_set_select$data %>% filter(sym(!!column_) <= !!current_)
    #     } else if (grep('s_lh', current_)) {
    #       rdata_set_select$data = rdata_set_select$data %>% arrange(sym(!!column_))
    #     } else if (grep('s_hl', current_)) {
    #       rdata_set_select$data = rdata_set_select$data %>% arrange(desc(sym(!!column_)))
    #     }
    #     # read in jpg files and print them!
    #     
    #     return(
    #       lapply(rdata_set_select$data$Filename, function(x) {
    #         print( paste(path_to_images$data, '/', x))
    #         paste(path_to_images$data, '/', x)
    #       })
    #     )
    #   }
    # })
  #}
  
  # Do the renderImage calls
  # 
  # csearch = function(x) {
  #   
  #   req(input$run)
  #   
  #   rdata_set_cumul$data = rdata_set$data
  #   #rdata_set$data # original
  #   #data_names = colnames(rdata_set$data)
  #   search_settings_ = search_settings # list
  #   #print(search_settings_)
  #   #print(search_settings)
  #   #rdata_set_select$data # selected and whatever else, filter
  #   for (ipv in 1:length(search_settings_)) {
  #     current_ = search_settings_[[ipv]]
  #     column_ = sym(str_extract(current_, '[^_]+'))
  #     input_ = input[[ search_settings_[[ipv]] ]]
  #     if(grep('search', current_)) {
  #       rdata_set_cumul$data = rdata_set_cumul$data %>% filter(!!column_ == !!input_)
  #     } else if (grep('f_gt', current_)) {
  #       rdata_set_cumul$data = rdata_set_cumul$data %>% filter(!!column_ >= !!input_)
  #     } else if (grep('f_lt', current_)) {
  #       rdata_set_cumul$data = rdata_set_cumul$data %>% filter(!!column_ <= !!input_)
  #     } else if (grep('s_lh', current_)) {
  #       rdata_set_cumul$data = rdata_set_cumul$data %>% arrange(!!input_)
  #     } else if (grep('s_hl', current_)) {
  #       rdata_set_cumul$data = rdata_set_cumul$data %>% arrange(desc(!!input_))
  #     }
  #     # read in jpg files and print them!
  #   }
  #   
  #   file_list <<- rdata_set_cumul$data$Filename
  #   
  #   return(
  #     lapply(seq_along(file_list), function(i) {
  #       print(file_list)
  #   output[[paste0("images", i)]] <- renderImage({
  #     return(list(
  #       src = file_list[i],
  #       filetype = "image/jpeg",
  #       height = 200,
  #       width = 300
  #     ))
  #   }, deleteFile = FALSE)
  # })
  #   )
  # }
  # 
  
  
  ### new
  
  filtering = function() {
    # remember to isolate reactive things
    
    rdata_set_cumulate = isolate(rdata_set$data)
    print(head(rdata_set_cumulate))
    search_settings_ = search_settings
    print(search_settings_)
    for (ipv in 1:length(search_settings_)) {
      current_ = search_settings_[[ipv]]
      column_ = sym(str_extract(current_, '[^_]+'))
      input_ = input[[ search_settings_[[ipv]] ]]
      if(grep('search', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ == !!input_)
      } else if (grep('f_gt', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ >= !!input_)
      } else if (grep('f_lt', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ <= !!input_)
      } else if (grep('s_lh', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% arrange(!!input_)
      } else if (grep('s_hl', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% arrange(desc(!!input_))
      }
    }
    
    file_list <<- rdata_set_cumulate$Filename
    #print(file_list)
  }
  
  core = reactive({
    
    req(input$run)
    # reactive on run only 
    input$run
    #print('input$run called')
    # call new filtering function on the selections (isolated)
    filtering()
    #print(paste0(isolate(mdat_path()), file_list[1], sep = '/'))
    # store images in output
    lapply(seq_along(file_list), function(i) {
      #print(file_list)
      output[[paste0("images", i)]] <- renderImage({
        print(paste(isolate(mdat_path()), file_list[i], sep = '/'))
        list(
          src = paste(isolate(mdat_path()), file_list[i], sep = '/'),
          filetype = "image/jpeg",
          height = 500,
          width = 700
        )
      }, deleteFile = FALSE)
    })
    

    
  })
  
  
  
  output$imageUI <- renderUI({
    core()
    print('core run complete')
    lapply(seq_along(file_list), function(i) {
      imageOutput(paste0("images", i))
    })
  })
  
  ###
  
  
  
}


shinyApp(ui = ui, server = server)
