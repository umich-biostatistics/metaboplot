

library(shiny)
library(shinyFiles)
library(shinymanager)
library(readr)
library(tidyverse)
library(DT)
library(purrr)
library(magick)
library(stringr)

search_settings = list()

file_list = list()

rdata_set_cumulate = data.frame()

rdata_set_select = data.frame()

rdata_select_prepared = data.frame()

rdata_set = data.frame()

xz = c()

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
                    checkboxGroupInput('select_sorting_options', label = 'Select sorting options',
                                       choices = c()),
                    actionButton('update_select_w_options', 'Update with options'),
                    br(),
                    br(),
                    uiOutput('sidebar_to_explore2'),
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
    print('tr 1')
    return(parseDirPath(volumes, isolate(input$dir)))
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
  # rdata_set = 
  #   reactiveValues(
  #     'data' = data.frame()
  #   )
  
  # rdata_set_select = 
  #   reactiveValues(
  #     'data' = data.frame()
  #   )
  
  # rdata_select_prepared = 
  #   reactiveValues(
  #     'data' = data.frame()
  #   )
  
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
    print('tr 2')
    rdata_set <<- read_csv(input$data_set$datapath)
  })
  
  output$contents = 
    renderDataTable({
      print('tr 3')
      req(rdata_set)
      return(rdata_set)
    })
  
  # observeEvent(input$update_select, {
  #   rdata_set_select$data = rdata_set$data %>% select(eval(input$update_select))
  # })
  
  observeEvent(input$view_options, {
    print('tr 4')
    req(nrow(rdata_set) > 0)
    print('made it here')
    req(input$data_set)
    #print(rdata_set$data)
    x = colnames(rdata_set)
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
    print('tr 4')
    #req(input$select_vars)
    input$update_select
    vars = isolate(input$select_vars)
    sel_data = rdata_set %>% select(!!vars)
    var_type = 
      (sel_data %>% 
         dplyr::summarise_all(class) %>% 
         tidyr::gather(variable, class))
    rdata_set_select <<- var_type
  })
  
  proc_selected = reactive({
    print('tr 5')
    input$update_select
    req('tbl_df' %in% class(rdata_set_select)) 
    rdata_select_prepared <<- as_tibble(t(rdata_set_select))
    print(rdata_select_prepared)
    
    print('tr 5.5')
    
    
    var_names_ = as.character(rdata_select_prepared[1,])
    print(var_names_)
    types_ = as.character(rdata_select_prepared[2,])
    print(types_ )
    
    type_options_ = lapply(seq_along(var_names_), function(i) {
      if(types_[i] == 'numeric') {
        # create checkboxes for all possible options of interest
        xz <<- c(xz, paste0('Search ', var_names_[i], sep = ''),
                     paste0('Filter greater than ', var_names_[i], sep = ''),
                     paste0('Filter less than ', var_names_[i], sep = ''),
                     paste0('Sort low to high ', var_names_[i], sep = ''),
                     paste0('Sort high to low ', var_names_[i], sep = ''))
      } else if (types_[i] == 'character') {
        # create checkboxes for all possible options of interest
        xz <<- c(xz, paste0('Search ', var_names_[i], sep = ''))
      }
    })
    print(xz)
    updateCheckboxGroupInput(session, 'select_sorting_options',
                             label = 'Select search and sort options for each variable',
                             choices = xz,
                             selected = c())
    
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
  
  
  
  sidebar_to_explore2 = 
    function(reactor) {
      #print(x)
      renderUI({
        
        # # get name and type of ui component needed from info
        # var_names_ = as.character(rdata_select_prepared[1,])
        # print(var_names_)
        # types_ = as.character(rdata_select_prepared[2,])
        # print(types_ )
        # 
        # type_options_ = lapply(seq_along(var_names_), function(i) {
        #   if(types_[i] == 'numeric') {
        #     # create checkboxes for all possible options of interest
        #     xz <<- c(xz, paste0('Search ', var_names_[i], sep = ''),
        #              paste0('Filter greater than ', var_names_[i], sep = ''),
        #              paste0('Filter less than ', var_names_[i], sep = ''),
        #              paste0('Sort low to high ', var_names_[i], sep = ''),
        #              paste0('Sort high to low ', var_names_[i], sep = ''))
        #   } else if (types_[i] == 'character') {
        #     # create checkboxes for all possible options of interest
        #     xz <<- c(xz, paste0('Search ', var_names_[i], sep = ''))
        #   }
        # })
        
        create_UI_component = function(x) {
          # use grep to find each type
          colname = word(x, -1)
          ID = gsub(' ', '_', x)
          search_settings <<-  c(search_settings, ID)
          if(grepl('Search', x)) {
            return(renderUI({
              
              tagList(
                selectInput(ID, paste('Search', colname, sep = ' '),
                            choices = unique(rdata_set %>% select(!!colname))),
                br(),
                hr()
              )
            }))
          } else if(grepl('Filter greater than', x)) {
            return(renderUI({
              tagList(
                sliderInput(ID, paste('Fliter greater than', colname, sep = ' '),
                        min = min(rdata_set %>% select(!!colname)), 
                        max = max(rdata_set %>% select(!!colname)), 
                        value = min(rdata_set %>% select(!!colname))),
                br(),
                hr()
              )
            })
            )
          } else if(grepl('Filter less than', x)) {
            return(renderUI({
              tagList(
                sliderInput(ID, paste('Filter less than', colname, sep = ' '),
                            min = min(rdata_set %>% select(!!colname)), 
                            max = max(rdata_set %>% select(!!colname)), 
                            value = max(rdata_set %>% select(!!colname))),
                br(),
                hr()
              )
            })
            )
          } else if(grepl('Sort low to high', x)) {
            return(renderUI({
              tagList(
                actionButton(ID, paste('Sort low to high', colname, sep = ' ')),
                br(),
                hr()
              )
            })
            )
          } else if(grepl('Sort high to low', x)) {
            return(renderUI({
              tagList(
                actionButton(ID, paste('Sort high to low', colname, sep = ' ')),
                br(),
                hr()
              )
            })
            )
          } else {
            return()
          }
        }
        
        print('tr 6')
        lapply(xz, create_UI_component)
        
      })
    }
  
  
  
  # sidebar_to_explore2 = 
  #   function(x) {
  #     #print(x)
  #     renderUI({
  #       print('tr 6')
  #       req(length(rdata_select_prepared) > 0)
  #       #req(input$update_select)
  #       #req(input$update_select)
  #       create_UI_component = function(x) {
  #         colname = x[1]
  #         type = x[2]
  #         ID = paste(colname, type, 'search', sep = '_')
  #         
  #         if(type == "character") {
  #           # UI box with selectInput
  #           UI_component = renderUI({
  #             tagList(
  #               #renderPrint({print(colname)}),
  #               selectInput(ID, paste('Search', colname, sep = ' '),
  #                           choices = unique(rdata_set %>% select(!!colname))),
  #               br(),
  #               hr()
  #             )
  #           })
  #           
  #           # store search setting data
  #           search_settings <<-  c(search_settings, ID)
  #           #print(search_settings %>% list_merge(ID))
  #           #print(ID)
  #         } else if(type == "numeric") {
  #           # UI box with filter (two sliders) and sort (low high, high low)
  #           IDs = paste(colname, type, 'search', sep = '_')
  #           IDfgt = paste(colname, type, 'f_gt', sep = '_')
  #           IDflt = paste(colname, type, 'f_lt', sep = '_')
  #           IDslh = paste(colname, type, 's_lh', sep = '_')
  #           IDshl = paste(colname, type, 's_hl', sep = '_')
  #           
  #           UI_component = renderUI({
  #             tagList(
  #               #renderPrint({print(colname)}),
  #               selectInput(IDs, paste('Search', colname, sep = ' '),
  #                           choices = unique(rdata_set %>% select(!!colname))),
  #               sliderInput(IDfgt, paste('Fliter greater than', colname, sep = ' '),
  #                           min = min(rdata_set %>% select(!!colname)), 
  #                           max = max(rdata_set %>% select(!!colname)), 
  #                           value = min(rdata_set %>% select(!!colname))),
  #               sliderInput(IDflt, paste('Fliter less than', colname, sep = ' '),
  #                           min = min(rdata_set %>% select(!!colname)), 
  #                           max = max(rdata_set %>% select(!!colname)), 
  #                           value = max(rdata_set %>% select(!!colname))),
  #               actionButton(IDslh, paste('Sort low to high', colname, sep = ' ')),
  #               actionButton(IDshl, paste('Sort high to low', colname, sep = ' ')),
  #               br(),
  #               hr()
  #             )
  #           })
  #           search_settings <<- c(search_settings, IDs, IDfgt, IDflt, IDslh, IDshl)
  #           #print(search_settings %>% list_merge(IDs, IDfgt, IDflt, IDslh, IDshl))
  #           # store search setting data
  #           #print(search_settings %>% 
  #           #  list_merge(list(IDs, IDfgt, IDflt, IDslh, IDshl)))
  #           #print(search_settings)
  #         }
  #         return(UI_component)
  #       }
  #       print('tr 7')
  #       return(
  #         lapply(, create_UI_component)
  #       )
  #       
  #     })
  #   }
  
   
  observeEvent(input$update_select, {
    print('tr 8')
    search_settings <<- list()
    xz <<- c()
    #output$sidebar_to_explore = sidebar_to_explore(reactive(input$update_select))
    
    update_select_helper()
    proc_selected()
    #output$sidebar_to_explore2 = sidebar_to_explore2(reactive(input$update_select_w_options))
    
  })
  
  observeEvent(input$update_select_w_options, {
    req(input$update_select)
    print('beyonce 0.5')
    #req()
    #input$update_select
    #input$update_select_w_options
    print('beyonce')
    output$sidebar_to_explore2 = sidebar_to_explore2(reactive(input$update_select_w_options))
    print('gaga')
  })
  
  # observeEvent(input$update_select_w_options {
  #   
  # })
  
  # updates list of selection options ( what to sort on for each variable )
  # update_options_helper = reactive({
  # 
  # })
    
  
  
  get_data_on_click = 
    eventReactive(input$data_preview, {
      print('tr 9')
      return(rdata_set)
    })
  
  output$contents = 
    renderDataTable({
      print('tr 10')
      return(get_data_on_click())
    }, options = list(pageLength = 20))

  
  filtering = function() {
    print('tr 11')
    rdata_set_cumulate = rdata_set
    print(head(rdata_set_cumulate))
    search_settings_ = search_settings
    print(search_settings_)
    for (ipv in 1:length(search_settings_)) {
      current_ = search_settings_[[ipv]]
      column_ = sym(word(gsub('_', ' ', current_), -1))
      input_ = input[[ search_settings_[[ipv]] ]]
      if(grep('Search', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ == !!input_)
      } else if (grep('Filter_greater_than', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ >= !!input_)
      } else if (grep('Filter_less_than', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ <= !!input_)
      } else if (grep('Sort_low_to_high', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% arrange(!!input_)
      } else if (grep('Sort_high_to_low', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% arrange(desc(!!input_))
      }
    }
    
    file_list <<- rdata_set_cumulate$Filename
  }
  
  core = reactive({
    print('tr 12')
    req(input$run)
    # reactive on run only 
    input$run
    #print('input$run called')
    # call new filtering function on the selections (isolated)
    isolate(filtering())
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
    print('tr 13')
    print('core run complete')
    lapply(seq_along(file_list), function(i) {
      imageOutput(paste0("images", i))
    })
  })
  
  
}


shinyApp(ui = ui, server = server)
