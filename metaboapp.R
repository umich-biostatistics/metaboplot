
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
                  sidebarPanel(width = 4,
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
                  mainPanel(width = 8,
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
    print('tr 1')
    return(parseDirPath(volumes, isolate(input$dir)))
  })
  
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
  
  observeEvent(input$view_options, {
    print('tr 4')
    req(nrow(rdata_set) > 0)
    print('made it here')
    req(input$data_set)
    x = colnames(rdata_set)
    updateCheckboxGroupInput(session, 'select_vars',
                             label = 'Select variables on which to explore metabolites',
                             choices = x,
                             selected = c()
    )
  })
  
  update_select_helper = reactive({
    print('tr 4')
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
    print('Print xz: ')
    print(xz)
    updateCheckboxGroupInput(session, 'select_sorting_options',
                             label = 'Select search and sort options for each variable',
                             choices = xz,
                             selected = c())
    
  })
  
  sidebar_to_explore2 = 
    function(reactor) {
      renderUI({
        create_UI_component = function(x) {
          # use grep to find each type
          colname = word(x, -1)
          ID = gsub(' ', '_', x)
          search_settings <<-  c(search_settings, ID)
          if(grepl('Search', x)){ #} & (input[[ID]])) { #################################################################################################
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
        print(xz)
        #print(xz_select)
        xz_select = lapply(xz, function(x) {
          #ID = gsub(' ', '_', x)
          #print('ID:')
          #print(ID)
          input_ = input[[xz]]
          print(input_)
          if((!is.na(input_)) & (input_ == TRUE)) {
            return(x)
          } else { return(NA) }
        })
        xz_select = xz_select[complete.cases(xz_select)]
        lapply(xz_select, create_UI_component)
      })
    }
  
  observeEvent(input$update_select, {
    print('tr 8')
    search_settings <<- list()
    xz <<- c()
    update_select_helper()
    proc_selected()
  })
  
  observeEvent(input$update_select_w_options, {
    req(input$update_select)
    print('beyonce 0.5')
    print('beyonce')
    output$sidebar_to_explore2 = sidebar_to_explore2(reactive(input$update_select_w_options))
    print('gaga')
  })
  
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
      if(is.null(input_) | (input_ == FALSE)) { next }
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
    input$run
    isolate(filtering())
    lapply(seq_along(file_list), function(i) {
      output[[paste0("images", i)]] <- renderImage({
        print(paste(isolate(mdat_path()), file_list[i], sep = '/'))
        list(
          src = paste(isolate(mdat_path()), file_list[i], sep = '/'),
          filetype = "image/jpeg",
          height = 700,
          width = 1000
        )
      }, deleteFile = FALSE)
    })
  })
  
  output$imageUI <- renderUI({
    core()
    print('tr 13')
    print('core run complete')
    return(
      flowLayout(
        lapply(seq_along(file_list), function(i) {
          imageOutput(paste0("images", i), width = "100%", height = "100%")
        })
      )
    )
    
  })
  
}

shinyApp(ui = ui, server = server)
