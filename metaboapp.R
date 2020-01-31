
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
                  br(),  
                  fluidRow(
                    column(width = 7,
                           wellPanel(
                             h3(tags$b("Instructions")),
                             hr(),
                             h4("This app works sequentially; stages have to be followed in the order provided. If you would like to start over, 
                                restart the app. If you are searching and sorting on a set of variables and would like to change options, always
                                return to Step 3."),
                             h4(""),
                             h4("")
                           )
                    )
                  ),
                  br(),  
                  sidebarPanel(width = 4,
                               wellPanel(
                                 h4("Step 1: "),
                                 h5("Select the CSV plot attributes data set from your files."),
                                 fileInput("data_set", "Choose CSV data set",
                              multiple = TRUE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv"))
                               ),
                               wellPanel(
                                 h4("Step 2: "),
                                 h5("Select the directy to the plot jpeg files. Use the black arrows in the left window to navigate your file system,
                                    and then select the final directory with your curser (it should highligh blue). The right window should display
                                    the list of plots."),
                                 shinyDirButton('dir', label='File select', title='Select Metabolite image directory'),
                                textOutput("mdat_path_display")
                               ),
                               wellPanel(
                                 h4("Step 3: "),
                                 h5("Click 'View options' to display variables which you can dynamically explore. Then, make your selections using the 
                                    check boxes."),
                                 actionButton('view_options', 'View options'),
                                 h5(''),
                                 h5(''),
                                 checkboxGroupInput('select_vars', label = 'Select variables on which to explore metabolites',
                                       choices = c()),
                                 actionButton('update_select', 'Update selected variable')
                               ),
                               wellPanel(
                                 h4("Step 4: "),
                                 h5("Select all options you would like to use in the sorting, filtering, and searching process."),
                                 checkboxGroupInput('select_sorting_options', label = 'Select sorting options',
                                       choices = c()),
                    actionButton('update_select_w_options', 'Update with options')
                               ),
                               wellPanel(
                                 h4("Step 5: "),
                                 h5("Click 'Run' to generate a new set of results:"),
                                 uiOutput('sidebar_to_explore2'),
                                 actionButton('run', 'Run')
                               ),
                               wellPanel(
                                 h4('Extra: '),
                                 h5("You can print the complete data set to sort and search: "),
                                 actionButton('data_preview', 'Preview attributes data')
                               )
                  ),
                  mainPanel(width = 8,
                    dataTableOutput('contents'),       
                    uiOutput('imageUI')
                  )
                  
                )#)

server = function(input, output, session) {
  
  result_auth = secure_server(check_credentials = check_credentials(credentials))
  
  volumes <- getVolumes()
  
  shinyDirChoose(input, 'dir', roots = volumes, session = session, 
                 filetypes = c('', 'jpg'))
  
  mdat_path <- reactive({
    return(parseDirPath(volumes, input$dir))
  })
  
  output$mdat_path_display = renderText({
    req(input$dir)
    #mdat_path()
    dir_ = unlist(input$dir)
    dir2_ = dir_[-length(dir_)]
    dir_start_ = dir_[length(dir_)]
    dir_new_ = c(dir_start_, dir2_)
    return(paste0(dir_new_, sep = '/'))
  })
  
  observeEvent(input$data_set, {
    #print('tr 2')
    rdata_set <<- read_csv(input$data_set$datapath)
  })
  
  output$contents = 
    renderDataTable({
      #print('tr 3')
      req(rdata_set)
      return(rdata_set)
    })
  
  observeEvent(input$view_options, {
    #print('tr 4')
    req(nrow(rdata_set) > 0)
    #print('made it here')
    req(input$data_set)
    x = colnames(rdata_set)
    updateCheckboxGroupInput(session, 'select_vars',
                             label = 'Select variables on which to explore metabolites',
                             choices = x,
                             selected = c()
    )
  })
  
  update_select_helper = reactive({
    #print('tr 4')
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
    #print('tr 5')
    input$update_select
    req('tbl_df' %in% class(rdata_set_select)) 
    rdata_select_prepared <<- as_tibble(t(rdata_set_select))
    #print(rdata_select_prepared)
    #print('tr 5.5')
    var_names_ = as.character(rdata_select_prepared[1,])
    #print(var_names_)
    types_ = as.character(rdata_select_prepared[2,])
    #print(types_ )
    
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
    #print('Print xz: ')
    #print(xz)
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
          #print(paste0('NEW ID ************************************************** = ', ID))
          search_settings <<-  c(search_settings, ID)
          if(grepl('Search', x)){ #} & (input[[ID]])) { #################################################################################################
            return(renderUI({
              tagList(
                selectInput(ID, paste('Search', colname, sep = ' '),
                            choices = unique(rdata_set %>% select(!!colname))),
                #print(paste0('New select input created: ', ID)),
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
                #print(paste0('New slider input created: ', ID)),
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
                #print(paste0('New slider input created: ', ID)),
                br(),
                hr()
              )
            })
            )
          } else if(grepl('Sort low to high', x)) {
            return(renderUI({
              tagList(
                actionButton(ID, paste('Sort low to high', colname, sep = ' ')),
                #print(paste0('New action input created: ', ID)),
                br(),
                hr()
              )
            })
            )
          } else if(grepl('Sort high to low', x)) {
            return(renderUI({
              tagList(
                actionButton(ID, paste('Sort high to low', colname, sep = ' ')),
                #print(paste0('New action input created: ', ID)),
                br(),
                hr()
              )
            })
            )
          } else {
            return()
          }
        }
        #print('tr 6')
        #print(xz)
        #print(xz_select)
        # xz_select = lapply(xz, function(x) {
        #   #ID = gsub(' ', '_', x)
        #   #print('ID:')
        #   #print(ID)
        #   input_ = input[[gsub(' ', '_', x)]]
        #   #print('Printing input:')
        #   #print(input_)
        #   #print(gsub(' ', '_', x))
        #   if((!is.na(input_)) & (!is.null(input_)) & (input_ == TRUE)) {
        #     return(x)
        #   } else { return(NA) }
        # })
        #print('XZ here: ')
        #print(xz)
        # xz_select = xz_select[complete.cases(xz_select)]
        # print('XZ select:')
        # print(xz_select)
        #print('TRIP 20')
        vars_ = isolate(input$select_sorting_options)
        #print(vars_)
        #xz_format_ = gsub('_', ' ', vars_)
        #print(xz_format_)
        
        xz_select = sapply(xz, function(x) {
          #ID = gsub(' ', '_', x)
          #print('ID:')
          #print(ID)
          if(x %in% vars_) {
            return(TRUE)
          } else {
            FALSE
          }
        })
        #print(xz_select)
        xz_select = xz[xz_select]
        #print(paste0('HERE IS XZ SELECT ************************* ==== ',xz_select ))
        lapply(xz_select, create_UI_component)
      })
    }
  
  observeEvent(input$update_select, {
    #print('tr 8')
    search_settings <<- list()
    xz <<- c()
    update_select_helper()
    proc_selected()
  })
  
  observeEvent(input$update_select_w_options, {
    req(input$update_select)
    rdata_set_cumulate <<- rdata_set
    #print('beyonce 0.5')
    #print('beyonce')
    output$sidebar_to_explore2 = sidebar_to_explore2(reactive(input$update_select_w_options))
    #print('gaga')
  })
  
  get_data_on_click = 
    eventReactive(input$data_preview, {
      #print('tr 9')
      return(rdata_set)
    })
  
  output$contents = 
    renderDataTable({
      #print('tr 10')
      return(get_data_on_click())
    }, options = list(pageLength = 20))

  
  filtering = function() {
    #print('tr 11')
    rdata_set_cumulate = rdata_set
    #print(head(rdata_set_cumulate))
    search_settings_ = search_settings
    #print(search_settings_)
    for (ipv in 1:length(search_settings_)) {
      current_ = search_settings_[[ipv]]
      print('current:')
      print(current_)
      column_ = sym(word(gsub('_', ' ', current_), -1))
      input_ = input[[ search_settings_[[ipv]] ]]
      print('input:')
      print(input_)
      if(is.null(input_) | (input_ == FALSE)) { next }
      if(grepl('Search', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ == !!input_)
      } else if (grepl('Filter_greater_than', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ >= !!input_)
      } else if (grepl('Filter_less_than', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% filter(!!column_ <= !!input_)
      } else if (grepl('Sort_low_to_high', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% arrange(!!column_)
      } else if (grepl('Sort_high_to_low', current_)) {
        rdata_set_cumulate = rdata_set_cumulate %>% arrange(desc(!!column_))
      }
    }
    file_list <<- rdata_set_cumulate$Filename
  }
  
  core = reactive({
    #print('tr 12')
    req(input$run)
    input$run
    isolate(filtering())
    rdata_set_cumulate <<- rdata_set
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
    #print('tr 13')
    #print('core run complete')
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


