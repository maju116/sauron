#Adjusting the maximum size of file accepted by the fileInput.
options(shiny.maxRequestSize=100*1024^2)
library(shiny)
library(sauron)

shinyServer(function(input, output, session){
  #todo: Multiple models loading
  #init value is used to indicate whether to display initial screen. Used later in the output$init)_screen renderUI.
  #When set to FALSE the fileInput for an image is shown.
  init = reactiveVal(TRUE)
  #In model we store the loaded model (todo: list of multiple models), indicator of how many models we uploaded (for now fixed 1),
  #preprocessing function (loaded from methods list created in global.R)
  model = reactiveValues()

  observeEvent(input$model_file, {
    #Later it will be used when loading multiple models
    model$uploaded = 1
    #Temp cache for a loaded file data (format, path to temp etc.)
    file <- input$model_file
    ext <- tools::file_ext(file$datapath)
    req(file)
    #Format check (maybe todo: js alert when inproper format is used)
    validate(need(ext == "h5", "Please upload a h5 file"))
    print('TEST: Model is being loaded')
    model$model = load_model_hdf5(file$datapath)
    print('TEST: Model loaded succesfully')
  })

  observeEvent(input$preprocessing_method, {
    #Loading the preprocessing function
    model$preprocessing_function = preprocessing_functions[[input$preprocessing_method]]
  })

  observeEvent(input$explain, {
    #This is used to hide the prompts for preprocessing and ID i.e. whole explain_specifics UI part
    output$explain_specifics = NULL
    #Init value forces app to display the fileInput for images, used in output$init_screen
    init(FALSE)
    #The explainer creation
    model$explainer <- CNNexplainer$new(model$model,
                                        model$preprocessing_function,
                                        str(input$modelId))

  })

  # observeEvent(input$image_file, {
  #   file <- input$image_file
  #   ext <- tools::file_ext(file$datapath)
  #   input_imgs_paths = file$datapath
  #   req(file)
  #   #Checking the format
  #   validate(need(ext == "jpg" | ext == 'jpeg', "Please upload an image file"))
  #   print('TEST: Image read properly')
  #   #Method set to 'GD' to cut the computation time while testing
  #   model$explanations <- model$explainer$explain(input_imgs_paths, class_index = NULL,
  #                                                 methods = 'GB',
  #                                                 batch_size = length(input_imgs_paths),
  #                                                 num_samples = 5, noise_sd = 0.1,
  #                                                 steps = 10, patch_size = c(50, 50),
  #                                                 grayscale = FALSE)
  #   print('TEST: Explainer created')
  #   model$explanations$plot_and_save(TRUE, 'data/')
  # })

   output$explanation_plot <- renderCachedPlot({
     file <- input$image_file
     ext <- tools::file_ext(file$datapath)
     input_imgs_paths = file$datapath
     req(file)
     #Checking the format
     validate(need(ext == "jpg" | ext == 'jpeg', "Please upload an image file"))
     print('TEST: Image read properly')
     #Method set to 'GD' to cut the computation time while testing
     model$explanations <- model$explainer$explain(input_imgs_paths, class_index = NULL,
                                                   methods = 'GB',
                                                   batch_size = length(input_imgs_paths),
                                                   num_samples = 5, noise_sd = 0.1,
                                                   steps = 10, patch_size = c(50, 50),
                                                   grayscale = FALSE)
     print('TEST: Explainer created')
     model$explanations$plot_and_save(TRUE, 'data/')},
     cacheKeyExpr = {input$image_file}
   )

  output$init_screen <- renderUI({
    if(init()){
      return(column(12,
                    actionButton(
                      inputId = 'init_explain',
                      label = 'Create explainer'),
                    conditionalPanel(
                      condition = "input.init_explain",
                      fileInput(
                        inputId = 'model_file',
                        label = 'Select model to explain',
                        multiple = FALSE,
                        accept = NULL,
                        width = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"
                      )
                    )
      ))}
    else{return(
      #todo: fix this message
      #textOutput(paste('Summary of model: ', str(input$modelId), ' , preprocessing: ', str(input$modelId), '.')),
      column(12,
             fileInput(
               inputId = 'image_file',
               label = 'Select image to explain',
               multiple = FALSE,
               accept = NULL,
               width = NULL,
               buttonLabel = "Browse...",
               placeholder = "No file selected"
             )
      ))}
  })

  output$explain_specifics <- renderUI({
    if(is.null(model$model)){return(NULL)}
    column(12,
           selectInput(
             inputId = 'preprocessing_method',
             label = 'Choose preprocessing method',
             choices = names(preprocessing_functions),
             selected ='rescale',
             multiple = FALSE,
             width = NULL,
             size = NULL
           ),
           textInput(
             inputId = 'modelId',
             label = 'Provide Your model ID',
             value = ''
           ),
           actionButton(
             inputId = 'explain',
             label = 'Create the explainer'
           )
    )
  })

  output$main_screen <- renderUI({
    sidebarLayout(
      mainPanel(
        #nothing returned for now
        imageOutput('explanation_plot')
        #img(src = 'draft_plot.png')
      ),
      div(class = "ui grid",
          sidebarPanel(
            fluidRow(
              uiOutput('init_screen'),
              uiOutput('explain_specifics')
            )
          )
      )
    )
  })


})
