#testing
library(reticulate)
reticulate::use_condaenv('C:/Users/Kuba/anaconda3.7/', required = TRUE)

library(sauron)

#testing
testing = TRUE
if (testing){
  test_models = list.files('test_data/', pattern = '*\\.h5')
  if (length(test_models) == 0){
    model <- application_xception()
    model2 <- application_densenet121()
    model %>% save_model_hdf5('test_data/model_test_xception.h5')
    model2 %>% save_model_hdf5('test_data/model_test_densenet.h5')
  }
  test_imgs = list.files('test_data/', pattern = '*\\.jpg|\\.jpeg')
  if (length(test_imgs) == 0){
    file.copy(input_imgs_paths, 'R/xai_app/test_data/')
  }
}

preprocessing_functions = c(xception_preprocess_input, densenet_preprocess_input, function(x) x/255)
names(preprocessing_functions) = c('xception_preprocess_input', 'densenet_preprocess_input', 'rescale')
