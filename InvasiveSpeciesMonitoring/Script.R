library(BiocInstaller)

setwd("~/GitHub/Kaggle/InvasiveSpeciesMonitoring")

train_generator <- flow_images_from_directory("./test", generator = image_data_generator(),
                                              target_size = c(img_width, img_height), color_mode = "rgb",
                                              class_mode = "binary", batch_size = batch_size, shuffle = TRUE,
                                              seed = 123)

layer_conv_2d(filter = 32, kernel_size = c(3,3), input_shape = c(img_width, img_height, 3)) 
layer_activation("relu") 
layer_max_pooling_2d(pool_size = c(2,2)) 
  
layer_conv_2d(filter = 32, kernel_size = c(3,3)) 
layer_activation("relu") 
layer_max_pooling_2d(pool_size = c(2,2)) 
  
layer_conv_2d(filter = 64, kernel_size = c(3,3)) 
layer_activation("relu") 
layer_max_pooling_2d(pool_size = c(2,2)) 
  
layer_flatten() 
layer_dense(64) 
layer_activation("relu") 
layer_dropout(0.5) 
layer_dense(1) 
layer_activation("sigmoid")

  
  