dfMaker <- function(input.folder, config.path, output.file = NULL, output.path=NULL, no_save=FALSE) {
  
  # 'arrow' auto-installation
  if (!require("arrow", quietly = TRUE)) {
    install.packages("arrow", dependencies = TRUE, ask = FALSE)
    library(arrow)
  }
  
  
  # Load the required library
  library(arrow)
  
  # Initialize variables to store metadata and final data
  all_data <- list()
  default_config <- list(
    extract_datetime = FALSE,
    extract_time = FALSE,
    extract_exp_search = FALSE,
    extract_country_code = FALSE,
    extract_network_code = FALSE,
    extract_program_name = FALSE,
    extract_time_range = FALSE,
    timezone = "America/Los_Angeles"
  )
  
  # Check if config path is provided and read the configuration; use default if not provided
  if (missing(config.path)) {
    config <- default_config
  } else {
    config <- read_json_arrow(config.path, as_data_frame = TRUE) |> as.list()
  }
  

  # List all JSON files in the input directory
  files <- list.files(input.folder, pattern = "*.json", full.names = TRUE)
  
  # Control variable to ensure message is printed only once
  message_printed <- FALSE  
  
  # Función para determinar si la lista 'people' está vacía o no
  is_lista_vacia <- function(objeto) {
    return(length(objeto$people) == 0)
  }
  
  archived <- list()
  
  
  # Loop through each file to process
  for (frame_file in files) {
    
    # Read the JSON file and extract the keypoints data
    rawData <- read_json_arrow(frame_file, as_data_frame = TRUE)[[2]][[1]][2:5]
  
    if (sum(capture.output(rawData)!="<unspecified> [4]")!=0) {
      
    total_points <- sum(sapply(rawData, function(x) length(unlist(x)) / 3))
    # Define the expected number of points for each type of keypoints
    model_type <- ifelse(total_points > 25, "137_points", "25_points")
    rawData<- if (model_type == "25_points") rawData[1] else rawData
    check_points <- if (model_type == "137_points") c(25, 70, 21, 21) else c(25)

    if (!message_printed) {  # Check if the message has not been printed yet
      if (model_type == "25_points") {
        print("model b_25")
      } else {
        print("regular model")
      }
      message_printed <- TRUE  # Update the control variable
    }

    # Metadata extraction based on the configuration
    metadata <- gsub(".*[\\\\/]", "", frame_file)
    frame <- as.numeric(regmatches(metadata, regexec("[0-9]{12}", metadata)))
    id<-gsub("_\\d{12}_keypoints.json", "", metadata)
    
    # Extract additional metadata if enabled in configuration
    
    if (config$extract_datetime) {
      timezone <- ifelse(is.null(config$timezone), default_config$timezone, config$timezone)
      datetime_str <- sub("^(\\d{4}-\\d{2}-\\d{2})_(\\d{4})_.*$", "\\1 \\2", metadata)
      datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%d %H%M", tz = timezone)
    }else{
      datetime<-NA
    }
    exp_search <- ifelse(config$extract_exp_search, gsub(".*[0-9]_(.*)_\\d{12}_keypoints\\.json$", "\\1", metadata), NA)
    country_code <- ifelse(config$extract_country_code, sub(".*?_(\\w{2})_.*", "\\1", metadata), NA)
    network_code <- ifelse(config$extract_network_code, sub("^.*_\\d{4}_\\w{2}_([^_]+)_.*$", "\\1", metadata), NA)
    program_name <- ifelse(config$extract_program_name, sub("^.*_\\d{4}_\\w{2}_[^_]+_(.*?)_\\d+-\\d+.*$", "\\1", metadata), NA)
    time_range <- ifelse(config$extract_time_range, sub("^.*_(\\d+-\\d+)_.*$", "\\1", metadata), NA)
    # Process keypoints data and compile into data frames
    for (i in 1:nrow(rawData)) {
      for (j in 1:ncol(rawData)) {
        matrix_data <- matrix(unlist(rawData[i, j]), ncol = 3, nrow = check_points[j], byrow = TRUE)
        matrix_data <- apply(matrix_data, 2, as.numeric)
        matrix_data[,1:2][matrix_data[,1:2] == 0] <- NA #Zeros as NAs
        
        # Crear el vector origen cuando j=1
        if (j == 1) {
          origen <- matrix_data[2, 1:2] # Extraer la segunda fila y las primeras dos columnas
          v.i<- c(matrix_data[6,1],0)
          v.j<- v.i[2:1]*-1 # orthonormal
        }
        
        m<-sweep(matrix_data[, 1:2], 2, origen, FUN = "-")
        
        
        # Pre-allocar la matriz con NA o ceros
        newm <- matrix(NA, nrow = nrow(m), ncol = 2)
        
        # Matriz identidad
  
        a<- matrix( data = c( v.i,v.j ) , nrow = 2 )
        
        # Calcular denominador común fuera del bucle
        denominador_comun <- a[1, 1] * a[2, 2] - a[2, 1] * a[2, 1]
        
        for ( k in 1:nrow( m ) ) {
          
          b<- as.matrix( c( m[ k , ] ) , ncol = 1 )
          
          
          newx <- (a[2, 2] * b[1] - b[2] * a[1, 2]) / denominador_comun
          newy <- (a[1, 1] * b[2] - b[1] * a[2, 1]) / denominador_comun
          
          newm[k, ] <- c(newx, newy)
        }
        # Combine individual keypoints data into a data frame with metadata
        frame_data_list <- list(matrix_data = matrix_data,
                                newm = newm,
                                type_points = gsub("_2d", "", colnames(rawData[j])),
                                people_id = i,
                                points = c(0:(nrow(matrix_data) - 1)),
                                id = id, 
                                frame = frame)
        
        # Aggregate dynamic only no NA variables
        if (!is.na(exp_search)) frame_data_list$exp_search <- exp_search
        if (!is.na(datetime)) frame_data_list$datetime <- datetime
        if (!is.na(country_code)) frame_data_list$country_code <- country_code
        if (!is.na(network_code)) frame_data_list$network_code <- network_code
        if (!is.na(program_name)) frame_data_list$program_name <- program_name
        if (!is.na(time_range)) frame_data_list$time_range <- time_range
        df <- data.frame(frame_data_list)
        all_data[[length(all_data) + 1]] <- df
      }
    }
    
    cat("\n")  # Separador entre archivos
    
    cat("\nThe frame ", frame, " has been read\n")
    
    }  else {
      # If rawData is empty, print a message indicating it
      cat("File:", basename(frame_file), "\n")
      cat("File is  empty\n\n")
      # archived[length(archived) + 1] <- frame_file # activar ne caso de querer info de los frames vacíos.
    }
    
  }
  # Combine all the individual frames into one data frame
  final_data <- do.call(rbind, all_data)
  colnames(final_data)[1:5] <- c("x", "y", "c","nx","ny")
  
  if (!no_save) {
    # Use processed_id for auto-naming if output.file is NULL or empty
    if (is.null(output.file) || output.file == "") {
      if (length(unique(final_data$id))!=1) {
        stop(paste("Error: Multiple unique IDs found:", paste(unique(final_data$id), collapse=", ")))
      }else{
        if (!is.null(output.path)) {
          # add last "/" 
          if (!grepl("/$", output.path)) {
            output.path <- paste0(output.path, "/")
          }
          dir.create(output.path,recursive = TRUE,showWarnings = FALSE)
          output.file <- paste0(output.path,unique(final_data$id), ".parquet")
        }else{
        dir.create("./df_outputs/",recursive = TRUE,showWarnings = FALSE)
        output.file <- paste0("./df_outputs/",unique(final_data$id), ".parquet")
        }
      }
    }
  
    
    # Determine the output format based on the file extension
    if (!is.null(output.file)) {
      file_ext <- tools::file_ext(output.file)
      if (file_ext == "csv") {
        write.csv(final_data, output.file, row.names = FALSE)
      } else if (file_ext == "parquet") {
        arrow::write_parquet(final_data, sink = output.file)
      } else {
        warning("Unsupported file extension. Returning data frame.")
      }
    }
  }
  
 
  
  return(final_data)
}


# save new version
# save(dfMaker,file="dfMaker.rda")


