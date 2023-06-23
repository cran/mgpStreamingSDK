#' Interface class for interacting with WMS, WFS, and WMTS Streaming classes.
#'
#' This class provides a unified interface to access the WMS, WFS, and WMTS Streaming classes.
#' @title Interface
#' @importFrom R6 R6Class 
#' @import reticulate
#' @export Interface
Interface <- R6::R6Class(
  "Interface",
  public = list(
    #' @field mgp_sdk (Optional) An instance of the MGP_SDK Python library. If NULL, a new instance will be created. Default is NULL.
    mgp_sdk = NULL,
    #' @field py_interface (Optional) An instance of the Interface class from the MGP_SDK Python library. If NULL, a new instance will be created. Default is NULL.
    py_interface = NULL,
    #' @field env_name = (Optional) The name of the environment where the MGP_SDK Python library is installed. Default is "R-MGP-SDK".
    env_name = NULL,
    #' @description
    #' Initializes the `Interface` object. Sets up the environment for using the MGP_SDK Python library.
    #'
    #' @param mgp_sdk (Optional) An instance of the MGP_SDK Python library. If NULL, a new instance will be created. Default is NULL.
    #' @param py_interface (Optional) An instance of the Interface class from the MGP_SDK Python library. If NULL, a new instance will be created. Default is NULL.
    #' @param env_name (Optional) The name of the environment where the MGP_SDK Python library is installed. Default is "R-MGP-SDK".
    initialize = function(mgp_sdk=NULL, py_interface = NULL, env_name = "R-MGP-SDK") {
      Sys.unsetenv("HOME")
      Sys.setenv(HOME = getwd())
      if (private$check_virtualenv(env_name)) {
        #do nothing the venv is already set up
        library(reticulate)
        reticulate::use_virtualenv(file.path(Sys.getenv("HOME"), ".virtualenvs", env_name))
        }
      else{
        private$initialize_python_env("MGP-SDK")
        library(reticulate)
        reticulate::use_virtualenv(file.path(Sys.getenv("HOME"), ".virtualenvs", env_name))
      }
      self$mgp_sdk <<- reticulate::import("MGP_SDK")
      self$py_interface <<- self$mgp_sdk$interface$Interface()
      },
    
    #' @description Perform a search for features within the specified bounding box and/or with a specified filter.
    #'
    #' @param bbox A string indicating the bounding box of the area of interest (miny,minx,maxy,maxx).
    #' @param filter A string containing a CQL filter used to refine the data of the search. Default is NULL.
    #' @param shapefile A logical indicating whether to return a shapefile. Default is FALSE.
    #' @param csv A logical indicating whether to return a CSV file. Default is FALSE.
    #' @param ... Additional arguments to pass to the `search` method.
    #'
    #' @details The `search` function performs a search for features within the specified bounding box and/or with a specified filter.
    #'
    #' @return If `shapefile` is TRUE, the function returns a shapefile of all features and associated metadata.
    #' If `csv` is TRUE, the function returns a CSV file. If neither is specified, the function returns a list of features.
    #' @export
    search = function(bbox=NULL, filter=NULL, shapefile=FALSE, csv=FALSE, ...) {
      kwargs <- list(...)
      args <- c(list(bbox = bbox, filter = filter, shapefile = shapefile, csv = csv), kwargs)
      result <- do.call(self$py_interface$streaming$search, args)
      return(result)
    },
    
    #' @description Download an image from a WMS or WMTS service
    #'
    #' @description This function allows you to download an image from a Web Map Service (WMS) or a
    #' @description Web Map Tile Service (WMTS). You can specify the bounding box, image dimensions, image
    #' @description format, and other parameters to customize the downloaded image.
    #'
    #' @param bbox A vector of four numeric values specifying the bounding box of the image.
    #' @param srsname A string specifying the spatial reference system (SRS) of the bounding box. Default is "EPSG:4326".
    #' @param height The height of the image in pixels.
    #' @param width The width of the image in pixels.
    #' @param img_format A string specifying the image format. Must be one of "jpeg", "png", or "geotiff".
    #' @param identifier A string specifying the identifier of the image.
    #' @param gridoffsets A vector of two numeric values specifying the grid offsets of the image.
    #' @param zoom_level An integer specifying the zoom level of the WMTS image.
    #' @param download A logical value indicating whether to download the image (TRUE) or return the raw image data (FALSE).
    #' @param outputpath A string specifying the directory where the downloaded image should be saved.
    #' @param display A logical value indicating whether to display the downloaded image (TRUE) or not (FALSE).
    #' @param ... Additional parameters to be passed to the WMS or WMTS service.
    #'
    #' @return If `download` is TRUE, the function returns the filename of the downloaded image. If `download` is FALSE,
    #' the function returns the raw image data as a binary vector.
    #' @export
    download_image = function(bbox = NULL, srsname = "EPSG:4326", height = NULL, width = NULL, img_format = "jpeg", identifier = NULL,  zoom_level = NULL, download = TRUE, outputpath = NULL, display = FALSE) {
      args <- c(list(bbox = bbox, srsname = srsname, height = as.integer(height), width = as.integer(width), img_format = img_format, identifier = identifier, zoom_level = zoom_level, download = download, outputpath = outputpath, display = display, legacyId = legacyId))
      result <- do.call(self$py_interface$streaming$download_image, args)
      #result <- self$py_interface$streaming$download_image(bbox = bbox, srsname = srsname, height = as.integer(height), width = as.integer(width), img_format = img_format, identifier = identifier, zoom_level = zoom_level, download = download, outputpath = outputpath, display = display)
      return(result)
      },
    
    #'
    #' @description This function is a wrapper for a Python function that retrieves full resolution images.
    #' @description The function downloads an image with the specified feature ID and additional parameters.
    #'
    #' @param featureid A character string representing the unique ID of the feature for which the image is required.
    #' @param thread_number An integer indicating the number of threads to use for the download process. Default is 100.
    #' @param bbox A character string representing the bounding box coordinates in the format 'xmin, ymin, xmax, ymax'. If NULL, the bounding box will be determined based on the feature ID. Default is NULL.
    #' @param mosaic A logical value indicating whether to mosaic the images or not. If TRUE, images covering the defined area will be combined into a single image. Default is FALSE.
    #' @param srsname A character string representing the spatial reference system to be used for the image. Default is 'EPSG:4326'.
    #' @param outputdirectory A character string representing the directory where the image should be saved. If NULL, the image will be saved in the current working directory. Default is NULL.
    #' @param image_format A character string representing the format of the image file to be downloaded. Default is 'jpeg'.
    #' @param filename A character string representing the name of the file to be saved. Default is "Maxar_Download".
    #'
    #' @return The function returns the result of the Python function call. The nature of this result depends on the Python function implementation.
    #' @export

    get_full_res_image = function(featureid, thread_number = 100, bbox = NULL, mosaic = FALSE,
                                   srsname = 'EPSG:4326', outputdirectory=getwd(),image_format='jpeg', filename="Maxar_Download") {
      arguments <- c(list(featureid=featureid,thread_number=as.integer(thread_number),bbox=bbox,mosaic=mosaic,srsname=srsname,outputdirectory=outputdirectory,image_format=image_format,filename=filename))
      result <- do.call(self$py_interface$streaming$get_full_res_image,arguments)
      return(result)
    }
  ),
  private = list(
    initialize_python_env = function(package_name, env_name = "R-MGP-SDK") {
      # Get the current Python configuration
      base_python = private$get_system_python_path()
      # Create virtual environment if it does not exist
      if (!private$check_virtualenv(env_name)) {
        # create the virtual environment
        private$create_virtualenv(package_name = package_name, env_name = env_name)
      }
    },
    get_system_python_path = function() {
      # Attempt to find Python in common locations
      base_python_path <- NULL
      if (Sys.info()["sysname"] == "Windows") {
        base_python_path <- system("where python", intern = TRUE)[1]
      } else { # For Unix and MacOS
        base_python_path <- system("which python", intern = TRUE)
      }

      # If base_python_path is NULL or empty, Python was not found
      if (is.null(base_python_path) || base_python_path == "") {
        message("Python not found, installing...")
        reticulate::install_python(version = "3.9:latest", list = FALSE, force = FALSE)
        # Try finding Python again after installing Miniconda
        if (Sys.info()["sysname"] == "Windows") {
          base_python_path <- system("where python", intern = TRUE)[1]
        } else { # For Unix and MacOS
          base_python_path <- system("which python", intern = TRUE)
        }
        # If Python is still not found, stop the function
        if (is.null(base_python_path) || base_python_path == "") {
          stop("Python installation failed.")
        }
      }
      return(base_python_path)
    },
    check_virtualenv = function(env_name) {
      env_path <- file.path(Sys.getenv("HOME"), ".virtualenvs", env_name)
      dir.exists(env_path)
      return(dir.exists(env_path))
    },
    create_virtualenv = function(package_name, env_name = "R-MGP-SDK") {
      # Define the base Python interpreter. You might need to adjust this depending
      # on where Python is installed on your system.
      base_python <- private$get_system_python_path()
      # Define the path to the new virtual environment
      venv_path <- file.path(Sys.getenv("HOME"), ".virtualenvs", env_name)
      # Command to create the virtual environment
      create_venv_cmd <- paste(paste0("\"", base_python, "\""), "-m venv", venv_path)
      system(create_venv_cmd)
      if (Sys.info()['sysname'] == "Windows") {
        library(reticulate)
        reticulate::use_virtualenv(file.path(Sys.getenv("HOME"), ".virtualenvs", env_name))
        reticulate::py_install("MGP_SDK", envname = file.path(Sys.getenv("HOME"), ".virtualenvs", env_name))
      } else {
        pip_install_cmd = paste("source", file.path(venv_path, "bin", "activate"), "&&", "python -m pip install", package_name)
        system(pip_install_cmd)
      }
      # Set the RETICULATE_PYTHON environment variable to the Python interpreter of the new environment
      if (Sys.info()['sysname'] == "Windows") {
        python_interpreter <- file.path(venv_path, "Scripts", "python.exe")
      } else {
        python_interpreter <- file.path(venv_path, "bin", "python")
      }
      Sys.setenv(RETICULATE_PYTHON = python_interpreter)
    }
  )
)


