load_settings <- function(package) {
  #path <- file.path(pkg_sd_path(package), "docs.r")
  #if (!file.exists(path)) return(list())

    tmp_local_file <- file.path(pkg_sd_path(package), "docs.r")
    tmp_default_file <- file.path(inst_path(), "docs.r")
    
    if( file.exists(tmp_local_file) & length(file.exists(tmp_local_file))>0 ){
        source(tmp_local_file)$value
    }else{
        source(tmp_default_file)$value
    }


  #source(path)$value
}


#' Define a section for the index page
#'
#' @param name Name of the section. Used as title.
#' @param description Paragraph description of the section. May use markdown.
#' @param elements Either a list containing either strings giving function
#'   names, or if you want to override defaults from the rd file,
#    objects created by \code{\link{sd_item}}
#' @export
sd_section <- function(name, description, elements) {
  elements <- as.list(elements)
  strings <- vapply(elements, is.character, logical(1))
  elements[strings] <- lapply(elements[strings], sd_item)
  
  topics <- unlist(lapply(elements, "[[", "name"))
  
  list(name = name, description = description, elements = elements, 
    topics = topics)
}

#' Define an item in a section of the index.
#'
#' @param name name of the function
#' @param title override the default title extracted from the corresponding Rd
#'   file
#' @export
sd_item <- function(name, title = NULL) {
  list(name = name, title = title)
}

#' Define the icon for a function.
#'
#' @param expr,func Either a bare expression or a function with no arguments
#'   that uses grid to create a icon that represents the function.
#' @param inherits Alternatively, use an existing icon specified by a 
#'   function name
#' @export
sd_icon <- function(expr = NULL, func = NULL, inherits = NULL) {
  expr <- substitute(expr)

  if ((is.null(func) + is.null(inherits) + is.null(expr)) != 2) {
    stop("Specify one of expr, func or inherits", call. = FALSE)
  }
  
  if (!is.null(inherits)) {
    return(list(inherits = inherits))
  } else {
    if (!is.null(expr)) {
      func <- make_function(list(), expr)
    }
    list(func = func)
    
  }  
}

make_function <- function(args, expr, env = globalenv()) {
  args <- as.pairlist(args)
  
  f <- eval(call("function", args, expr))
  environment(f) <- env
  f
}