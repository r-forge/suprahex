#' Render complete page.
#' 
#' @param name of the template (e.g. index, demo, topic)
#' @param data data for the template
#' @param path location to create file.  If \code{""} (the default), 
#'   prints to standard out.
#' @export
render_page <- function(package, name, data, path = "") {
  # render template components
  pieces <- c("head", "navbar", "header", "content", "footer")
  components <- lapply(pieces, render_template, package = package, name, data)
  names(components) <- pieces
  
  # render complete layout
  out <- render_template(package, "layout", name, components)
  cat(out, file = path)
}

#' @importFrom whisker whisker.render
render_template <- function(package, type, name, data) {
  template <- readLines(find_template(package, type, name))
  if (length(template) <= 1 && str_trim(template) == "") return("")
  
  whisker.render(template, data)
}

# Find template by looking first in package/staticdocs then in 
# staticdocs/templates, trying first for a type-name.html otherwise 
# defaulting to type.html
find_template <- function(package, type, name) {

    names <- c(
        str_c(type, "-", name, ".html"),
        str_c(type, ".html")
    )
    
    tmp_local_path <- file.path(pkg_sd_path(package), "templates")
    tmp_default_path <- file.path(inst_path(), "templates")
    
    tmp_local_file <- as.vector(t(outer(tmp_local_path, names, FUN = "file.path")))
    tmp_default_file <- as.vector(t(outer(tmp_default_path, names, FUN = "file.path")))
    
    
    if(sum(file.exists(tmp_local_file))){
        locations <- tmp_local_file[file.exists(tmp_local_file)]
    }else if(sum(file.exists(tmp_default_file))){
        locations <- tmp_default_file[file.exists(tmp_default_file)]
    }else{
        stop("Can't find template for ", type, "-", name, ".", call. = FALSE)
    }
}


