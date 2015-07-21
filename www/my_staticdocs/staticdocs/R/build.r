#' Build complete static documentation for a package.
#'
#' Currently, knitr builds documentation for:
#'
#' \itemize{
#'   \item Rd files.  Files
#'   \item Demos. Must be listed in \file{demo/List_demos.txt}.
#'   \item Vignettes.
#' }
#'
#' @param package path to source version of package.  See
#'   \code{\link[devtools]{as.package}} for details on how paths and package
#'   names are resolved.
#' @param base_path root directory in which to create documentation
#' @param examples include examples or not?  Examples are particularly
#'   slow to render because all code must be run, so turning them off makes
#'   it easier to tweak templates etc.
#' @param flag_demos include demos or not? Disable this if no update 
#' @export
#' @import stringr
#' @importFrom devtools load_all
#' @aliases staticdocs-package
build_package <- function(package, base_path = NULL, examples=T, replace.examplefiles.forced=T, flag_demos=T, flag_faqs=T) {
  load_all(package)
  
  package <- package_info(package, base_path, examples=examples)
  if (!file.exists(package$base_path)) dir.create(package$base_path)
  copy_bootstrap(base_path)
    
    # if there is a directory 'images', copy to the destinaton
    images_local_path <- file.path(pkg_sd_path(package), "images")
    if(file.exists(images_local_path)){
        file.copy(dir(images_local_path, full.names = TRUE), base_path, recursive = TRUE)
    }
    
    # copy a directory 'R'
    dest <- file.path(package$base_path, "R")
    if (!file.exists(dest)) dir.create(dest)
    R_local_path <- file.path(gsub('/inst/staticdocs','',pkg_sd_path(package)), "R")
    if(file.exists(R_local_path)){
        file.copy(dir(R_local_path, full.names = TRUE), dest, recursive = TRUE)
    }
    
    # copy a directory 'man'
    dest <- file.path(package$base_path, "man")
    if (!file.exists(dest)) dir.create(dest)
    R_local_path <- file.path(gsub('/inst/staticdocs','',pkg_sd_path(package)), "man")
    if(file.exists(R_local_path)){
        file.copy(dir(R_local_path, full.names = TRUE), dest, recursive = TRUE)
        
        R_functions <- package$collate
        #######################
        ## rename *.Rd to *.txt becaue the web cannot identify *.Rd files
        all_files <- list.files(path=dest, pattern="*.Rd",full.name=T)
        sapply(all_files, function(x){
        
            z <- gsub(".Rd$", ".pdf", x, perl=T)
            t <- gsub(".*/", "", x, perl=T)
            s <- gsub(".Rd$", "", t, perl=T)
            w <- gsub(".Rd$", ".r", t, perl=T)
            #R CMD Rd2pdf --no-preview --force --no-index --no-description --title=dcAlgoPredict --output=dcAlgoPredict.pdf dcGOR/man/dcAlgoPredict.Rd
            if (w %in% R_functions) {
                system(paste(shQuote(file.path(R.home("bin"), "R"))," CMD", " Rd2pdf", " --no-preview --force --no-index --no-description --output=", z, " --title=",s," ",x, sep=''))
            }
            
            y <- gsub(".Rd$", ".txt", x, perl=T)
            file.rename(x, y)
        })
        #######################
    }
    
    # copy a directory 'demo'
    dest <- file.path(package$base_path, "demo")
    if (!file.exists(dest)) dir.create(dest)
    R_local_path <- file.path(pkg_sd_path(package), "demo")
    if(file.exists(R_local_path)){
        file.copy(dir(R_local_path, full.names = TRUE), dest, recursive = TRUE)
    }

    # copy a directory 'faq'
    dest <- file.path(package$base_path, "faq")
    if (!file.exists(dest)) dir.create(dest)
    R_local_path <- file.path(pkg_sd_path(package), "faq")
    if(file.exists(R_local_path)){
        file.copy(dir(R_local_path, full.names = TRUE), dest, recursive = TRUE)
    }

  package$vignettes <- build_vignettes(package)
  package$manuals <- build_manual(package)
  package$readme <- readme(package)
  
  package$installation <- install_cite_rdata(package, "INSTALLATIONs.md")
  package$citation <- install_cite_rdata(package, "CITATIONs.md")
  package$rdata <- install_cite_rdata(package, "RData.md")
  
  package$demos <- build_demos(package, flag_demos=flag_demos)
  package$faqs <- build_faqs(package, flag_faqs=flag_faqs)
  
  package$topics <- build_topics(package, replace.examplefiles.forced=replace.examplefiles.forced)
  
  package <- build_index(package)

  ###############
  # for index html
  message("Generating index.html")
  out <- file.path(package$base_path, "index.html")
  package$pagetitle <- "Home"
  render_page(package, "index", package, out)

  ###############
  # for docs html
  message("Generating docs.html")
  out <- file.path(package$base_path, "docs.html")
  package$pagetitle <- "Docs"
  render_page(package, "docs", package, out)
  
  ###############
  # for install html
  message("Generating install.html")
  out <- file.path(package$base_path, "install.html")
  package$pagetitle <- "Install"
  render_page(package, "install", package, out)
  
  ###############
  # for cite html
  message("Generating cite.html")
  out <- file.path(package$base_path, "cite.html")
  package$pagetitle <- "Cite"
  render_page(package, "cite", package, out)
  
  ###############
  # for RData_README html
  message("Generating rdata.html")
  out <- file.path(package$base_path, "rdata.html")
  package$pagetitle <- "RData"
  render_page(package, "rdata", package, out)
  
  ###############
  # for demos html
  message("Generating demos.html")
  out <- file.path(package$base_path, "demos.html")
  package$pagetitle <- "Demos"
  render_page(package, "demos", package, out)
  
  ###############
  # for faqs html
  message("Generating faqs.html")
  out <- file.path(package$base_path, "faqs.html")
  package$pagetitle <- "FAQs"
  render_page(package, "faqs", package, out)
  
  ###############
  
  if (interactive()) {
    browseURL(normalizePath(file.path(base_path, "index.html")))
  }
  
  invisible(TRUE)
}


#' Generate all topic pages for a package.
#'
#' @export
#' @inheritParams build_package
#' @param package_info A list containing information about the package,
#'   as generated by \code{\link{package_info}}
#' @keywords internal
build_topics <- function(package, replace.examplefiles.forced) {

  # for each file, find name of one topic
  index <- package$rd_index
  paths <- file.path(package$base_path, index$file_out)

  # create columns for extra topic info
  index$title <- ""
  index$in_index <- TRUE
  
    ###############
    R_functions <- package$collate
    wh <- dev.size(units="px")
    wth <- 1200
    hgt <- wth*wh[2]/wh[1]
    ###############
  
  for (i in seq_along(index$name)) {
    message("Generating ", basename(paths[[i]]))
    
    rd <- package$rd[[i]]
    html <- to_html(rd, 
      env = new.env(parent = globalenv()), 
      topic = str_replace(basename(paths[[i]]), "\\.html$", ""),
      package = package)
    html$pagetitle <- html$name

    html$package <- package[c("package", "version")]
    
    # inherit several items from package
    if(1){
        package_items <- names(package)
        html_items <- names(html)
        for(item in package_items){
            if(!(item %in% html_items)){
                html[[item]] <- package[[item]]
            }
        }
    }
    
    if ("internal" %in% html$keywords) {
      index$in_index[i] <- FALSE
    }
    index$title[i] <- html$title
    
    #####################################################################################
    if(replace.examplefiles.forced==F){
        # only those that do not exist will be regenerated
        if(file.exists(paths[[i]])){
            next
        }
    }
    #####################################################################################
    
    ## popup window for source code
    ##########################################
    temp <- paste(index$name[i],'.r', sep='')
    if (temp %in% R_functions) {
        #html$sourcecode <- paste("\n<code><a href=R/",temp,">",temp,"</a></code>\n", sep='')
        name <- paste("R/",temp, sep='')
        html$sourcecode <- str_c("<a href=\"javascript:newWin('", name,"', '", name,"', '",wth,"', '",hgt,"')\" title=\"Click to view\"><B><code>",temp,"</code></B></a>")
    }
    ###########################################
    
    ## popup window for Rd
    ##########################################
    #temp <- paste(index$name[i],'.Rd', sep='')
    temp <- paste(index$name[i],'.txt', sep='')       
    if (temp %in% gsub(".r$",".txt",R_functions,perl=T)) {
        name <- paste("man/",temp, sep='')
        html$sourceman <- str_c("<a href=\"javascript:newWin('", name,"', '", name,"', '",wth,"', '",hgt,"')\" title=\"Click to view\"><B><code>",gsub(".txt$",".Rd",temp,perl=T),"</code></B></a>")
        
        name_temp <- gsub(".txt$",".pdf",name,perl=T)
        html$sourceman_pdf <- str_c("<a href=\"javascript:newWin('", name_temp,"', '", name_temp,"', '",wth,"', '",hgt,"')\" title=\"Click to view\"><B><code>",gsub(".txt$",".pdf",temp,perl=T),"</code></B></a>")
    }
    ###########################################
    
    ########
    # remove those cell empty
    flag <- sapply(html$sections, function(x) length(x)>0)
    html$sections <- html$sections[flag>=1]
    ########
    
    render_page(package, "topic", html, paths[[i]])
    graphics.off()

  }

  index
}

readme <- function(package) {
    #if (package$readme!="") return(markdown(package$readme))
    
    top_path <- file.path(package$path, "README.md")
    local_path <- file.path(pkg_sd_path(package), "README.md")
  
    if(file.exists(top_path)){
        markdown(path = top_path)
    }else if(file.exists(local_path)){
        markdown(path = local_path)
    }else{
        # use description if no README.md is available
        #return( package$description )
        return( paste("<h2>",package$title,"</h2>\n\n", package$description, sep="") )
    }
}

install_cite_rdata <- function(package, input_md_file="") {
    
    local_path <- file.path(pkg_sd_path(package), input_md_file)
  
    if(file.exists(local_path)){
        markdown(path = local_path)
    }else{
        return()
    }
}

copy_bootstrap <- function(base_path) {
  bootstrap <- file.path(inst_path(), "bootstrap")
  file.copy(dir(bootstrap, full.names = TRUE), base_path, recursive = TRUE)
}


#' List all package vignettes.
#'
#' Copies all vignettes and returns data structure suitable for use with
#' whisker templates.
#'
#' @keywords internal
#' @inheritParams build_package
#' @importFrom tools buildVignettes
#' @return a list, with one element for each vignette containing the vignette
#'   title and file name.
build_vignettes <- function(package) {  
  # Locate source and built versions of vignettes
  path <- dir(file.path(package$path, c("inst/doc", "vignettes")), ".Rnw", 
    full.names = TRUE)
  if (length(path) == 0) return()
  
  message("Building vignettes")
  buildVignettes(dir = package$path)
  
  message("Copying vignettes (.pdf and .Rnw)")
  src <- str_replace(path, "\\.Rnw$", ".pdf")
  filename_pdf <- basename(src)
  filename_Rnw <- basename(path)
  dest <- file.path(package$base_path, "vignettes")

  if (!file.exists(dest)) dir.create(dest)
  file.rename(src, file.path(dest, filename_pdf))  
  file.copy(path, file.path(dest, filename_Rnw))  

  # Extract titles
  title <- vapply(path, FUN.VALUE = character(1), function(x) {
    contents <- str_c(readLines(x), collapse = "\n")
    str_match(contents, "\\\\VignetteIndexEntry\\{(.*?)\\}")[2]
  })
  
  list(vignette = unname(apply(cbind(filename_pdf,filename_Rnw,title), 1, as.list)))
}


build_manual <- function(package) {

    message("Building reference manual")
    path <- package$path
    if (length(path) == 0) return()
    
    src <- file.path(path, str_c(package$package, ".pdf"))
    filename_pdf <- basename(src)
    
    system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", "--no-preview --force -o", src, shQuote(path))) 

    message("Moving reference manual")
    
    dest <- file.path(package$base_path, "vignettes")
    if (!file.exists(dest)) dir.create(dest)
    file.rename(src, file.path(dest, filename_pdf))

    # Extract titles
    title <- paste(package$package, "Reference Manual")
  
    list(manual=unname(apply(cbind(filename_pdf,title), 1, as.list)))
}


build_demos <- function(package, flag_demos) {
  demo_dir <- file.path(pkg_sd_path(package), "demo")

  if (!file.exists(demo_dir)) return()
  
  message("Rendering demos")
  demos <- readLines(file.path(demo_dir, "List_demos.txt"))
  
  pieces <- str_split_fixed(demos, "\\s+", 2)
  in_path <- str_c(pieces[, 1], ".r")
  filename <- str_c("demo-", pieces[,1], ".html")
  title <- pieces[, 2]
  
  if(sum(flag_demos)){
    
        ## controlling which demo will be run
        tmp <- rep(TRUE, length(title))
        tmp[1:length(flag_demos)] <- flag_demos
        flag_demos <- tmp
        
        ##################################################################################
    
        ###############
        wh <- dev.size(units="px")
        wth <- 1200
        hgt <- wth*wh[2]/wh[1]
        ###############
    
      for(i in seq_along(title)) {
        
        if(flag_demos[i]==FALSE) next
        
        message("\t", title[i])
        
        demo_code <- readLines(file.path(demo_dir, in_path[i]))
        #demo_expr <- evaluate(demo_code, new.env(parent = globalenv()))
        demo_expr <- my_evaluate(demo_code, new.env(parent = globalenv()))
		
        package$demo <- replay_html(demo_expr, package = package, name = str_c(pieces[i], "-"))
        
        ########################################################
        # Functions used are hyperlinked to the relevant documentation

        tmp_demo <- package$demo
        
        #all_fun <- package$rd_index[,1]
        all_fun <- str_replace_all(package$collate, '.r$', '')
        
        # add hypelink
        
        ## all functions found within demo
        functions_found_within_demo <- vector()
        
        tmp_demo_lines <- unlist(str_split(tmp_demo, '\n'))
        for(k in 1:length(tmp_demo_lines)){
            
            source <- tmp_demo_lines[k]
            
            ## find all (e.g. "sCompReorder"   "visCompReorder")
            list_find <- vector()
            for(t in 1:length(all_fun)){
                target <- all_fun[t]
                
                if(!is.na(str_match(source, target))){
                    list_find <- c(list_find,target)
                }
            }
            
            ## remain the longest string
            if(length(list_find)){
                target_final <- list_find[which.max(nchar(list_find))]
                tmp_replace <- str_c("<a href='", target_final, ".html' target=", target_final, ">",target_final,"</a>")
                tmp_demo_lines[k] <- str_replace_all(source, target_final, tmp_replace)
                
                functions_found_within_demo <- c(functions_found_within_demo, target_final)
            }
            
        }
        replace_demo <- str_c(tmp_demo_lines, collapse='\n')
        package$demo <- replace_demo
        
        name <- unique(functions_found_within_demo)
        if(length(name)){
            hypername <- str_c(name, collpase='.html')
            package$demo_funcs <- list(demo_func = unname(apply(cbind(name, hypername), 1, as.list)))
        }
        
        ########################################################
        
        ## popup window for demo
        ##########################################
        temp <- in_path[i]
        name <- paste("demo/",temp, sep='')
        package$sourcedemo <- str_c("<a href=\"javascript:newWin('", name,"', '", name,"', '",wth,"', '",hgt,"')\" title=\"Click to view\"><B><code>",temp,"</code></B></a>")
        ###########################################
        
        
        package$pagetitle <- title[i]
        render_page(package, "demo", package, file.path(package$base_path, filename[i]))
        graphics.off()
      }
  }
  
  list(demo = unname(apply(cbind(filename, title), 1, as.list)))
}

build_faqs <- function(package, flag_faqs) {
  faq_dir <- file.path(pkg_sd_path(package), "faq")

  if (!file.exists(faq_dir)) return()
  
  message("Rendering faqs")
  faqs <- readLines(file.path(faq_dir, "List_faqs.txt"))
  
  pieces <- str_split_fixed(faqs, "\\s+", 2)
  in_path <- str_c(pieces[, 1], ".r")
  filename <- str_c("faq-", pieces[,1], ".html")
  title <- pieces[, 2]
  
  if(sum(flag_faqs)){
    
        ## controlling which faq will be run
        tmp <- rep(TRUE, length(title))
        tmp[1:length(flag_faqs)] <- flag_faqs
        flag_faqs <- tmp
        
        ##################################################################################
    
        ###############
        wh <- dev.size(units="px")
        wth <- 1200
        hgt <- wth*wh[2]/wh[1]
        ###############
    
      for(i in seq_along(title)) {
        
        if(flag_faqs[i]==FALSE) next
        
        message("\t", title[i])
        
        faq_code <- readLines(file.path(faq_dir, in_path[i]))
        faq_expr <- evaluate(faq_code, new.env(parent = globalenv()))

        package$faq <- replay_html(faq_expr, package = package, name = str_c(pieces[i], "-"))
        
        
        ########################################################
        # Functions used are hyperlinked to the relevant documentation

        tmp_faq <- package$faq
        
        #all_fun <- package$rd_index[,1]
        all_fun <- str_replace_all(package$collate, '.r$', '')
        
        # add hypelink
        
        ## all functions found within faq
        functions_found_within_faq <- vector()
        
        tmp_faq_lines <- unlist(str_split(tmp_faq, '\n'))
        for(k in 1:length(tmp_faq_lines)){
            
            source <- tmp_faq_lines[k]
            
            ## find all (e.g. "sCompReorder"   "visCompReorder")
            list_find <- vector()
            for(t in 1:length(all_fun)){
                target <- all_fun[t]
                
                if(!is.na(str_match(source, target))){
                    list_find <- c(list_find,target)
                }
            }
            
            ## remain the longest string
            if(length(list_find)){
                target_final <- list_find[which.max(nchar(list_find))]
                tmp_replace <- str_c("<a href='", target_final, ".html' target=", target_final, ">",target_final,"</a>")
                tmp_faq_lines[k] <- str_replace_all(source, target_final, tmp_replace)
                
                functions_found_within_faq <- c(functions_found_within_faq, target_final)
            }
            
        }
        replace_faq <- str_c(tmp_faq_lines, collapse='\n')
        package$faq <- replace_faq
        
        
        name <- unique(functions_found_within_faq)
        if(length(name)){
            hypername <- str_c(name, collpase='.html')
            package$faq_funcs <- list(faq_func = unname(apply(cbind(name, hypername), 1, as.list)))
        }
        ########################################################
        
        ## popup window for faq
        ##########################################
        temp <- in_path[i]
        name <- paste("faq/",temp, sep='')
        package$sourcefaq <- str_c("<a href=\"javascript:newWin('", name,"', '", name,"', '",wth,"', '",hgt,"')\" title=\"Click to view\"><B><code>",temp,"</code></B></a>")
        ###########################################
        
        
        package$pagetitle <- title[i]
        render_page(package, "faq", package, file.path(package$base_path, filename[i]))
        graphics.off()
      }
  }
  
  list(faq = unname(apply(cbind(filename, title), 1, as.list)))
}



my_evaluate <- function (input, envir = parent.frame(), enclos = NULL, debug = FALSE, 
    stop_on_error = 0L, keep_warning = TRUE, keep_message = TRUE, 
    new_device = TRUE, output_handler = default_output_handler) 
{
    parsed <- parse_all(input)
    stop_on_error <- as.integer(stop_on_error)
    stopifnot(length(stop_on_error) == 1)
    if (is.null(enclos)) {
        enclos <- if (is.list(envir) || is.pairlist(envir)) 
            parent.frame()
        else baseenv()
    }
    if (new_device) {
        if (identical(grDevices::pdf, getOption("device"))) {
            dev.new(file = NULL)
        }
        else dev.new()
        dev.control(displaylist = "enable")
        dev <- dev.cur()
        on.exit(dev.off(dev))
    }
    on.exit(assign("last_plot", NULL, envir = environment(plot_snapshot)), 
        add = TRUE)
    out <- vector("list", nrow(parsed))
    for (i in seq_along(out)) {
        expr <- parsed$expr[[i]]
        if (!is.null(expr)){
        	#expr <- paste(expr, "\n", sep='')
            expr <- as.expression(expr)
        }
        out[[i]] <- evaluate_call(expr, parsed$src[[i]], envir = envir, 
            enclos = enclos, debug = debug, last = i == length(out), 
            use_try = stop_on_error != 2L, keep_warning = keep_warning, 
            keep_message = keep_message, output_handler = output_handler)
        if (stop_on_error > 0L) {
            errs <- vapply(out[[i]], is.error, logical(1))
            if (!any(errs)) 
                next
            if (stop_on_error == 1L) 
                break
        }
    }
    unlist(out, recursive = FALSE, use.names = FALSE)
}
environment(my_evaluate) <- asNamespace('evaluate')
