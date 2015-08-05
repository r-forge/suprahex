#' Function to animate multiple component planes of a supra-hexagonal grid
#'
#' \code{visHexAnimate} is supposed to animate multiple component planes of a supra-hexagonal grid. The output can be a pdf file containing a list of frames/images, a mp4 video file or a gif file. To support video output file, the software 'ffmpeg' must be first installed (also put its path into the system PATH variable; see Note). To support gif output file, the software 'ImageMagick' must be first installed (also put its path into the system PATH variable; see Note).
#'
#' @param sMap an object of class "sMap"
#' @param filename the without-extension part of the name of the output file. By default, it is 'visHexAnimate'
#' @param filetype the type of the output file, i.e. the extension of the output file name. It can be one of either 'pdf' for the pdf file, 'mp4' for the mp4 video file, 'gif' for the gif file
#' @param image.type the type of the image files temporarily generated. It can be one of either 'jpg' or 'png'. These temporary image files are used for producing mp4/gif output file. The reason doing so is to accommodate that sometimes only one of image types is supported so that you can choose the right one
#' @param num.frame a numeric value specifying the number of frames/images. By default, it sets to the number of columns in the codebook matrix
#' @param sec_per_frame a numeric value specifying how long (seconds) it takes to stream a frame/image. This argument only works when producing mp4 video or gif file.
#' @param margin margins as units of length 4 or 1
#' @param height a numeric value specifying the height of device
#' @param title.rotate the rotation of the title
#' @param title.xy the coordinates of the title
#' @param colormap short name for the colormap. It can be one of "jet" (jet colormap), "bwr" (blue-white-red colormap), "gbr" (green-black-red colormap), "wyr" (white-yellow-red colormap), "br" (black-red colormap), "yr" (yellow-red colormap), "wb" (white-black colormap), and "rainbow" (rainbow colormap, that is, red-yellow-green-cyan-blue-magenta). Alternatively, any hyphen-separated HTML color names, e.g. "blue-black-yellow", "royalblue-white-sandybrown", "darkgreen-white-darkviolet". A list of standard color names can be found in \url{http://html-color-codes.info/color-names}
#' @param ncolors the number of colors specified
#' @param zlim the minimum and maximum z values for which colors should be plotted, defaulting to the range of the finite values of z. Each of the given colors will be used to color an equispaced interval of this range. The midpoints of the intervals cover the range, so that values just outside the range will be plotted
#' @param border.color the border color for each hexagon
#' @param gp an object of class gpar, typically the output from a call to the function gpar (i.e., a list of graphical parameter settings)
#' @return 
#' If specifying the output file name (see argument 'filename' above), the output file is either 'filename.pdf' or 'filename.mp4' or 'filename.gif' in the current working directory. If no output file name specified, by default the output file is either 'visHexAnimate.pdf' or 'visHexAnimate.mp4' or 'visHexAnimate.gif'
#' @note When producing mp4 video, this function requires the installation of the software 'ffmpeg' at \url{https://www.ffmpeg.org}. Shell command lines for ffmpeg installation in Terminal (for both Linux and Mac) are:
#' \itemize{
#' \item{1) \code{wget -O ffmpeg.tar.gz http://www.ffmpeg.org/releases/ffmpeg-2.7.1.tar.gz}}
#' \item{2) \code{mkdir ~/ffmpeg | tar xvfz ffmpeg.tar.gz -C ~/ffmpeg --strip-components=1}}
#' \item{3) \code{cd ffmpeg}}
#' \item{4a) # Assuming you want installation with a ROOT (sudo) privilege: \cr\code{./configure --disable-yasm}}
#' \item{4b) # Assuming you want local installation without ROOT (sudo) privilege: \cr\code{./configure --disable-yasm --prefix=$HOME/ffmpeg}}
#' \item{5) \code{make}}
#' \item{6) \code{make install}}
#' \item{7) # add the system PATH variable to your ~/.bash_profile file if you follow 4b) route: \cr\code{export PATH=$HOME/ffmpeg:$PATH}}
#' \item{8) # make sure ffmpeg has been installed successfully: \cr\code{ffmpeg -h}}
#' }
#' When producing gif file, this function requires the installation of the software 'ImageMagick' at \url{http://www.imagemagick.org}. Shell command lines for ImageMagick installation in Terminal are:
#' \itemize{
#' \item{1) \code{wget http://www.imagemagick.org/download/ImageMagick.tar.gz}}
#' \item{2) \code{mkdir ~/ImageMagick | tar xvzf ImageMagick.tar.gz -C ~/ImageMagick --strip-components=1}}
#' \item{3) \code{cd ImageMagick}}
#' \item{4) \code{./configure --prefix=$HOME/ImageMagick}}
#' \item{5) \code{make}}
#' \item{6) \code{make install}}
#' \item{7) # add the system PATH variable to your ~/.bash_profile file. \cr For Linux: \cr\code{export MAGICK_HOME=$HOME/ImageMagick} \cr\code{export PATH=$MAGICK_HOME/bin:$PATH} \cr\code{export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$MAGICK_HOME/lib} \cr For Mac: \cr\code{export MAGICK_HOME=$HOME/ImageMagick} \cr\code{export PATH=$MAGICK_HOME/bin:$PATH} \cr\code{export DYLD_LIBRARY_PATH=$MAGICK_HOME/lib/}}
#' \item{8a) # check configuration: \cr\code{convert -list configure}}
#' \item{8b) # check image format supported: \cr\code{identify -list format}}
#' \item{Tips: \cr Prior to 4), please make sure \code{libjpeg} and \code{libpng} are installed. If NOT, for Mac try this: \cr\code{brew install libjpeg libpng} \cr To check whether ImageMagick does work, please get additional information from: \cr\code{identify -list format} \cr\code{convert -list configure} \cr On details, please refer to \url{http://www.imagemagick.org/script/advanced-unix-installation.php}}
#' }
#' @export
#' @seealso \code{\link{visHexMulComp}}
#' @include visHexAnimate.r
#' @examples
#' \dontrun{
#' # 1) generate data with an iid matrix of 1000 x 3
#' data <- cbind(matrix(rnorm(1000*3,mean=0,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=0.5,sd=1), nrow=1000, ncol=3), 
#' matrix(rnorm(1000*3,mean=-0.5,sd=1), nrow=1000, ncol=3))
#' colnames(data) <- c("S1","S1","S1","S2","S2","S2","S3","S3","S3")
#'
#' # 2) sMap resulted from using by default setup
#' sMap <- sPipeline(data=data)
#'
#' # 3) animate sMap
#' # output as a <a href="visHexAnimate.pdf">pdf</a> file
#' visHexAnimate(sMap, filename="visHexAnimate", filetype="pdf")
#' # output as a <a href="visHexAnimate.mp4">mp4</a> file
#' visHexAnimate(sMap, filename="visHexAnimate", filetype="mp4")
#' # output as a <a href="visHexAnimate.gif">gif</a> file
#' visHexAnimate(sMap, filename="visHexAnimate", filetype="gif")
#' }

visHexAnimate <- function (sMap, filename="visHexAnimate", filetype=c("pdf", "mp4", "gif"), image.type=c("jpg","png"), num.frame=ncol(sMap$codebook), sec_per_frame=1, margin=rep(0.1,4), height=7, title.rotate=0, title.xy=c(0.45, 1), colormap=c("bwr","jet","gbr","wyr","br","yr","rainbow","wb"), ncolors=40, zlim=NULL, border.color="transparent", gp=grid::gpar())
{

    ## check input graph
    if (class(sMap) != "sMap"){
        stop("The funciton must apply to 'sMap' object.\n")
    }
    
    filetype <- match.arg(filetype)
    if(is.null(filename)){
        outputfile <- paste("visHexAnimate", filetype, sep=".")
    }else{
        outputfile <- paste(filename, filetype, sep=".")
    }
	
	cnames <- colnames(sMap$codebook)
    sMap_part <- sMap
	
	# Using functions 'recordPlot' and 'replayPlot' to save the current plot in an R variable, and to replay it
	n <- 0
	rplots <- list()
    for(t in seq(from=1, to=ncol(sMap$codebook), length.out=num.frame)){
        	
    	k <- floor(t)
        sMap_part$codebook <- matrix(sMap$codebook[,k], ncol=1)
        colnames(sMap_part$codebook) <- cnames[k]
			
		visHexMulComp(sMap=sMap_part, margin=margin, height=height, title.rotate=title.rotate, title.xy=title.xy, colormap=colormap, ncolors=ncolors, zlim=zlim, border.color=border.color, gp=gp)
		
		n <- n+1
		rplots[[n]] <- grDevices::recordPlot()
		grDevices::graphics.off()  
    }

    if(filetype=="pdf"){
        
        cnames <- colnames(sMap$codebook)
        sMap_part <- sMap
        
        if(height>100){
            height <- ceiling(height/100)
        }
        
        grDevices::pdf(outputfile, width=height, height=height)

		for(rplot in rplots){
    		grDevices::replayPlot(rplot)
		}
		grDevices::graphics.off()
        
        if(file.exists(file.path(getwd(), outputfile))){
            message(sprintf("Congratulations! A file '%s' (in the directory %s) has been created!", outputfile, getwd()), appendLF=T)
        }
        
        invisible()
        
    }else if(filetype=="mp4" | filetype=="gif"){
        
        ## num.frame: how many frames in total
        ## sec_per_frame: seconds per frame
        ## frame_per_sec: frames per second
        frame_per_sec <- 1/sec_per_frame
        
        image.type <- match.arg(image.type)
        
        ## specify the temporary image files
        tdir <- tempdir()
        if(image.type=='png'){
        	image_files <- file.path(tdir, "Rplot%06d.png")
        	## remove the existing temporary png files
        	unlink(file.path(tdir, "Rplot*.png"), recursive=T, force=T)
        }else if(image.type=='jpg'){
        	image_files <- file.path(tdir, "Rplot%06d.jpg")
        	## remove the existing temporary jpg files
        	unlink(file.path(tdir, "Rplot*.jpg"), recursive=T, force=T)
        }
        unlink(file.path(tdir, outputfile), recursive=T, force=T)
        
        if(height<10){
            height <- ceiling(height*100)
        }
        
        if(image.type=='png'){
        	grDevices::png(image_files, width=height, height=height)
        }else if(image.type=='jpg'){
        	grDevices::jpeg(image_files, width=height, height=height)
        }
        
		for (rplot in rplots) {
    		grDevices::replayPlot(rplot)
		}
		grDevices::graphics.off()
        
        if(filetype=="mp4"){
			ffmpeg1 <- paste("ffmpeg -y -v quiet -r", frame_per_sec, "-i", image_files, "-q:v 1", file.path(tdir, outputfile))
			ffmpeg2 <- paste("$HOME/ffmpeg -y -v quiet -r", frame_per_sec, "-i", image_files, "-q:v 1", file.path(tdir, outputfile))
			ffmpeg_local <- c(ffmpeg1, ffmpeg2)
			cmd_flag <- 1
			for(i in 1:length(ffmpeg_local)){
				cmd <- try(system(ffmpeg_local[i]), silent=TRUE)
				if(cmd==0){
					cmd_flag <- 0
					message(sprintf("Executing this command: '%s'\n", ffmpeg_local[i]), appendLF=T)
					break
				}
			}
			
		}else if(filetype=="gif"){
		
			## http://www.r-bloggers.com/animate-gif-images-in-r-imagemagick/
			## -delay ticks: '100 ticks' corresponds to 1 second
			## ticks/100: seconds per image/frame
			## 100/ticks: images/frames per second
		
			image_files <- paste('Rplot','*.', image.type, sep='')
			convert1 <- paste("convert -delay", 100*sec_per_frame, file.path(tdir, image_files), file.path(tdir, outputfile))
			convert2 <- paste("$HOME/ImageMagick/bin/convert -delay", 100*sec_per_frame, file.path(tdir, image_files), file.path(tdir, outputfile))
			convert_local <- c(convert1, convert2)
			cmd_flag <- 1
			for(i in 1:length(convert_local)){
				cmd <- try(system(convert_local[i]), silent=TRUE)
				if(cmd==0){
					cmd_flag <- 0
					message(sprintf("Executing this command: '%s'\n", convert_local[i]), appendLF=T)
					break
				}
			}
			
		}
		
        if(cmd_flag==0){
            if(file.exists(file.path(tdir, outputfile))){
                file.copy(from=file.path(tdir, outputfile), to=outputfile, overwrite=T, recursive=F, copy.mode=T)
                message(sprintf("Congratulations! A file '%s' (in the directory %s) has been created!", outputfile, getwd()), appendLF=T)
            }
        }else{
            stop("Unfortunately, fail to produce the file. Please install ffmpeg or ImageMagick first. Also make sure its path being put into the system PATH variable (see Help). Alternatively, produce the pdf file instead\n")
        }

        invisible(cmd)
    }
    
}
