# TODO: quite a lot
# Generakize and replacing the the repeats with functions  
# Perhaps a list with name, retuen parameter and corresponding regular expression can generate both a 
# template for the ini-file and the block with if-expressions in this script
# Author: Folke Larsson, Boden Sweden
###############################################################################

library(stringr)
library(methods)


#' @author Folke Larsson in Boden Sweden 
#' @name read_arguments
#' @title read_arguments ini-file command prompt
#' @description reads parameters from an ini-file or command prompt into a dataframe that it returns. part of specific project.  
#' @return dataframe with processed arguments
#' @param  v_filepath path to file 
#' @param  v_filename name of the file
#' @param  v_read_from_inifile using inifile(TRUE) command line(FALSE)
#' @param v_task is the task parameters are supporting 
#' @example 
#' /dontrun{
#' df_args <- read_arguments("file path", "params.ini", TRUE, "calculate_means")
#' }
#' @export

#calculating statistics from shape-points and interpreted satelite-images. 

read_arguments <- function(v_filepath, v_filename, v_read_from_inifile, v_task) {

  task_list <- list(
  	task_clip_create_rasters = "clip_and_create_rasters",
	task_calculate_means = "calculate_means",
	task_calculate_classes_means = "calculate_classes_means",
	task_calculate_all = "all"
  )# list

 
 # currently not used 
 argument_regexp_list <- list(
	g_path <- "((\\/)|C(\\:)|c(\\:))(([A-Z|a-z]|[0-9]|(\\_))+((\\/)|(\\\\))?)+",
	g_identifier <- "([A-Z|a-z|0-9]|(\\_))+",
	task = "(clip_and_create_rasters|calculate_means|calculate_classes|all)",
	g_position <- "([0-9]+(\\.)*[0-9]))+",
	file = paste(g_path, "(\\.(tif|shp|rec))", sep=""),
	ulx = g_position,
	uly = g_position,
	lrx = g_position,
	lry = g_position,
	minlimit = "[0-9]{1,2}",
	incrs = g_identifier,
	outcrs = g_identifier,
	mapfile = file,
	shapefile = file,
	classfile = file,
	grass_raster = g_identifier,
	grass_raster_classes = g_identifier,
	prefix = g_identifier,
	suffix = g_identifier,
	radiuses = "(([0-9]+)(\\,)?)+",
	radius_seq ="(([0-9]+)(\\,)?){3}",
	convert_crs = "(TRUE|FALSE)",
	is_new_location = "(TRUE|FALSE)",
	shape_attribute = g_identifier
 )# argument_regexp_list
  
# regular_expressionx
	regexp_identifier <- "([A-Z|a-z|0-9]|(\\_))+"
	#regexp_path <- "((\\/)|C(\\:)|c(\\:))(([A-Z|a-z]|[0-9]|(\\_))+((\\/)|(\\\\))?)+"	
	regexp_path <- regexp_path <- "((\\/)|(C:\\\\)|(c:\\\\))((([A-Z|a-z]|[0-9]|(\\_))+)((\\/)|(\\\\)){1})+"
	regexp_file <- paste(regexp_path, "(\\.(tif|shp|rec))", sep="")
	regexp_task <- "^TASK(\\:)(clip_and_create_rasters|calculate_means|calculate_classes)"
	#regexp_pos <- "(ULX(\\:)|ULY(\\:)|LRX(\\:)|LRY(\\:))([0-9]+(\\.)*[0-9])+"
	regexp_position <- "([0-9]+(\\.)*[0-9]+)"
	regexp_ulx <- paste("^ULX(\\:)", regexp_position, sep="")
	regexp_uly <- paste("^ULY(\\:)", regexp_position, sep="")
	regexp_lrx <- paste("^LRX(\\:)", regexp_position, sep="")
	regexp_lry <- paste("^LRY(\\:)", regexp_position, sep="")
	
	#regexp_ulx <- "(^ULX(\\:)([0-9]+(\\.)*[0-9]))+"
	#regexp_uly <- "(^ULY(\\:)([0-9]+(\\.)*[0-9]))+"
	#regexp_lrx <- "(^LRX(\\:)([0-9]+(\\.)*[0-9]))+"
	#regexp_lry <- "(^LRY(\\:)([0-9]+(\\.)*[0-9]))+"
	
	
	
	regexp_minlimit <- "^MIN_LIMIT(\\:)[0-9]{1,2}"
	regexp_incrs <-   paste("^IN_CRS(\\:)", regexp_identifier, sep="")
	regexp_outcrs <-  paste("^OUT_CRS(\\:)", regexp_identifier, sep="")
	regexp_mapfile <- paste("^MAP_FILE(\\:)", regexp_file, sep="")
	regexp_grassraster <- paste("^GRASS_RASTER(\\:)", regexp_identifier, sep="")
	regexp_grassrasterclasses <- paste("^GRASS_RASTER_CLASSES(\\:)", regexp_identifier, sep="")
	regexp_shapefile <- paste("^SHAPE_FILE(\\:)", regexp_file, sep="")
	regexp_shapeattribute <- paste("^SHAPE_ATTRIBUTE(\\:)", regexp_identifier, sep="")
	regexp_isnewlocation <- "^IS_NEW_LOCATION(\\:)(TRUE|FALSE)"
	regexp_location <- paste("^LOCATION(\\:)", regexp_identifier, sep="")
	regexp_mapset <-   paste("^MAPSET(\\:)", regexp_identifier, sep="")
	regexp_classfile <- paste("^CLASS_FILE(\\:)", regexp_file, sep="")
	regexp_prefix <- paste("^PREFIX(\\:)", regexp_identifier, sep="")
	regexp_suffix <- paste("^SUFFIX(\\:)", regexp_identifier, sep="")
	regexp_gisbase <- paste("^GISBASE(\\:)", regexp_path, sep="")
	regexp_gisdbase <- paste("^GISDBASE(\\:)", regexp_path, sep="")
	regexp_convert_CRS <- "^CONVERT_CRS(\\:)(TRUE|FALSE)"
	regexp_radius_seq <- "^RADIUS_SEQ(\\:)(([0-9]+)(\\,)?)+"
	regexp_radiuses <- "^RADIUSES(\\:)(([0-9]+)(\\,)?)+"
	df_args <- data.frame("arguments")
	
	if (v_read_from_inifile == "TRUE") {	
		#df_ini_file <- read_ini_file("/home/follar/grass7/maps/parameter_files", "RGrass_parameters.ini")
		df_ini_file <- read_ini_file(v_filepath, v_filename)
		#print(str(df_ini_file))
		nr_file_rows <- nrow(df_ini_file)
		for(ind in 1:nr_file_rows) {
			#browser()
			curr_par <- df_ini_file[ind,1]
			
			if(regexpr(regexp_task, curr_par)[1] > 0 ) {
				df_args$task <- regmatches(curr_par, regexpr(regexp_task, curr_par))
			}
			
			if(regexpr(regexp_radius_seq, curr_par)[1] > 0 ) {
				df_args$radiusseq <- regmatches(curr_par, regexpr(regexp_radius_seq, curr_par))
			}
			
			if(regexpr(regexp_radiuses, curr_par)[1] > 0 ) {
				df_args$radiuses <- regmatches(curr_par, regexpr(regexp_radiuses, curr_par))
			}	

			if(regexpr(regexp_shapefile, curr_par)[1] > 0 ) {
				df_args$shapefile <- regmatches(curr_par, regexpr(regexp_shapefile, curr_par))
			}
			
			if(regexpr(regexp_mapfile, curr_par)[1] > 0) {
				df_args$mapfile <- regmatches(curr_par, regexpr(regexp_mapfile, curr_par))
			}		
			
			if(regexpr(regexp_classfile, curr_par)[1] > 0) {
				df_args$classfile <- regmatches(curr_par, regexpr(regexp_classfile, curr_par))
			}	
			
			if(regexpr(regexp_isnewlocation, curr_par)[1] >0 ) {
				df_args$isnewlocation <- regmatches(curr_par, regexpr(regexp_isnewlocation, curr_par))
			}
			
			if(regexpr(regexp_gisbase, curr_par)[1] >0 ) {
				df_args$gisbase <- regmatches(curr_par, regexpr(regexp_gisbase, curr_par))
			}
			
			if(regexpr(regexp_gisdbase, curr_par)[1] >0 ) {
				df_args$gisdbase <- regmatches(curr_par, regexpr(regexp_gisdbase, curr_par))
			}
			
			if(regexpr(regexp_ulx, curr_par)[1] >0 ) {
				df_args$ulx <- regmatches(curr_par, regexpr(regexp_ulx, curr_par))
			}
			
			if(regexpr(regexp_uly, curr_par)[1] >0 ) {
				df_args$uly <- regmatches(curr_par, regexpr(regexp_uly, curr_par))
			}
			
			if(regexpr(regexp_lrx, curr_par)[1] >0 ) {
				df_args$lrx <- regmatches(curr_par, regexpr(regexp_lrx, curr_par))
			}
			
			if(regexpr(regexp_lry, curr_par)[1] >0 ) {
				df_args$lry <- regmatches(curr_par, regexpr(regexp_lry, curr_par))
			}
			
			if(regexpr(regexp_minlimit, curr_par)[1] >0 ) {
				df_args$minlimit <- regmatches(curr_par, regexpr(regexp_minlimit, curr_par))
			}
			
			if(regexpr(regexp_shapeattribute, curr_par)[1] > 0) {
				df_args$shapeattribute <- regmatches(curr_par, regexpr(regexp_shapeattribute, curr_par))
			}
			
			if(regexpr(regexp_grassraster, curr_par)[1]  > 0)  {
				df_args$grassraster <- regmatches(curr_par, regexpr(regexp_grassraster, curr_par))
			}
			
			if(regexpr(regexp_grassrasterclasses, curr_par)[1]  > 0)  {
				df_args$grassrasterclasses <- regmatches(curr_par, regexpr(regexp_grassrasterclasses, curr_par))
			}
			
			if(regexpr(regexp_location, curr_par)[1]  > 0)  {
				df_args$location <- regmatches(curr_par, regexpr(regexp_location, curr_par))
			}

			if(regexpr(regexp_mapset, curr_par)[1] > 0) {
				df_args$mapset <- regmatches(curr_par, regexpr(regexp_mapset, curr_par))
			}
			
			if(regexpr(regexp_prefix, curr_par)[1] > 0) {
				df_args$prefix <- regmatches(curr_par, regexpr(regexp_prefix, curr_par))
			}
			
			if(regexpr(regexp_suffix, curr_par)[1] > 0) {
				df_args$suffix <- regmatches(curr_par, regexpr(regexp_suffix, curr_par))
			}
			
			if(regexpr(regexp_incrs, curr_par)[1] > 0) {
				df_args$incrs <- regmatches(curr_par, regexpr(regexp_incrs, curr_par))
			}
			
			if(regexpr(regexp_outcrs, curr_par)[1] > 0) {
				df_args$outcrs <- regmatches(curr_par, regexpr(regexp_outcrs, curr_par))
			}
			
			if(regexpr(regexp_convert_CRS, curr_par)[1] > 0) {
				df_args$convert_crs <- regmatches(curr_par, regexpr(regexp_convert_CRS, curr_par))
			}
			
		} # for

		if (length(df_args$gisbase)>0) { 
			dir_list <- unlist(strsplit(df_args$gisbase, ":"))
			df_args$gisbase    <- dir_list[2]
		}
		
		if (length(df_args$gisdbase)>0) { 
			dir_list <- unlist(strsplit(df_args$gisdbase, ":"))
			df_args$gisdbase    <- dir_list[2]
		}

		if (length(df_args$radiusseq)>0) { 
			dir_list <- unlist(strsplit(df_args$radiusseq, ":"))
			df_args$radiusseq    <- dir_list[2]
		}
		
		if (length(df_args$radiuses)>0) { 
			dir_list <- unlist(strsplit(df_args$radiuses, ":"))
			df_args$radiuses    <- dir_list[2]
		}	
		
		if (length(df_args$radiuses_p)>0) { 
			dir_list <- unlist(strsplit(df_args$radiuses_p, ":"))
			df_args$radiuses_p <- dir_list[2]
		}		
		
		if (length(df_args$radiuses_test)>0) { 
			dir_list <- unlist(strsplit(df_args$radiuses_test, ":"))
			df_args$radiuses_test    <- dir_list[2]
		}		

		if (length(df_args$radiuses_test2)>0) { 
			dir_list <- unlist(strsplit(df_args$radiuses_test2, ":"))
			df_args$radiuses_test2    <- dir_list[2]
		}
		
		if (length(df_args$minlimit)>0) { 
			dir_list <- unlist(strsplit(df_args$minlimit, ":"))
			df_args$minlimit    <- dir_list[2]
		}
		
		if (length(df_args$prefix)>0) { 
			dir_list <- unlist(strsplit(df_args$prefix, ":"))
			df_args$prefix    <- dir_list[2]
		}
		
		if (length(df_args$task)>0) { 
			dir_list <- unlist(strsplit(df_args$task, ":"))
			df_args$task    <- dir_list[2]
		}
		
		if (length(df_args$mapset)>0) { 
			dir_list <- unlist(strsplit(df_args$mapset, ":"))
			df_args$mapset    <- dir_list[2]
		}
		
		if (length(df_args$location)>0) { 
			dir_list <- unlist(strsplit(df_args$location, ":"))
			df_args$location    <- dir_list[2]
		}
		
		if (length(df_args$incrs)>0) { 
			dir_list <- unlist(strsplit(df_args$incrs, ":"))
			df_args$incrs    <- dir_list[2]
		}
		
		if (length(df_args$outcrs)>0) { 
			dir_list <- unlist(strsplit(df_args$outcrs, ":"))
			df_args$outcrs    <- dir_list[2]
		}
		
		if (length(df_args$classfile)>0) { 
			dir_list <- unlist(strsplit(df_args$classfile, ":"))
			df_args$classfile <- dir_list[2]
		}	
		
		if (length(df_args$mapfile)>0) { 
			dir_list <- unlist(strsplit(df_args$mapfile, ":"))
			df_args$mapfile <- dir_list[2]
		}
		
		if (length(df_args$shapefile)>0) { 
			dir_list <- unlist(strsplit(df_args$shapefile, ":"))
			df_args$shapefile <- dir_list[2]
		}# if
		
		if (length(df_args$grassraster)>0) { 
			dir_list <- unlist(strsplit(df_args$grassraster, ":"))
			df_args$grassraster <- dir_list[2]
		}# if
		
		if (length(df_args$grassrasterclasses)>0) { 
			dir_list <- unlist(strsplit(df_args$grassrasterclasses, ":"))
			df_args$grassrasterclasses <- dir_list[2]
		}# if
		
		if (length(df_args$ulx)>0) { 	
			dir_list <- unlist(strsplit(df_args$ulx, ":"))
			df_args$ulx <- dir_list[2]
		}
		
		if (length(df_args$uly)>0) { 
			dir_list <- unlist(strsplit(df_args$uly, ":"))
			df_args$uly <- dir_list[2]
		}
		
		if (length(df_args$lrx)>0) { 
			dir_list <- unlist(strsplit(df_args$lrx, ":"))
			df_args$lrx <- dir_list[2]
		}
		
		if (length(df_args$lry)>0) { 
			dir_list <- unlist(strsplit(df_args$lry, ":"))
			df_args$lry <- dir_list[2]
		}
		
		if (length(df_args$suffix)>0) { 
			dir_list <- unlist(strsplit(df_args$suffix, ":"))
			df_args$suffix <- dir_list[2]
		}
		
		if (length(df_args$isnewlocation)>0) { 
			dir_list <- unlist(strsplit(df_args$isnewlocation, ":"))
			df_args$isnewlocation    <- dir_list[2]
		}
		
		if (length(df_args$convert_crs)>0) { 
			dir_list <- unlist(strsplit(df_args$convert_crs, ":"))
			df_args$convert_crs    <- dir_list[2]
		}
		
	} else {
		args<-commandArgs(TRUE)
		if (is.null(args)) {
			stop("--- no arguments from command prompt !!! ---")
		}

		if(v_task == task_list$task_clip_create_rasters  ) { # "task_clip_create_rasters") {
			df_args$mapfile        <- args[1]  # source GTiff map file
			df_args$ulx            <- args[2]  # Upper-Left X-coordinate 
			df_args$uly            <- args[3]  # Upper-Left Y-coordinate 
			df_args$lrx            <- args[4]  # Lower-Left X-coordinate 
			df_args$lry            <- args[5]  # Lower-Left Y-coordinate 
			df_args$prefix         <- args[6]  # directory name for log/error files
			df_args$suffix         <- args[7]  # specific name of the new map, like area-name, added to given source filename 
			df_args$classfile      <- args[8]  # ASCII-file with rules for creating classes in Grass raster 
			df_args$location       <- args[9]  # new or existing Grass location
			df_args$mapset         <- args[10] # existing Grass mapset, if new localion always PERMANENT
			df_args$isnewlocation  <- args[11] # TRUE or FALSE if a new location should be created 
			df_args$convert_crs    <- args[12] # if an additional map will be created of cutted area with anoter CRS
		} else if(v_task == task_list$task_calculate_means ) { #"task_calculate_means") {       # if clip_and_create_rasters calculate_means  
			df_args$minlimit       <- args[1]  # for adjusting min-value in maps from 0 to a defined one, in this case excluding open areas, clearcuts etc
			df_args$prefix         <- args[2]  # directory name for both logging dir and csv-files etc
			df_args$grassraster    <- args[3]  # name of raster map from Grass 
			df_args$shapefile      <- args[4]  # path and name of shape file 
			df_args$location       <- args[5]  # existing Grass location
			df_args$mapset         <- args[6]  # existing Grass mapset
			df_args$shapeattribute <- args[7]  # name of attribute in shape layer, not mandatory if name is in shape file 
		} else if (v_task == task_list$task_calculate_classes_means  ) { # "task_calculate_classes_means") {
			df_args$prefix              <- args[1] #directory name for both logging dir and csv-files etc
			df_args$grassrasterclasses  <- args[2] # name of raster map with classes from Grass 
			df_args$shapefile           <- args[3] # # path and name of shape file 
			df_args$location            <- args[4]  # new or existing Grass location
			df_args$mapset              <- args[5]  # existing Grass mapset, if new localion always PERMANENT
			df_args$shapeattribute      <- args[6] # name of attribute in shape layer, not mandatory if name is in shape file 
		} else if(v_task ==  task_list$task_calculate_all ) { #task_calculate_all) {
			df_args$mapfile        <- args[1]  # source GTiff map file
			df_args$ulx            <- args[2]  # Upper-Left X-coordinate 
			df_args$uly            <- args[3]  # Upper-Left Y-coordinate 
			df_args$lrx            <- args[4]  # Lower-Left X-coordinate 
			df_args$lry            <- args[5]  # Lower-Left Y-coordinate 
			df_args$prefix         <- args[6]  # directory name for log/error files
			df_args$suffix         <- args[7]  # specific name of the new map, like area-name, added to given source filename 
			df_args$classfile      <- args[8]  # ASCII-file with rules for creating classes in Grass raster 
			df_args$location       <- args[9]  # new or existing Grass location
			df_args$mapset         <- args[10] # existing Grass mapset, if new localion always PERMANENT
			df_args$isnewlocation  <- args[11] # TRUE or FALSE if a new location should be created 
			df_args$minlimit       <- args[12]  # for adjusting min-value in maps from 0 to a defined one, in this case excluding open areas, clearcuts etc	
			df_args$shapefile      <- args[13] # # path and name of shape file 
			df_args$convert_crs    <- args[14] # if an additional map will be created of cutted area with anoter CRS
			df_args$shapeattribute <- args[15] # name of attribute in shape layer, not mandatory if name is in shape file 
		} else {
			stop(" --- no given task for processing ")	
		}# if-else tasks 
		
	} # if-else	command line args
	
	return(df_args)
}# read_arguments

