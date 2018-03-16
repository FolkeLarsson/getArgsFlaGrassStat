# TODO: Add comment
# 
# Author: Folke Larsson, Boden Sweden
###############################################################################
library(stringr)
library(methods)
#options(expression = 20)

#' @author Folke Larsson Boden Sweden 
#' @name read_ini_file
#' @title read_ini_file parameters 
#' @description read variables from an ini-file into a dataframe that it returns. Part of specific project. 
#' @return dataframe with rows conaining arguments
#' @param v_path path to file
#' @param v_filename name of the file
#' @example
#' /donttest {
#' 	df_argument_rows <- read_ini_file("/grass7/parameter_files", "parameters.ini")
#' }
#' @export

read_ini_file <- function(v_path, v_filename){
	full_filename <- paste(paste(v_path, "/", sep=""), v_filename, sep="")
	fpath <- file.path(v_path, v_filename, sep=".Platform$file.sep")
	if(file.exists(full_filename)) {
		df_parameters <- utils::read.csv2(full_filename, header=FALSE)
	} else {
		stop(paste(full_filename, ": ini-file doesnt exists !!!! ----"), sep="")
	}# if
	return(df_parameters)
}# read_ini_file



