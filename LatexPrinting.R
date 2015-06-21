# R2LaTeX - Utility file for constructing LaTeX output for R dataframes or matrices 
# for direct insertion into LaTeX documents. 
#
# Dependency: stringr library
# 
# Author: Christopher Frantz
# Version 0.01 - June 2015
###############################################################################

#' Checks for installation of required R packages.
checkPrerequisites <- function() {
	packageInstalled = FALSE
	for(p in installed.packages()[, c("Package")]) {
		if(p == "stringr") {
			packageInstalled = TRUE
		}
	}
	if(!packageInstalled) {
		install.packages("stringr")
	}
}

checkPrerequisites()

library(stringr)

#' Determines the number of decimal places in a given input value.
#'
#' @param val Input value to be tested
#'
determineDecimalPlaces <- function(val) {
	#Test for decimal places
	if ((val %% 1) != 0) {
		#Count those if existing by splitting value on . and counting places
		return(nchar(strsplit(sub('0+$', '', as.character(val)), ".", fixed=TRUE)[[1]][[2]]))
	} else {
		return(0)
	}
}

#' Determines the LaTeX numeric column format (for siunitx package) based on decimal places of contained values.
#'
#' @param arrayOfValues Array of values to be tested
#' @param determineDecimalPlaces = TRUE Indicates whether decimal places should be detected or rely on default values
#' @param defaultDecimalPlaces = 3 Default number of decimal places if not detected
#' @param fixedLeadingPlaces = FALSE Indicates whether leading places are fixed despite detection of decimal places
#' @param defaultLeadingPlaces = 1 Default number of leading places
#'
determineFormat <- function(arrayOfValues, determineDecimalPlaces = TRUE, defaultDecimalPlaces = 3, fixedLeadingPlaces = FALSE, defaultLeadingPlaces = 1){
	#Check for trailing decimal places
	if(determineDecimalPlaces){
		#print(paste("Input:", arrayOfValues))
		decimalPlaces = max(sapply(arrayOfValues[!is.na(arrayOfValues)], function(x){determineDecimalPlaces(as.numeric(x))}))
		#print(paste("Longest dec. places:", decimalPlaces))
		#Override with default places if too long
		if(decimalPlaces > defaultDecimalPlaces){
			decimalPlaces = defaultDecimalPlaces
		}
	} else {
		decimalPlaces = defaultDecimalPlaces
	}
	#Check on leading decimal places
	pos = str_locate(as.character(max(arrayOfValues)), "\\.")
	if(is.na(pos[1])){
		#no decimal places
		plc = nchar(as.character(max(arrayOfValues)))
	} else {
		plc = pos[1] - 1
	}
	if(fixedLeadingPlaces){
		plc = defaultLeadingPlaces
	}
	#Generate siunitx column header
	colHeader = paste("S[table-format=", plc, ".", decimalPlaces, "]", sep="")
	return(colHeader)
}

#' Determines whether a given input contains text (or rather non-numeric content).
#'
#' @param val Value to test for non-numeric content
#'
containsText <- function(val){
	result <- any(!grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", val, perl=TRUE))
}


#' Generates LaTeX tabular or table environment from given R data frame or matrix.
#' Notes: 
#' If automatic column formatting is used, the output code will require the 'siunitx' package in LaTeX.
#' If separateHeadersFromData is activated, the output code will require the 'booktabs' package in LaTeX.
#' 
#' @param dataToPrint Data frame or matrix holding data
#' @param filename = "GenericTableOutput.txt" File the generated output is saved in.
#' @param printColumnHeaders = TRUE Adds column headers to output. Uses colnames of input data frame
#' @param printRowHeaders = FALSE Adds row headers to output. Uses rownames of input data frame.
#' @param printColumnSeparators = FALSE Indicates if columns should be separated by '|'
#' @param printRowSeparators = FALSE Indicates if rows should be separated by '\hline'
#' @param NonSeparatedHeaders = TRUE Does not print separators between column headers even if activated for columns.
#' @param separateHeadersFromData = FALSE Indicates if separators should be printed between headers and data (both for row and column headers).
#' @param boldColumnHeaders = TRUE Plots the column headers in bold face
#' @param columnLabel = NA Column label that spans across all columns (in addition to column headers).
#' @param rowLabel = NA Row label that spans across all rows (in addition to row headers).
#' @param roundToDecimalPlaces = 3 Decimal places values should be rounded to if numeric (and used for automated decimal point-centered column formatting).
#' @param fixedLeadingDecimalPlaces = NA Fixes leading decimal places (i.e. before the decimal point) to fixed length for the purpose of automated decimal place-centered column formatting. By default automatically determined from data.
#' @param determineLeadingDecimalPlacesPerColumn = TRUE Determines leading decimal places for centered formatting per numeric column (instead of global value)
#' @param determineTrailingDecimalPlacesPerColumn = TRUE Determines trailing decimal places for centered formatting per numeric column (instead of global value)
#' @param rowHeaderColumnFormat = "c" Column format for row header column (if activated).
#' @param dataColumnFormat = NA Column format for data entries. If not specified and if data is numeric and continuous, values are aligned by decimal point and column width determined based on largest value.
#' @param writeFullTableEnv = TRUE Writes complete table environment for immediate paste into LaTeX, otherwise only tabular environment.
#' 
#' 
#' @example 
#' 
#' #Simple example:
#' 
#' m <- matrix(rexp(100), 10)
#' printLatexTable(m)
#' 
#' #More complex example:
#' 
#' attach(mtcars)
#' printLatexTable(mtcars, "mtcarsTable.txt", TRUE, TRUE, TRUE, boldColumnHeaders = TRUE, columnLabel = "Attributes", rowLabel = "Car Models")
#' 
#' 
printLatexTable <- function(dataToPrint, filename = "GenericTableOutput.txt", printColumnHeaders = TRUE, printRowHeaders = FALSE, printColumnSeparators = FALSE, printRowSeparators = FALSE, NonSeparatedHeaders = TRUE, separateHeadersFromData = FALSE, boldColumnHeaders = TRUE, columnLabel = NA, rowLabel = NA, roundToDecimalPlaces = 3, fixedLeadingDecimalPlaces = NA, determineLeadingDecimalPlacesPerColumn = TRUE, determineTrailingDecimalPlacesPerColumn = TRUE, rowHeaderColumnFormat = "c", dataColumnFormat = NA, writeFullTableEnv = TRUE){
	
	if(is.null(dataToPrint)) {
		stop("Passed data is null.")
	}
	if(is.data.frame(dataToPrint) || is.matrix(dataToPrint)) {
		if(length(row(dataToPrint)) == 0) {
			stop("Passed dataframe or matrix does not have entries.")
		}
	}
	
	#Formatting-related stuff
	decimalPlaces = 3 #decimal places for rounding and centered formatting
	if(!is.na(roundToDecimalPlaces)) {
		decimalPlaces = roundToDecimalPlaces
	}
	
	#Column specification for text
	defaultTextDataColumn = "c"
	
	#Test which columns hold numbers in order to figure out if it makes sense to perform decimal place-based centering
	colsWithNumbers <- apply(dataToPrint, 2, function(x){!containsText(x)})

	#print(colsWithNumbers)
	if(length(colsWithNumbers) == 0) {
		#Alignment only works for numeric columns
		defaultDataColumn = "c"
		print("No numeric columns")
	} else {
		#Automatically determine length of decimal places

		#combine values of different columns
		vec <- vector(length=0)
		for(i in 1:length(colsWithNumbers)) {
			if(colsWithNumbers[i] != FALSE){
				vec <- c(vec, dataToPrint[,i], recursive = TRUE)
			}
		}
		#ignore NAs
		vec <- vec[!is.na(vec)]	
		#Determine column format
		defaultDataColumn = determineFormat(vec, determineTrailingDecimalPlacesPerColumn, decimalPlaces, !is.na(fixedLeadingDecimalPlaces), fixedLeadingDecimalPlaces)
		#Override with user settings if applicable
		if(!is.na(dataColumnFormat)) {
			defaultDataColumn = dataColumnFormat
		}
	}
	
	defaultRowHeaderColumn = "c" #Column alignment for column holding row headers
	if(!is.na(rowHeaderColumnFormat)) {
		defaultRowHeaderColumn = rowHeaderColumnFormat
	}
	
	colSep = " & "
	rowSep = " \\\\"
	line = paste("\\", "hline", sep="")
	
	#Determine dimensions - check for colnames, which may consider columns appended after initialisation
	rowLength = length(dataToPrint[,1])
	if(!is.null(rownames(dataToPrint)) && length(rownames(dataToPrint)) > rowLength) {
		rowLength = length(rownames(dataToPrint))
	}
	
	colLength = length(dataToPrint[1,])
	if(!is.null(colnames(dataToPrint)) && length(colnames(dataToPrint)) > colLength) {
		colLength = length(colnames(dataToPrint))
	}

	strVec = vector(length=0)
	if(writeFullTableEnv) {
		#Write table environment
		strVec = c(strVec, "\\begin{table}[h!]")
		strVec = c(strVec, "")
		strVec = c(strVec, "\\caption{Table}")
		strVec = c(strVec, "")
		strVec = c(strVec, "\\label{tab:GeneratedTable}")
		strVec = c(strVec, "")
	}
	#Writing table column specifications
	strVec = c(strVec, "\\begin{tabular}{")
	if(printColumnSeparators) {
		strVec[length(strVec)] <- paste(strVec[length(strVec)], "|", sep="")
	}
	#Add additional column for row label
	if(!is.na(rowLabel)) {
		if(printColumnSeparators) {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "c", "|", sep="")
		} else {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "c", " ", sep="")
		}
	}
	#Add additional column format if rowname printing is activated
	if(printRowHeaders) {
		#Consider separation of header and data
		if(printColumnSeparators | separateHeadersFromData){
			strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultRowHeaderColumn, "|", sep="")
		} else {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultRowHeaderColumn, " ", sep="")
		}
	}
	#Formatting for data columns
	for(headerIndex in 1:colLength) {
		if(printColumnSeparators) {
			if(colsWithNumbers[headerIndex] == TRUE) {
				if(defaultDataColumn != defaultTextDataColumn && (determineLeadingDecimalPlacesPerColumn || determineTrailingDecimalPlacesPerColumn) && !is.na(dataToPrint[,headerIndex])){
					strVec[length(strVec)] <- paste(strVec[length(strVec)], determineFormat(dataToPrint[,headerIndex], determineTrailingDecimalPlacesPerColumn, roundToDecimalPlaces), "|", sep="")
				} else {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultDataColumn, "|", sep="")
				}
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultTextDataColumn, "|", sep="")
			}
		} else {
			if(colsWithNumbers[headerIndex] == TRUE) {
				if(defaultDataColumn != defaultTextDataColumn && (determineLeadingDecimalPlacesPerColumn || determineTrailingDecimalPlacesPerColumn) && !is.na(dataToPrint[,headerIndex])){
					strVec[length(strVec)] <- paste(strVec[length(strVec)], determineFormat(dataToPrint[,headerIndex], determineTrailingDecimalPlacesPerColumn, roundToDecimalPlaces), " ", sep="")
				} else {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultDataColumn, " ", sep="")
				}
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultTextDataColumn, " ", sep="")
			}
		}
	}
	strVec[length(strVec)] <- paste(strVec[length(strVec)], "}", sep="")
	if(printRowSeparators) {
		strVec = c(strVec, line)
	}
	
	#Print toprule if data is to be separated from headers
	if(separateHeadersFromData) {
		strVec = c(strVec, "")
		strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\toprule", sep="")
	}
	
	strVec = c(strVec, "")
	
	if(!is.na(columnLabel)) {
		#Add column for row label
		if(NonSeparatedHeaders) {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{c}{ }", colSep, sep="")
		} else {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
		}
		#Add additional empty header column if rowname printing is activated
		if(printRowHeaders) {
			if(NonSeparatedHeaders) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{c}{ }", colSep, sep="")
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
			}
		}
		strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{", length(colnames(dataToPrint)), "}{c}{", columnLabel, "}", sep="")
		strVec = c(strVec, rowSep)
		strVec = c(strVec, "")
	}
	
	if(printColumnHeaders == TRUE && !is.null(colnames(dataToPrint))) {
		
		#Add column for row label if specified
		if(!is.na(rowLabel)) {
			if(NonSeparatedHeaders) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{c}{ }", colSep, sep="")
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
			}
		}
		
		#Add additional empty header column if rowname printing is activated
		if(printRowHeaders) {
			if(NonSeparatedHeaders) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{c}{ }", colSep, sep="")
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
			}
		}
		
		#Writing table header
		for(headerIndex in 1:length(colnames(dataToPrint))) {
			if(boldColumnHeaders) {
				#Make headers bold
				headerValue <- paste("\\textbf{", colnames(dataToPrint)[headerIndex], "}", sep="")	
			} else {
				#Leave column headers in plain text
				headerValue <- colnames(dataToPrint)[headerIndex]
			}
			if(headerIndex == length(colnames(dataToPrint))) {
				#leave out separator
				if(NonSeparatedHeaders) {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{c}{", headerValue, "}", rowSep, sep="")
				} else {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], headerValue, rowSep, sep="")
				}
			} else {
				#add separator
				if(NonSeparatedHeaders){
					strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{c}{", headerValue, "}", colSep, sep="")
				} else {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], headerValue, colSep, sep="")
				}
			}
			strVec[length(strVec)] = gsub("_", " ", strVec[length(strVec)])
		}
		
		if(separateHeadersFromData) {
			strVec = c(strVec, "")
			#now uses toprule for separation of headers from data - requires booktabs package in LaTeX
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\toprule", sep="")
#Legacy: determining columns and manually spanning cmidrule - now replaced by toprule
#			add = 1
#			if(!is.na(rowLabel)){
#				add = add + 1
#			}
#			if(printRowHeaders){
#				add = add + 1
#			}
			#strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\cmidrule{", add , "-", add - 1 + length(colnames(dataToPrint)), "}", sep="")
		}
		
		if(printRowSeparators) {
			strVec = c(strVec, line)
		}
		strVec = c(strVec, "")
	}
	if(printRowSeparators) {
		strVec = c(strVec, line)
	}
	strVec = c(strVec, "")
	
	#Flag whether row label has been printed
	rowLabelPrinted = FALSE
	
	#Iterate of colnames/rownames arrays, since length() may not be accurate if columns have been added after initialisation
	for(rowIndex in 1:rowLength) {
		if(!is.na(rowLabel)){
			if(rowIndex == 1 && !rowLabelPrinted) {
				#Write label to first row
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\parbox[t]{2mm}{\\multirow{", length(rownames(dataToPrint)), "}{*}{\\rotatebox[origin=c]{90}{", rowLabel, "}}}", colSep, sep="")
				rowLabelPrinted = TRUE
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
			}
		}
		if(printRowHeaders && !is.null(rownames(dataToPrint))) {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], rownames(dataToPrint)[rowIndex], colSep, sep="")
		}
		#Now print the actual data
		for(colIndex in 1:colLength) {
			data = ""
			if(!is.na(dataToPrint[rowIndex, colIndex])) {
				if(!containsText(dataToPrint[rowIndex, colIndex])) {
					#Round if numeric (does not contain patterns that would prohibit that)
					data = round(as.numeric(dataToPrint[rowIndex, colIndex]), decimalPlaces)
				} else {
					#Else just use 'as is'
					data = dataToPrint[rowIndex, colIndex]
				}
			}
			if(colIndex == colLength) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], data, rowSep, sep="")
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], data, colSep, sep="")
			}
		}
		if(printRowSeparators) {
			strVec = c(strVec, line)
		}
		strVec = c(strVec, "")
	}
	#Terminate tabular environment
	strVec = c(strVec, "\\end{tabular}")
	if(writeFullTableEnv) {
		#Write table environment
		strVec = c(strVec, "")
		strVec = c(strVec, "\\end{table}")
	}
	#Write String to file
	fileConn<-file(filename)
	writeLines(strVec, fileConn)
	close(fileConn)
	print(paste("Printed data to '", filename, "'", sep=""))
}
