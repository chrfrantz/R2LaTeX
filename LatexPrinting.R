# R2LaTeX - Utility file for generating LaTeX output from R data frames or matrices 
# for direct insertion into LaTeX documents. 
#
# R Dependency: stringr library (you will be prompted during execution if not installed)
# LaTeX Dependency: booktabs, siunitx (depends on chosen features). 
#					Compatibility mode avoids the use of any additional package.
#
# Usage: 
# - Copy this file into your R work directory.
# - Run source("LatexPrinting.R") in R.
# - Use the printLatexTable() function (see documentation for parameters and examples)
# 
# Author: Christopher Frantz (christopherfrantz 'at' web 'dot' de)
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
	# Test for decimal places
	if ((val %% 1) != 0) {
		# Count those if existing by splitting value on . and counting places
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
	
	# Check for trailing decimal places (ignore any NA values)
	if(determineDecimalPlaces){
		#print(paste("Input:", arrayOfValues))
		decimalPlaces = max(sapply(arrayOfValues[!is.na(arrayOfValues)], function(x){determineDecimalPlaces(as.numeric(as.character(x)))}))
		#print(paste("Longest dec. places:", decimalPlaces))
		# Override with default places if too long
		if(decimalPlaces > defaultDecimalPlaces){
			decimalPlaces = defaultDecimalPlaces
		}
	} else {
		decimalPlaces = defaultDecimalPlaces
	}
	
	if(length(arrayOfValues) == 0){
		print("Attempted to determine format of empty array (should never happen). Switching to default format.")
		# Override check if input array is empty
		fixedLeadingPlaces = TRUE
	} else {
		# Check on leading decimal places (ignoring any NA values
		pos = str_locate(as.character(max(as.character(as.numeric(arrayOfValues[!is.na(arrayOfValues)])))), "\\.")
		if(is.na(pos[1])){
			# no decimal places
			plc = nchar(as.character(max(as.character(as.numeric(arrayOfValues[!is.na(arrayOfValues)])))))
		} else {
			plc = pos[1] - 1
		}
	}
	if(fixedLeadingPlaces){
		plc = defaultLeadingPlaces
	}
	# Generate siunitx column header
	colHeader = paste("S[table-format=", plc, ".", decimalPlaces, "]", sep="")
	return(colHeader)
}

#' Determines whether a given input contains text (or rather non-numeric content). Tolerates whitespace entries.
#'
#' @param val Value to test for non-numeric content
#'
containsText <- function(val){
	# Tests for numeric content (and associated symbols), or alternatively whitespace symbols
	return(any(!grepl("^([-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)|\\s*$", val, perl=TRUE)))
}


#' Generates LaTeX tabular or table environment from given R data frame or matrix and saves it into file.
#' Notes: 
#' If automatic column formatting is used, the output code will require the 'siunitx' package in LaTeX.
#' If separateColumnHeadersFromData is activated, the output code will require the 'booktabs' package in LaTeX.
#' Use compatibilityMode to prevent any external dependencies.
#' 
#' @param dataToPrint Data frame or matrix holding data
#' @param filename = "GenericTableOutput.txt" File the generated output is saved in.
#' @param printColumnHeaders = TRUE Adds column headers to output. Uses colnames of input data frame.
#' @param replaceColumnHeaderFullStopsWithWhitespace = TRUE Replaces '.'s in column headers with whitespace.
#' @param printRowHeaders = FALSE Adds row headers to output. Uses rownames of input data frame.
#' @param printColumnSeparators = FALSE Indicates if columns should be separated by '|'.
#' @param printRowSeparators = FALSE Indicates if rows should be separated by '\hline' or '\midrule'/'\cmidrule'. Consider using the compatibilityMode if relying on row separators.
#' @param spanRowSeparatorsAcrossAllColumns = FALSE Indicates if row separators (for actual table data entries) are spanned across all columns or only over data columns (using '\crule'/'\cmidrule'). Will not extend to row label column. Depends on printRowSeparators.
#' @param nonSeparatedColumnHeaders = TRUE Does not print separators between column headers even if activated for columns.
#' @param separateColumnHeadersFromData = FALSE Indicates if separators should be printed between headers and data (both for row and column headers). Depends on separateColumnHeadersFromData.
#' @param spanDataHeaderSeparatorAcrossAllColumns = FALSE If set to true, uses toprule that spans across all columns including columns containing row headers and row label.
#' @param separateRowHeadersFromData = FALSE Indicates whether row headers should be separated from data.
#' @param boldColumnHeaders = TRUE Plots the column headers in bold face.
#' @param columnLabel = NA Column label that spans across all columns (in addition to column headers).
#' @param rowLabel = NA Row label that spans across all rows (in addition to row headers).
#' @param boldColumnAndRowLabel = TRUE Plots the spanning columnLabel and rowLabel in bold face.
#' @param printDataAsIs = FALSE Prints data as provided, without any modification such as rounding or value substitution. 
#' @param fillEmptyCellsWith = NA Fills eventual empty data entries with the value specified for this parameter. Needs to be specified as string (e.g. "empty"); value NA is ignored.
#' @param roundToDecimalPlaces = 3 Decimal places values should be rounded to if numeric (and used for automated decimal point-centered column formatting).
#' @param fixedLeadingDecimalPlaces = NA Fixes leading decimal places (i.e. before the decimal point) to fixed length for the purpose of automated decimal place-centered column formatting. By default automatically determined from data.
#' @param determineLeadingDecimalPlacesPerColumn = TRUE Determines leading decimal places for centered formatting per numeric column (instead of global value for all columns).
#' @param determineTrailingDecimalPlacesPerColumn = TRUE Determines trailing decimal places for centered formatting per numeric column (instead of global value for all columns).
#' @param rowHeaderColumnFormat = "c" Column format for row header column (if activated).
#' @param dataColumnFormat = NA Column format for data entries. Needs to be specified as string (e.g. "c", ""S[table-format=1.2]"). If not specified and if data is numeric and continuous, values are aligned by decimal point and column width determined based on largest value.
#' @param writeFullTableEnv = TRUE Writes complete table environment for immediate paste into LaTeX, otherwise only tabular environment.
#' @param caption = "Generated Table" Caption for generated table. Requires activation of writeFullTableEnv.
#' @param label = "tab:generatedTable" LaTeX ref label for generated table. Requires activation of writeFullTableEnv. 
#' @param compatibilityMode = FALSE Overrides use of features that require additional LaTeX packages (especially data column formatting and separators). Affects aesthetics of output.
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
#' printLatexTable(mtcars, "mtcarsTable.txt", printColumnHeaders = TRUE, printRowHeaders = TRUE, boldColumnHeaders = TRUE, boldColumnAndRowLabel = TRUE, separateColumnHeadersFromData = TRUE, separateRowHeadersFromData = TRUE, columnLabel = "Attributes", rowLabel = "Car Models", caption = "Car Models with Attributes", label = "tab:cars")
#' 
#' #Useful examples to test compatibility mode:
#' 
#' attach(mtcars)
#' printLatexTable(mtcars, "mtcarsTable.txt", printColumnHeaders = TRUE, printRowHeaders = TRUE, boldColumnHeaders = TRUE, boldColumnAndRowLabel = TRUE, separateColumnHeadersFromData = TRUE, separateRowHeadersFromData = TRUE, printRowSeparators = TRUE, spanRowSeparatorsAcrossAllColumns = TRUE, columnLabel = "Attributes", rowLabel = "Car Models", caption = "Car Models with Attributes", label = "tab:cars", compatibilityMode = FALSE)
#' printLatexTable(mtcars, "mtcarsTable.txt", printColumnHeaders = TRUE, printRowHeaders = TRUE, boldColumnHeaders = TRUE, boldColumnAndRowLabel = TRUE, separateColumnHeadersFromData = TRUE, separateRowHeadersFromData = TRUE, printRowSeparators = TRUE, spanRowSeparatorsAcrossAllColumns = FALSE, columnLabel = "Attributes", rowLabel = "Car Models", caption = "Car Models with Attributes", label = "tab:cars", compatibilityMode = TRUE)
#' printLatexTable(mtcars, "mtcarsTable.txt", printColumnHeaders = TRUE, printRowHeaders = TRUE, boldColumnHeaders = TRUE, boldColumnAndRowLabel = TRUE, separateColumnHeadersFromData = TRUE, separateRowHeadersFromData = TRUE, printRowSeparators = TRUE, spanRowSeparatorsAcrossAllColumns = TRUE, columnLabel = "Attributes", rowLabel = "Car Models", caption = "Car Models with Attributes", label = "tab:cars", compatibilityMode = TRUE)
#' 
printLatexTable <- function(dataToPrint, filename = "GenericTableOutput.txt", printColumnHeaders = TRUE, replaceColumnHeaderFullStopsWithWhitespace = TRUE, printRowHeaders = FALSE, 
		printColumnSeparators = FALSE, printRowSeparators = FALSE, spanRowSeparatorsAcrossAllColumns = FALSE, nonSeparatedColumnHeaders = TRUE, separateColumnHeadersFromData = FALSE, 
		spanDataHeaderSeparatorAcrossAllColumns = FALSE, separateRowHeadersFromData = FALSE, boldColumnHeaders = TRUE, boldColumnAndRowLabel = TRUE, columnLabel = NA, rowLabel = NA, 
		printDataAsIs = FALSE, fillEmptyCellsWith = NA, roundToDecimalPlaces = 3, fixedLeadingDecimalPlaces = NA, determineLeadingDecimalPlacesPerColumn = TRUE, 
		determineTrailingDecimalPlacesPerColumn = TRUE, rowHeaderColumnFormat = "c", dataColumnFormat = NA, writeFullTableEnv = TRUE, caption = "Generated Table", label = "tab:generatedTable", compatibilityMode = FALSE){
	
	# Basic checks on input data
	if (is.null(dataToPrint)) {
		stop("Passed data (Parameter dataToPrint) is null.")
	}
	
	if (is.data.frame(dataToPrint) || is.matrix(dataToPrint)) {
		if (length(row(dataToPrint)) == 0) {
			stop("Passed dataframe or matrix (Parameter dataToPrint) does not have entries.")
		}
	}
	
	# Check whether value for empty cells is string
	if (!is.na(fillEmptyCellsWith)) {
		if (!is.character(fillEmptyCellsWith)) {
			stop("Substitute value for empty cells (Parameter 'fillEmptyCellWith') needs to be string.")
		}
	}
	
	# Explicitly declare default data column
	defaultDataColumn = NA
	# Column specification for text
	defaultTextDataColumn = "c"
	
	# Override format choice if provided by user
	if (!is.na(dataColumnFormat)) {
		if (!is.character(dataColumnFormat)) {
			stop("Data column format (Parameter 'dataColumnFormat') must be specified as string (e.g., \"c\").")
		}
		defaultDataColumn = dataColumnFormat
		defaultTextDataColumn = dataColumnFormat
	}
	
	colSep = " & "
	rowSep = " \\\\"
	line = paste("\\", "midrule", sep="")
	toprule = "toprule"
	bottomrule = "bottomrule"
	cmidrule = "cmidrule"
	
	# Initialises symbol for column separation
	if (printColumnSeparators) {
		colSepFormatSymbol = "|"
	} else {
		colSepFormatSymbol = " "
	}
	
	# Handles compatibility issues
	if (compatibilityMode) {
		# Prevents use of booktabs package features
		spanDataHeaderSeparatorAcrossAllColumns = FALSE
		line = paste("\\", "hline", sep="")
		toprule = "hline"
		bottomrule = "hline"
		cmidrule = "cline"
		
		# Prevents use of siunitx package features
		# Set column format to default to override automatic identification
		if (is.na(dataColumnFormat)) {
			defaultDataColumn = "c"
		}
		determineLeadingDecimalPlacesPerColumn = FALSE
		determineTrailingDecimalPlacesPerColumn = FALSE
	} else {
		if (printRowSeparators | separateColumnHeadersFromData) {
			cat("The generated LaTeX table requires the 'booktabs' package. Add \\usepackage{booktabs} to your preamble.\n")
		}
	}
	
	# Formatting-related stuff
	decimalPlaces = 3 #decimal places for rounding and centered formatting
	if (!is.na(roundToDecimalPlaces)) {
		decimalPlaces = roundToDecimalPlaces
	}
	
	# Test which columns hold numbers in order to figure out if it makes sense to perform decimal place-based centering
	colsWithNumbers <- apply(dataToPrint, 2, function(x){!containsText(x)})
	
	if (is.na(defaultDataColumn)) {
		# Only determine format if no default specified
		if (sum(colsWithNumbers == TRUE) == 0) {
			# Alignment only works for numeric columns
			defaultDataColumn = "c"
			print("No numeric columns")
			printDataAsIs = TRUE # Print raw data
		} else {
			# Automatically determine length of decimal places on global level (i.e. across all columns)
	
			# Combine values of different columns
			vec <- vector(length=0)
			for (i in 1:length(colsWithNumbers)) {
				if (colsWithNumbers[i] != FALSE) {
					# Convert to numeric via character to deal with factors
					vec <- c(vec, as.numeric(as.character(dataToPrint[,i])), recursive = TRUE)
				}
			}
			# Ignore NAs
			vec <- vec[!is.na(vec)]
			
			# Determine column format based on numeric format (also tolerates but ignores whitespace entries)
			# (The choice of making the default column based on number formal global is intentional)
			defaultDataColumn = determineFormat(vec, determineTrailingDecimalPlacesPerColumn, decimalPlaces, !is.na(fixedLeadingDecimalPlaces), fixedLeadingDecimalPlaces)
			
			cat("The generated LaTeX table requires the 'siunitx' package. Add \\usepackage{siunitx} to your preamble.\n")
		}
	}
	
	defaultRowHeaderColumn = "c" #Column alignment for column holding row headers
	if (!is.na(rowHeaderColumnFormat)) {
		defaultRowHeaderColumn = rowHeaderColumnFormat
	}
	
	# Determine dimensions - check for colnames, which may consider columns appended after initialisation
	rowLength = length(dataToPrint[,1])
	if (!is.null(rownames(dataToPrint)) && length(rownames(dataToPrint)) > rowLength) {
		rowLength = length(rownames(dataToPrint))
	}
	
	colLength = length(dataToPrint[1,])
	if (!is.null(colnames(dataToPrint)) && length(colnames(dataToPrint)) > colLength) {
		colLength = length(colnames(dataToPrint))
	}

	strVec = vector(length=0)
	if (writeFullTableEnv) {
		#Write table environment
		strVec = c(strVec, "\\begin{table}[h!]")
		strVec = c(strVec, paste("\\caption{", caption, "}", sep=""))
		strVec = c(strVec, paste("\\label{", label, "}", sep=""))
		strVec = c(strVec, paste("\\centering", sep=""))
	}
	
	# Reduce space between rows if printing vertical and horizontal separators if not in compatibility mode
	if (!compatibilityMode & (separateRowHeadersFromData | printColumnSeparators) & printRowSeparators) {
		strVec = c(strVec, "\\belowrulesep=0pt")
		strVec = c(strVec, "\\aboverulesep=0pt")
	}
	
	# Write table column specifications
	strVec = c(strVec, "\\begin{tabular}{")
	if (printColumnSeparators) {
		strVec[length(strVec)] <- paste(strVec[length(strVec)], colSepFormatSymbol, sep="")
	}
	
	# Add additional column for row label
	if (!is.na(rowLabel)) {
		if (separateRowHeadersFromData) {
			# If row headers should be separated from data (independently from global separation of columns)
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "c", "|", sep="")
		} else {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "c", colSepFormatSymbol, sep="")
		}
	}
	
	# Add additional column format if rowname printing is activated
	if (printRowHeaders) {
		# Consider separation of header and data
		if (separateRowHeadersFromData) {
			# If row headers should be separated from data (independently from global separation of columns)
			strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultRowHeaderColumn, "|", sep="")
		} else {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultRowHeaderColumn, colSepFormatSymbol, sep="")
		}
	}
	
	# Determine formatting for data columns
	for (headerIndex in 1:colLength) {
		if (colsWithNumbers[headerIndex] == TRUE) {
			# If column is numeric and global data column format has been determined, 
		    # but should be determined on column level, determine it per column
			if (defaultDataColumn != defaultTextDataColumn && 
					(determineLeadingDecimalPlacesPerColumn || determineTrailingDecimalPlacesPerColumn) && 
					!is.na(dataToPrint[,headerIndex])) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], 
					# Determine format, but consider factor input (convert via character)
					determineFormat(as.numeric(as.character(dataToPrint[,headerIndex])), 
					determineTrailingDecimalPlacesPerColumn, roundToDecimalPlaces), colSepFormatSymbol, sep="")
			} else {
				# Else fall back to global data column format
				strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultDataColumn, colSepFormatSymbol, sep="")
			}
		} else {
			# If column content is non-numeric, use text column format
			strVec[length(strVec)] <- paste(strVec[length(strVec)], defaultTextDataColumn, colSepFormatSymbol, sep="")
		}
	}
	# Close table format configuration
	strVec[length(strVec)] <- paste(strVec[length(strVec)], "}", sep="")
	strVec = c(strVec, "")
	
	# And move to column headers ...
	if (!is.na(columnLabel)) {
		# Add column for row label
		if (nonSeparatedColumnHeaders) {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{", defaultDataColumn, "}{ }", colSep, sep="")
		} else {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
		}
		# Add additional empty header column if rowname printing is activated
		if (printRowHeaders) {
			if(nonSeparatedColumnHeaders) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{", defaultDataColumn, "}{ }", colSep, sep="")
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
			}
		} 
		# Bold label
		if (boldColumnAndRowLabel) {
			columnLabel = paste("\\textbf{", columnLabel, "}", sep="")
		}
		strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{", length(colnames(dataToPrint)), "}{", defaultDataColumn, "}{", columnLabel, "}", sep="")
		strVec = c(strVec, rowSep)
		strVec = c(strVec, "")
	}
	
	# Print separators of headers from data
	if (printColumnHeaders == TRUE && !is.null(colnames(dataToPrint))) {
		# Print toprule if data is to be separated from headers
		if (separateColumnHeadersFromData) {
			if (spanDataHeaderSeparatorAcrossAllColumns) {
				# Use toprule for separation of headers from data - requires booktabs package in LaTeX
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", toprule, sep="")
			} else {
				# Determine data columns and manually spanning cmidrule
				add = 1
				if(!is.na(rowLabel)){
					add = add + 1
				}
				if(printRowHeaders){
					add = add + 1
				}
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", cmidrule, "{", add , "-", add - 1 + length(colnames(dataToPrint)), "}", sep="")
			}
			strVec = c(strVec, "")
		}
		
		# Add column for row label if specified
		if (!is.na(rowLabel)) {
			if (nonSeparatedColumnHeaders) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{", defaultDataColumn, "}{ }", colSep, sep="")
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
			}
		}
		
		# Add additional empty header column if rowname printing is activated
		if(printRowHeaders) {
			if(nonSeparatedColumnHeaders) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{", defaultDataColumn, "}{ }", colSep, sep="")
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
			}
		}
		
		# Write actual table header
		for (headerIndex in 1:length(colnames(dataToPrint))) {
			if (boldColumnHeaders) {
				# Make headers bold
				headerValue <- paste("\\textbf{", colnames(dataToPrint)[headerIndex], "}", sep="")	
			} else {
				# Leave column headers in plain text
				headerValue <- colnames(dataToPrint)[headerIndex]
			}
			if (replaceColumnHeaderFullStopsWithWhitespace) {
				# Replace column header .'s with white space
				headerValue = str_replace_all(headerValue, "\\.", " ")
			}
			if (headerIndex == length(colnames(dataToPrint))) {
				# Leave out separator
				if (nonSeparatedColumnHeaders) {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{", defaultDataColumn, "}{", headerValue, "}", rowSep, sep="")
				} else {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], headerValue, rowSep, sep="")
				}
			} else {
				# Add separator
				if (nonSeparatedColumnHeaders) {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\multicolumn{1}{", defaultDataColumn, "}{", headerValue, "}", colSep, sep="")
				} else {
					strVec[length(strVec)] <- paste(strVec[length(strVec)], headerValue, colSep, sep="")
				}
			}
			# Substitute any occurrence of _ with whitespace to prevent LaTeX errors
			strVec[length(strVec)] = gsub("_", " ", strVec[length(strVec)])
		}
		
		if (separateColumnHeadersFromData) {
			strVec = c(strVec, "")
			if (spanDataHeaderSeparatorAcrossAllColumns) {
				# Use toprule for separation of headers from data - requires booktabs package in LaTeX
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", toprule, sep="")
			} else {
				# Determine data columns and manually spanning cmidrule
				add = 1
				if (!is.na(rowLabel)) {
					add = add + 1
				}
				if (printRowHeaders) {
					add = add + 1
				}
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", cmidrule, "{", add , "-", add - 1 + length(colnames(dataToPrint)), "}", sep="")
			}
		}
		
		if (printRowSeparators & !separateColumnHeadersFromData) {
			if (spanRowSeparatorsAcrossAllColumns) {
				strVec = c(strVec, line)
			} else {
				# Determine data columns and manually spanning cmidrule
				add = 1
				if(!is.na(rowLabel)){
					add = add + 1
				}
				if(printRowHeaders){
					add = add + 1
				}
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", cmidrule, "{", add , "-", add - 1 + length(colnames(dataToPrint)), "}", sep="")
			}
		}
		strVec = c(strVec, "")
	} else {
		if (printRowSeparators) {
			if (spanRowSeparatorsAcrossAllColumns) {
				strVec = c(strVec, line)
			} else {
				# Determine data columns and manually spanning cmidrule
				add = 1
				if(!is.na(rowLabel)){
					add = add + 1
				}
				if(printRowHeaders){
					add = add + 1
				}
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", cmidrule, "{", add , "-", add - 1 + length(colnames(dataToPrint)), "}", sep="")
			}
		}
		strVec = c(strVec, "")
	}
	
	# Flag whether row label has been printed
	rowLabelPrinted = FALSE
	
	#Iterate of colnames/rownames arrays, since length() may not be accurate if columns have been added after initialisation
	for (rowIndex in 1:rowLength) {
		if (!is.na(rowLabel)) {
			if (rowIndex == 1 && !rowLabelPrinted) {
				#Write label to first row
				if (boldColumnAndRowLabel) {
					rowLabel = paste("\\textbf{", rowLabel, "}", sep="")
				}
				strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\parbox[t]{2mm}{\\multirow{", length(rownames(dataToPrint)), "}{*}{\\rotatebox[origin=c]{90}{", rowLabel, "}}}", colSep, sep="")
				rowLabelPrinted = TRUE
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], " ", colSep, sep="")
			}
		}
		if (printRowHeaders && !is.null(rownames(dataToPrint))) {
			strVec[length(strVec)] <- paste(strVec[length(strVec)], rownames(dataToPrint)[rowIndex], colSep, sep="")
		}
		
		# Now print the actual data
		for (colIndex in 1:colLength) {
			data = ""
			# If data is NA, don't print
			if (!is.na(dataToPrint[rowIndex, colIndex])) {
				# Final check whether data is not text, empty or ''. Still will produce NAs if forced through conversion
				if (!printDataAsIs && 
						!containsText(dataToPrint[rowIndex, colIndex]) && 
						!is.null(dataToPrint[rowIndex, colIndex]) && 
						dataToPrint[rowIndex, colIndex] != '') {
					# Round if numeric (does not contain patterns that would prohibit that); take detour via character in case data is factor
					data = round(as.numeric(as.character(dataToPrint[rowIndex, colIndex])), decimalPlaces)
				} else {
					if (!is.na(fillEmptyCellsWith) && 
							(is.null(dataToPrint[rowIndex, colIndex]) || dataToPrint[rowIndex, colIndex] == '')) {
						data = fillEmptyCellsWith
					} else {
						# Else just use 'as is'
						data = dataToPrint[rowIndex, colIndex]	
					}
				}
			}
			if (colIndex == colLength) {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], data, rowSep, sep="")
			} else {
				strVec[length(strVec)] <- paste(strVec[length(strVec)], data, colSep, sep="")
			}
		}
		
		# Print row separators depending on configuration
		if (printRowSeparators && (!separateColumnHeadersFromData | (rowIndex < rowLength))) {
			strVec = c(strVec, "")
			# Determining data columns and manually spanning cmidrule and considers row label when spanning across all columns
			add = 1
			sub = 1
			if (!is.na(rowLabel)) {
				add = add + 1
			}
			if (printRowHeaders & !spanRowSeparatorsAcrossAllColumns) {
				add = add + 1
			}
			if (spanRowSeparatorsAcrossAllColumns) {
				sub = 0
			}
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", cmidrule, "{", add , "-", add - sub + length(colnames(dataToPrint)), "}", sep="")
		}
		strVec = c(strVec, "")
	}
	# Finalize table with rule
	if (separateColumnHeadersFromData) {
		strVec = c(strVec, "")
		if (spanDataHeaderSeparatorAcrossAllColumns) {
			# Use toprule for separation of headers from data - requires booktabs package in LaTeX
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", bottomrule, sep="")
		} else {
			# Determine data columns and manually spanning cmidrule
			add = 1
			if(!is.na(rowLabel)){
				add = add + 1
			}
			if(printRowHeaders){
				add = add + 1
			}
			strVec[length(strVec)] <- paste(strVec[length(strVec)], "\\", cmidrule, "{", add , "-", add - 1 + length(colnames(dataToPrint)), "}", sep="")
		}
	}
	# Terminate tabular environment
	strVec = c(strVec, "\\end{tabular}")
	if (writeFullTableEnv) {
		# Write table environment
		strVec = c(strVec, "\\end{table}")
	}
	# Write String to file
	fileConn<-file(filename)
	writeLines(strVec, fileConn)
	close(fileConn)
	print(paste("Printed data to '", filename, "'", sep=""))
}
