R2LaTeX
=======

Script that facilitates exporting data from R to LaTeX

#### LatexPrinting.R - Produces LaTeX tables from R matrices or dataframes with a wide range of formatting options.

##### Selected features include: 
  * Printing column and row headers
  * Adding additional labels for columns and rows
  * Selectively adding separators for columns and rows and/or headers
  * Manual specification of default column format
  * Automatically determining column formatting based on decimal places of contained numeric data (per column or globally)
  * Substitution of empty data cell values
  * Compatibility mode that prevents use of features that require additional LaTeX packages (such as booktabs and sunitx)
##### Usage:
  1. Copy file into R working directory.
  2. Execute it (e.g. `source("LatexPrinting.R")`)
  3. Use `printLatexTable()` to print R data frame or matrix. See documentation for details and examples.

##### Examples:
  
1.) Simple Example
```
m <- matrix(rexp(100), 10)
printLatexTable(m)
```
 
2.) More Complex Example
```
attach(mtcars)
printLatexTable(mtcars, "mtcarsTable.txt", printColumnHeaders = TRUE, printRowHeaders = TRUE, boldColumnHeaders = TRUE, boldColumnAndRowLabel = TRUE, separateColumnHeadersFromData = TRUE, separateRowHeadersFromData = TRUE, columnLabel = "Attributes", rowLabel = "Car Models", caption = "Car Models with Attributes", label = "tab:cars")
```
