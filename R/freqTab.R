#' A function to create a frequency table
#'
#' For dichotomous variables, this function produces a table with the frequency, proportion, and N missing for each variable, and p-value for the Chi-squared test comparing groups (if group is specified)
#' @param df dataframe
#' @param vars list of dichotomous variables to calculate
#' @param group grouping categorical variable
#' @keywords descriptive statistics 
#' @examples
#' data(airquality)
#' airquality$tempGT70[airquality$Temp > 70] = 1
#' airquality$tempGT70[airquality$Temp <= 70] = 0
#' airquality$windGT10[airquality$Wind > 10] = 1
#' airquality$windGT10[airquality$Wind <= 10] = 0
#' freqTab(airquality, c('tempGT70', 'windGT10'), group = 'Month')

freqTab = function(df, vars, group=NULL){
	freqdf = data.frame(row.names = vars)
	for (k in 1:length(vars)){
		if (class(df[[vars[k]]]) == 'character' | class(df[[vars[k]]]) == 'factor'){
			df[[vars[k]]] = as.numeric(as.character(df[[vars[k]]]))
		}
	}
	if (!is.null(group)){
		levs = levels(factor(df[[group]]))
		i = 1
		for (j in 1:length(levs)){
			lev = levs[j]
			for (k in 1:length(vars)){
				freqdf[k, i] = (sum(df[[vars[k]]][df[[group]] == lev], na.rm=T)/sum(!is.na(df[[vars[k]]][df[[group]] == lev])))*100
				freqdf[k, i+1] = sum(is.na(df[[vars[k]]][df[[group]] == lev]))
			}
			i = i + 2
			if (length(levs) >= 2){
				col = ncol(freqdf) + 1
				for (k in 1:length(vars)){
					tbl = table(df[[vars[k]]], df[[group]])
					test = chisq.test(tbl) 
					freqdf[k, col] = test$p.value
				}
			}
		}
		names = rep(levs, each=2)
		p = seq(from = 1, to = length(names), by=2)
		miss = seq(from = 2, to = length(names), by=2)
		names[p] = paste(names[p], '%', sep = ' ')
		names[miss] = paste(names[miss], 'N missing', sep = ' ')
		names = c(names, 'Chi-square p-value')
		colnames(freqdf) = names
		return(freqdf)
	}
	if (is.null(group)){
		i = 1
		for (k in 1:length(vars)){
			freqdf[k, i] = (sum(df[[vars[k]]][df[[group]] == lev], na.rm=T)/sum(!is.na(df[[vars[k]]][df[[group]] == lev])))*100
				freqdf[k, i+1] = sum(is.na(df[[vars[k]]][df[[group]] == lev]))
		}
		names = levs
		m = length(names)
		names[m] = paste(names[m], '%')
		names = c(names, 'N missing')
		colnames(freqdf) = names
		return(freqdf)
	}
}
