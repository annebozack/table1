#' A function to create a table of means and SDs
#'
#' For continuous variables, this function produces a table with the mean, SD, and N missing of each variable for each group, and a p-value for the Wilcoxon rank-sum test comparing groups (if group is specified)
#' @param df dataframe
#' @param vars list of continuous variables to calculate
#' @param group grouping categorical variable
#' @keywords descriptive statistics 
#' @examples
#' data(airquality)
#' meanTab(airquality, c('Ozone', 'Temp', 'Wind'), group = 'Month')


meanTab = function(df, vars, group=NULL){
	meandf = data.frame(row.names = vars)
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
				meandf[k, i] = mean(df[[vars[k]]][df[[group]] == lev], na.rm=T)
				meandf[k, i+1] = sd(df[[vars[k]]][df[[group]] == lev], na.rm=T)
				meandf[k, i+2] = sum(is.na(df[[vars[k]]][df[[group]] == lev]))
			}
			i = i + 3
			if (length(levs) == 2){
				col = ncol(meandf) + 1
				for (k in 1:length(vars)){
					test = wilcox.test(df[[vars[k]]] ~ df[[group]])
					meandf[k, col] = test$p.value
				}
			}
			if (length(levs) > 2){
				col = ncol(meandf) + 1
				for (k in 1:length(vars)){
					test = kruskal.test(df[[vars[k]]] ~ df[[group]])
					meandf[k, col] = test$p.value
				}
			}
		}
		names = rep(levs, each=3)
		m = seq(from = 1, to = length(names), by=3)
		s = seq(from = 2, to = length(names), by=3)
		miss = seq(from = 3, to = length(names), by=3)
		names[m] = paste(names[m], 'mean', sep = ' ')
		names[s] = paste(names[s], 'SD', sep = ' ')
		names[miss] = paste(names[miss], 'N missing', sep = ' ')
		names = c(names, 'Wilcoxon rank sum p-value')
		colnames(meandf) = names
		return(meandf)
	}
	if (is.null(group)){
		i = 1
		for (k in 1:length(vars)){
				meandf[k, 1] = mean(df[[vars[k]]], na.rm=T)
				meandf[k, 1+1] = sd(df[[vars[k]]], na.rm=T)
				meandf[k, 1+2] = sum(is.na(df[[vars[k]]]))
		}
		colnames(meandf) = c('mean', 'SD', 'N missing')
		return(meandf)
	}
}
