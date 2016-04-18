##Description
#Given a data frame (one column contains character strings) and
#a pattern to be matched, this function returns an augmented data frame
#with two additional columns. The first column contains lists of matched patterns,
#and the second column contains the numbers of matched patterns.

##Require the package stringr.


##Arguments
#data: data frame with at least one column that contains character strings.
#subset: integer vector of the row indices to apply the function.
#char.col: single integer; indicate which column contains the character strings.
#pattern: character; the pattern to be matched.
#pat.name: character; the name of the pattern, e.g. "emoticon".

##Value
#An augmented data frame with two additional columns.
#The first additional column contains the list of matched patterns for each row.
#The second additional column contains the number of matched patterns for each row.

library(stringr)
getPattern = function(data, sub.index, char.col, pattern, pat.name){
  sub.data = data.frame(data[sub.index, ]) #subset the data frame.
  char.vec = as.character(sub.data[ , char.col]) #get the character strings.
  matched.list = apply(X = as.matrix(char.vec), MARGIN = 1,
                       FUN = str_extract_all, pattern = pattern)
  #list of matched pattern(s) for each character string.

  sub.data[ , ncol(sub.data) + 1] = rep(NA, times = nrow(sub.data))
  sub.data[ , ncol(sub.data) + 1] = rep(0, times = nrow(sub.data))
  #add two columns.

  n.col = ncol(sub.data) #number of columns in the augmented data frame.

  colnames(sub.data)[n.col - 1] = pat.name
  colnames(sub.data)[n.col] = paste(pat.name, "count")
  #change the names of the additional two columns.

  for (i in 1:nrow(sub.data)){
    num.matched = length(matched.list[[i]][[1]])
    if (num.matched > 0){
      sub.data[i, n.col - 1] = matched.list[i]
      sub.data[i, n.col] = num.matched
      #if there is at least one match (num.matched > 0),
      #record the list of the matched patterns and the number of matches.
    }
  }
  return(sub.data)
}

##Testing
testStr1 = "I really like the food :)))"
testStr2 = "This is not bad at all:D:D"
testStr3 = "This is negative D:"
testStr4 = "I LOVE the movie:D :)) (-:"
testDF = data.frame(c(testStr1, testStr2, testStr3, testStr4))

getPatternTest = getPattern(data = testDF, sub.index = 1:4, char.col = 1,
                            pattern = "[:;][-]?[)D]+|[(]+[-]?[:;]",
                            pat.name = "positive emoticon")
