#' The imported dataset should be a csv containing columns of categorical data that will correspond to the nodes. Cells can contain multiple values, 
#' but labelled categories must not be the same across multiple columns (e.g. 'Other' cannot exist in two columns - instead label column 2 'Other' 
#' as 'Other2')

#' Import data as csv
mydata <- read.csv(file.choose())
attach(mydata)

#' Generate a matrix of unique flows across columns: run the code below for each column corresponding to a node in the Sankey, in the order you want 
#' the nodes to be displayed. Assumes a separator of '|||', SysRev.com defaults.
library(tidyr)
mydata <- separate_rows(data = mydata, column1, sep = '\\|\\|\\|')
mydata <- separate_rows(data = mydata, column2, sep = '\\|\\|\\|')
mydata <- separate_rows(data = mydata, column3, sep = '\\|\\|\\|')
mydata <- separate_rows(data = mydata, column4, sep = '\\|\\|\\|')

#' Counts the number of links between adjacent nodes across your dataset
library(dplyr)
datasummary <- group_by(mydata, column1, column2, column3, column4) %>%
  count(column1, column2, column3, column4)

#' Trim white space from start of columns
datasummary$column1 <- trimws(datasummary$column1)
datasummary$column2 <- trimws(datasummary$column2)
datasummary$column3 <- trimws(datasummary$column3)
datasummary$column4 <- trimws(datasummary$column4)

#' Compress first part of the flows
summarypart1 <- data.frame(datasummary$column1, datasummary$column2, datasummary$n)
summarypart1 <- summarypart1 %>%
  group_by(datasummary.column1, datasummary.column2) %>% 
  summarise(across('datasummary.n', sum))

summarypart2 <- data.frame(datasummary$column2, datasummary$column3, datasummary$n)
summarypart2 <- summarypart2 %>%
  group_by(datasummary.column2, datasummary.column3) %>% 
  summarise(across('datasummary.n', sum))

summarypart3 <- data.frame(datasummary$column3, datasummary$column4, datasummary$n)
summarypart3 <- summarypart3 %>%
  group_by(datasummary.column3, datasummary.column4) %>% 
  summarise(across('datasummary.n', sum))

#' Rename columns
summarypart1 <- 
  rename(summarypart1, 
         source = datasummary.column1,
         target = datasummary.column2,
         n = datasummary.n
  )
summarypart2 <- 
  rename(summarypart2, 
         source = datasummary.column2,
         target = datasummary.column3,
         n = datasummary.n
  )
summarypart3 <- 
  rename(summarypart3, 
         source = datasummary.column3,
         target = datasummary.column4,
         n = datasummary.n
  )

summary <- rbind(summarypart1, summarypart2, summarypart3)

#' From these flows create a node data frame: it lists every entity involved in the flow
library(networkD3)
nodes <- data.frame(
  name=c(as.character(summary$source), 
         as.character(summary$target)) %>% unique()
)

#' With networkD3, connections must be provided using id, not using real name as in the links dataframe. So we need to reformat it.
summary$IDsource <- match(summary$source, nodes$name)-1 
summary$IDtarget <- match(summary$target, nodes$name)-1

#' Make the Network diagram. Node padding sets the spacing between different node lables. Iterations set to zero avoids changing the order of nodes, instead
#' using the oder in 'nodes'.
sankeyNetwork(Links = summary, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n", NodeID = "name", 
              sinksRight=FALSE, nodePadding=3, iterations=0, fontSize = 10)