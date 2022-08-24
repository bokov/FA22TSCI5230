#'---
#' title: "TSCI 5230: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);

library(ggplot2); # visualisation
library(GGally);
library(rio);# simple command for importing and exporting
library(pander); # format tables
library(printr); # set limit on number of lines printed
library(broom); # allows to give clean dataset
library(dplyr); #add dplyr library

options(max.print=42);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};
# R basic syntax ----
#'
#' # R basic syntax
#'
#' ## Assignment
#'
#' To store a value as variable `foo` in R the convention is to use the
#' `<-` operator, not `=`. This makes it easier to distinguish stand-alone
#' expressions from function arguments.

#+ assignment_operator
foo <- 500;
bar <- foo <- 500;
bar <- foo;
#' It's not a formal rule, it's rarely even written down, but `foo`, `baz`,
#' `bat`, etc. are throw-away variables people use for testing. If you need more
#' test variables, just make up three letter ones that start with `b`.
#' If you see one of those in a script you're reviewing it means it is left over
#' from when that code was being debugged. Example code shared with this class
#' will usually use `foo` and friends to represent the parts of an expression
#' that should be replaced with whatever values you are using instead of being
#' used literally. Shorter than having to write `YOURFUNCTIONHERE` or
#' `YOURARGUMENTHERE` each time.
#'
#' This is not specific to R-- it's just a little quirk of programming culture
#' in general. A quirk with a practical consequence: _never use `foo`, `bar`,
#' `baz`, `bat`, etc. in your production (i.e. finalized) code_ because
#' otherwise you or somebody else debugging your code will attempt to use those
#' names as test variables and in some situations this could overwrite the
#' existing variables!
#'
#' ## Comments
#'
#' `#'` This indicates that this line should be formatted as text. It must be
#' the first two characters in that line in order to work.
#'
#' `#+` This indicates that the following lines (until the next #' or #+) should
#' be treated as a "code chunk". I.e. the next lines (but not this one) will be
#' run, the code will be displayed according to your settings and the results
#' will be displayed according to your settings.
#'
#' `#` This is an ordinary comment. Everything after it on the same line is not
#' executed.
#'
#' # Functions and Data Types
#'
#' ## Functions and Simple Data Types
#'
#' Numeric. You can do arithmetic on them: `+`, `-`, `/`, `*`, `^`, `log()`,
#' `exp()`, `sqrt()`

#+ assignment_numeric
foo <- 2+2; foo
foo <- 5*2; foo
foo <- log(2); foo
foo <- exp(100); foo
log(foo)
bar <- 5*5; bar
whatisthis(bar);
print(foo <- 42)
print (shoo <-67)
log(((foo*3)+(shoo+2)*sqrt(144)+exp(10)))
#' Character strings. Create these by wrapping single (`'`) or double (`"`)
#' quotes around the value.

#+ assignment_string
a <- "Donot panic"
print(a)
coo <- "Donot panic"; coo
a <- "Donot panic"; a
b <- 'Donot panic'; b
c <- "Don't panic"; c
d <- 'The "Heart of Gold" comes equipped with heated leather seats and an infinite improbability drive'; d
print (d)
e <- 42; e
e <- "42"; e
#' Logical values are `TRUE` or `FALSE`. Can also be created using `>`, `<`,
#' `==`, `!=`, `>=`, `<=`

#+ assignment_logical
foo > 25     ;#' assign variable foo
shoo >100    ;#' assign variable shoo
foo >= 40    ; #' using operator
foo != 42    ; #' using NOT operator
foo == 42    ; #' using EQUAL operator
shoo == 42   ; #' using operator
foo <50 & shoo<100 ; #' using 'AND' operator
foo<50 | shoo>100; #' using OR operator
foo<50 | shoo<50
foo>50 | shoo<50
! foo>50
!(shoo>50);  #' using NOT operator
! foo>50 & shoo>50

#' Missing values are represented by `NA` (no quotes for any of these). Null
#' values are _not_ the same as missing and they are represented by `NULL`. In
#' some circumstances you might also run into `Inf`, `-Inf`, and `NaN`. These
#' often indicate errors somewhere else in your code.

#' Dates and times. Can be created with the `Sys.Date()` or `Sys.time()`
#' functions or converted from a character string using `as.Date()`.

#+ assignment_datetime
Sys.Date()
Sys.time()
#?as.Date
my_date <- "2022-01-31"; #' create example date
print (my_date)       ; #' print my_date variable
class (my_date)       ; #' check class of the variable
new_date <- as.Date(my_date) ; #' convert character string
new_date                      ; #' print new_date
class(new_date)               ;#' check class of new_date
as.Date(new_date, tryFormats = c("%y-%m-%d"))
#' Factors are basically integers that have labels. They are a human-readable
#' alternative to using integer codes for discrete data. These will make more
#' sense after we talk about vectors in the next section.

#+ factor_example

#+ assignment_wierd

#' ## Data Structures
#'
#' Of course we usually need to work with variables bundled together, not single
#' values.
#'
#' ### Vectors
#'
#' The default data structure in R is a `vector`. You create one with the `c()`
#' command with any number of arguments. All items in a vector have to be the
#' same type.

#+ vectors_c
print(foo <- c(56,78,34,97,2,86))
print(baz <- c(34,23,94,3,12,53))
#' Since the default data structure in R is a `vector`, if you
#' create some sort of simple value you are creating a `vector` even if you are
#' not using `c()`... it just happens to be a `vector` of length 1. These
#' are identical, and both return `1` when used with the `length()` function.

#+ vectors_length1
length(foo)
#' If you want to create a sequence of consecutive integers, you can use the `:`
#' operator.

#+ vectors_sequence
25:76
65:38
-32:12
seq_len(12)
#' In most other languages, you need to use a `for` loop in order to perform
#' some sort of change to a series of values. In R, you often don't have to
#' when you are working with vectors because a lot of functions (including all
#' the arithmetic and logical ones above) and be applied to a vector and they
#' work. If the function involves multiple vectors (e.g. `+`), usually you'll
#' want all of them to be either the same length or length 1.

#+ vectors_operators
foo+6
foo+baz
foo>=34
baz<=23
bob <- baz<=23
c(baz,foo)
c(baz,foo,"76")
# These work with all arithmetic operators
#' You can assign names to some or all members of a vector when you create it.
#' You can also assign or re-assign names later using the `names()` function.

#+ vectors_names1, error=TRUE
jar <- c(a="cat", best="dog", c= "fish", slow="tutle")
print(jar)
# jar[best]
jar["best"]
jar[c("best","c")]
#' You can also use it to see the currently assigned names.

#+ vectors_names2
names(jar) <- c("libby", "beau", "bob", "bob2") # renaming
print(jar)
names(jar)
names(jar)[3]
names(jar)[3] <- "milo" # renames only one element
jar
#' You can subset a vector by using `[...]` with the `...` replaced by _another_
#' vector, of integers indicating which positions you want to extract. Or you
#' could use a vector of names.

#+ vectors_subset1
foo[3]
#' If you just need a single value, use a single name or number.

#+ vectors_subset2

#' If you need a series of adjacent values, use `:`. If you need several
#' adjacent series with interruptions between them, use `c()` with `:`
#' expressions separated by commas `,`.

#+ vectors_subset3
foo[c(1,2,3)]
foo[1:3]
foo[c(1:3,5:6)]
baz
bob
baz[bob] # pulled the vector less than equal to 23
print(foo)
summary(foo)
table(foo)
table(jar) # frequency table
bat <- sample(1:10, 30, replace = TRUE)
table(bat)
bat <- sample(1:10, 30, replace = TRUE)*1000
table(bat)
bat
head(bat) # top 6 elements
tail(bat) # last 6 elements
diff(bat) # difference between two values
sum(bat) # sums all values
seq_along(bat) # sequence all values
sum(bat, na.rm= TRUE) # for missing values
#' Other useful functions for exploring vectors: `length()`, `summary()`,
#' `table()`, `head()`, `tail()`, `sum()`, `diff()`, `seq_along()`.

#+ vectors_explore


#' Here are some aggregation functions. For these, make sure to use `na.rm=T` if
#' your vector has `NA`s in it... `max()`, `min()`, `mean()`, `median()`,
#' `quantile()`.
quantile(bat) # tells quantiles
quantile(bat, na.rm=TRUE)
#+ vectors_aggregate

#' ### Data Frames
#'
#' You can bundle several vectors of the same length together into a
#' `data.frame` using the `data.frame()` command. A `data.frame` is a tabular
#' data structure where the columns can be different types from each other
#' (though within each column the type will be uniform because each column is
#' still a vector). Most data in R is in the form of a `data.frame` or a class
#' that inherits from `data.frame`. The `dplyr` package makes working with
#' `data.frames` easier and a lot of attention will be devoted to `dplyr`
#' [below](#data-frames-indepth). For now, here are some basic commands for
#' exploring `data.frames`: `dim()`, `nrow()`, `ncol()`, `names()` and (for
#' small datasets) `plot()`.

#+ df_explore
dim(iris)
nrow(iris)
ncol(iris)
names(iris)
head(iris)
tail(iris)
head(iris,10)
#' how to select rows
#+ df_subset
iris[3:20,]
iris[c(2:10,34,40:50,34,34,34),]
iris[-c(3:20),]
seq_len(nrow(iris))

sample(seq_len(nrow(iris)), 10)  # sample without replacement
sample(seq_len(nrow(iris)), 10, replace= TRUE) # sample with replacement
iris0 <- iris[sample(seq_len(nrow(iris)), 10),]

#' How to select coulmns in dataset
#+ df_columns, error=TRUE, results="hide"
iris[,1:3] # columns miss 4 and 5
iris[,c("Petal.Length","Petal.Width","Species")]
prevar <- c("Petal.Length","Petal.Width","Species")  # define columns together
iris[,prevar]



iris$Species # picks the column from dataset by adding $sign
outcome <- "Species"
iris$outcome
iris[[outcome]]
iris [["Species"]]

#' how to select columns and rows at same time
#+ df_columnsrows
iris[4:10,prevar]

#' # Datasets and `dplyr`
#+ Working with datasets and DPLYR

r"(/Users/YOURNAME/Desktop/projects/tsci/TSCI 5050 self/dataset)" %>% gsub("////","/",.) # to replace anything in the address
list.files("/Users/YOURNAME/Desktop/projects/tsci/TSCI 5050 self/dataset") # to see anyfiles in the folder

dtset <- list.files("/Users/YOURNAME/Desktop/projects/tsci/TSCI 5050 self/dataset", full.names = TRUE) %>%
  sapply(import) %>% setNames(.,basename(names(.))) # to change the base names
example1 <- dtset
example2 <- example1$Birthweight.sav

#+ file_import, echo = FALSE
# #' ## Importing a File
# birthweight <- import("/Users/harshitgarg/Desktop/projects/tsci/TSCI 5050 self/dataset/Birthweight.sav")
#
# #' ## Introduction to `dplyr`
# mutate(birthweight, AGE=AGE*12) %>% View() # converting age to months
# mutate(birthweight, AGEMonths =AGE*12) %>% head() # converting age to months and adding as column
# mutate(birthweight, AGEMonths = AGE*12, AGEdays= AGEMonths*30.25) %>% View
# table(birthweight$RACE) # extract a column from a dataset
# with(birthweight, case_when(RACE== 1~ "Caucasian", RACE== 2~"ASIAN", RACE== 3~ "AFRICAN AMERICAN/BLACK", TRUE~ as.character(RACE))) %>% table()
#
# #' Assigning Descriptive Values to a Code
# mutate(birthweight, AGEMonths = AGE*12, AGEdays= AGEMonths*30.25,
#        RACEName= case_when(RACE== 1~ "Caucasian",
#                            RACE== 2~"ASIAN",
#                            RACE== 3~ "AFRICAN AMERICAN/BLACK",
#                            TRUE~ as.character(RACE))) %>% head()
#
# summary(birthweight$BWT)
#
# #' ## The `summarise()` Function
# #'
# summary(birthweight$BWT) # gives summary of the particular column min/max, median, quartiles
# summary(birthweight) # gives summary of all columns min/max, median, quartiles
# summarise(birthweight) # gives columns and rows in dataset
# summarise(birthweight, age=median(AGE)) # gives summary measure for a column
# summarise(birthweight, age=median(AGE), height= median(HT), meanage= mean(AGE))
# table(birthweight$SMOKE)
# group_by(birthweight,SMOKE) %>% summarise(birthweight, age=median(AGE), height= median(HT), meanage= mean(AGE))
# group_by(birthweight,SMOKE) %>% summarise(height= median(HT), meanage= mean(AGE))
# group_by(birthweight,SMOKE) %>% summarise(across(where(is.numeric),mean)) # summarise dataset by group and give mean for each column
# group_by(birthweight,SMOKE) %>% summarise(across(where(is.numeric),sd))
#
# group_by(birthweight,SMOKE) %>% summarise(across(where(is.numeric),mean, .names = '{.col}_mean'),
#                                           across(where(is.numeric),sd, .names = '{.col}_sd'))  # gives mean and SD
#
# group_by(birthweight,SMOKE) %>% summarise(across(where(is.numeric),list(mean,sd))) # gives list of mean and SD of each column but doesnt tell the names
# group_by(birthweight,SMOKE) %>% summarise(across(where(is.numeric),list(Mn=mean,StD=sd,Md=median,InQR=IQR))) # gives list of mean and SD and also its names
# group_by(birthweight,SMOKE) %>% summarise(across(where(is.numeric),list(Mn=mean,StD=sd,Md=median,InQR=IQR))) %>% View # gives all the list and view it
# group_by(birthweight,SMOKE) %>% mutate(across(where(is.numeric),list(Mn=mean,StD=sd,Md=median,InQR=IQR))) %>% View # gives new columns of aggregate function for each group


#' Define location of your files
#'

#' # Linear Models
#+ linear_models
example(lm) # a sample for linear model

perf <- lm(mpg~hp+wt+qsec,mtcars)
summary(perf) # gives detail summary
summary(perf)$coeff # gives coefficient column
glance(perf) #gives brief
tidy(perf) # gives tidy cleaner version inside
lm(mpg~hp+wt+vs,mtcars) %>% tidy() %>% select(c("estimate","p.value"))
#+ Debugging
perf %>% tidy() %>% select(c("estimate","p.value"))
perf %>% tidy() %>% select(c("estimate","p.value")) %>% slice(-1) # removes top row
perf %>% tidy() %>% select(c("estimate","p.value")) %>% slice((1:3)) # gives 1 to 3 rows
perf %>% tidy() %>% select(c("estimate","p.value")) %>% slice(-(1:3)) # removes 1 to 3 rwos
whatisthis(perf) # gives class of the variable

#' View(perf) # view inside of object

#+ ## multiple comparison
perf %>% tidy() %>% select(c("p.value")) %>% slice(-1)
#'perf %>% tidy() %>% select(c("p.value")) %>% slice(-1) %>% p.adjust()
perf %>% tidy() %>% select(c("p.value")) %>% slice(-1) %>% unlist() %>% p.adjust()
