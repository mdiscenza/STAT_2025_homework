####
#Functions that I use a lot

all_possible_models <- function(x.column.names, y.col.name){
    Cols <- x.column.names
    n <- length(Cols)
    id <- unlist(
        lapply(1:n,
               function(i)combn(1:n,i,simplify=F)
        )
        ,recursive=F)
    Formulas <- sapply(id,function(i)
        paste(y.col.name, "~",paste(Cols[i],collapse="+"))
    )
    Formulas <- c(Formulas, paste( y.col.name, "~1"))
}


#for printing only the values of a list to a text file

lapply(mylist, write, "test.txt", append=TRUE, ncolumns=1000)

#alternative cost function for CV
costf <- function(r, pi = 0) mean((r - round(pi))^2) # need to define a cost function that rounds the binary response to either 1 or zero 
