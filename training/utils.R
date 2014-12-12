suppressMessages(library(TeachingDemos))
suppressMessages(library(compiler))
suppressMessages(library(rjson))
suppressMessages(library(methods))
suppressMessages(library(arules))
suppressMessages(library(compiler))
suppressMessages(library(R.oo))
suppressMessages(library(ggplot2))
#suppressMessages(library(ReadImages)) #No longer supported
suppressMessages(library(gridExtra))
suppressMessages(library(stringr))
suppressMessages(library(foreach))
suppressMessages(library(sqldf))
library(doParallel)
cl <- makeCluster(24,outfile='')
registerDoParallel(cl)
suppressMessages(enableJIT(3))
suppressPackageStartupMessages(library(Matrix))
suppressPackageStartupMessages(library(Rcpp))
library(data.table)



.ls.objects <- function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head = TRUE, n = 10) {
  # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
  # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session) 
  # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
  # a data frame of the objects and their associated storage needs.
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size) / 10^6 # megabytes
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

chunk <- function(x,n)
{
  ret <- vector("list",n)
  len <- length(x)
  size <- len %/% n
  extra <- len %% n
  adj <- 0
  for (i in 1:n)
  {
    beg <- (i-1) * size + 1 + adj
    if (i <= extra) adj <- adj + 1
    fin <- i * size + adj
    ret[[i]] <- x[beg:fin]
  }
  return (ret)
}

chunk <- cmpfun(chunk)








dtlookup<-rbind(
  c(4,'trait')
  ,c(5,'segment')
  ,c(7,'page')
  ,c(8,'host')
  ,c(9,'utm_source')
  ,c(10,'cm_list1')
  ,c(11,'cm_refer_page')
  ,c(12,'cm_refer_host')
  ,c(13,'cm_group_id')
  ,c(14, 'post')
  ,c(15,'tc_keyword')
  ,c(16,'top_level_dir')
  ,c(17,'stir_qm_title_words')
  )


#profile this as transactions
setAs("list", "transactions",
      function(from)
        new("transactions", as(from, "itemMatrix"), 
            transactionInfo = data.frame(transactionID = names(from))))


setAs("list", "itemMatrix", 
      function(from) {
        if (!length(from))
          return(new("itemMatrix"))
        
        ## some checks
        if (!all(sapply(from, is.atomic)))
          stop("can coerce list with atomic components only")
        if (any(unlist(lapply(from, duplicated))))
          stop("can not coerce list with transactions with duplicated items")
        
        ## fix Matrix mess (ceeboo 2009)
        from <- lapply(from, sort)
        p <- sapply(from, length)
        names(p) <- NULL
        p <- cumsum(p)
        i <- unlist(from, use.names = FALSE)
        i <- factor(i)
        
        p <- new("ngCMatrix", p   = c(0L, p),
                 i   = c(i) - 1L,
                 Dim = c(length(levels(i)), length(p)))
        
        
        new("itemMatrix", 
            data        = p,
            itemInfo    = data.frame(labels    = I(levels(i))),
            itemsetInfo = data.frame(itemsetID = I(labels(from))))
      }
)

lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}