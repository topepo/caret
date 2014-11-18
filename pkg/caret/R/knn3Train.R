
knn3Train <- function(train, test, cl, k=1, l=0, prob = TRUE, use.all=TRUE)
{
	train <- as.matrix(train)
	if(is.null(dim(test))) dim(test) <- c(1, length(test))
	test <- as.matrix(test)
        if(any(is.na(train)) || any(is.na(test)) || any(is.na(cl)))
            stop("no missing values are allowed")
	p <- ncol(train)
	ntr <- nrow(train)
	if(length(cl) != ntr) stop("'train' and 'class' have different lengths")
	if(ntr < k) {
            warning(gettextf("k = %d exceeds number %d of patterns", k, ntr),
                    domain = NA)
	   k <- ntr
	}
	if (k < 1)
            stop(gettextf("k = %d must be at least 1", k), domain = NA)
	nte <- nrow(test)
	if(ncol(test) != p) stop("dims of 'test' and 'train differ")
	clf <- as.factor(cl)
	nc <- max(unclass(clf))
	Z <- .C("knn3",
		as.integer(k),
		as.integer(l),
		as.integer(ntr),
		as.integer(nte),
		as.integer(p),
		as.double(train),
		as.integer(unclass(clf)),
		as.double(test),
		integer(nc+1),
		as.integer(nc),
		as.integer(FALSE),
		as.integer(use.all),
		all_vote=double(as.integer(nte*nc))

		)

	classProbs <- matrix(Z$all_vote,nrow=nte,ncol=nc,byrow=TRUE)
	colnames(classProbs)<-sort(unique(clf))

   bestClass <- function(x)
   {
      out <- which(x == max(x))
      if(length(out) > 1) out <- sample(out, 1)
      out
   }
     
   res <- colnames(classProbs)[apply(classProbs, 1, bestClass)]

   votes <- apply(classProbs * k, 1, max)
   inDoubt <- (votes < l)
   if(any(inDoubt)) res[inDoubt] <- NA

   if (prob) attr(res, "prob") <- classProbs
   res
}
