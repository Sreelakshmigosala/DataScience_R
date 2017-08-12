 my.display <- function(x, display = FALSE, type = "", prob = TRUE) {
+ #type = 'hist' || 'density'
+ if (display & type == "") {
+ return(cat("Please specify type as either hist or density"))
+ } else if (display & type == 'hist') {
+ return(hist(x, freq = prob))
+ } else if (display & type == 'density') {
+ return(density(x))
+ } else {
+ #return('N args specified')
+ median(x)
+ }
+ cat('summary of input:\n', summary(x))
+ }




my.display<- function (A)
{
	cat("Summary of input \n")
	summary(A)
}

my.display<- function (x,display=FALSE,type,prob=TRUE)
{
	#cat("Summary of input \n")
	#print (summary(x))
	if(display == TRUE)
	{

		if (type != "hist" || "density")
		{
 			cat("Please specify type as either hist or density \n")
		}
		else if (type=="hist")
		{
			return(hist(x,freq=prob))
		}
		elseif (type=="density")
		{
			return(plot(density(x)))
		}
	}
	else
		{
			median(x)
		}
}