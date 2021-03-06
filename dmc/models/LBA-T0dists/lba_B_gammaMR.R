# Gamma non-decision n-choice LBA, B=b-A parameterization
#   External parameters types: A, B, t0, mean_v, sd_v, scale, shape
#   Internal parameters types: A, b, t0, mean_v, sd_v, scale, shape

# User edited funcitons for the DMC (Dynamic Models of Choice)
#    Source in all appliciatons

# This function transfroms parameters to a form suitbale for the model 
#   being used. Called inside of get.par.mat. 
# "par.df" is a data frame of parameters types , some of which may need to be 
#   transformed, or new columns created, so that the full set of internal 
#   parameter types, specified in "type.par.names", required by the type of 
#   evidence accumulation model being used ("norm" etc.) is present.
transform.dmc <- function(par.df) 
{
  # User supplied tranforms go here
  par.df$b <- par.df$B+par.df$A
  
#   # COMMENT OUT this check for speed after debugging
#   if ( !all(type.par.names %in% names(par.df)) )
#     stop("Trasform has not created parameter(s) required by the model.")
  
  par.df[,c("A","b","t0","mean_v","sd_v","shape","scale","AS")]
}

random.dmc<- function(n,p.df,model)
{rlba.norm.gammaMR(n,A=p.df$A,b=p.df$b,t0=p.df$t0,
                    mean_v=p.df$mean_v,sd_v=p.df$sd_v,
                    shape=p.df$shape[1],scale=p.df$scale[1],
                    AS=p.df$AS[1])}



likelihood.dmc <- function(p.vector,data,ok.types=c("normgammaMR"),min.like=1e-10) 
# Returns vector of likelihoods for each RT in data (in same order)
# !!! TO DO: types other than norm
{

#   # COMMENT OUT this check for speed after debugging
#   if ( !all(attr(model,"type") %in% ok.types) )
#     stop("Distribution funciton type not supported by likelihood.dmc")
  
  likelihood <- numeric(dim(data)[1])
  for ( i in row.names(attr(data,"model")) ) if ( !attr(data,"cell.empty")[i] )
  {
    p.df <- p.df.dmc(p.vector,i,attributes(data)$model,n1order=TRUE)
    likelihood[ attr(data,"cell.index")[[i]] ] <-
      switch(attr(attributes(data)$model,"type"),
        normgammaMR=n1PDF.norm.gammaMR(
          t=data$RT[attr(data,"cell.index")[[i]]],
          A=p.df$A,
          b=p.df$b,
          t0=p.df$t0[1], 
          mean_v=p.df$mean_v,
          sd_v=p.df$sd_v,
          shape=p.df$shape[1],
          AS=p.df$AS[1],
          scale=p.df$scale[1])
      )
 }
 pmax(likelihood,min.like)
}


