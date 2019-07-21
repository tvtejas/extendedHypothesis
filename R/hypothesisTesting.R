#'Extended part of Hypothesis testing
#'
#'Will do hypothesis testing as per user requirements
#'@param data= dataframe
#'@param vect= list of column number to be pass
#'@param y= target variable (Optional)
#'@param mu= Population mean if single popultion test need to be performed
#'@param tail= two.sided/greater/less(if not provided "two.sided" will be default)
#'@param aplha= provide significance value (default=0.05)
#'@author Tejas Vartak and Shivani Goyal
#'@export
hypothesisTesting<-function(data,vect=c(),y=NULL,mu=NULL, tail="two.sided", alpha=0.05)
{
  if (!is.null(vect)&& is.data.frame(data))
  {
    if (!is.null(vect)&& length(vect)==1)
    {
      if (is.numeric(data[,vect[1]]))
      {
        if (is.null(mu))
        {
          print("Please provide Population mean as mu=Value for hypothesis testing")
        }
        else
        {
          print(paste("t test for single population",names(data)[vect[1]]))
          print(t.test(data[,vect[1]],mu=mu,alternative = tail,conf.level = (1-alpha)))
        }

      }
      else
      {
        print(paste("Chi square test for",names(data)[vect[1]]))
        print(chisq.test(table(data[,vect[1]])))
      }
    }
    if (!is.null(vect)&& length(vect)==2)
    {
      if (is.numeric(data[,vect[1]])&&is.numeric(data[,vect[2]]) )
      {
        print(paste("t test for two population",names(data)[vect[1]],"and",names(data)[vect[2]]))
        print(t.test(data[,vect[1]],data[,vect[2]],alternative = tail,conf.level = (1-alpha)))
      }
      if (class(data[,vect[1]])!="factor" && length(unique(data[,vect[2]]))==2)
      {
        a<-data[,vect[1]][data[,vect[2]]==unique(data[,vect[2]])[1]]
        b<-data[,vect[1]][data[,vect[2]]==unique(data[,vect[2]])[2]]
        print(t.test(a,b))

      }
      else if (class(data[,vect[2]])!="factor" && length(unique(data[,vect[1]]))==2)
      {
        a<-data[,vect[2]][data[,vect[1]]==unique(data[,vect[1]])[1]]
        b<-data[,vect[2]][data[,vect[1]]==unique(data[,vect[1]])[2]]
        print(t.test(a,b))
      }
      else if(length(vect)>2&&!is.null(y))
      {
        print("Anova Test")
        print(aov(y~as.matrix(data)))
      }
      else
      {
        print("No test possible")
      }
    }
  }
  else
  {
    print("Hypothesis testing cannot be performed as only dataframe is given, please provide column number in c(1,2,..etc) ")
  }

}
