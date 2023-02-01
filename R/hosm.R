#' Creates High Order Spatial Matrix
#' @export
#' @param data dataframe
hosm <-function(data){
  x<-data[,-1];
  x<-data.matrix(x);
  rownames(x)<-colnames(x);
  rad<-diag(x)<-NA;
  rad<-x[lower.tri(x)]<-NA
  rad<-x[order(x)];
  max.rad<-max(rad,na.rm=TRUE);
  min.rad<-min(rad,na.rm=TRUE);
  y <- 0;
  repeat {if (y*min.rad>max.rad) break
    y=y+1
  };
  max.order<-y;
  for (order in 0:max.order) {};
  order = c();
  for(k in 0:max.order){order=c(order,k)};
  order;
  rad<-min.rad*order;
  m.order<-data[,-1];
  for (i in 1:max.order){m.order[m.order>rad[i] & m.order<=rad[i+1]]<-order[i+1]};
  m.order;
  k<-data[,-1];
  hosm<-(1/k)^m.order;
  diag(hosm)<-0;
  rownames(hosm)<-colnames(hosm);
  return(hosm);
}
