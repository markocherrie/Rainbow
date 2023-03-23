#' Make rainbows
#'
#' Create dataframe for rainbow plot
#' @param ... Vector specifying the boxes for each band
#' @return Dataframe
#' @examples
#' rainbowdf(1,1,1,1,1,1,1,1);
#' @export

makerainbows<-function(...){

  numberofcatsband <- unlist(list(...))
  numberofbands <- length(numberofcatsband)


  xx1 <- car::recode(numberofcatsband, '1=6; 2=3; 3=2; 4=1.5; 5=1.2; 6=1')
  xx2 <- numberofcatsband

  yy1 <- seq(1, 200, by = 2)[1:numberofbands]
  yy2 <- seq(3, 200, by = 2)[1:numberofbands]

  argss<-data.frame(xx1=xx1, xx2=xx2, yy1=yy1, yy2=yy2)

  makethecoords<-function(xx1, xx2, yy1, yy2){

    x1<- c(seq(0, 10/6 * pi, xx1*pi/3))
    y1<- c(rep(yy1, xx2))
    x2<- c(seq(0, 10/6 * pi, xx1*pi/3) + xx1*pi/3)
    y2<- c(rep(yy2, xx2))

    # return the dataframe
    df<-data.frame(x1,x2,y1,y2)
    return(df)
  }

  df <-plyr::mdply(argss, makethecoords)
  df <- df[order(df$y1),]

}


