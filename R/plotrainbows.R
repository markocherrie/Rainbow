#' Plot rainbows
#'
#' Create a rainbow plot using ggplot2
#' @param df Dataframe from makerainbows function
#' @param shape Specify "circle" or "semi-circle"
#' @param colourby Colours determined by individual box - "individual" or by band - "band"; default is individual box
#' @param colourpal Colour palette - can specify "GilbertBaker1978"
#' @param alpha Vector of alpha values for boxes, specify centre ring clockwise outward; default is 0.9
#' @param labels Vector of labels for boxes, specify centre ring clockwise outward
#' @param innercircletext Vector of labels for boxes, specify centre ring clockwise outward; default is blank
#' @return ggplot2 rainbow plot
#' @examples
#'
#' # Wider Determinants of Health
#' df <- makerainbows(1,1,6,1)
#' labelsWDH<-c("Individual Lifestyle Factors",
#'              "Social and Community Networks",
#'              "Food prod.", "Education", "Work Env.", "Water/Sanitation", "Healthcare", "Housing",
#'              "General Socioeconomic, Cultural and Environmental Factors")
#' plotrainbows(df, shape="semi-circle", colourby="band", colourpal="",labels=labelsWDH, innercircletext = "Age, sex and constitutional factors")
#'
#'
#' # Pride Rainbow
#' df <- makerainbows(1,1,1,1,1,1,1,1)
#' plotrainbows(df, shape="semi-circle", colourby="individual", colourpal="GilbertBaker1978", labels=c("Sprit", "Serenity", "Magic", "Nature", "Sunlight", "Healing","Life", "Sex"));
#' @export

plotrainbows<-function(df, shape, colourby="individual", colourpal="", alpha=0.9, labels, innercircletext=""){

  # get the labels
  #labels <- unlist(list(...))
  #labels <- text

  #labels
  #print(labels)

  # make the text in the middle
  i<-300
  while(i %% nrow(df) != 0 ){
    i = i+1
  }

  # get the right labels for the right rainbow
  lbs<-rep(c(labels), each = round(i/nrow(df)))

  # NEED TO REFACTOR THIS>>>>
  if(length(unique(df$y1))==2){

    y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))

    x1a<-seq(0,2 * pi, length = length(y1a))

    y1b<-rep(4, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+1):
          (table(df$y1)[1]+table(df$y1)[2])
      ]]))

    x1b<-seq(0,2 * pi, length = length(y1b))
    x1<-c(x1a, x1b)
    y1<-c(y1a,y1b)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)


  }else if(length(unique(df$y1))==3){

    y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))

    x1a<-seq(0,2 * pi, length = length(y1a))

    y1b<-rep(4, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+1):
          (table(df$y1)[1]+table(df$y1)[2])
      ]]))


    x1b<-seq(0,2 * pi, length = length(y1b))


    y1c<-rep(6, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3])
      ]]))
    x1c<-seq(0,2 * pi, length = length(y1c))


    x1<-c(x1a, x1b, x1c)
    y1<-c(y1a,y1b, y1c)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)


  }else if(length(unique(df$y1))==4){


    y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))
    x1a<-seq(0,2 * pi, length = length(y1a))

    y1b<-rep(4, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+1):
          (table(df$y1)[1]+table(df$y1)[2])
      ]]))
    x1b<-seq(0,2 * pi, length = length(y1b))


    y1c<-rep(6, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3])
      ]]))
    x1c<-seq(0,2 * pi, length = length(y1c))

    y1d<-rep(8, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
      ]]))
    x1d<-seq(0,2 * pi, length = length(y1d))


    x1<-c(x1a, x1b, x1c, x1d)
    y1<-c(y1a,y1b, y1c, y1d)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)




  }else if(length(unique(df$y1))==5){

    y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))
    x1a<-seq(0,2 * pi, length = length(y1a))

    y1b<-rep(4, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+1):
          (table(df$y1)[1]+table(df$y1)[2])
      ]]))
    x1b<-seq(0,2 * pi, length = length(y1b))


    y1c<-rep(6, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3])
      ]]))
    x1c<-seq(0,2 * pi, length = length(y1c))

    y1d<-rep(8, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
      ]]))
    x1d<-seq(0,2 * pi, length = length(y1d))


    y1e<-rep(10, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5])
      ]]))
    x1e<-seq(0,2 * pi, length = length(y1e))

    x1<-c(x1a, x1b, x1c, x1d, x1e)
    y1<-c(y1a,y1b, y1c, y1d, y1e)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)


  }else if(length(unique(df$y1))==6){


    y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))
    x1a<-seq(0,2 * pi, length = length(y1a))

    y1b<-rep(4, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+1):
          (table(df$y1)[1]+table(df$y1)[2])
      ]]))
    x1b<-seq(0,2 * pi, length = length(y1b))


    y1c<-rep(6, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3])
      ]]))
    x1c<-seq(0,2 * pi, length = length(y1c))

    y1d<-rep(8, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
      ]]))
    x1d<-seq(0,2 * pi, length = length(y1d))


    y1e<-rep(10, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5])
      ]]))
    x1e<-seq(0,2 * pi, length = length(y1e))


    y1f<-rep(12, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6])
      ]]))
    x1f<-seq(0,2 * pi, length = length(y1f))


    x1<-c(x1a, x1b, x1c, x1d, x1e, x1f)
    y1<-c(y1a,y1b, y1c, y1d, y1e, y1f)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)



  }else if(length(unique(df$y1))==7){

    y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))
    x1a<-seq(0,2 * pi, length = length(y1a))

    y1b<-rep(4, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+1):
          (table(df$y1)[1]+table(df$y1)[2])
      ]]))
    x1b<-seq(0,2 * pi, length = length(y1b))


    y1c<-rep(6, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3])
      ]]))
    x1c<-seq(0,2 * pi, length = length(y1c))

    y1d<-rep(8, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
      ]]))
    x1d<-seq(0,2 * pi, length = length(y1d))


    y1e<-rep(10, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5])
      ]]))
    x1e<-seq(0,2 * pi, length = length(y1e))


    y1f<-rep(12, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6])
      ]]))
    x1f<-seq(0,2 * pi, length = length(y1f))



    y1g<-rep(14, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6] + table(df$y1)[7])
      ]]))
    x1g<-seq(0,2 * pi, length = length(y1g))


    x1<-c(x1a, x1b, x1c, x1d, x1e, x1f, x1g)
    y1<-c(y1a,y1b, y1c, y1d, y1e, y1f, y1g)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)
  }else if(length(unique(df$y1))==8){

    y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))
    x1a<-seq(0,2 * pi, length = length(y1a))

    y1b<-rep(4, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+1):
          (table(df$y1)[1]+table(df$y1)[2])
      ]]))
    x1b<-seq(0,2 * pi, length = length(y1b))


    y1c<-rep(6, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3])
      ]]))
    x1c<-seq(0,2 * pi, length = length(y1c))

    y1d<-rep(8, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
      ]]))
    x1d<-seq(0,2 * pi, length = length(y1d))


    y1e<-rep(10, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5])
      ]]))
    x1e<-seq(0,2 * pi, length = length(y1e))


    y1f<-rep(12, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6])
      ]]))
    x1f<-seq(0,2 * pi, length = length(y1f))



    y1g<-rep(14, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6] + table(df$y1)[7])
      ]]))
    x1g<-seq(0,2 * pi, length = length(y1g))


    y1h<-rep(16, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6]+table(df$y1)[7]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6] + table(df$y1)[7]+ table(df$y1)[8])
      ]]))
    x1h<-seq(0,2 * pi, length = length(y1h))



    x1<-c(x1a, x1b, x1c, x1d, x1e, x1f, x1g, x1h)
    y1<-c(y1a,y1b, y1c, y1d, y1e, y1f, y1g, y1h)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)


  }else{
    x1<-seq(0, 2 * pi, length = i)
    y1 = rep(2, i)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)

  }

  # for semi-circle
  if(shape=="semi-circle"){
    df$x1<-df$x1/2
    df$x2<-df$x2/2
    textdf$x1<-textdf$x1/2

    startval=-(pi / 2)
  } else{
    startval=0
  }

  # aesthetics
  #n <- length(df$x1)


  if(colourby=="individual"){
    df$group<-letters[1:nrow(df)]
  }else if(colourby=="band"){
    df$group<-df$y1
  }

  # alpha
  df$alpha<-alpha


  # plot
  p <- ggplot2::ggplot(df, aes(x1, y1)) +
    geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group,
                  alpha = alpha),
              color = "white", size = 2) +
    geom_textpath(data = textdf,
                  aes(label = label),
                  linetype = 0,
                  size = 4.6,
                  color = "white",
                  upright = TRUE) +
    scale_y_continuous(limits = c(-5, max(df$y2+1))) +
    scale_x_continuous(limits = c(0, pi*2)) +
    scale_alpha_identity() +
    theme_void() +
    theme(legend.position = "none")

  # semi-circle shape

  plotout<- p + coord_polar(start = startval)


  if(colourpal=="GilbertBaker1978"){

    rainbow<- c("#8e008e" ,"#400098", "#00c0c0", "#008e00","#ffff00", "#ff8e00" ,"#ff0000" ,"#ff69b4")
    rainbowcols<-rainbow[1:length(df$x1)]

    plotout<-plotout +
      scale_fill_manual(values=rainbowcols)
  }

  # Add inner text here
  if(innercircletext!=""){

    plotout <- plotout + annotate("text", x=0, y=-5, label = wrapper(innercircletext, width = 10), size = 4.6) #+
    #geom_point(aes(x=x, y=y), data=data.frame(x=runif(1), y=runif(1)-7), size=65, shape=1, lwd=2, color="black")
  }

  return(plotout)


}

wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")
