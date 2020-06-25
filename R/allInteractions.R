## This Script displays 2-way variable interaction

#' allInt
#'
#' @description Plots a Heatmap-tyle display showing Variable Importance and Variable Interaction.
#'
#'
#' @param mat A matrix of values to be plotted. Either added by the user or created using the prepFunc() function.
#' @param type The type of plot to display, either "lollipop", "barplot", or "circleBar".
#' @param top A value set by the user to only display the top x amount variables.
#' @param ... Not currently implemented
#'
#' @importFrom ggplot2 "ggplot"
#' @import progress
#'
#'@examples
#' # Load in the data:
#' aq <- data.frame(airquality)
#' aq <- na.omit(aq)
#'
#' # Run an mlr ranger model:
#' library(mlr3)
#' aq_Task = TaskRegr$new(id = "airQ", backend = aq, target = "Ozone")
#' aq_lrn = lrn("regr.ranger", importance = "permutation")
#' aq_Mod <- lrn$train(aq_Task)
#'
#' # Create matrix
#' myMat <- prepFunc(task = aq_Task, learner = aq_Lrn, model = aq_Mod)
#'
#'
#' # Create plot:
#' allInt(myMat)
#'
#' @export


# Function ----------------------------------------------------------------

allInt <- function(mat, type = "lollipop", top = 0, ...){

  # if(type != "lollipop" || type != "barplot" || type != "circleBar"){
  #   stop("Invalid plotting type. See ?allInt for available plotting types")
  # }

  dinteraction <- mat
  diag(dinteraction) <- 0

  df <- transform(data.frame(dinteraction), y=row.names(dinteraction))

  # Reshape long
  long_df <- reshape(df, varying = colnames(dinteraction), times = colnames(dinteraction),
                     timevar="x", v.names="value", direction="long")

  # Order values
  long_df <- with(long_df, long_df[order(value),])
  long_df$xy <- with(long_df, paste(x, y, sep=":"))

  # Select correct data
  df <- long_df[,c(3,5)]

  # Replace low vales with zero
  df[df < 0.04 ] <- 0

  # Remove zeros
  df<-df[!apply(df[,1:2] == 0, 1, FUN = any, na.rm = TRUE),]

  # Show only top X results
  if(top > 0){
    df <- tail(df,top)
  }
# -------------------------------------------------------------------------
# Plotting
  if(type == "barplot"){
  # barplot
    pp <-  ggplot(df, aes(x = reorder(xy, value), y = value)) +
      geom_col(aes(fill = value)) +
      scale_fill_gradient(low = "floralwhite",
                          high = "dodgerblue4") +
      theme_minimal() +
      xlab('Features ') +
      ylab("Interaction Strength") +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
      coord_flip()
  return(pp)
  }else if(type == "circleBar"){
  # Circle barplot
  ppp <- ggplot(df, aes(x = reorder(xy, value), y = value)) +
    geom_col(aes(fill = value)) +
    scale_fill_gradient2(low = "floralwhite",
                         high = "dodgerblue4") +
    # Limits of the plot. The negative value controls the size of the inner circle,
    # the positive is to add size over each bar
    ylim(-0.5,0.3) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.margin = unit(rep(-2,4), "cm")) +    # This remove unnecessary margin around plot
    coord_polar(start = 0) + # This makes the coordinate polar instead of cartesian.
    geom_text(aes(label = round(value,3)), vjust = 2, color = "black", size = 3.5)
    #geom_text(aes(label = intRound),vjust = 0, color = "black", size = 3)
  return(ppp)
  }else if(type == "lollipop"){
    # lollipop plot
    p <- ggplot(df, aes(x = reorder(xy, value), y = value)) +
      geom_linerange(ymin=0, aes(ymax=value)) + geom_point()+
      xlab('Features') +
      ylab("Interaction Strength") +
      coord_flip() +
      theme_bw()
    return(p)
    }
}
