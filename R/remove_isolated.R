#' Tool to draw more complex extent using
#' @param vect_fish vector of fishing event
#' @param state event to focus on
#' @param replacement replace state event to replacement one if considered as isolated
#' @param replacement threshold, number of point to be considered as isolated
#' @export

remove_isolated <- function(vect_fish, state, replacement, lim = 10){
    if ( is.factor(vect_fish ))  {
      vect_fish <- as.character(vect_fish)
    }
    segment <- rle(vect_fish)
    fin <- cumsum(segment$lengths)
    nseg <- length(fin)
    debut <- c( 1, fin[-nseg] + 1 )
    df2 <- data.frame(debut=debut, fin=fin, state=segment$values) %>% mutate(longueur= fin-debut + 1)

    new_predict_list <- lapply(1:nseg,  function(d_ = row_){
      row_ <- df2[d_,]
      if(row_[['longueur']] < lim  & row_[['state']]==state){
        res <-  as.character( rep(replacement, row_[['longueur']]))
      } else {
        res <- as.character( rep(row_[['state']], row_[['longueur']] ) )
      }
      return(res)
    })
    new_predict <- do.call('c', new_predict_list)
    new_predict <- as.factor(new_predict)
    return(new_predict)
  }
