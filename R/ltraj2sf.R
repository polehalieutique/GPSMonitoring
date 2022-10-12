#' ltraj2sf
#' @param ltrajobj ltraj object produced when redistribute trajectory to have constant duration beetween pings

#' @examples
#'
#' @export
#'

ltraj2sf<- function (ltrajobj){


  compteur<-1
    for (compteur in seq(1,length(ltrajobj)))
{
    ltrajobj[[compteur]]$no_trajet<-as.numeric(burst(ltrajobj[compteur]))
    ltrajobj[[compteur]]$track_fid<-as.numeric(adehabitatLT::id(ltrajobj[compteur]))
 }

return(do.call(rbind, ltrajobj))

}


