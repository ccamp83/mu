#' @export
kin.lateralDeviation <- function(datatraj, ...)
{
  straight.path.ortho.a <- function(b, x, z)
  {
    intercept <- z - b*x
    return(intercept)
  }
  
  straight.path.proportion <- function(path.a, path.b, ortho.path.b, x, z, homeX, homeZ, straight.path.length, ...)
  {
    m.coef <- matrix(c(1, -ortho.path.b, 
                       1, -path.b), nrow=2, byrow=T)
    m.rhs <- c(straight.path.ortho.a(ortho.path.b, x, z), path.a)
    m.inv <- solve(m.coef)
    result.temp <- m.inv %*% m.rhs
    result <- c(result.temp[2], result.temp[1])
    
    #   calcolo la distanza tra la traiettoria della mano e la straight path (deviazione laterale)
    dist <- sqrt((z-result[2])^2 + (x - result[1])^2)
    
    #   calcolo la lunghezza del percorso lungo la straight path fino a quel punto sulla traiettoria
    straight.path.traveled <- sqrt((result[1] - homeX)^2 + (result[2] - homeZ)^2)
    
    #   calcolo la proporzione di straight path percorsa
    prop <- straight.path.traveled/straight.path.length
    
    output <- list(lat.deviation = dist,
                   traveled.path = prop)
    return(output)
  }
  
  temp <- ddply(datatraj, .(trialN), mutate,
                homeX = na.omit(GPX)[1],
                homeZ = na.omit(GPZ)[1],
                arrivalX = na.omit(GPX)[length(na.omit(GPX))],
                arrivalZ = na.omit(GPZ)[length(na.omit(GPZ))],
                straight.path.b = (arrivalZ - homeZ) / (arrivalX - homeX),
                straight.path.a = arrivalZ - straight.path.b*arrivalX,
                straight.path.lengthX = (homeX + arrivalX)^2,
                straight.path.lengthZ = (homeZ + arrivalZ)^2,
                straight.path.length = sqrt(straight.path.lengthX + straight.path.lengthZ),
                straight.path.ortho.b = - 1 / straight.path.b
  )
  
  testv <- data.frame(straight.path.b = temp$straight.path.b, 
                      GPX = temp$GPX, 
                      GPZ = temp$GPZ,
                      straight.path.a = temp$straight.path.a, 
                      straight.path.ortho.b = temp$straight.path.ortho.b, 
                      homeX = temp$homeX,
                      homeZ = temp$homeZ,
                      straight.path.length = temp$straight.path.length
  )
  
  prop <- apply(testv, 1, function(w) straight.path.proportion(w['straight.path.a'], 
                                                               w['straight.path.b'], 
                                                               w['straight.path.ortho.b'], 
                                                               w['GPX'], 
                                                               w['GPZ'],
                                                               w['homeX'],
                                                               w['homeZ'],
                                                               w['straight.path.length'])$traveled.path)
  
  deviation <- apply(testv, 1, function(w) straight.path.proportion(w['straight.path.a'], 
                                                                    w['straight.path.b'], 
                                                                    w['straight.path.ortho.b'], 
                                                                    w['GPX'], 
                                                                    w['GPZ'],
                                                                    w['homeX'],
                                                                    w['homeZ'],
                                                                    w['straight.path.length'])$lat.deviation)
  
  newcols <- data.frame(traveled.path = prop,
                       lat.deviation = deviation)
  
  output <- cbind(datatraj, newcols)
  
  return(output)
}
