   
       log.growth.theta <- function(t, y, p) {
         N <- y[1]
         with(as.list(p), {
           dN.dt <- r * N * (1 - (N / K)^theta)
           return(list(dN.dt))
         })
       }
       p <- c('r' = 0.2, 'K' = 1.05, 'theta' = 1.05)
       y0 <- c('N' = 0.05)
       t <- 1:100
       
       ##blue is tomatoes
       
       sim <- ode(y = y0, times = t, func = log.growth.theta, parms = p, method = 'lsoda')
       sim <- as.data.frame(sim)
       plot(N ~ time, data = sim, type = 'l', lwd = 2, bty = 'l', col = 'blue')
       
       p.grapes <- c('r' = 0.28, 'K' = .75, 'theta' = 1.25)
       sim.grapes<- ode(y = y0, times = t, func = log.growth.theta, parms = p.grapes, method = 'lsoda')
       sim.grapes <- as.data.frame(sim.grapes)       
       
       points(N ~ time, data = sim.grapes, type = 'l', lwd = 2, bty = 'l', lty = 2, col = 'red')
       
       p.peaches <- c('r' = 0.15, 'K' = 1, 'theta' = 1)
       sim.peaches<- ode(y = y0, times = t, func = log.growth.theta, parms = p.peaches, method = 'lsoda')
       sim.peaches <- as.data.frame(sim.peaches) 
       
       points(N ~ time, data = sim.peaches, type = 'l', lwd = 2, bty = 'l', lty = 2, col = 'black')
       
       max.Ns <- c(sim$N[which(sim$deriv == max(sim$deriv, na.rm = TRUE))],
                   sim.grapes$N[which(sim.grapes$deriv == max(sim.grapes$deriv, na.rm = TRUE))],
                   sim.peaches$N[which(sim.peaches$deriv == max(sim.peaches$deriv, na.rm = TRUE))])
       
       theta <- c(p['theta'], p.grapes['theta'], p.peaches['theta']
                   
      plot(max.Ns ~ theta, pch = 21, bg = 'orange', type = 'b', lty = 2)             
       
      
      ##this farmer should grow peaches to maximize revenue
