

hhPrediction <- function(base.data, metroName, base.comp=FALSE,
                         spec.comp=NULL){
  
  if(metroName == 'All'){
    hh.data <- base.data
  } else {
    hh.data <- subset(base.data, cityName == metroName)
  }
  
  # Extract necessary data
  loc.data <- buildLQData(hh.data, metroName=metroName, logScale=1)
  lq.data <- loc.data$LQs
  dist.names <- names(lq.data[[1]])
  
  # Convert LQs to a DF
  lq.df<- as.data.frame(matrix(as.numeric(unlist(lq.data)), ncol=8))
  rownames(lq.df) <- dist.names
  colnames(lq.df) <- names(hh.data)[8:15]

  # Compute totals and percentages
  col.tot <- colSums(hh.data[ ,8:15])
  all.tot <- sum(hh.data[ ,7])
  all.per <- col.tot / all.tot
  
  # Compute distance based percentages
  dist.per <- lq.df
  for(i in 1:101){
    if(!base.comp){
      dist.per[i,] <- all.per * lq.df[i, ]
    } else {
      dist.per[i,] <- all.per
    }
    if(!is.null(spec.comp)){
      dist.per[i,] <- spec.comp
    }
  }

  # Split up hh.data by distance
  age.dist <- split(hh.data[,8:15], hh.data$distScaled)
  tot.dist <- split(hh.data[,7], hh.data$distScaled)
  
  # Make predictions
  pred.list <- list()
  pred.error <- list()
  for(j in 1:101){
    pred.list[[j]] <- hhPredEngine(dist.per[j, ], tot.dist[[j]])
    pred.error[[j]] <- hhPredError(pred.list[[j]], age.dist[[j]])
  }
  
  # Calc Error
  a.pe <- lapply(pred.error, abs)
  rs.pe <- lapply(a.pe, function(x) rowSums(x)/2)
  
  # Per distance
  dist.pe <- list()
  for(j in 1:101){
    dist.pe[[j]] <- 1 - (sum(rs.pe[[j]]) / sum(tot.dist[[j]]))
  }
  
  # Overall
  error.sum <- sum(unlist(lapply(rs.pe, sum)))
  error.total <- 1 - (error.sum / all.tot)
  
  return(list(total=error.total,
              dist=unlist(dist.pe)))
  
}

hhPredError <- function(x, y){
  x-y
}


hhPredEngine <- function(dist.pers, tot.counts){
  
  raw.hh <- outer(tot.counts, as.numeric(dist.pers))
  round.hh <- round(raw.hh, 0)
  dif.count <- rowSums(round.hh) - tot.counts
  dif.hh <- abs(raw.hh - round.hh)
  dif.order <- t(apply(dif.hh,1,order, decreasing=T))
  
  for(jj in 1:nrow(dif.hh)){
    
    if(dif.count[jj] < 0) {
      dseq <- dif.order[jj,1:(abs(dif.count[jj]))]
      round.hh[jj,dseq] <- round.hh[jj, dseq] + 1
    }
    if(dif.count[jj] > 0) {
      dseq <- dif.order[jj,1:(abs(dif.count[jj]))]
      round.hh[jj,dseq] <- round.hh[jj, dseq] - 1
    }
  }
  return(round.hh)
}




if(FALSE){
  plot((1:101/100),bb$dist, type='l', 
       xlab= '% of distance from CBD', ylab='% correct',
       ylim=c(.55, .77)) 
  lines((1:101/100),aa$dist, col=2)
  lines((1:101/100),cc$dist, col='gray50')
  legend('bottomleft', c('Ours', 'Base', 'NULL'), col=c(2,1,'gray50'), lwd=c(2,2,2))
  
}

