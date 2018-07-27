#データ点間の距離を求める関数
distance<-function(origin){
  
  dist<-matrix(0, (1/2)*nrow(origin)*(nrow(origin)-1), 3)
  colnames(dist)<-c("start", "goal", "distance")
  r<-1
  
  for (k in 1:nrow(origin)) {
    for (l in (k+1):nrow(origin)) {
      
      #if((k!=l) && !(k %in% dist[,1]) && !(l %in% dist[,2])){
      if(l <= nrow(origin) && k!=l){
        #debugText(k ,l, r)
        dist[r,1]<-k
        dist[r,2]<-l
        dist[r,3]<-sum((origin[k,]-origin[l,])^2)
        r<-r+1
      }
      #}
    }
    
  }
  return(dist)
}

#任意の点から最も近いnvic点を求める関数
get.vicinity<-function(dis, center, nvic){
  
  choice<-rbind(dis[which(dis[, "start"]==center), ], dis[which(dis[, "goal"]==center), ])
  choice<-choice[order(choice[,3]),]
  
  vic<-choice[1:nvic,]
  
  return(vic)
  
}