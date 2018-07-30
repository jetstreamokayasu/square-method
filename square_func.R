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

#任意(center)の点から最も近いnvic点を求める関数
get.vicinity<-function(dis, center, nvic){
  
  choice<-rbind(dis[which(dis[, "start"]==center), ], dis[which(dis[, "goal"]==center), ])
  choice<-choice[order(choice[,3]),]
  
  vic<-choice[1:nvic,]
  
  return(vic)
  
}

#任意の点(center)の近傍による最小二乗法で平面を作る
square.method<-function(vic, center){
  
  A<-rbind(x[center,], x[vic[which(vic[,2]!=center), 2], ])
  A<-rbind(A, x[vic[which(vic[,1]!=center), 1], ])
  z<-A[,3]
  A[,3]<-1
  
  a<-solve(t(A)%*%A)%*%t(A)%*%z
  
  return(a)
  
}

#近傍の点から作られた平面へ下した垂線の足を求める
# mapping<-function(vic, center, coe){
#   
#   vics<-sapply(1:length(vic[,2]), function(t){
#     
#     if(vic[t,2]==center) return(vic[t, 1])
#     else return(vic[t, 2])
#     
#   })
#   
#   vics<-c(center, vics)
#   
#   O<-x[center, ]
#   O[3]<-coe[1]*O[1]+coe[2]*O[2]+coe[3]
#   
#   if(vic[10,2]==center) P<-x[vic[10, 1], ]
#   else P<-x[vic[10, 2], ]
#   
#   P[3]<-coe[1]*P[1]+coe[2]*P[2]+coe[3]
#   
#   if(vic[5,2]==center) Q<-x[vic[5, 1], ]
#   else Q<-x[vic[5, 2], ]
#   
#   Q[3]<-coe[1]*Q[1]+coe[2]*Q[2]+coe[3]
#   
#   vec.OP<-O-P
#   vec.OQ<-O-Q
#   
#   R<-cbind(vec.OP, vec.OQ)
#   
#   foot<-sapply(vics, function(a){
#     
#     st<-solve(t(R)%*%R)%*%(t(R)%*%cbind(x[a, ]))
#     return(c(a,(R%*%st)+O))
#     
#   })
#   
#   #st<-solve(t(R)%*%R)%*%(t(R)%*%cbind(x[center, ]))
#   
#   #foot<-(R%*%st)+O
#   
#   foot<-t(foot)
#   debugText(foot)
#   rownames(foot)<-NULL
#   debugText(foot)
#   colnames(foot)<-c("origin", "x", "y", "z")
#   debugText(foot)
#   return(foot)
#   
# }

#任意の点(centr)の近傍点の一覧を作る関数
line.vics<-function(centr, vic){
  
  vics<-sapply(1:length(vic[,2]), function(t){
    
    if(vic[t,2]==centr) return(vic[t, 1])
    else return(vic[t, 2])
    
  })
  
  names(vics)<-NULL
  
  vics<-c(centr, vics)
  
  return(vics)
  
}

#近傍の点から作られた平面への射影点を求める関数
mapping2<-function(vic, center, coe){
  
  vics<-line.vics(center, vic)
  
  feet<-sapply(vics, function(p){
    
    t<-(x[p, 3]-coe[1]*x[p, 1]-coe[2]*x[p, 2]-coe[3])/(sum((c(coe[1:2], 1))^2))
    
    return(c(p, coe[1]*t+x[p, 1], coe[2]*t+x[p, 2], -t+x[p, 3]))
    
  })
  
  feet<-t(feet)
  debugText(feet)
  colnames(feet)<-c("origin", "x", "y", "z")
  debugText(feet)
  return(feet)
  
}