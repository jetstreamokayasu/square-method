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
  
  #debugText(center)
  
  vic<-choice[1:nvic,]
  
  #debugText(center)
  
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
mapping2<-function(vic, center, coe, torus){
  
  vics<-line.vics(center, vic)
  
  feet<-sapply(vics, function(p){
    
    t<-(torus[p, 3]-coe[1]*torus[p, 1]-coe[2]*torus[p, 2]-coe[3])/(sum((c(coe[1:2], 1))^2))
    
    return(c(p, coe[1]*t+torus[p, 1], coe[2]*t+torus[p, 2], -t+torus[p, 3]))
    
  })
  
  feet<-t(feet)
  #debugText(feet)
  colnames(feet)<-c("origin", "x", "y", "z")
  #debugText(feet)
  return(feet)
  
}
#データを補間する関数
# interpolation<-function(centr, feet, coe){
#   
#   q<-rep(0, 3)
#   c<-feet[1, 2:4]
#   p<-feet[length(feet[,1]), 2:4]
#   r<-p-c
#   
#   q[1]<-feet[(length(feet[,1])%/%2)+1, "x"]
#   
#   q[2]<-((-q[1])*(r[1]+r[3]*coe[1])+sum(r*c)-r[3]*coe[3])/(r[2]+coe[2]*r[3])
#   
#   q[3]<-coe[1]*q[1]+coe[2]*q[2]+coe[3]
#   
#   d<-sqrt(sum(r^2))
#   e1<-r/d
#   e2<-(q-c)/sqrt(sum((q-c)^2))
#   base<-cbind(e1[1:2], e2[1:2])
#   
#   mapped<-sapply(1:length(feet[,1]), function(l){
#     
#     k<-solve(base)%*%(feet[l, 2:3]-feet[1, 2:3])
#     
#     return(c(feet[l, 1], k))
#     
#   })
#   
#   mapped<-t(mapped)
#   
#   colnames(mapped[,2:3])<-c("e1", "e2")
#   flag<-0
#   div<-2
#   
#   for (i in 0:(div-1)){
#     
#     for (j in 0:(div-1)){
#       
#       check1<-existence(c(i*(d/div), j*(d/div)), c((i+1)*(d/div), (j+1)*(d/div)), mapped)
#       
#       if(check1){
#         
#         expand1<-c+(((i+1)*(d/div))/2)*e1+(((j+1)*(d/div))/2)*e2
#         
#         if(flag>0) plus<-rbind(plus, expand1)
#          
#         else{
#           
#           plus<-expand1 
#           flag<-1
#           
#         }
#         
#       }
#       
#       check2<-existence(c(-i*(d/div), j*(d/div)), c(-(i+1)*(d/div), (j+1)*(d/div)), mapped)
#       
#       if(check2){
#         
#         expand2<-c+((-(i+1)*(d/div))/2)*e1+(((j+1)*(d/div))/2)*e2
#         
#         if(flag>0) plus<-rbind(plus, expand2)
#           
#         else{
#           
#           plus<-expand2 
#           flag<-1
#           
#         }
#         
#       }
#       
#       check3<-existence(c(-i*(d/div), -j*(d/div)), c(-(i+1)*(d/div), -(j+1)*(d/div)), mapped)
#       
#       if(check3){
#         
#         expand3<-c+((-(i+1)*(d/div))/2)*e1+((-(j+1)*(d/div))/2)*e2
#         
#         if(flag>0) plus<-rbind(plus, expand3)
#         
#         else{
#           
#           plus<-expand3
#           flag<-1
#           
#         }
#         
#       }
#       
#       check4<-existence(c(i*(d/div), -j*(d/div)), c((i+1)*(d/div), -(j+1)*(d/div)), mapped)
#       
#       if(check4){
#         
#         expand4<-c+(((i+1)*(d/div))/2)*e1+((-(j+1)*(d/div))/2)*e2
#         
#         if(flag>0) plus<-rbind(plus, expand4)
#           
#         
#         else{
#           
#           plus<-expand4
#           flag<-1
#           
#         }
#         
#       }
#       
#       #debugText(plus, flag)
#       
#     }
#     
#   }
#   
#   return(plus)
#   
# }

#start, endで区切られた範囲にデータ点が存在しするかしないかを判定する関数
existence<-function(start, end, mapped){
  
  exit.e1<-length((1:length(mapped[,1]))[mapped[,2]>=start[1] & mapped[,2]<end[1]])
  
  if(exit.e1>=1){
    
    exie1.mem<-(1:length(mapped[,1]))[mapped[,2]>=start[1] & mapped[,2]<end[1]]
    exit.e2<-length((1:length(mapped[exie1.mem, 1]))[mapped[exie1.mem, 3]>=start[2] & mapped[exie1.mem, 3]<end[2]])
    
    if(exit.e2>=1) return(F)
    
    else return(T)
    
  }
  
  else return(T)
  
}

#すべての点に対し10近傍で面を作り、収縮処理
# leastSquares<-function(){
#   
#   torus.dist<-distance(x)
#   
#   flag<-0
#   
#   for (k in 1:length(x[,1])){
#     
#     vic<-get.vicinity(torus.dist, k, 10)
#     vics<-line.vics(k, vic)
#     result<-lm(x[vics, 3]~x[vics, 1]+x[vics, 2])
#     
#     coe<-rep(0, length(coef(result)))
#     coe[1]<-coef(result)[2]
#     coe[2]<-coef(result)[3]
#     coe[3]<-coef(result)[1]
#     
#     feet<-mapping2(vic, k, coe)
#     plus<-interpolation(k, feet, coe)
#     
#     if(flag>0) intrpo<-rbind(intrpo, plus)
#     
#     else{
#       
#       intrpo<-plus
#       flag<-1
#       
#     } 
#     
#   }
#   
#   return(intrpo)
#   
# }

#1度近傍の中心となった点はなるべく2度とならないようにした関数
leastSquares2<-function(torus){
  
  torus.dist<-distance(torus)
  
  stlen<-standartLength(torus, torus.dist)
  
  flag<-0
  
  check<-rep(0, length(torus[,1]))
  
  for (k in 1:length(torus[,1])){
    
    if(check[k]<=0){
      
      plus<-recurSquare(check, k, torus.dist, stlen, torus)
      
      check<-plus[[2]]
      
      if(flag>0) intrpo<-rbind(intrpo, plus[[1]])
      
      else{
        
        intrpo<-plus[[1]]
        flag<-1
        
      }
      
      #debugText(k)
      
    }
    
    #debugText(check)
    
  }
  
  #debugText(check)
  
  return(intrpo)
  
}

#10近傍の中で最も遠い点を次の中心として補間する再帰関数
recurSquare<-function(check, k, torus.dist, stlen, torus){
  
  flag<-0
  
  #debugText(k)
  #cat("recur k=", k, "\n")
  
  vic<-get.vicinity(torus.dist, k, 10)
  vics<-line.vics(k, vic)
  result<-lm(torus[vics, 3]~torus[vics, 1]+torus[vics, 2])
  
  coe<-rep(0, length(coef(result)))
  coe[1]<-coef(result)[2]
  coe[2]<-coef(result)[3]
  coe[3]<-coef(result)[1]
  
  feet<-mapping2(vic, k, coe, torus)
  plus<-interpolation2(k, feet, coe, stlen)
  
  for (m in 1:length(vics)) {
    
    if(check[vics[length(vics)-m+1]]<=0){
      
      check[vics]<-check[vics]+1
      
      add<-recurSquare(check, vics[length(vics)-m+1], torus.dist, stlen, torus)
      
      flag<-1
      
      break
      
    }
    
  }
  
  if(flag==0){
    
    check[vics]<-check[vics]+1
    
    return(plus)
    
  } 
  
  else return(list(rbind(plus, add[[1]]), check))
  
}

#データを補間する関数
interpolation2<-function(centr, feet, coe, stlen){
  
  q<-rep(0, 3)
  c<-feet[1, 2:4]
  p<-feet[length(feet[,1]), 2:4]
  r<-p-c
  
  q[1]<-feet[(length(feet[,1])%/%2)+1, "x"]
  
  q[2]<-((-q[1])*(r[1]+r[3]*coe[1])+sum(r*c)-r[3]*coe[3])/(r[2]+coe[2]*r[3])
  
  q[3]<-coe[1]*q[1]+coe[2]*q[2]+coe[3]
  
  stlen<-sqrt(sum(r^2))
  e1<-r/stlen
  e2<-(q-c)/sqrt(sum((q-c)^2))
  base<-cbind(e1[1:2], e2[1:2])
  
  mapped<-sapply(1:length(feet[,1]), function(l){
    
    k<-solve(base)%*%(feet[l, 2:3]-feet[1, 2:3])
    
    return(c(feet[l, 1], k))
    
  })
  
  mapped<-t(mapped)
  
  colnames(mapped[,2:3])<-c("e1", "e2")
  flag<-0
  div<-2
  
  for (i in 0:(div-1)){
    
    for (j in 0:(div-1)){
      
      check1<-existence(c(i*(stlen/div), j*(stlen/div)), c((i+1)*(stlen/div), (j+1)*(stlen/div)), mapped)
      
      if(check1){
        
        expand1<-c+(((i+1)*(stlen/div))/2)*e1+(((j+1)*(stlen/div))/2)*e2
        
        if(flag>0) plus<-rbind(plus, expand1)
        
        else{
          
          plus<-expand1 
          flag<-1
          
        }
        
      }
      
      check2<-existence(c(-i*(stlen/div), j*(stlen/div)), c(-(i+1)*(stlen/div), (j+1)*(stlen/div)), mapped)
      
      if(check2){
        
        expand2<-c+((-(i+1)*(stlen/div))/2)*e1+(((j+1)*(stlen/div))/2)*e2
        
        if(flag>0) plus<-rbind(plus, expand2)
        
        else{
          
          plus<-expand2 
          flag<-1
          
        }
        
      }
      
      check3<-existence(c(-i*(stlen/div), -j*(stlen/div)), c(-(i+1)*(stlen/div), -(j+1)*(stlen/div)), mapped)
      
      if(check3){
        
        expand3<-c+((-(i+1)*(stlen/div))/2)*e1+((-(j+1)*(stlen/div))/2)*e2
        
        if(flag>0) plus<-rbind(plus, expand3)
        
        else{
          
          plus<-expand3
          flag<-1
          
        }
        
      }
      
      check4<-existence(c(i*(stlen/div), -j*(stlen/div)), c((i+1)*(stlen/div), -(j+1)*(stlen/div)), mapped)
      
      if(check4){
        
        expand4<-c+(((i+1)*(stlen/div))/2)*e1+((-(j+1)*(stlen/div))/2)*e2
        
        if(flag>0) plus<-rbind(plus, expand4)
        
        
        else{
          
          plus<-expand4
          flag<-1
          
        }
        
      }
      
      #debugText(plus, flag)
      
    }
    
  }
  
  return(plus)
  
}

#ランダムに選択したトーラスの20点を中心としたそれぞれの10近傍で平面を作り
#そこに写像した点で面を格子状に分割するための長さを決める関数
standartLength<-function(torus, torus.dist){
  
  sam<-sample(length(torus[,1]), 20)
  
  stlen<-sapply(sam, function(n){
    
    vic<-get.vicinity(torus.dist, n, 10)
    vics<-line.vics(n, vic)
    result<-lm(torus[vics, 3]~torus[vics, 1]+torus[vics, 2])
    
    coe<-rep(0, length(coef(result)))
    coe[1]<-coef(result)[2]
    coe[2]<-coef(result)[3]
    coe[3]<-coef(result)[1]
    
    feet<-mapping2(vic, n, coe, torus)
    len<-sqrt(sum((feet[1, 2:4]-feet[11, 2:4])^2))
    
    return(len)
    
  })
  
  return(mean(stlen))
  
}
