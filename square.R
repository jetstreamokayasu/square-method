require(TDA)
require(myfs)
require(rgl)

x<-torusUnif(100, 1, 2.5)

plot3d(x)
aspect3d("iso")

# x1<-1:20
# x0<-rep(1, 20)
# X<-cbind(x0, x1)
# y<-x1^2+1
# 
# plot(x1, x1^2+1)
# 
# a<-solve(t(X)%*%X)%*%t(X)%*%y
# par(new=T)
# plot(x1, a[2]*x1+a[1]*x0, type="l")

torus.dist<-distance(x)

s<-runif(100, -5, 5)
t<-runif(100, -5, 5)
#plot3d(s, t, u<-3*s+4*t+2, col="red")

vic3<-get.vicinity(torus.dist, 3, 10)

#任意の点(center)の近傍による最小二乗法で平面を作る
square.method<-function(vic, center){

  A<-rbind(x[center,], x[vic[which(vic[,2]!=center), 2], ])
  A<-rbind(A, x[vic[which(vic[,1]!=center), 1], ])
  z<-A[,3]
  A[,3]<-1
  
  a<-solve(t(A)%*%A)%*%t(A)%*%z
  
  return(a)
  
}

a3<-square.method(vic3, 3)

#plot3d(s, t, v<-a3[1,1]*s+a3[2,1]*t+a3[3,1], col="red")
sur<-cbind(s, t, v<-a3[1]*s+a3[2]*t+a3[3])
points3d(sur, col="red")
# test.x<-rbind(x, sur)
# plot3d(test.x)
# aspect3d("iso")

#近傍の点から作られた平面へ下した垂線の足を求める
mapping<-function(vic, center, coe){
  
  vics<-sapply(1:length(vic[,2]), function(t){
    
    if(vic[t,2]==center) return(vic[t, 1])
    else return(vic[t, 2])
    
  })
  
  vics<-c(center, vics)
  
  O<-x[center, ]
  O[3]<-coe[1]*O[1]+coe[2]*O[2]+coe[3]
  
  if(vic[10,2]==center) P<-x[vic[10, 1], ]
  else P<-x[vic[10, 2], ]
  
  P[3]<-coe[1]*P[1]+coe[2]*P[2]+coe[3]
  
  if(vic[5,2]==center) Q<-x[vic[5, 1], ]
  else Q<-x[vic[5, 2], ]
  
  Q[3]<-coe[1]*Q[1]+coe[2]*Q[2]+coe[3]
  
  vec.OP<-O-P
  vec.OQ<-O-Q
  
  R<-cbind(vec.OP, vec.OQ)
  
  foot<-sapply(vics, function(a){
    
    st<-solve(t(R)%*%R)%*%(t(R)%*%cbind(x[a, ]))
    return(c(a,(R%*%st)+O))
    
  })
  
  #st<-solve(t(R)%*%R)%*%(t(R)%*%cbind(x[center, ]))
  
  #foot<-(R%*%st)+O
  
  foot<-t(foot)
  debugText(foot)
  rownames(foot)<-NULL
  debugText(foot)
  colnames(foot)<-c("origin", "x", "y", "z")
  debugText(foot)
  return(foot)
  
}

foot3<-mapping(vic3, 3, a3)
points3d(foot3[,2:4], col=3)

