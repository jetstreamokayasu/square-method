require(TDA)
require(myfs)
require(rgl)

x<-torusUnif(10, 1, 2.5)

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

distance<-function(origin){
  
  dist<-matrix(0, (1/2)*nrow(origin)*(nrow(origin)-1), 2)
  r<-1
  
  for (k in 1:nrow(origin)) {
    for (l in 1:nrow(origin)) {
     #debugText(((k*l) %in% dist[,1])) 
      if((k!=l) && !((k*l) %in% dist[,1])){
        debugText(r, k, l, dist[,1])
        dist[r,1]<-k*l
        dist[r,2]<-sum((origin[k,]-origin[l,])^2)
        r<-r+1
        
      }
    }
    
  }
  return(dist)
}

torus.dist<-distance(x)
