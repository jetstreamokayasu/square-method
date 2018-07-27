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

s<-runif(100, 1, 100)
t<-runif(100, 1, 100)
plot3d(s, t, u<-3*s+4*t+2, col="red")

get.vicinity<-function(dis, center, nvic){
  
  choice<-rbind(dis[which(dis[, "start"]==center), ], dis[which(dis[, "goal"]==center), ])
  choice<-choice[order(choice[,3]),]
  
  vic<-choice[1:nvic,]
  
  return(vic)
  
}

vic3<-get.vicinity(torus.dist, 3, 10)
