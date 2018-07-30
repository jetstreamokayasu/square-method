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

a3<-square.method(vic3, 3)

result<-lm(x[vic3s, 3]~x[vic3s, 1]+x[vic3s, 2])

#plot3d(s, t, v<-a3[1,1]*s+a3[2,1]*t+a3[3,1], col="red")
sur<-cbind(s, t, v<-a3[1]*s+a3[2]*t+a3[3])
points3d(sur, col=4)
# test.x<-rbind(x, sur)
# plot3d(test.x)
# aspect3d("iso")

foot3<-mapping(vic3, 3, a3)
points3d(foot3[,2:4], col=3)


plot3d(x[-vic3[,2],])
aspect3d("iso")
points3d(x[vic3[,2],], col=3)

vic3s<-line.vics(3, vic3)

feet3<-mapping2(vic3, 3, a3)
points3d(feet3[-c(1, 11), 2:4], col=2)
points3d(feet3[c(1, 11), 2:4], col="pink")

interpolation<-function(centr, feet, coe){
  
  q<-rep(0, 3)
  c<-feet[1, 2:4]
  p<-feet[length(feet[,1]), 2:4]
  r<-p-c
  
  q[1]<-feet[(length(feet3[,1])%/%2)+1, "x"]
  
  q[2]<-((-q[1])*(r[1]+r[3]*coe[1])+sum(r*c)-r[3]*coe[3])/(r[2]+coe[2]*r[3])
  
  q[3]<-coe[1]*q[1]+coe[2]*q[2]+coe[3]
  
  d<-sqrt(sum(r^2))
  e1<-r/d
  e2<-(q-c)/sqrt(sum((q-c)^2))
  
  return(q)
  
}

q3<-interpolation(3, feet3, a3)
points3d(rbind(c(0,0,0), q3), col="orange")
