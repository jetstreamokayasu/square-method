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

#トーラスの各点間の距離を求める
torus.dist<-distance(x)

s<-runif(100, -5, 5)
t<-runif(100, -5, 5)
#plot3d(s, t, u<-3*s+4*t+2, col="red")

#10近傍を求める
vic3<-get.vicinity(torus.dist, 3, 10)

#10近傍で作る平面の方程式の係数を求める
a3<-square.method(vic3, 3)
result<-lm(x[vic3s, 3]~x[vic3s, 1]+x[vic3s, 2])

#plot3d(s, t, v<-a3[1,1]*s+a3[2,1]*t+a3[3,1], col="red")

#作られた平面を描画
sur<-cbind(s, t, v<-a3[1]*s+a3[2]*t+a3[3])
points3d(sur, col=4)
# test.x<-rbind(x, sur)
# plot3d(test.x)
# aspect3d("iso")


# foot3<-mapping(vic3, 3, a3)
# points3d(foot3[,2:4], col=3)

#x[3,]の10近傍を除いたトーラスの描写
plot3d(x[-vic3[,2],])
aspect3d("iso")
#x[3,]の10近傍の描写
points3d(x[vic3[,2],], col=3)

#x[3,]とその10近傍の要素番号を並べる
vic3s<-line.vics(3, vic3)

#10近傍の平面への射影を描画
feet3<-mapping2(vic3, 3, a3)
points3d(feet3[-c(1, 11), 2:4], col=2)
#10近傍の中心と中心から最も遠い点を描画
points3d(feet3[c(1, 11), 2:4], col="pink")



# q3<-interpolation(3, feet3, a3)
# points3d(rbind(c(0,0,0), q3), col="orange")

#mapped3<-interpolation(3, feet3, a3)

#データを補間
plus3<-interpolation(3, feet3, a3)
#補間した点を描画
points3d(rbind(c(0,0,0), plus3), col="orange")




intrpo.test<-leastSquares()

points3d(rbind(c(0,0,0), intrpo.test), col="orange")


#データ補間テスト
intrpo.test2<-leastSquares2(x)
points3d(intrpo.test2, col="orange")

stlen<-standartLength()

ts.x<-torusUnif(300, 1, 2.5)

plot3d(ts.x)
aspect3d("iso")

intrpotest.tsx<-leastSquares2(ts.x)
points3d(intrpotest.tsx, col="orange")

diag.tsx<-ripsDiag(rbind(ts.x, intrpotest.tsx), maxdimension = 2, maxscale = 3)

y<-torusUnif(200, 1, 2.5)
plot3d(y)
aspect3d("iso")

intrpotest.y<-leastSquares2(y)
points3d(intrpotest.y, col="orange")

dist.tsx<-distance(ts.x)
tsxvic3<-get.vicinity(dist.tsx, 3, 10)

for (k in 1:length(ts.x[,1])) {
  
  tsvic<-get.vicinity(dist.tsx, k, 10)
  
  if(k==1) tsxvic<-tsvic
  
  else tsxvic<-rbind(tsxvic, tsvic)
  
}

tsxvic121<-get.vicinity(dist.tsx, 121, 10)

# z<-sphereUnif(n = 200, d = 2, r = 1)
# plot3d(z)
# aspect3d("iso")
# intrpotest.z<-leastSquares2(z)
# points3d(intrpotest.z, col="orange")
