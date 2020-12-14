rm(list=ls())
# install.packages('matlib')
library(matlib)

xlim = c(0, 6)
ylim = c(0, 6)
plot(xlim, ylim, type='n', asp=1)
grid()

a = c(4,2)
b = c(1,3)

# 벡터 ploting (vectors함수 이용)
vectors(b, labels = 'b', pos.lab=4, frac.lab=.5, col='green')

vectors(a, labels='a', pos.lab=4, frac.lab=.5)

vectors(a+b, labels='a+b', pos.lab=4, frac.lab=.5, col='red')


a = c(0, 4)
b = c(5,0)

# cos(theta) a와 b가 공유하는 정보는 0이다.
a %*% b / (sqrt(a%*%a) %*% sqrt(b%*%b))

plot(xlim, ylim, type='n', asp=1)
grid()

vectors(a, labels='a', pos.lab=4, frac.lab=.5)
vectors(b, labels = 'b', pos.lab=3, frac.lab=.5, col='green')

# 2번째 예제
a = c(2,4)
b = c(3, 1)
length_a = sqrt(2^2 + 4^2)
length_b = sqrt(3^2 + 1^2)
cos_theta =  0

# |a| * |b| * cos(theta)
length_a * length_b * cos_theta

a%*%b

# cos(theta)가 0.7일 때 theta의 값
res = matrix(0, 980, 1) # 결과 저장
for(i in 1:980){ # 각도를 키워가며 코사인 값 계산
  res[i,1]= cos(i*pi/180) # 수치를 각도로 변환
}
which(res<=0.7) # 0.7보다 처음 작아지는 값값

abline(h=.7, col='red')

a = matrix(1:9,3,3)

3*a

# install.packages('pracma')
library(pracma)

v = c(1,2,1)

Norm(v, p=2)
P = matrix(c(2,4,3,2), ncol=2)
Norm(P, p=1)

mat = matrix(c(5,25,35,25,166,175,35,175,325), ncol=3)
mat

eanalysis = eigen(mat, symmetric = T)
eanalysis 

eanalysis$vectors %*% t(eanalysis$vectors)
  
# prod 요소들의 곱
prod(eanalysis$values)
