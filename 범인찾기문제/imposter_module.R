rm(list=ls())
# 범죄현장 데이터 함수화
data = function(){
  df1 = data.frame(site=c(1:10),
                   x = c(2,5,5,5,1,5,7,4,5,6),
                   y = c(0,4,1,2,8,9,5,2,4,1))
  # 범죄이력
  df2 = data.frame(col1=c(1,4,8),
                   col2=c(5,7,1),
                   col3=c(5,1,3))
  
  row.names(df2) = c('Axy', 'Bxy', 'Cxy')
  
  return(list(df1, df2))
}

# (x-y)t((x-y)) = 거리의 합을 의미

# t()를 2번 쓰는 이유
# 첫번째 계산은 하나의 숫자값이 나오기 때문에 nuemric으로 나오게 된다. 
# t()를 한번 쓰면 matrix타입으로 변경이 되고, t()를 2번써야 transpose 기능이 적용이된다.

# distance 함수
ed = function(df1_location_mat, df2_location_mat){
  dis = matrix(0, nrow(df1_location_mat), nrow(df2_location_mat)) # distance A,B,C 별 10개의 거리
  
  for(j in 1:nrow(df2_location_mat)){
    for(i in 1:nrow(df1_location_mat)){
      dis[i,j] = sqrt((df1_location_mat[i,] - df2_location_mat[j,]) %*% t(t((df1_location_mat[i,] - df2_location_mat[j,]))))
    }
  }
  colnames(dis) = c('A', 'B', 'C')
  
  return(dis)
}

## 과제2

imposter = function(df2, dis){
  
  impost = matrix(rep(df2$col3,times=10), 10,3, byrow=T) # 전과횟수 행렬화
  
  formula= dis/impost # 거리 / 전과횟수
  
  imposter = matrix(0, nrow(impost),1) # 초기값 
  for(i in 1:nrow(impost)){ # 거리 / 전과횟수가 최소화인 사람을 imposter로 봄 
    imposter[i] = colnames(formula)[which(formula[i,] == min(formula[i,]))]
  }
  
  result=cbind(df1$site, imposter)
  result=as.data.frame(result)
  names(result)=c('Site','Imposter')
  
  return(result)
}

# plot 함수
# 그리드에 의해 데이터가 가려지는 것을 방지하고자, type='n'으로 설정 후 points 함수로 표시
plt_scene = function(df1, df2){
  plot(df1$x, df1$y, type='n', xlim=c(0,10), ylim=c(0,10), main='범죄현장', xlab='x 좌표', ylab='y 좌표', cex.main=2.5,
       cex.lab=1.5)
  grid()
  points(df1$x, df1$y, type='p', pch=19, cex=1.5, col='steelblue')
  points(df2, col='red', pch=19, cex=2)
  text(df2$col1, df2$col2+1, labels=c('A(5)', 'B(1)','C(3)'), font=2)
}