#1. 設計機率
win_prob = function(y){
  x=.1
  M = c(0.001,0.999,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  for(i in 1:18){
    temp_array = numeric(20)
    temp_array[1] = x+i*y
    temp_array[i+2] = 1-temp_array[1]
    M = rbind(M,c(temp_array))
  }
  M = rbind(M,c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
  for(i in 1:20){
    M = M%*%M
  }
  return(M[1,1]-.15)
}uniroot(win_prob,c(0,.05),tol=0.001)
#2. 跑模擬
set.seed(100)
whether_price = function(){
  return(ifelse(runif(1)<(1/3),1,2))
}
Num_lottery = 1000
Num_people = 1000
big_win_list = matrix(0,ncol = Num_lottery,nrow = Num_people)
for(people in 1:Num_people){
  ind_first_five_win = sample(1:5,1)
  num_loss = 0
  whether_win_last_time = F
  win_list = numeric(Num_lottery)
  prob_x = .1
  prob_y = 0.01646118
  for(lottery in 1:Num_lottery){
    if(lottery == ind_first_five_win){
      win_list[lottery] = whether_price();whether_win_last_time = T;num_loss=0
    }else if(num_loss==19){
      win_list[lottery] = whether_price();whether_win_last_time = T;num_loss=0
    }else if((!whether_win_last_time)&&(runif(1)<(prob_x+num_loss*prob_y))){
      win_list[lottery] = whether_price();whether_win_last_time = T;num_loss=0
    }else if((whether_win_last_time)&&(runif(1)<0.001)){
      win_list[lottery] = whether_price();whether_win_last_time = T;num_loss=0
    }else{
      num_loss = num_loss+1
      whether_win_last_time = F
    }
    #print(c(lottery,num_loss))
  }
  #print(mean(win_list))
  big_win_list[people,] = win_list
}
#平均頭獎print(length(which(big_win_list==1))/(Num_lottery*Num_people))
#平均貳獎
print(length(which(big_win_list==2))/(Num_lottery*Num_people))
#大家前五次都有中獎嗎
for(people in 1:Num_people){
  if(sum(big_win_list[people,1:5])<=0){
    print('有人前五次沒中')
  }
}
#有沒有連續 20 次沒中獎的
for(people in 1:Num_people){
  temp_ind_win = which(big_win_list[people,]!=0)
  if(length(which(temp_ind_win[-1] - temp_ind_win[-length(temp_ind_win)]>21))!=0){
    print('有人連續 20 次沒中')
    print(people)
  }
}
#連續中獎比例
tt = 0
for(people in 1:Num_people){
  temp_ind_win = which(big_win_list[people,]!=0)
  if(length(which(temp_ind_win[-1] - temp_ind_win[-length(temp_ind_win)]==1))!=0){
    #print('有人連續中獎')
    tt = tt+1
  }
}
print(tt/(length(which(big_win_list!=0))))