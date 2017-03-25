Y = c("1","2");
x = c("x1","x2","x3");
w = c(1,0.5,1,1,0.2,1,0.5,0.8,0.5);

f = function(y1 = NULL, y2 = NULL, x = NULL, i, w)
{
  f1 = ifelse(i%in%c(2,3) && y1=="1" && y2=="2", 1, 0);
  f2 = ifelse(i==2 && y1=="1" && y2=="2", 1, 0);
  f3 = ifelse(i==3 && y1=="2" && y2=="1", 1, 0);
  f4 = ifelse(i==2 && y1=="2" && y2=="1", 1 ,0);
  f5 = ifelse(i==3 && y1=="2" && y2=="2", 1, 0);
  f6 = ifelse(i==1 && y2=="1", 1, 0);
  f7 = ifelse(i%in%c(1,2) && y2=="2", 1, 0);
  f8 = ifelse(i%in%c(2,3) && y2=="1", 1, 0);
  f9 = ifelse(i==3 && y2=="2", 1, 0);
  fi = c(f1,f2,f3,f4,f5,f6,f7,f8,f9)%*%w;
  return(fi[1]);
}

F = function(Y, x, i, w, f)
{
  Y_length = length(Y);
  Fi = matrix(0, nrow = Y_length, ncol = Y_length);
  for(k in 1:Y_length)
    for(j in 1:Y_length)
    {
      Fi[k,j] = f(y1 = Y[k], y2 = Y[j], i=i, w=w);
    }
  colnames(Fi) = paste("Y_i= ",Y,sep="");
  rownames(Fi) = paste("Y_i-1= ",Y,sep="");
  return(Fi);
};


crf_viterbi=function(f,w,x,Y)
{
  Y_length = length(Y);
  x_length = length(x);
  delta = matrix(0, nrow = x_length, ncol = Y_length);
  phi = delta;
  F_matrix_list = list();
  for(i in 1:x_length)
  {
    if(i==1)
    {
      for(j in 1:Y_length)
        delta[i,j] = f(y1 = "start", y2 = Y[j], i=i, w=w);
    }
    else
    {
      Fi = F(Y = Y, x = x, i = i, w = w, f = f);
      delta_Fi = delta[i-1,] + Fi;
      delta[i,] = apply(delta_Fi, 2, max);
      phi[i, ] = apply(delta_Fi, 2, which.max);
      F_matrix_list[[i-1]] = Fi;
    }
  }
  y_index = vector(length = 3);
  for(i in x_length:1)
  {
    if(i==x_length)
    {
      y_index[i] = which.max(delta[i,]);
    }else
      y_index[i] = phi[i+1,y_index[i+1]];
  }
  y_hat = Y[y_index];
  result = list();
  names(F_matrix_list) = paste("F",2:x_length,sep="");
  result$F_matrix = F_matrix_list;
  result$delta = delta;
  result$phi = phi;
  result$y_hat = y_hat;
  return(result);
}

crf_viterbi(f=f,w=w,x=x,Y=Y);
