options(digits = 16)
g=as.integer(Sys.time()) #initial garbled for generating event
Ge<-function(g){
  g<<-(g*16807)%%(2^55-1)
  return(g/(2^55-1))
}
which.book<-function(e){ #which book I take next time 
  if(e<.06){
    return(x=1)
  }else if(e<.07){ return(x=2)
  }else if(e<.19){
    return(x=3)
  }else if(e<.33){
    return(x=4)
  }else if(e<.41){
    return(x=5)
  }else if(e<.59){
    return(x=6)
  }else if(e<.63){
    return(x=7)
  }else if(e<.73){
    return(x=8)
  }else if(e<.95){
    return(x=9)
  }else{
    return(x=10)
  }
}
######Policy A######
S=c(1:10) #state space
hand.length.m.A=0 #stat. counter mean
hand.length.v.A=0 #stat. counter second moment mean
m.A=0
for(i in 1:10000){
  x=which.book(Ge(g)) #eventlist
  S[c(1,which(S==x))]=S[c(which(S==x),1)]
}
for(i in 1:200){
  x=which.book(Ge(g)) #eventlist
  m.A=m.A+which(S==x)
  if(i%%5==0){
    hand.length.m.A=hand.length.m.A+(m.A/5)
    hand.length.v.A=hand.length.v.A+(m.A/5)^2
    m.A=0
  }
  S[c(1,which(S==x))]=S[c(which(S==x),1)]
}
b40=hand.length.m.A*5/i
v40=(hand.length.v.A-(i/5)*(hand.length.m.A*5/i)^2)/(i/5-1)
N.A=max(41,ceiling(2.786^2*v40/.01))hand.length.m.A.second=0
for(i in 1:(N.A*5-200)){
  x=which.book(Ge(g)) #eventlist
  m.A=m.A+which(S==x)
  if(i%%5==0){
    hand.length.m.A.second=hand.length.m.A.second+(m.A/5)
    m.A=0
  }
  S[c(1,which(S==x))]=S[c(which(S==x),1)]
}
W=40*(1+sqrt(1-N.A*(1-(N.A-40)*.01/(2.786^2*v40))/40))/N.A
W*hand.length.m.A/200+(1-W)*hand.length.m.A.second/(N.A-40)
######Policy A######
######Policy B######
S=c(1:10) #state space
hand.length.m.B=0 #stat. counter mean
hand.length.v.B=0 #stat. counter second moment mean
m.B=0 #local
for(i in 1:10000){
  x=which.book(Ge(g)) #eventlist
  if(which(S==x)!=1){
    S[c((which(S==x)-1),which(S==x))]=S[c(which(S==x),(which(S==x)-1))]
  }
}
for(i in 1:200){
  x=which.book(Ge(g)) #eventlist
  m.B=m.B+which(S==x)
  if(i%%5==0){
    hand.length.m.B=hand.length.m.B+(m.B/5)
    hand.length.v.B=hand.length.v.B+(m.B/5)^2
    m.B=0
  }
  if(which(S==x)!=1){
    S[c((which(S==x)-1),which(S==x))]=S[c(which(S==x),(which(S==x)-1))]
  }
}
b40=hand.length.m.B*5/i
v40=(hand.length.v.B-(i/5)*(hand.length.m.B*5/i)^2)/(i/5-1)
N.B=max(41,ceiling(2.786^2*v40/.01))
hand.length.m.B.second=0
for(i in 1:(N.B*5-200)){
  x=which.book(Ge(g)) #eventlist m.B=m.B+which(S==x)
  if(i%%5==0){
    hand.length.m.B.second=hand.length.m.B.second+(m.B/5)
    m.B=0
  }
  if(which(S==x)!=1){
    S[c((which(S==x)-1),which(S==x))]=S[c(which(S==x),(which(S==x)-1))]
  }
}
W=40*(1+sqrt(1-N.B*(1-(N.B-40)*.01/(2.786^2*v40))/40))/N.B
W*hand.length.m.B/200+(1-W)*hand.length.m.B.second/(N.B-40)
######Policy B######
######Policy C######
S=c(1:10) #state space
counter=numeric(10)
hand.length.m.C=0 #stat. counter mean
hand.length.v.C=0 #stat. counter second moment mean
m.C=0 #local
for(i in 1:10000){
  x=which.book(Ge(g)) #eventlist
  counter[x]=counter[x]+1
  S=order(counter)[10:1]
}
for(i in 1:200){
  x=which.book(Ge(g)) #eventlist
  m.C=m.C+which(S==x)
  if(i%%5==0){
    hand.length.m.C=hand.length.m.C+(m.C/5)
    hand.length.v.C=hand.length.v.C+(m.C/5)^2
    m.C=0
  }
  counter[x]=counter[x]+1
  S=order(counter)[10:1]
  
}
b40=hand.length.m.C*5/i
v40=(hand.length.v.C-(i/5)*(hand.length.m.C*5/i)^2)/(i/5-1)
N.C=max(41,ceiling(2.786^2*v40/.01))
hand.length.m.C.second=0
for(i in 1:(N.C*5-200)){
  x=which.book(Ge(g)) #eventlist m.C=m.C+which(S==x)
  if(i%%5==0){
    hand.length.m.C.second=hand.length.m.C.second+(m.C/5)
    m.C=0
  }
  counter[x]=counter[x]+1
  S=order(counter)[10:1]
}
W=40*(1+sqrt(1-N.C*(1-(N.C-40)*.01/(2.786^2*v40))/40))/N.C
W*hand.length.m.C/200+(1-W)*hand.length.m.C.second/(N.C-40)