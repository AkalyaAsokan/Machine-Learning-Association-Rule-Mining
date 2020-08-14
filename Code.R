#read the dataset from excel
library(readxl)
globalterrorismdb_0919dist <- read_excel("Desktop/globalterrorismdb_0919dist.xlsx",n_max = 5000)
View(globalterrorismdb_0919dist)
globalterrorismdb_0919dist

#attributes selected for creating association rules
targtype1_txt<-c()
city<-c()
location<-c()
weaptype1_txt<-c()
weapsubtype1_txt<-c()
natlty1_txt<-c()
country_txt<-c()
attacktype1_txt<-c()

#generate all the one itemsets
#extract attribute value and attribute number
for(i in 1:nrow(globalterrorismdb_0919dist)){
  #cat(paste( unlist(globalterrorismdb_0919dist[i,'targtype1_txt']), collapse=''),'\n')
  targtype1_txt<-c(targtype1_txt,paste( unlist(globalterrorismdb_0919dist[i,'targtype1_txt']), collapse=''),1)
  city<-c(city,paste( unlist(globalterrorismdb_0919dist[i,'city']), collapse=''),2)
  location<-c(location,paste( unlist(globalterrorismdb_0919dist[i,'location']), collapse=''),3)
  weaptype1_txt<-c(weaptype1_txt,paste( unlist(globalterrorismdb_0919dist[i,'weaptype1_txt']), collapse=''),4)
  weapsubtype1_txt<-c(weapsubtype1_txt,paste( unlist(globalterrorismdb_0919dist[i,'weapsubtype1_txt']), collapse=''),5)
  natlty1_txt<-c(natlty1_txt,paste( unlist(globalterrorismdb_0919dist[i,'natlty1_txt']), collapse=''),6)
  country_txt<-c(country_txt,paste( unlist(globalterrorismdb_0919dist[i,'country_txt']), collapse=''),7)
  attacktype1_txt<-c(attacktype1_txt,paste( unlist(globalterrorismdb_0919dist[i,'attacktype1_txt']), collapse=''),8)
}

#combine all the attributes' value and number
one_item_set<-c(targtype1_txt,city,location,weaptype1_txt,weapsubtype1_txt,natlty1_txt,country_txt,attacktype1_txt)
cat('Extracting one itemsets\n')
one_item_set
min_sup=20
conf=0.5
#combination of all attributes
library(rlist)
all_attr<-cbind(list.remove(targtype1_txt,which(targtype1_txt==1)),
                list.remove(city,which(city==2)),
                list.remove(location,which(location==3)),
                list.remove(weaptype1_txt,which(weaptype1_txt==4)),
                list.remove(weapsubtype1_txt,which(weapsubtype1_txt==5)),
                list.remove(natlty1_txt,which(natlty1_txt==6)),
                list.remove(country_txt,which(country_txt==7)),
                list.remove(attacktype1_txt,which(attacktype1_txt==8)))
colnames(all_attr)<-NULL
#convert into a 2 column matrix
one_item_set<-matrix(one_item_set,ncol=2,byrow=TRUE)
cat('One Itemsets as a matrix along with the corresponding attribute number\n')
one_item_set
#compute the frequency of all the  unique attributes
one_item_set_freq<-c()
for(i in unique(one_item_set[,1])){
  one_item_set_freq<-c(one_item_set_freq,length(which(one_item_set[,1] == i)))
}
#combine unique attribute value, attribute number and frequency
one_item_set<-cbind(unique(one_item_set),one_item_set_freq)
colnames(one_item_set)<-NULL
cat('One itemsets and their frequencies\n')
one_item_set
#check support
one_item_set<-subset(one_item_set,one_item_set[,3]>=min_sup)
cat('One itemsets with minimum support count\n\n')
one_item_set
cat('Sorted One itemsets')
one_item_set<-one_item_set[order(one_item_set[,1]),]
one_item_set

#generate all the two itemsets
#generating attribute value and attribute number
attrValNum<-function(prev_item_set){
  curr_item_set<-c()
  for(i in 1:(length(prev_item_set[,1])-1)){
    for(j in (i+1):length(prev_item_set[,1])){
      flag=0
      k=1
      while(k<=(((ncol(prev_item_set)-1)%/%2)-1)){
        if(prev_item_set[i,k*2-1]!=prev_item_set[j,k*2-1]){
          flag=1 
          break
        }
        else k=k+1
      }
      if(flag==0){
        if(prev_item_set[i,ncol(prev_item_set)-1]!=prev_item_set[j,ncol(prev_item_set)-1])
          curr_item_set<-c(curr_item_set,prev_item_set[i,1:ncol(prev_item_set)-1],
                                         prev_item_set[j,(ncol(prev_item_set)-2):(ncol(prev_item_set)-1)])
      }
    }
  }
  return (curr_item_set)
}
#convert into a 4 column matrix
convMatrix<-function(curr_item_set,s){
  curr_item_set<-matrix(curr_item_set,ncol=s,byrow=TRUE)
  return (curr_item_set)
}
#subset
subsets<-function(a,i){
  b<-c()
  for(ij in 1:length(a)){
    if((ij%%2)!=0)
      b<-c(b,a[ij])
  }
  return(t(combn(b,i)))
}
#extract attrVal
attrVal<-function(a){
  b<-c(a[,1])
  for(ij in 2:(ncol(a)-1)){
    if((ij%%2)!=0)
      b<-cbind(b,a[,ij])
  }
  colnames(b)<-NULL
  return (b)
}
#prunning for two itemsets
pruningTwo<-function(curr_item_set,prev_item_set){
  for(i in 1:nrow(curr_item_set)){
    for(j in subsets(curr_item_set[i,],1)){
      flag=0
      for(k in prev_item_set[,1]){
        if(j==k){
          flag=1
          break
        }
      }
      if(flag==0){
        two_item_set<-two_item_set[-i,]
        i=i-1
        break
      }
    }
  }
  return(curr_item_set)
}
#pruning for Three or More Itemsets
pruningThreeMore<-function(curr_item_set,prev_item_set,c){
  for(i in 1:nrow(curr_item_set)){
    for(j in 1:nrow(subsets(curr_item_set[i,],c))){
      rowj=subsets(curr_item_set[i,],c)[j,]
      flag=0
      for(k in 1:nrow(attrVal(prev_item_set))){
        rowk=attrVal(prev_item_set)[k,]
        if(all(rowj==rowk)==TRUE){
          flag=1
          break
        }
      }
      if(flag==0){
        two_item_set<-two_item_set[-i,]
        i=i-1
        break
      }
    }
  }
  return(curr_item_set)
}
#compute the frequency 
freq<-function(curr_item_set,c){
  curr_item_set_freq<-c()
  for(i in 1:nrow(curr_item_set)){
    rowi<-curr_item_set[i,]
    count=0
    for(j in 1:nrow(all_attr)){
      rowj<-all_attr[j,]
      cc=0
      for(k in 1:c){
        kt=rowi[k*2]
        if(identical(rowi[(k*2)-1],rowj[as.numeric(kt)])==TRUE)
          cc=cc+1
      }
      if(cc==c){
        count=count+1
      }
    }
    curr_item_set_freq<-c(curr_item_set_freq,count)
  }
  return(curr_item_set_freq)
}

#occurance of a list in transaction
occ<-function(a){
  count=0
  for(j in 1:nrow(all_attr)){
    rowj<-all_attr[j,]
    cc=0
    for(k in 1:(length(a)%/%2)){
      if(identical(a[(k*2)-1],rowj[as.numeric(a[k*2])])==TRUE)
        cc=cc+1
    }
    if(cc==(length(a)/2)){
      count=count+1
    }
  }
  return(count)
}
#if the rule has confidence more than min confidence
ifRule<-function(a,b){
  if((as.numeric(occ(b))%/%as.numeric(occ(a)))>=conf){
    return (TRUE)
  }
  else
    return (FALSE)
}
#split into two sets
splitTwo<-function(a){
  for(i in 1:(2^((length(a)/2)-1)-1)){
    b<-as.binary(i,n=((length(a)/2)-1))
    a12<-c()
    a1<-c()
    a2<-c()
    k=1
    a12<-a
    a2<-c(a2,a[1],a[2])
    for(j in b){
      k=k+2
      if(j==0)
        a1<-c(a1,a[as.numeric(k)],a[as.numeric(k+1)]) 
      else
        a2<-c(a2,a[as.numeric(k)],a[as.numeric(k+1)])
    }
    if(ifRule(a1,a12)==TRUE)
      cat(a1,'->',a2,'is a rule\n')
    if(ifRule(a2,a12)==TRUE)
      cat(a2,'->',a1,'is a rule\n')
  }
}
#generating rules
generateRules<-function(item_set){
  for(i in 1:nrow(item_set)){
    rowi=splitTwo(item_set[i,1:(ncol(item_set)-1)])
  }
}
library(binaryLogic)
curr_item_set<-c()
prev_item_set<-one_item_set
curr_item_set_freq<-c()
item=2
while(is.null(prev_item_set)==FALSE){
  curr_item_set<-attrValNum(prev_item_set)
  if(is.null(curr_item_set)){
    break
  }
  curr_item_set<-convMatrix(curr_item_set,item*2)
  cat('Self Joining\n')
  print(curr_item_set)
  if(item==2){
    curr_item_set<-pruningTwo(curr_item_set,prev_item_set)
  }
  else{
    curr_item_set<-pruningThreeMore(curr_item_set,prev_item_set,(item-1))
  }
  cat('Pruning\n')
  print(curr_item_set)
  if(is.null(curr_item_set)){
    break
  }
  curr_item_set_freq<-freq(curr_item_set,item)
  #combine attribute value, attribute number and frequency
  curr_item_set<-cbind(curr_item_set,curr_item_set_freq)
  colnames(curr_item_set)<-NULL
  cat('Computation of frequency\n')
  print(curr_item_set)
  #check support
  curr_item_set<-subset(curr_item_set,curr_item_set[,((item*2)+1)]>=min_sup)
  if(nrow(curr_item_set)==0)
    curr_item_set<-c()
  cat('Check Minimum support\n')
  print(curr_item_set)
  generateRules(curr_item_set)
  prev_item_set<-curr_item_set
  item=item+1
}
