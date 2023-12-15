deck=matrix(c(rep( c(2:10,"J","Q","K","A"),4),rep(c("C","D","H","S"),rep(13,4))), ncol=2,dimnames=list(NULL,c("rank","suit")))
library(dplyr)



deal_hand = function(X)
{
  return( deck[c(9,22,sample(c(1:8,10:21, 23:52),X,replace=F)),])
}


what_hand = function(hand)
{    
  ranks_acehigh = c(2:10,"J","Q","K","A")
  ranks_acelow = c("A",2:10,"J","Q","K")
  
  rank_i_ah = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acehigh)) )
  rank_i_al = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acelow)) )
  
  
  
  ################Flop#############################
  
  if (length(hand) == 10){
    
    ############Sequencia############
    
   
    
    secuencia0 = function(hand){
      is_straight = F
      rank_i_ah = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acehigh)) )
      rank_i_al = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acelow)) )
      is_straight = all( rank_i_ah-min(rank_i_ah)+1 == 1:5 ) | all( rank_i_al-min(rank_i_al)+1 == 1:5 )
    return (is_straight)}
    is_straight = secuencia0(hand)
    ################################
    
    
    ############Flush###############
    
    is_flush = length(unique(hand[,"suit"])) == 1
    
    ###############################
    
    
    #########Straight Flush########
    
    
    if (is_straight && is_flush) {
      if (all(c("K","A") %in% hand[,"rank"])) return( "Royal flush" )
      else return( "Straight flush" )
    }
    
    ##############################
    
    if (is_straight) return( "Straight" )
    if (is_flush) return( "Flush" )
    
    
    #####Quadra ou Full House#####
    
    
    tab = sort( table(hand[,"rank"]) )
    if (length(tab)==2) {
      if (all(tab == c(1,4))) return( "Four of a kind" )
      if (all(tab == c(2,3))) return( "Full house")
    }
    
    #############################
    
    
    #######Trinca ou 2 Pares#######
    
    
    if (length(tab)==3) {
      if (all(tab == c(1,1,3))) return( "Three of a kind")
      if (all(tab == c(1,2,2))) return( "Two pair" )
    }
    
    
    #####Par ou Carta Alta#####
    
    
    
    if (length(tab)==4) {
      return( "Pair" )
    }
    
  }
  
  #################################################
  
  
  
  ################Turn#############################
  
  
  if (length(hand) == 12){
    
    is_flush = F
    ############Sequencia############ 
    secuencia1 = function(handi){ 
      rank_i_ah = sort( sapply(handi[,"rank"],function(x) which(x == ranks_acehigh)) )
      rank_i_al = sort( sapply(handi[,"rank"],function(x) which(x == ranks_acelow)) )
      is_straight = F
      
      a <- which(diff(rank_i_ah) != 1)
      b <- which(diff(rank_i_al) != 1)
      if (length(a) == 1){
        
        if( a <= 1 | a == 5) is_straight =  T}
      
      if (length(b) == 1){
        if( b <= 1 | b == 5) is_straight =  T}
      
      
      if(length(a) == 0 | length(b) == 0){
        is_straight = T
        
      }
      return(is_straight)}
    
    is_straight = secuencia1(hand)
    
    
   
    
    
    #################################
    
    tab = sort( table(hand[,"rank"]) )
    
    ######Quadra ou Full House#######
    
    if (length(tab)==2) {
      if (all(tab== c(2,4))) return("Four of a kind")
      if (all(tab== c(3,3))) return("Full house")
    }
    
    
    ######Quadra, Full House ou 2 Pares#######
    
    if (length(tab)==3){
      if (all(tab == c(1,1,4))) return("Four of a kind")
      if (all(tab == c(1,2,3))) return( "Full house")
      if (all(tab == c(2,2,2))) return( "Two pair")
    }
    
    
    #############Trinca ou 2 Pares############
    
    
    if (length(tab)==4){
      if (all(tab == c(1,1,1,3))) return("Three of a kind")
      if (all(tab == c(1,1,2,2))) return( "Two pair")
    }
    
    
    #################Par#####################
    
    if (length(tab)==5){return("Pair")}
  }
  
  
  
  
  
  
  ################River############################
  
  
  
  if (length(hand) == 14){
    is_flush = F
    
    ##############Sequencia##############
secuencia2 = function(hand){
  
  rank_i_ah = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acehigh)) )
  rank_i_al = sort( sapply(hand[,"rank"],function(x) which(x == ranks_acelow)) )
  is_straight = F
  
  a <- which(diff(rank_i_ah) != 1)
  b <- which(diff(rank_i_al) != 1)
    
    if (length(a) == 1){
      if( a <= 2 | a == 5| a== 6) is_straight =  T}
    
    if (length(b) == 1){
      if( b <= 2 | b == 5| b == 6) is_straight =  T}
    
    if (length(a) == 2){
      if (all(c(1,2) %in% a) |  all(c(1,6) %in% a) | all(c(5,6) %in% a)  ) is_straight = T}
    
    if (length(b) == 2){
      if (all(c(1,2) %in% b) |  all(c(1,6) %in% b) | all(c(5,6) %in% b)  ) is_straight = T}
  if(length(a) == 0 | length(b) == 0){ is_straight = T}
  
  return(is_straight)}
    
  is_straight = secuencia2(hand)  
   
    
    ##############Quadra###############
    
    
      tab = sort( table(hand[,"rank"]) )
      if (length(tab)==2) {
        if (all(tab== c(3,4))) return("Four of a kind")
      }
      
      
      ######Quadra ou Full House######
      
      if (length(tab)==3){
        if (all(tab == c(1,2,4))) return("Four of a kind")
        if (all(tab == c(2,2,3))) return( "Full house")
        if (all(tab == c(1,3,3))) return( "Full house")
      }
      
      
      ##Quadra, Full House ou 2 Pares#
      
      if (length(tab)==4){
        if (all(tab == c(1,1,1,4))) return("Four of a kind")
        if (all(tab == c(1,1,2,3))) return( "Full house")
        if (all(tab == c(1,2,2,2))) return( "Two pair")
      }
      
      #######Trinca ou 2 Pares#######
      
      if (length(tab)==5){
        if (all(tab == c(1,1,1,1,3))) return( "Three of a kind")
        if (all(tab == c(1,1,1,2,2))) return( "Two pair")
      }
      
      
      ##############Par#############
      
      if (length(tab)==6){return("Pair")}
      
    } 
    


  

tab = sort( table(hand[,"suit"]) )
if(c(5) %in% tab| c(6) %in% tab|c(7) %in% tab) is_flush = T

a <- names(table(hand[,"suit"])[table(hand[,"suit"]) == max(table(hand[,"suit"]))]) 
if (is_straight && is_flush) {
  df <-  data.frame(hand)
  df <- filter(df, suit == a)
  
  if (all(c("K","A", "Q", "J", "10") %in% df[,"rank"])) return( "Royal flush" )}


if(length(df) == 5){ is_straight = secuencia0(df)}
if(length(df) == 6){ is_straight = secuencia1(df)}
if(length(df) == 7){ is_straight = secuencia2(df)}

if (is_straight && is_flush ) return( "Straight flush" )



if(is_flush) return( "Flush" )
if(is_straight) return("Straight")


return( "No pair" )
}
what_hand(a)


simulate = function(N=10000, x) 
{
  hands = c("Royal flush", "Straight flush", "Four of a kind",
            "Full house", "Flush", "Straight",
            "Three of a kind", "Two pair", "Pair", "No pair")
  
  res = matrix(rep(0,length(hands)),ncol=1)
  rownames(res) = hands
  colnames(res) = "Counts"
  
  pb = txtProgressBar(min = 0, max = N, style = 3)
  for(i in 1:N) {
    hand = what_hand(deal_hand(x))
    
    res[hand,1] = res[hand,1]+1
    setTxtProgressBar(pb, i)
  }
  
  return(res)
}

###############################################################

maos <- replicate(10000, sample(c(1:11,14:52),5,replace=F))



hands = c("Royal flush", "Straight flush", "Four of a kind",
          "Full house", "Flush", "Straight",
          "Three of a kind", "Two pair", "Pair", "No pair")

res = matrix(rep(0,length(hands)),ncol=1)
rownames(res) = hands
colnames(res) = "Counts"


sucesso <- rep(0, times = length(maos[1, ]))

maos <- maos[, -sucesso]

for(i in 1: length(maos[1, ])) {
 
  hand <-  what_hand(deck[c(12,13, maos[1:5 , i] ),])
  res[hand,1] = res[hand,1]+1
  if ( hand == "Flush"){
    sucesso[i] <- sucesso[i]+i
    
  }
}

res1 <- res
res2 <- res
sum(res)



}

deck[c(12,13, maos[1:x , i] ),]


sucesso
