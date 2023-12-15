baralho=matrix(c(rep( c(2:10,"J","Q","K","A"),4),rep(c("Paus","Ouros","Copas","Espadas"),rep(13,4))), ncol=2,dimnames=list(NULL,c("valor_carta","naipe")))
library(dplyr)



mao_aposta = function(X,Y)
{
  baralho_inicial <- c(1:52)
  baralho_sem_x_y <- baralho_inicial[!baralho_inicial %in% c(X,Y)]
  jogo <- c(X,Y,sample(baralho_sem_x_y,5,replace=F))
  return(jogo)
}


qual_mao = function(mao)
{    
  valor_a_maior = c(2:10,"J","Q","K","A")
  valor_a_menor = c("A",2:10,"J","Q","K")
  
  valor_carta_i_ah = sort( sapply(mao[,"valor_carta"],function(x) which(x == valor_a_maior)) )
  valor_carta_i_al = sort( sapply(mao[,"valor_carta"],function(x) which(x == valor_a_menor)) )
  
  
  
  ################Flop#############################
  
  if (length(mao) == 10){
    
    ############Sequencia############
    
   
    
    secuencia0 = function(mao){
      is_Sequencia = F
      valor_carta_i_ah = sort( sapply(mao[,"valor_carta"],function(x) which(x == valor_a_maior)) )
      valor_carta_i_al = sort( sapply(mao[,"valor_carta"],function(x) which(x == valor_a_menor)) )
      is_Sequencia = all( valor_carta_i_ah-min(valor_carta_i_ah)+1 == 1:5 ) | all( valor_carta_i_al-min(valor_carta_i_al)+1 == 1:5 )
    return (is_Sequencia)}
    is_Sequencia = secuencia0(mao)
    ################################
    
    
    ############Flush###############
    
    is_flush = length(unique(mao[,"naipe"])) == 1
    
    ###############################
    
    
    #########Straight flush########
    
    
    if (is_Sequencia && is_flush) {
      if (all(c("K","A") %in% mao[,"valor_carta"])) return( "Royal flush" )
      else return( "Straight flush" )
    }
    
    ##############################
    
    if (is_Sequencia) return( "Sequencia" )
    if (is_flush) return( "Flush" )
    
    
    #####Quadra ou Full House#####
    
    
    tab = sort( table(mao[,"valor_carta"]) )
    if (length(tab)==2) {
      if (all(tab == c(1,4))) return( "Quadra" )
      if (all(tab == c(2,3))) return( "Full house")
    }
    
    #############################
    
    
    #######Trinca ou 2 Pares#######
    
    
    if (length(tab)==3) {
      if (all(tab == c(1,1,3))) return( "Trinca")
      if (all(tab == c(1,2,2))) return( "Dois Pares" )
    }
    
    
    #####Par ou Carta Alta#####
    
    
    
    if (length(tab)==4) {
      return( "Par" )
    }
    
  }
  
  #################################################
  
  
  
  ################Turn#############################
  
  
  if (length(mao) == 12){
    
    is_flush = F
    ############Sequencia############ 
    secuencia1 = function(maoi){ 
      valor_carta_i_ah = sort( sapply(maoi[,"valor_carta"],function(x) which(x == valor_a_maior)) )
      valor_carta_i_al = sort( sapply(maoi[,"valor_carta"],function(x) which(x == valor_a_menor)) )
      is_Sequencia = F
      
      a <- which(diff(valor_carta_i_ah) != 1)
      b <- which(diff(valor_carta_i_al) != 1)
      if (length(a) == 1){
        
        if( a <= 1 | a == 5) is_Sequencia =  T}
      
      if (length(b) == 1){
        if( b <= 1 | b == 5) is_Sequencia =  T}
      
      
      if(length(a) == 0 | length(b) == 0){
        is_Sequencia = T
        
      }
      return(is_Sequencia)}
    
    is_Sequencia = secuencia1(mao)
    
    
   
    
    
    #################################
    
    tab = sort( table(mao[,"valor_carta"]) )
    
    ######Quadra ou Full House#######
    
    if (length(tab)==2) {
      if (all(tab== c(2,4))) return("Quadra")
      if (all(tab== c(3,3))) return("Full house")
    }
    
    
    ######Quadra, Full House ou 2 Pares#######
    
    if (length(tab)==3){
      if (all(tab == c(1,1,4))) return("Quadra")
      if (all(tab == c(1,2,3))) return( "Full house")
      if (all(tab == c(2,2,2))) return( "Dois Pares")
    }
    
    
    #############Trinca ou 2 Pares############
    
    
    if (length(tab)==4){
      if (all(tab == c(1,1,1,3))) return("Trinca")
      if (all(tab == c(1,1,2,2))) return( "Dois Pares")
    }
    
    
    #################Par#####################
    
    if (length(tab)==5){return("Par")}
  }
  
  
  
  
  
  
  ################River############################
  
  
  
  if (length(mao) == 14){
    is_flush = F
    
    ##############Sequencia##############
secuencia2 = function(mao){
  
  valor_carta_i_ah = sort( sapply(mao[,"valor_carta"],function(x) which(x == valor_a_maior)) )
  valor_carta_i_al = sort( sapply(mao[,"valor_carta"],function(x) which(x == valor_a_menor)) )
  is_Sequencia = F
  
  a <- which(diff(valor_carta_i_ah) != 1)
  b <- which(diff(valor_carta_i_al) != 1)
    
    if (length(a) == 1){
      if( a <= 2 | a == 5| a== 6) is_Sequencia =  T}
    
    if (length(b) == 1){
      if( b <= 2 | b == 5| b == 6) is_Sequencia =  T}
    
    if (length(a) == 2){
      if (all(c(1,2) %in% a) |  all(c(1,6) %in% a) | all(c(5,6) %in% a)  ) is_Sequencia = T}
    
    if (length(b) == 2){
      if (all(c(1,2) %in% b) |  all(c(1,6) %in% b) | all(c(5,6) %in% b)  ) is_Sequencia = T}
  if(length(a) == 0 | length(b) == 0){ is_Sequencia = T}
  
  return(is_Sequencia)}
    
  is_Sequencia = secuencia2(mao)  
   
    
    ##############Quadra###############
    
    
      tab = sort( table(mao[,"valor_carta"]) )
      if (length(tab)==2) {
        if (all(tab== c(3,4))) return("Quadra")
      }
      
      
      ######Quadra ou Full House######
      
      if (length(tab)==3){
        if (all(tab == c(1,2,4))) return("Quadra")
        if (all(tab == c(2,2,3))) return( "Full house")
        if (all(tab == c(1,3,3))) return( "Full house")
      }
      
      
      ##Quadra, Full House ou 2 Pares#
      
      if (length(tab)==4){
        if (all(tab == c(1,1,1,4))) return("Quadra")
        if (all(tab == c(1,1,2,3))) return( "Full house")
        if (all(tab == c(1,2,2,2))) return( "Dois Pares")
      }
      
      #######Trinca ou 2 Pares#######
      
      if (length(tab)==5){
        if (all(tab == c(1,1,1,1,3))) return( "Trinca")
        if (all(tab == c(1,1,1,2,2))) return( "Dois Pares")
      }
      
      
      ##############Par#############
      
      if (length(tab)==6){return("Par")}
      
    } 
    


  

tab = sort( table(mao[,"naipe"]) )
if(c(5) %in% tab| c(6) %in% tab|c(7) %in% tab) is_flush = T

a <- names(table(mao[,"naipe"])[table(mao[,"naipe"]) == max(table(mao[,"naipe"]))]) 
if (is_Sequencia && is_flush) {
  df <-  data.frame(mao)
  df <- filter(df, naipe == a)
  
  if (all(c("K","A", "Q", "J", "10") %in% df[,"valor_carta"])) return( "Royal flush" )}


if(length(df) == 5){ is_Sequencia = secuencia0(df)}
if(length(df) == 6){ is_Sequencia = secuencia1(df)}
if(length(df) == 7){ is_Sequencia = secuencia2(df)}

if (is_Sequencia && is_flush ) return( "Straight flush" )



if(is_flush) return( "Flush" )
if(is_Sequencia) return("Sequencia")


return( "Sem jogo" )
}


simulate = function(N=10, n = 5, X, Y, jogada = c("Royal flush", "Straight flush", "Quadra", "Full house", "Flush", "Sequencia", "Trinca", "Dois Pares", "Par", "Sem jogo")) 

  {
  jogadas = c("Royal flush", "Straight flush", "Quadra",
            "Full house", "Flush", "Sequencia",
            "Trinca", "Dois Pares", "Par", "Sem jogo")
  
  jogadas_retiradas = c()
  
  for (i in 1:match(jogada, jogadas)){
    jogadas_retiradas[i] = jogadas[i]
  }
  
  res = matrix(rep(0,length(jogadas)),ncol=1)
  rownames(res) = jogadas
  colnames(res) = "Counts"
  
  matriz_jogos <- matrix(nrow = 7, ncol = 1)
  matriz_jogos <- matriz_jogos[, -1]
  
  for(i in 1:N) {
    jogo <- mao_aposta(X = X, Y = Y)
    matriz_jogos <- cbind(matriz_jogos, jogo)
    
  }
  
  pb = txtProgressBar(min = 0, max = N, style = 3)
  sucesso <- rep(0, times = length(matriz_jogos[1, ]))
  
  if (n == 5){
    
  for (i in 1:length(matriz_jogos[1, ])) {
  mao = qual_mao(baralho[matriz_jogos[1:n , i],])
  res[mao,1] = res[mao,1]+1
  setTxtProgressBar(pb, i)

  }}
  
  if (n == 6){
    
  for (i in 1:length(matriz_jogos[1, ])) {
  mao = qual_mao(baralho[matriz_jogos[1:(n-1) , i],])
  
  if ( mao %in% jogadas_retiradas){
    sucesso[i] <- sucesso[i]+i
    
  }}
  
  if (sum(sucesso) != 0){    
  
    matriz_jogos <- matriz_jogos[, -sucesso]
  }
  
  
  for (i in 1:length(matriz_jogos[1, ])) {
    mao = qual_mao(baralho[matriz_jogos[1:n , i],])
    res[mao,1] = res[mao,1]+1
    setTxtProgressBar(pb, i)
    
  }}
  
  if (n == 7){
    
    for (i in 1:length(matriz_jogos[1, ])) {
      mao = qual_mao(baralho[matriz_jogos[1:(n-2) , i],])
      
      if ( mao %in% jogadas_retiradas){
        sucesso[i] <- sucesso[i]+i
        
      }}
    
    if (sum(sucesso) != 0){    
      
      matriz_jogos <- matriz_jogos[, -sucesso]
    }
    
    
    for (i in 1:length(matriz_jogos[1, ])) {
      mao = qual_mao(baralho[matriz_jogos[1:(n-1) , i],])
      
      if ( mao %in% jogadas_retiradas){
        sucesso[i] <- sucesso[i]+i
        
      }}
    
    if (sum(sucesso) != 0){    
      
      matriz_jogos <- matriz_jogos[, -sucesso]
    }
    
    for (i in 1:length(matriz_jogos[1, ])) {
      mao = qual_mao(baralho[matriz_jogos[1:n , i],])
      res[mao,1] = res[mao,1]+1
      setTxtProgressBar(pb, i)
    }
    
    
    
    }
  
  
  return(res)
}

###############################################################