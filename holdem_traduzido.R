### Package Import
library(dplyr)

### Deck
baralho = matrix(c(rep(c(2:10,"J","Q","K","A"), 4), # Card ranks
                   rep(c("Paus","Ouros","Copas","Espadas"), rep(13,4))), # Suits
                   ncol=2,
                   dimnames = list(NULL,c("valor_carta","naipe")))



### Deal Game function
mao_aposta = function(X,Y) # Hole cards as argument
{
  baralho_inicial <- c(1:52) # Whole deck
  baralho_sem_x_y <- baralho_inicial[!baralho_inicial %in% c(X,Y)] # Deck without hole cards
  jogo <- c(X, Y, sample(baralho_sem_x_y, 5, replace = F)) # Generated game
  return(jogo)
}


### Classify hand function
qual_mao = function(mao) # Game as argument
{    
  valor_a_maior = c(2:10,"J","Q","K","A") # Sequence with A at the end
  valor_a_menor = c("A",2:10,"J","Q","K") # Sequence with A at the beginning
  
  valor_carta_i_ah = sort( sapply(mao[,"valor_carta"], function(x) which(x == valor_a_maior)) ) # Check straight with A High 
  valor_carta_i_al = sort( sapply(mao[,"valor_carta"], function(x) which(x == valor_a_menor)) ) # Check straight with A Low
  
  tab_n = sort( table(mao[,"naipe"]) ) # Frequency table for suits
  
  ################ Flop #############################
  
  if (length(mao) == 10){
    
    ############ Straight ############
    
   
    
    secuencia0 = function(mao){
      is_Sequencia = F
      
      is_Sequencia = all( valor_carta_i_ah - min(valor_carta_i_ah) + 1 == 1:5 ) | # Check for sequence with High A
                     all( valor_carta_i_al - min(valor_carta_i_al) + 1 == 1:5 ) # Check for sequence with low A
    return (is_Sequencia)}
    
    is_Sequencia = secuencia0(mao)
    ################################
    
    
    ############ Flush ###############
    
    if(c(5) %in% sort( table(mao[,"naipe"]) )) is_flush = T else is_flush = F # If there is only one suit in five cards, it's a Flush
    
    ###############################
    
    
    ######### Straight flush ########
    
    
    if (is_Sequencia && is_flush) { # If it's a Sequence and a Flush, then it's a Straight Flush
      if (all(c("K","A") %in% mao[,"valor_carta"])) return( "Royal flush" ) # A Straight Flush with K and A is always a Royal Flush
      else return( "Straight flush" ) 
    }
    
    ##############################
    
    if (is_flush) return( "Flush" )
    if (is_Sequencia) return( "Sequencia" )
    
    
    ##### Four-of-a-Kind or Full House #####
    
    tab = sort( table(mao[,"valor_carta"]) ) # Frequency table for card values
    
    if (length(tab)==2) { # If there is only two different values
      if (all(tab == c(1,4))) return( "Quadra" ) # If there is a card value that happens 4 times, it's a Four-of-a-Kind
      if (all(tab == c(2,3))) return( "Full house") # If there is a card value that happens 2 times and another that happens 3 times, it's a Full House
    }
    
    #############################
    
    
    ####### Three-of-a-Kind or 2 Pair #######
    
    
    if (length(tab)==3) { # If there is only three different values
      if (all(tab == c(1,1,3))) return( "Trinca") # If there is a card value that happens 3 times, it's a Three-of-a-Kind
      if (all(tab == c(1,2,2))) return( "Dois Pares" ) # If there is two different values that happens 2 times, it's a Two Pair
    }
    
    
    ##### Pair or High Card #####
    
    
    
    if (length(tab)==4) { # If there is 4 different values, then it's a Pair
      return( "Par" )
    }
    
  }
  
  #################################################
  
  
  
  ################ Turn #############################
  
  
  if (length(mao) == 12){
    
    is_flush = F
    
    a <- which(diff(valor_carta_i_ah) != 1) # Find values outside of the straight sequence with High A
    b <- which(diff(valor_carta_i_al) != 1) # Find values outside of the straight sequence with Low A
    
    ############ Straight ############ 
    secuencia1 = function(maoi){ 
      
      is_Sequencia = F
      
      if (length(a) == 1){ # If there is only one value outside of the sequence
        
        if( a <= 1 | a == 5) is_Sequencia =  T} # If the "outside" card is the lowest or highest card
      
      if (length(b) == 1){ # If there is only one value outside of the sequence
        if( b <= 1 | b == 5) is_Sequencia =  T} # If the "outside" card is the lowest or highest card
      
      
      if(length(a) == 0 | length(b) == 0){ # If there is no card outside of the sequence
        is_Sequencia = T
        
      }
      return(is_Sequencia)}
    
    is_Sequencia = secuencia1(mao)
    
    ##################################
    
    ############ Flush ############
    
    if(c(5) %in% tab_n | c(6) %in% tab_n) is_flush = T # If there is a suit that occurs 5 or 6 times, then it's a Flush
    
    ##################################
    
    ############ Straight Flush ############
    
    if (is_Sequencia && is_flush) { # If it's a Sequence and a Flush, then it can be a Straight Flush
      
      find_sequence <- function(my_vector) { # Function to retrieve the specific values of a straight
        start_index <- NA
        sequence_values <- NA
        
        for (i in 1:(length(my_vector) - 4)) { # Potential starting values for the sequence
          if (all(diff(my_vector[i:(i + 4)]) == 1)) { # Find the straight values
            start_index <- i
            sequence_values <- my_vector[start_index:(start_index + 4)]
            break
          }
        }
        
        return(sequence_values)
      }
      
      seq_ah <- find_sequence(valor_carta_i_ah) # Find the straight with High A
      seq_al <- find_sequence(valor_carta_i_al) # Find the straight with Low A
      
      if (!is.na(find_sequence(valor_carta_i_ah))[1] == TRUE) {seq_cards = seq_ah} else {seq_cards = seq_al} # Straight values
      
      
      if (all(c("K","A", "Q", "J", "10") %in% names(seq_cards))) return( "Royal flush" ) # If a Straight Flush contains these cards, then it's a Royal Flush
      if (max(table(mao[mao[, 1] %in% names(seq_cards), "naipe"])) >= 5) return( "Straight flush" ) # If the cards from the straight sequence contains at least 5 occurrences of the same suit, it's a Straight Flush
      
      
    }
    
    if (is_flush) return( "Flush" )
    if (is_Sequencia) return( "Sequencia" )
    
    #################################
    
    tab = sort( table(mao[,"valor_carta"]) ) # Frequency table for card values
    
    ###### Four-of-a-Kind or Full House #######
    
    if (length(tab)==2) { # If there is only two values
      if (all(tab== c(2,4))) return("Quadra") # If there is a card value that happens 4 times, it's a Four-of-a-Kind
      if (all(tab== c(3,3))) return("Full house") # If there is two card values that happens 3 times each, it's a Full House 
    }
    
    
    ###### Four-of-a-Kind, Full House or 2 Pair #######
    
    if (length(tab)==3){ # If there is only three values
      if (all(tab == c(1,1,4))) return("Quadra") # If there is a card value that happens 4 times, it's a Four-of-a-Kind
      if (all(tab == c(1,2,3))) return( "Full house") # If there is a card value that happens 2 times and another that happens 3 times, it's a Full House
      if (all(tab == c(2,2,2))) return( "Dois Pares") # If there is three card values that happens 2 times each, then it's a Two Pair
    }
    
    
    ############# Three-of-a-Kind or 2 Pair ############
    
    
    if (length(tab)==4){ # If there is only four values
      if (all(tab == c(1,1,1,3))) return("Trinca") # If there is a card value that happens 3 times, it's a Three-of-a-Kind
      if (all(tab == c(1,1,2,2))) return( "Dois Pares") # If there is two card values that happens 2 times each, then it's a Two Pair
    }
    
    
    ################# Pair #####################
    
    if (length(tab)==5){return("Par")} # If there is only five values, it's a Pair
  }
  
  
  
  
  
  
  ################ River ############################
  
  
  
  if (length(mao) == 14){
    is_flush = F
    
    a <- which(diff(valor_carta_i_ah) != 1) # Find values outside of the sequence with High A
    b <- which(diff(valor_carta_i_al) != 1) # Find values outside of the sequence with Low A
    
    ############## Sequence ##############
    secuencia2 = function(mao){
  
      is_Sequencia = F
    
     if (length(a) == 1){ # If there is only one value outside of the sequence
       
       if( a <= 2 | a == 5 | a== 6) is_Sequencia =  T} # If the "outside" values are the first or last ones
    
     if (length(b) == 1){ # If there is only one value outside of the sequence
       if( b <= 2 | b == 5 | b == 6) is_Sequencia =  T} # If the "outside" values are the first or last ones
    
     if (length(a) == 2){ # If there is two values outside of the sequence
       if (all(c(1,2) %in% a) |  all(c(1,6) %in% a) | all(c(5,6) %in% a)  ) is_Sequencia = T} # If the "outside" values are the first and or last ones
    
     if (length(b) == 2){ # If there is two values outside of the sequence
       if (all(c(1,2) %in% b) |  all(c(1,6) %in% b) | all(c(5,6) %in% b)  ) is_Sequencia = T} # If the "outside" values are the first and or last ones
      
     if(length(a) == 0 | length(b) == 0){ is_Sequencia = T} # If there is no "outside" values
  
     return(is_Sequencia)}
    
    is_Sequencia = secuencia2(mao)
    
    ###################################
    
    ############## Flush ##############

    if(c(5) %in% tab_n | c(6) %in% tab_n | c(7) %in% tab_n) is_flush = T # If there is a suit that occurs 5, 6 or 7 times, then it's a Flush
    
    ###################################
    
    ############## Straight Flush ##############
    if (is_Sequencia && is_flush) { # If it's a Sequence and a Flush, then it can be a Straight Flush
      
      find_sequence <- function(my_vector) { # Function to retrieve the specific values of a straight
        start_index <- NA
        sequence_values <- NA
        
        for (i in 1:(length(my_vector) - 4)) { # Potential starting values for the sequence
          if (all(diff(my_vector[i:(i + 4)]) == 1)) { # Find the straight values
            start_index <- i
            sequence_values <- my_vector[start_index:(start_index + 4)]
            break
          }
        }
        
        return(sequence_values)
        }
      
      seq_ah <- find_sequence(valor_carta_i_ah) # Find the straight with High A
      seq_al <- find_sequence(valor_carta_i_al) # Find the straight with Low A
      
      if (!is.na(find_sequence(valor_carta_i_ah))[1] == TRUE) {seq_cards = seq_ah} else {seq_cards = seq_al} # Straight values
      
      
      if (all(c("K","A", "Q", "J", "10") %in% names(seq_cards))) return( "Royal flush" ) # If the Straight Flush contains these cards, then it's a Royal Flush
      if (max(table(mao[mao[, 1] %in% names(seq_cards), "naipe"])) >= 5) return( "Straight flush" ) # If the Straight values are all from the same suit, then it's a Straight Flush
        
         
    }
    
    if (is_flush) return( "Flush" )
    if (is_Sequencia) return( "Sequencia" )
   
    #############################################
    
    ############## Four-of-a-Kind ###############
    
      tab = sort( table(mao[,"valor_carta"]) ) # Frequency table for card values
    
      if (length(tab)==2) { # If there is only two values
        if (all(tab== c(3,4))) return("Quadra") # If there is a card value that happens 4 times, it's a Four-of-a-Kind
      }
      
      
      ###### Four-of-a-Kind or Full House ######
      
      if (length(tab)==3){ # If there is only three values
        if (all(tab == c(1,2,4))) return("Quadra") # If there is a card value that happens 4 times, it's a Four-of-a-Kind
        if (all(tab == c(2,2,3))) return( "Full house") # If there is a card value that happens 3 times, and other card values that make a pair, it's a Full House
        if (all(tab == c(1,3,3))) return( "Full house") # If there is a card value that happens 3 times, and other card values that make a pair, it's a Full House
      }
      
      
    ###### Four-of-a-Kind, Full House or 2 Pair #####
      
      if (length(tab)==4){ # If there is only four values
        if (all(tab == c(1,1,1,4))) return("Quadra") # If there is a card value that happens 4 times, it's a Four-of-a-Kind
        if (all(tab == c(1,1,2,3))) return( "Full house") # If there is a card value that happens 3 times, and other card values that make a pair, it's a Full House
        if (all(tab == c(1,2,2,2))) return( "Dois Pares") # If there is a card value that happens 2 times, and other card values that also happens two times, it's a Two Pair
      }
      
      ####### Three-of-a-Kind or 2 Pair #######
      
      if (length(tab)==5){ # If there is only 5 values
        if (all(tab == c(1,1,1,1,3))) return( "Trinca") # If there is a card value that happens 3 times, it's a Three-of-a-Kind
        if (all(tab == c(1,1,1,2,2))) return( "Dois Pares") # If there is a card value that happens 2 times, and other card values that also happens two times, it's a Two Pair
      }
      
      
      ############## Pair #############
      
      if (length(tab)==6){return("Par")} # If there is 6 different values in play, then it's a pair
      
    } 
  

  return( "Sem jogo" ) # High Card or no specific play
}

###############################################################

############### Simulation Function ###########################

simulate = function(N=10000, n = 5, X, Y, jogada = c("Sem jogo")) # Simulation function for poker plays. N is the number of rounds, 
                                                                  # n represents the number of cards to consider (5 for Flop, 6 for Turn and 7 for River),
                                                                  # X and Y are your hole cards, and "jogada" is the lowest hand rank you are interested in
{
  jogadas = c("Royal flush", "Straight flush", "Quadra", # Hand Ranks
              "Full house", "Flush", "Sequencia",
              "Trinca", "Dois Pares", "Par", "Sem jogo")
  
  jogadas = jogadas[1:match(jogada, jogadas)] # Filtering hand ranks according to the "jogada" argument
  
  
  res = matrix(rep(0,length(jogadas)),ncol=1) # Matrix for hand ranking counts
  rownames(res) = jogadas
  colnames(res) = "Flop Counts"
  
  matriz_jogos <- matrix(nrow = 7, ncol = 1)[, -1] # Matrix of poker rounds
  
  for(i in 1:N) { # Run N poker rounds
    jogo <- mao_aposta(X = X, Y = Y) # Draw cards for a poker round
    matriz_jogos <- cbind(matriz_jogos, jogo) # add the round cards into the respective matrix
    
  }
  
  pb = txtProgressBar(min = 0, max = N, style = 3) # Visual progress bar
  
  for (i in 1:length(matriz_jogos[1, ])) { # For every poker round
    mao = qual_mao(baralho[matriz_jogos[1:5 , i],]) # Classify the hand rank
    if (mao %in% jogadas) {res[mao,1] = res[mao,1]+1} # If the hand rank is relevant, then add it to the rank matrix
    setTxtProgressBar(pb, i) # Progress bar update
  }
  
  if(n == 5) {return(res)} # If only the Flop is relevant, then return the matrix
  
  res <- cbind(res, "Turn Counts" = 0)  # Add Turn's hand ranks count to the matrix
  
  for (i in 1:length(matriz_jogos[1, ])) { # For every poker round
    mao = qual_mao(baralho[matriz_jogos[1:6 , i],]) # Classify the hand rank
    if (mao %in% jogadas) {res[mao,2] = res[mao,2]+1} # If the hand rank is relevant, then add it to the rank matrix
    setTxtProgressBar(pb, i) # Progress bar update
  }
  
  if(n == 6) {return(res)} # If only the Turn is relevant, then return the matrix
  
  res <- cbind(res, "River Counts" = 0)   # Add River's hand ranks count to the matrix
  
  for (i in 1:length(matriz_jogos[1, ])) {  # For every poker round
    mao = qual_mao(baralho[matriz_jogos[1:7, i],]) # Classify the hand rank
    if (mao %in% jogadas) {res[mao,3] = res[mao,3]+1} # If the hand rank is relevant, then add it to the rank matrix
    setTxtProgressBar(pb, i) # Progress bar update
  }
  
  return(res) # Return the results
}

###############################################################