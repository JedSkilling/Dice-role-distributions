# Since a 0 is present on the distribution vector, the index of a probability is
# one above the corresponding value
dNfunc <- function(N, x, y){
  if(x < y & x - y > -(N+1)){
    return(1/N)
  }
  else{
    return(0)
  }
}

applydN <- function(inVec, N, maxVal){
  outVec <- numeric(maxVal)
  for(i in 1:length(inVec)){
    for(j in 1:maxVal){
      outVec[j] <- outVec[j] + dNfunc(N, i, j)*inVec[i]
    }
  }
  return(outVec)
}

##############################


# Single d10 rolled
diceToRole <-  integer(10)
diceToRole[10] <- 1

# Single d20 rolled + d6
diceToRole <-  integer(20)
diceToRole[20] <- 1
diceToRole[6] <- 1

# d8 + d4 + 3
diceToRole <-  integer(8)
diceToRole[8] <- 1
diceToRole[4] <- 1
diceToRole[1] <- 1

# 2d6
diceToRole <-  integer(14)
diceToRole[6] <- 2

# Cannot do negative dice modifiers
# Length of diceToRole is the max dice size
# This would ideally have a nice text interface for picking dice but I didn't put the time in

adjustedDiceToRole = diceToRole * 1:length(diceToRole)
maxVal = 1 + sum(adjustedDiceToRole)

distribution <- numeric(maxVal)
distribution[1] <- 1
length(diceToRole)

for(i in 1:length(diceToRole)){
  #print(i)
  if(diceToRole[i] > 0){
    for(j in 1:diceToRole[i]){
      #print(distribution)
      #sprintf("i: %s \n j: %s", i, j)
      distribution <- applydN(distribution, i, maxVal)
    }
  }
}
print(distribution)
for(i in 1:length(distribution)){
  if(distribution[i] != 0){
    #print("Hi")
    actualOutput = i - 1
    roundedProb = round(distribution[i],3)
    print(sprintf("Chance of getting a %s is %s", actualOutput, roundedProb))
  }
}
print("Chance of anything else is zero")

