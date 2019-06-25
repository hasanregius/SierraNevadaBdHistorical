# Random Name Generator for Chores
ChoreTable = matrix(nrow=5, ncol=3)
ChoreTable[,1] = c("floors", "surfaces", "traps and droppings", 
                   "Dishes & Food Fridges", "Sample Room")
colnames(ChoreTable) = c("Chores", "Person 1", "Person 2")
names=data.frame(matrix(nrow=10))
names[,1]=c("Alan", "Hasan", "Alessandra", "Adrienne", "Rene",
          "Gordon", "Kurt", "Helen", "Vance", "Newt")
randomize=sample(names[,1], 10)
col1=randomize[1:5]
ChoreTable[,2]=col1
col2=randomize[6:10]
ChoreTable[,3]=col2