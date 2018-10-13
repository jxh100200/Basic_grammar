x <- 7
y <- 5
x+y
#식으로 표현하기
cat(x,"+",y,"=",x+y)
plus <- function(x,y){
  return(cat(x,"+",y,"=",x+y))
}
plus(3,8)
minus <- function(x,y){
  return(cat(x,"-",y,"=",x-y))
}
minus(11,5)
multi <- function(x,y){
  return(cat(x,"*",y,"=",x*y))
}

div.interger <- function(x,y){
  return(cat(x,"/",y,"=",x/y))
}
div.float <- function(x,y){
  return(cat(x,"%/%",y,"=",x%/%y))
 }
  
div.remainder <- function(x,y){
  return(cat(x,"%%",y,"=",x%%y))
}

##계산기
x <- 3
op <- "+"
y <- -2

if(op=="+")plus(x,y)else
if(op=="-")minus(x,y)else
if(op=="*")multi(x,y)else
if(op=="/")div.float(x,y)else
if(op=="%/%")div.interger(x,y)else
if(op=="%%")div.remainder(x,y)else
print("error")

calc <- function(x,op,y){
  if(op=="+")plus(x,y)else
  if(op=="-")minus(x,y)else
  if(op=="*")multi(x,y)else
  if(op=="/")div.float(x,y)else
  if(op=="%/%")div.interger(x,y)else
  if(op=="%%")div.remainder(x,y)else
  print("error")
}

calc2 <- function(x,op,y){
  switch(
   op,
    "+"=plus(x,y)else
    "-"=minus(x,y)else
    "*"=multi(x,y)else
    "/"=div.float(x,y)else
    "%/%"=div.interger(x,y)else
    "%%"=div.remainder(x,y)else
    print("error")
  )
}

