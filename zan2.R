# обратный порядок элементов
a<-seq(-8,10,.5)
a
x=a[length(a):1]
x
# add number 99 in the middle of the vector
y=c(a[1:round(length(a)/2)],99,a[(round(length(a)/2)+1):length(a)])
y
# proof that arcos(cos(x))=x
x=0:3
y=acos(cos(x))
x==y
x=c(sin(10),log(5,8),log(3,exp(1)),log(38.6,10),cos(pi+1), exp(1)^(cos(0)),atan(+Inf))
names(x)=c("sin(10)","log(5,8)","ln3","lg(38.6)","cos(pi+1)","e^cos(0)","arctn(+Inf)");
sort(x)
# logical operations !-not, | = or, & = and
x=seq(-10,10,.01)
y=x[(x>-10 & x< -3) | (x>0 & x<6 & x!=3)]
# Факторы и функция summary() x = factor(“Mouse”)
f=c(rep("mouse",3),rep("rat",2),rep("fly",11))
f=rep(c("mouse","rat","fly"),c(3,2,11))
x=factor(f)
summary(x)
summary(f)
summary(1:10)
summary(1:10)[4]
# Задание посчитайте долю каждого нуклеотида в векторе содержащем последовательность ДНК
dna=factor(rep(c("T","G","C","A"),c(16,38,6,10)))
ratio=summary(dna)/sum(summary(dna))
x=factor(dna)
summary(x)
summary(dna)
summary(1:10)
summary(1:10)[4]
# Функция sample() Создает подвыборку из выборки, в том числе путем перемешивания
dna=factor(rep(c("T","G","C","A"),c(16,38,6,10)))
sample(dna)
dna
sample(dna,size=84, replace=TRUE)
sampledna=sample(c("A","T","G","C"), size=10000,replace=TRUE)
ratio=summary(factor(sampledna))/length(sampledna)
ratio2=summary(factor(dna))/length(dna)
ratio
ratio2
# Функции union(x,y),intersect(x,y),setdiff(x,y)
x=1:10
y=6:14
union(x,y)
intersect(x,y)
setdiff(x,y)
setdiff(y,x)
# Уничтожение всех переменных в памяти
rm(list=ls())
# Функция cumsum()
cumsum(1:10)
# Задание. Создайте длинные последовательности с заданными пропорциями элементов
dna2=sample(c("A","T","G","G","G","C"), size=3826513,replace=TRUE)
ratio=summary(factor(dna2))/length(dna2)
ratio
dna2=sample(c("A","T","G","C"), size=3826513,prob=c(.5/3,.5/3,.5,.5/3),replace=TRUE)
ratio=summary(factor(dna2))/length(dna2)
ratio
## Data frames -Таблицы
iris[iris$Species==c("setosa","virginica"),]
iris[iris$Species=="setosa" | iris$Species=="virginica",c("Petal.Length","Species")]
# Виды у которых Petal.Length>2 но <3, вывести виды и количество попадений в условия
summary(factor(iris[(iris$Petal.Length>2 & iris$Petal.Length<6),"Species" ]))
# Создание функций - Functioning
my_func = function(p) 
{
  x=p^2
  y=x^3
  print("result")
  return(c(x,y))
  
}
my_func(2)
mathses = function(p,q)
{
  result = c(p+q,log(p+q,10),exp(p+q))
  names(result) = c("summ", "log","exp")
  return(result)
}
mathses(2,3)
# Проверки условий
mf = function(p)
{
  
  if (p>0)
  {
    x=log(p)
    return(x)
  }
  else
  {
    return(NULL)      
  }
}
mf(-1)
# gamma(x) gives !(x-1), we need to make function which makes true factorial
myfactorial = function(p) 
{
  if (p>0 & p%%1==0)
  {return(gamma(p+1))}
  else
  {return(NULL)}
}

myfactorial(5)
myfactorial = function(p) 
{
  if (p>0 & p%%1==0)
  {return(gamma(p+1))}
  else
  {return(NULL)}
}

mydistribution = function(x,p)
{
  if ( x>=0 & x%%1==0 & p<=1 & p>0 )
  {
    return(p*(1-p)^x)        
  }
  else
  {
    return(0)
  }
  
}
mydistribution(3,.4)