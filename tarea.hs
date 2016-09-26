esMulti::Int->Int->Int
esMulti a 1 = a
esMulti a b = a + esMulti a (b-1)
esDiv::Int->Int->Int
esDiv a b
    | a==b = 1
    | a<b  = 0
    | a>b  = 1+esDiv (a-b) b
-------------------------------------------------------
dife::Float->Float
dife x = last[x-y**2 | y<-[1..100], y**2<=x]
funcion::Float->Float
funcion c = last[k | k<-[1..100], k**2<=c]
raiz::Float->Float
raiz n 
   | n==0 = 0 
   | n>0 = (dife n)/(2*(funcion n)) + funcion n
   | otherwise = error "facto: argumento negativo"
-------------------------------------------------------
esDiv::Int->Int->Int
esDiv a b
    | a==b = 1
    | a<b  = 0
    | a>b  = 1+esDiv (a-b) b 
esPotencia::Float->Int->Float
esPotencia a 1 = a
esPotencia a 0 = 1
esPotencia a b
    | b>0 = a*esPotencia a (b-1)
    | b<0 = (1/a)*esPotencia a (b+1)
pot2::Int->Int
pot2 0 = 1
pot2 1 = 2
pot2 n
   | (mod n 2) == 0 = 4*fun (par-1)
   | (mod n 2) /= 0 = 2*4*fun (impar-1)
   where
       par=esDiv n 2
       impar=esDiv (n-1) 2
       fun 0=1
       fun 1=4
       fun m =4*fun (m-1)
