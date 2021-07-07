---
author: Programação Funcional
title: Estudos de caso
date: Prof. Rodrigo Ribeiro
---

Setup
=====

\begin{code}

module Aula08 where

import Data.Char (chr, ord, isLower)

main :: IO ()
main = return ()
\end{code}

-> Defina uma função para calcular o quadrado do dobro do seu argumento.

-> Defina uma função para calcular o dobro do quadrado do seu argumento.

-> Os lados de qualquer triângulo respeitam a seguinte restrição:
  A soma dos comprimentos de quaisquer dois lados de um triângulo é superior ao comprimento
  do terceiro lado.
  Escreva uma função que receba o comprimento de três segmentos de reta e resulte em um valor lógico
  indicando se satisfazem esta restrição.

-> Defina funções para calcular a compressão e a energia potencial armazenada em uma mola, dadas a
constante elástica da mola e a força usada para comprimi-la.

-> Sabe-se que o quilowatt de energia elétrica custa um quinto do salário mínimo. Defina uma função que
receba o valor do salário mínimo e a quantidade de quilowatts consumida por uma residência, e resulta
no valor a ser pago com desconto de 15%.

-> Defina uma função que receba a indutância L e a capacitância C, e resulta na frequência de ressonância
desse aparelho de rádio
Teste seu programa pelo cálculo da frequência do rádio quando L = 0;25mH e C = 0;10nF.


\begin{code}

squareDouble :: Int -> Int
squareDouble n = (n * 2) ^ 2

doubleSquare :: Int -> Int
doubleSquare n = (n ^ 2) * 2

isTriangle :: Double -> Double -> Double -> Bool
isTriangle a b c 
  | a + b <= c = False
  | b + c <= a = False
  | a + c <= b = False
  | otherwise = True
  
calcCompression :: Float -> Float -> Float
calcCompression k f = k * f

calcPotentialEnergy :: Float -> Float -> Float
calcPotentialEnergy k f = (k * f ^ 2) / 2

calcKW :: Float -> Float -> Float
calcKW salary kw = (kw * (salary / 5)) * 0.85

calcResonance :: Double -> Double -> Double
calcResonance l c = 1 / ((2 * pi) * (sqrt (l * c)))

\end{code}

-> Essa lei é formalizada pela seguinte equação:
F = G * (m1m2/d2)
onde:
 F é força de atração em Newtons (N),
 G é a constante de gravitação universal (6:67  10􀀀11 Nm2=kg2),
 m1 e m2 são as massas dos corpos envolvidos, em quilos (kg), e
 d é a distância entre os corpos em metros (m).

1. Defina uma variável para denotar a constante de gravitação universal.
2. Defina uma função que recebe as massas dos dois corpos e a distância entre eles, e resulta na força
de atração entre esses dois corpos. Use a variável definida em 1.
3. Teste suas definições no ambiente interativo calculando a força de atração entre a terra e a lua
sabendo que a massa da terra é 6  1024 kg, a massa da lua é 1  1023 kg, e a distância entre eles é
4  105 km.

-> Defina uma função que recebe o salário base de um funcionário e resulta no salário líquido a receber,
sabendo-se que o funcionário tem gratificação de 10% sobre o salário base e paga imposto de 7% sobre o
salário base.
Use uma anotação de tipo para a função.

\begin{code}

calcAtractionForce :: Double -> Double -> Double -> Double
calcAtractionForce m1 m2 d = force
  where
    {- 
      temos 3 tipos diferentes de exponenciacao
      (^), (^^) and (**). 
      ^ is non-negative integral exponentiation, 
      ^^ is integer exponentiation, 
      and ** is floating-point exponentiation:
    -}
    g = 6.67 * 10 ** (-11)
    force = g * ((m1 * m2)/d ^ 2)

calcLiquidSalary :: Float -> Float
calcLiquidSalary salary = ((salary * 1.1) - (salary * 0.07))

\end{code}

-> Defina uma função que verifica se uma equação do segundo grau
  ax2 + bx + c = 0
possui raízes reais. Para tanto é necessário que o discriminante d = b2 - 4ac seja não negativo.
Determine o tipo mais geral da função e use-o em uma anotação de tipo na sua definição.

\begin{code}

-- tenho que definir todas as classes que o numero pode assumir
-- mesmo uma sendo subclasse da outra
calcRoots :: (Num a, Ord a) => a -> a -> a -> Bool
calcRoots a b c = hasRoots
  where
    delta = (b^2) - (4*a*c)
    hasRoots = delta > 0

\end{code}

-> Defina uma função que recebe a medida do raio r de um círculo e resulta na área A do círculo, dada por:
A = pi x r2
Indique o tipo mais geral da função usando uma anotação de tipo.

-> Defina uma função que recebe a altura dos degraus de uma escada e a altura que o usuário deseja alcançar
subindo a escada, e resulta na quantidade mínima de degraus que ele deverá subir para atingir seu objetivo,
sem se preocupar com a altura do usuário.
Faça uma anotação do tipo mais geral da função.

\begin{code}

calcCircleArea :: (Floating a) => a -> a
calcCircleArea ray = ray ^ 2 * pi

calcSteps :: (Integral b, RealFrac a) => a -> a -> b
calcSteps number height = ceiling (height/number)

\end{code}

-> Defina uma função max3 que recebe três valores e resulta no maior deles. Use expressões condicionais
aninhadas.
Faça uma anotação de tipo para a função em seu código, usando o tipo mais geral da mesma.
Teste sua função no ambiente interativo.

\begin{code}
max3:: (Ord a) => a -> a -> a -> a
max3 a b c = if a>=b && a>=c
              then a 
              else if b>=a && b>=c
                then b
                else c
\end{code}

-> Redefina a função a seguir usando guardas no lugar de expressões condicionais.
describeLetter :: Char -> String
describeLetter c =
  if c >= 'a' && c <= 'z'
    then "Lower case"
    else if c >= 'A' && c <= 'Z'
      then "Upper case"
      else "Not an ASCII letter"

\begin{code}

-- descobrir qual tipo Char é
describeLetter :: Char -> String
describeLetter c 
  | c >= 'a' && c <= 'z' = "Lower case"
  | c >= 'A' && c <= 'Z' = "Upper case"
  | otherwise = "Not an ASCII letter"

\end{code}

-> Defina uma função para calcular a área de um triângulo de lados a, b e c usando as esquações apresentadas.
Caso a , b e c não possam ser lados de um triângulo a função deve resultar em zero.
Dica Faça definições locais para os valores cos alpha, sin alpha e h.

\begin{code}

calcTriangleArea :: Double -> Double -> Double -> Double
calcTriangleArea a b c = area
  where 
    cosAlpha = (b^2 + c^2 - a^2) /(2 * b * c)
    sinAlpha = sqrt (1 - cosAlpha ^ 2)
    h = b * sinAlpha
    area  
      | isTriangle a b c = (c * h)/b 
      | otherwise = 0
    
\end{code}