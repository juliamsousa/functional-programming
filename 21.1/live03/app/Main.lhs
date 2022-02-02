---
author: Programação Funcional
title: Live de dúvidas e resolução de exercícios.
date: Prof. Rodrigo Ribeiro
---

> module Main where

> import Data.Char
> import ParseLib hiding (failure, success)
> import Validate
> import System.Environment (getArgs)


Validação como parsing
----------------------

-- idade de um cliente

> type Age = Int

-- cpf

> type CPF = String

-- nome

> type Name = String


-- definição de um cliente

> data Client
>   = Client Name CPF Age
>     deriving (Eq, Ord, Show)

-- Regras de negócio

1. Nome deve ser formado apenas por espaços ou letras
   e o tamanho mínimo é de 3 caracteres.

2. O CPF deve ser uma string formada por 11 dígitos

3. Idade deve ser um número natural.

Vamos utilizar as funções da biblioteca de parsing
para garantir que essas regras de negócio sejam
atendidas.

runParser single "abc" = [('a', "bc")]

> single :: Parser Char Char
> single = sat valid
>  where
>     valid c = isSpace c || isLetter c

> nameParser :: Parser Char Name
> nameParser
>    = (++) <$> three <*> greedy single
>      where
>        three = repeatParser 3 single


> repeatParser :: Int -> Parser s a -> Parser s [a]
> repeatParser n p
>     | n <= 0 = pure []
>     | otherwise = (:) <$> p <*> repeatParser (n - 1) p

> cpfParser :: Parser Char CPF
> cpfParser = repeatParser 11 digitChar

> ageParser :: Parser Char Age
> ageParser = natural

> clientParser :: Parser Char Client
> clientParser
>   = f <$> nameParser <*> whitespace <*>
>           cpfParser  <*> whitespace <*>
>           ageParser
>     where
>       f n _ c _ a = Client n c a

> test :: String
> test = "Diogenes 12345678901 80"


> main :: IO ()
> main = undefined
