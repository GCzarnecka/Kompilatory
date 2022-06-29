# Kompilatory

## Przygotowanie środowiska

Zainstaluj kompilator GNU g++ (lub inny odpowiedni kompilarot języka C/C++)

```sh
sudo apt-get install g++
```

Zainstaluj narzędzia: Bison v>=3.0.4 (generator parserów) i Flex (skaner)

```sh
sudo apt-get install bison
sudo apt-get install flex
```

## Uruchomienie projektu

Będąc w katologu głównym repozytorium przejdź do folderu "prototype/":

```sh
cd ./prototype/
```

Nadaj uprawnienia do wywołania skryptowi kompilującemu i czyszczącemu projekt.

```sh
chmod +x ./compile.sh
chmod +x ./clean.sh
```

Wykonaj skrypt kompilujący.

```sh
./compile.sh
```

Uruchom parser i wprowadź dane.

```sh
./a.out ..//code_examples//good_1.txt ..//haskell_out_files//good_1.hs
```

## Przykłady składni języka "Rad" i odpowiedniki kodu w języku Haskell

### 1. Komentarze

```
// To jest komentarz jednoliniowy
```

Haskell:

```haskell
-- Single line comments start with two dashes.
```

```
/*
To jest komentarz
wieloliniowy
*/
```

Haskell:

```haskell
{- Multiline comments can be enclosed
in a block like this.
-}
```

### 2. Deklaracja zmiennej w postaci stałej funkcji

```
def magic_number = { 7 }
```

Haskell:

```haskell
magic_number = 7 
```

### 3. Deklaracja funkcji

```
def function_name x = { x - 1 }
```

Haskell:

```haskell
function_name x = x - 1   
```

### 4. Wywołanie funkcji

```
def times_two x = { Maybe x * 2 }

def main = { times_two 5 }
```

Haskell:

```
times_two x = x * 2

times_two 5
```

### 5. Deklaracja dopasowywania wzorców

```
map some_data_type to {
    Null -> { 0 }
    Datatype field -> { field }
}
```

Haskell:

```
case some_data_type of
    (Nothing) -> 0
    (Datatype value) -> value
```

### 6. Złożenie

```
def f x = { x + x }
def g x = { x * x }

def h x = { f (g x) }

def main = { h 2 }
```

Haskell:

```haskell
f x = x + x
g x = x * x

h x = f (g x)

h 2
```

Out:
```
>> 8
```

### 7. Definicja struktury danych

```
data List = { Integer, List }
```

Haskell:

```haskell
data List = List { x :: Maybe Integer, next_node :: Maybe List }
```

## Przykład poprawnego programu

```
data List = { Integer, List }

def length l = {
    map l to {
        Null -> { 0 } 
        List x y -> { 1 + length y }
    }
}

def my_list = { 
    List { 1, 
    List { 2, 
    List { 3, Null }}}
}

def main = { length my_list }
```

Haskell:

```haskell
module Main where

main :: IO ()

data List = List { x :: Maybe Integer, next_node :: Maybe List }

my_length l =
    case l of
        (Nothing) -> 0
        (Just (List x y)) -> 1 + my_length y

my_list =
    Just List { 
        x = 10, next_node = Just List { 
        x = 200, next_node = Just List { 
        x = 3000, next_node = Nothing }}
}

main = print ( my_length my_list )
```

---
---
