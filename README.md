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
./a.out
```

## Przykłady składni języka "Rad"

### 1. Komentarze

```
// To jest komentarz jednoliniowy
```

```
/*
To jest komentarz
wieloliniowy
*/
```

### 2. Deklaracja zmiennej w postaci stałej funkcji

```
def magic_number = { 7 }
```

### 3. Deklaracja funkcji

```
def function_name x = { x - 1 }
```

### 4. Wywołanie funkcji

```
def times_two x = { x * 2 }

def main = { times_two 5 }
```

Out:
```
>> 10
```

### 5. Deklaracja dopasowywania wzorców

```
map value to {
    Null -> { 0 }
    Const x xs -> { x }
}
```

### 6. Złożenie

```
def f x = { x + x }
def g x = { x * x }

def h x = { f g x }

def main = { h 2 }
```

Out:
```
>> 8
```

### 7. Definicja struktury danych

```
data List = { Null, Cons Int List }
```

## Przykład poprawnego programu

```
data List = { Null, Cons Int List }

def length l = {
    map l to {
        Null -> { 0 }
        Cons x xs -> { 1 + length xs }
    }
}
```
