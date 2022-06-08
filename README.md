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


### 2. Deklaracja zmiennej

```
var variable_name = 3
var text = "Hello, World!"
```

### 3. Deklaracja funkcji

```
def function_name x = { x - 1 }
```

### 4. Wywołanie funkcji

```
def times_two x = { x * 2 } 

times_two(5)
```

Out:
```
>> 10
```

### 5. Deklaracja dopasowywania wzorców

```
map value to {
    Null -> { 0 }
    some_var_name -> { 6 }
}
```

### 6. deklaracja tabeli

```
var list = [0, 1, 2, 3]

var empty_list = []
```

### 7. Indexing

```
list @ 2
list @ 4
list @ variable_name
```

Out:
```
>> 2
>> Null
>> 3
```

### 8. Złożenie

```
def f x = { x + x }
def g x = { x * x }

var h = f . g
def h = { g . g }


h(2)
```

Out:
```
>> 8
```

### 9. Definicja struktury danych

```
data List = { Null, Cons Int List }
```
