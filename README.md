# Kompilatory

## Przykłady składni języka "Rad"

## Komentarze

```
// To jest komentarz jednoliniowy
```

```
/*
To jest komentarz
wieloliniowy
*/
```


### Deklaracja zmiennej

```
var variable_name = 3
var text = "Hello, World!"
```

### Deklaracja funkcji

```
def function_name x = { x - 1 }
```

### Wywołanie funkcji

```
def times_two x = { x * 2 } 

times_two(5)
```

Out:
```
>> 10
```

### Deklaracja dopasowywania wzorców

```
map value to {
    Null -> { 0 }
    some_var_name -> { 6 }
}
```

### deklaracja stuktury danych (tabeli)

```
var list = [0, 1, 2, 3]

var empty_list = []
```

### Indexing

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

### Złożenie

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

### Definicja struktury danych

```
data List = { Null, Cons Int List }
```
