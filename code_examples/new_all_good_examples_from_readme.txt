// To jest komentarz jednoliniowy



/*
To jest komentarz
wieloliniowy
*/



def function_name x = { x - 1 }



def times_two x = { x * 2 }

def main = { times_two 5 }



def f x = { x + x }
def g x = { x * x }

def h x = { f (g x) }

def main = { h 2 }



data List = { Integer , List }
data List = { Integer, List }


def magic_number = { 
    7
}



def main = { length my_list }


def my_list = { 
    List 1 (
    List 2 (
    List 3 Null ))
}



def length l = {
    map l to {
        Null -> { 0 } 
        List x y -> { 1 + length y }
    }
}



def wrapper_function = {
    map some_data_type to {
        Null -> { 0 }
        Datatype field_one -> { field_one }
    }
}