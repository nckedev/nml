# NML

## type decl
```rust
mod List = 
    sig = 
        pub len = List a -> Uint

    ..
type RecordType = { a U32, b Str, c { x Int, y U8 } }
type RecordTypeGeneric a = { a a, b Str, c { x U8, y a } }
type RecordType2 = RecordType & { extrafield Str }
type RecordType3 = RecordType | { extrafield Str }

type EnumType = [
    // enum variant, zero sized 
    Variant1,
    // tuple variant
    Variant2 Str,
    // tuple variant
    Variant2 Str Int,
    // struct varinat
    Variant4 { a Int, b Int }
]

trait MyTrait a =
    fn sort = List a, fn a,a -> [Gt, Lt, Eq] -> List a

let a = 1
let b = { a 3, my_string "sdfsf" }
let c = (3, 3)
let d = c.0

fn my_func = { a Int, b Int -> Int => 
    a + b
}

fn my_f = a Int -> b Int -> Int => {
    a + b
}

let my_func2 = { 
    a, b : Int -> Int -> Int =>
        if a == 0 and b > 0 then
            a + b
        else if a > 0
            a
        else 
            b
}

fn MyModule.my_func4 = { self, b => 
}
fn sort a = { a List a, pred (x a , y b -> [GT, LT, EQ]) -> b List a => 
    let newList = []
    for x in a do 

}
let sorted = List.sort [1,2,3,5] { a, b => a > b }
let sorted = List.sort [1,2,3,5] { $0 > $1 }
let sorted = List.sort [1,2,3,5] { > }
let sorted = List.sort [1,2,3,5]  > 


test "my_func2 1 2 == 3"
    assert my_func2 1 2, 3
```

type Result v e = [Ok v, Err e]

// type alias 
type a = y

## function decl

my_func : int, int -> int
my_func = |a,b| 
    a + b

let my_func 0 b where b : Add = b 
let my_func a b = 
    a + b

div : Str ( () -> Html) -> Html
div = { class Str, cl () -> Html -> Html in 
    Html class { cl }
}

fn is_true = x : Bool -> Bool do
    x == true 
    
div class="btn" 
    span "test"

