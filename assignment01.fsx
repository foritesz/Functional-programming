open System

//1.Given a list of integers, return a new list with each element squared. (1 point)
let lnum=[1..10]
List.map(fun i ->i*i) lnum
//2.Write a function that filters out all odd numbers from a given list of integers, returning a list of only the even numbers. (1 point)
let odd = List.filter(fun i-> i % 2 = 0) lnum
//3.Write a function that takes a list of numbers and returns the sum of all positive numbers in the list. (1 point)
let lnumnegative = [-1;1;4;-2;3]
let negative = List.filter (fun i-> i > 0) lnumnegative
let sum_positive=List.sum negative
//4.Given a list of names (strings), return a new list with each name capitalized. (1 point)
let stringforcap=["adam";"sandor";"szabolcs";"bence"]
let capitalized (s:string list) :string list=
   s |> List.map(fun s -> s.ToUpper())
let upper= stringforcap |>capitalized
//5.Write a function that takes a list of strings and an integer n, returning a list of strings where each string has a length greater than n. (1 point)
let n=4
let stringlength (lst: string list) (n: int) : string list =
    lst |> List.filter (fun str -> String.length str > n)
let filteredList = stringlength stringforcap n

//6.Write a function that takes a list of integers and an divisor, and returns the count of numbers which are divisible by it. (2 points)

let listdiv = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let divisor = 3

let divisible (lst: int list) (divisor: int) : int =
    lst |> List.filter (fun x -> x % divisor = 0) |> List.length

let resultdiv = divisible listdiv divisor

// 7.Create a function that takes a list and an element, and returns all the indices of the list where this element can be found. (2 points)

let index = [1; 2; 3; 4; 5; 2; 6; 2; 7; 8; 2]
let Indices (lst: 'a list) (element: 'a) : int list =
    lst |> List.mapi (fun i x -> if x = element then Some i else None) |> List.choose id

let indicesOf2 = Indices index 2
//8.Given a list of strings, write a function that concatenates only those strings that are longer than a given length n. (2 points)

let longstrings (lst: string list) (n: int) : string =
    lst |> List.filter (fun str -> String.length str > n) |> List.reduce (+)

//9.Assuming a list of tuples where each tuple contains an (id, value), write a function to find the tuple with the maximum value. (2 points)

let samplelist = [(1, 10); (2, 20); (3, 15); (4, 25)]
let findMaxTuple (lst: (int * int) list) : (int * int) =
    lst |> List.maxBy (fun value -> value)
let maxTuple = findMaxTuple samplelist

//10.Given a list of elements that could repeat, write a function that returns a list of tuples, each containing an element from the input list and the number of times it appears. (2 points)

let tupelslist (lst: 'a list) : ('a * int) list =
    lst |> List.groupBy id |> List.map (fun (key, values) -> (key, List.length values))


//11.Define a DU for a traffic light (Red, Yellow, Green). Write a function that takes a traffic light state and returns the next state. (2 points)
type TrafficLight =
    | Red
    | Yellow
    | Green

let nextTrafficLight (current: TrafficLight) : TrafficLight =
    match current with
    | Red -> Green
    | Yellow -> Red
    | Green -> Yellow
let testNextTrafficLight () =
    printfn "Current state: Red, Next state: %A" (nextTrafficLight Red)
    printfn "Current state: Yellow, Next state: %A" (nextTrafficLight Yellow)
    printfn "Current state: Green, Next state: %A" (nextTrafficLight Green)
//12.Create a DU for basic arithmetic operations (Add, Subtract, Multiply, Divide). Implement a function that takes two numbers and an operation, then performs the operation on the numbers. (2 points)

type ArithmeticOperation =
    | Add
    | Subtract
    | Multiply
    | Divide

let calculate (op: ArithmeticOperation) (x: float) (y: float) : float =
    match op with
    | Add -> x + y
    | Subtract -> x - y
    | Multiply -> x * y
    | Divide -> x / y

let testAdd= calculate Add 3.3 4.5
let testSub= calculate Subtract 3.3 4.5
let testMulty= calculate Multiply 3.3 4.5
let testDiv= calculate Divide 3.3 4.5

// 13.Define a DU for different shapes (e.g., Circle, Rectangle, Square, etc.) Write a function that calculates the area of the given shape. (2 points)

type Shape =
    | Circle of float // radius
    | Rectangle of float * float // length, width
    | Square of float // side length

let calculateshape (shape: Shape) : float =
    match shape with
    | Circle(radius) -> Math.PI * radius * radius
    | Rectangle(length, width) -> length * width
    | Square(side) -> side * side

let circle = Circle 5.0
let circletest = calculateshape circle
let rectangle = Rectangle (5.0, 5.7)
let rectangletest = calculateshape rectangle
let square = Square 5.0
let squaretest = calculateshape square
//14.Define a DU for temperature scales (Celsius, Fahrenheit). Write a function that converts temperatures between the scales. (1 points)

type TemperatureScale =
    | Celsius
    | Fahrenheit

let converttemp (value: float) (fromScale: TemperatureScale) (toScale: TemperatureScale) : float =
    match fromScale, toScale with
    | Celsius, Fahrenheit -> (value * 9.0 / 5.0) + 32.0
    | Fahrenheit, Celsius -> (value - 32.0) * 5.0 / 9.0

let temp1=converttemp 73.2 Celsius Fahrenheit
let temp2=converttemp 73.2 Fahrenheit Celsius
//15.Create a DU to represent a simplified JSON value. Include cases for JsonObject, JsonArray, JsonString, JsonNumber, and JsonBoolean. Write a function that takes such a JSON value and pretty-prints it to a string. (3 points)

type JsonValue =
    | JsonObject of (string * JsonValue) list
    | JsonArray of JsonValue list
    | JsonString of string
    | JsonNumber of float
    | JsonBoolean of bool

let rec Json (json: JsonValue) : string =
    match json with
    | JsonObject properties ->
        "{ " +
        String.Join(", ", properties |> List.map (fun (key, value) -> sprintf "\"%s\": %s" key (Json value))) +
        " }"
    | JsonArray elements ->
        "[ " +
        String.Join(", ", elements |> List.map Json) +
        " ]"
    | JsonString str -> sprintf "\"%s\"" str
    | JsonNumber num -> sprintf "%f" num
    | JsonBoolean b -> string b

let jsonObject = JsonObject [("name", JsonString "John"); ("age", JsonNumber 30.0); ("isStudent", JsonBoolean false)]
let jsonArray = JsonArray [JsonString "apple"; JsonString "banana"; JsonString "cherry"]
let jsonString = JsonString "Hello, world!"
let jsonNumber = JsonNumber 42.0
let jsonBoolean = JsonBoolean true
//16.Write a recursive function to compute the nth Fibonacci number. (3 points)
let rec fib n =
    match n with
    | 0 | 1 -> n
    | n -> fib (n-1) + fib (n-2)

let testFib () =
    // Test with various values of n
    for n = 0 to 10 do
         printfn "Fibonacci number for n = %d: %d" n (fib n)

//17.Implement a recursive binary search algorithm that searches for a given element within a sorted array. (3 points)
let arr = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
let target = 7


let rec binarySearch (arr: int array) (target: int) (low: int) (high: int) : int option =
    if low > high then None
    else
        let mid = (low + high) / 2
        if arr.[mid] = target then Some mid
        elif arr.[mid] < target then binarySearch arr target (mid + 1) high
        else binarySearch arr target low (mid - 1)

let resultIndex = binarySearch arr target 0 (Array.length arr - 1)
printfn "Index of %d in the array: %A" target resultIndex
//18.Write a recursive function to sort a list of integers using the merge sort algorithm. (3 points)
let lst = [5; 3; 8; 2; 9; 1; 7; 4; 6]
let rec mergeSort (lst: int list) : int list =
    let rec merge (left: int list) (right: int list) : int list =
        match left, right with
        | [], _ -> right
        | _, [] -> left
        | x::xs, y::ys ->
            if x < y then x :: merge xs (y::ys)
            else y :: merge (x::xs) ys
    let len = List.length lst
    if len <= 1 then lst
    else
        let left = List.take (len / 2) lst
        let right = List.skip (len / 2) lst
        merge (mergeSort left) (mergeSort right)
let sortedList = mergeSort lst
printfn "Sorted list: %A" sortedList
//19.Given a binary tree (as a nested structure of nodes), write a recursive function to compute the depth of the tree. (3 points)
let arr1 = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]
let target1 = 7
type BinaryTree<'a> =
    | Empty
    | Node of 'a * BinaryTree<'a> * BinaryTree<'a>

let rec treeDepth<'a> (tree: BinaryTree<'a>) : int =
    match tree with
    | Empty -> 0
    | Node(_, left, right) -> 1 + max (treeDepth left) (treeDepth right)

let resultree = binarySearch arr target 0 (Array.length arr - 1)
printfn "Index of %d in the array: %A" target resultIndex

//20.Create a recursive function to check whether a given string is a palindrome (reads the same backward as forward). (3 points)
let palidrome (string:string) =
  let rec exam i =
    if i > string.Length / 2 then true
    else string.[i] = string.[string.Length-1-i] && exam (i + 1)
  exam 0

let result = palidrome "abba"
printfn "Is 'abba' a palidrome? %b" result

//2022 assignment


// 1.Write a function that determines whether a number is even or odd. Example: IsEven 12 -> true. (1 point)

let isEven (num:int) = num % 2 = 0
printfn "%b" (isEven 12)

//2.Filter a list of numbers to find all even ones. Example: FindEvenNumbers [1 .. 10] -> [2; 4; 6; 8; 10]. (1 point)

let findEvenNumbers (numbers:int list) = List.filter isEven numbers

printfn "%A" (findEvenNumbers [1 .. 10])

//3.Calculate the sum of the first 100 even numbers. (1 point)
let FindEvenNumbers n =
    let evenNumbers = [2 .. 2*n] |> List.filter isEven
    List.sum evenNumbers
printfn "%d" (FindEvenNumbers 100)
// 4.Find the difference between the sum of the squares of all even and odd numbers between 1 and 100. (2 points)
let sumsOfsquares n =
    let evenSum = [2 .. n] |> List.filter isEven |> List.sumBy (fun x -> x * x)
    let oddSum = [1 .. n] |> List.filter (fun x -> not (isEven x)) |> List.sumBy (fun x -> x * x)

    evenSum - oddSum
printfn "%d" (sumsOfsquares 100)

//5.Write a function that computes every other character in a string
let hello (str:string) =
    str |> Seq.mapi (fun i c -> if i % 2 = 0 then string c else "") |> Seq.filter (fun s -> s <> "")

printfn "%A" (hello "Hello")

// Use the Sieve of Erathostenes to find all prime numbers from 2 to a given number.
let Primes n =
    let sieve = Array.create (n+1) true
    sieve.[0] <- false
    sieve.[1] <- false
    let rec markMultiples prime multiplier =
        let next = prime * multiplier
        if next <= n then
            sieve.[next] <- false
            markMultiples prime (multiplier + 1)
    for i = 2 to int(sqrt(double(n))) do
        if sieve.[i] then markMultiples i 2
    [2 .. n] |> List.filter (fun i -> sieve.[i])

printfn "%A" (Primes 10)


// 7.Compute the sum of the differences between consequtive prime numbers smaller than a given number.
let sumPrimeDifferencesUpTo n =
    let primes = Primes n
    List.pairwise primes |> List.sumBy (fun (a, b) -> b - a)
printfn "%d" (sumPrimeDifferencesUpTo 1000)

// 8.Write a function to find the nth prime number.
let findNthPrime n =
    let rec findprimes count num =
        if count = 0 then []
        else if prime num then num :: findprimes (count - 1) (num + 1)
        else findprimes count (num + 1)
    and prime num =
        let limit = int(sqrt(double(num)))
        let rec primehelper i =
            if i > limit then true
            else if num % i = 0 then false
            else primehelper (i + 1)
        primehelper 2
    findprimes n 2
printfn "%A" (findNthPrime 50)



// Given the following type to represent people and a list of such people as (people: Person list):
type Person =
    {
        FirstName: string
        LastName: string
        Age: int
    }


let people : Person list = [
    { FirstName = "John"; LastName = "Dave"; Age = 36 }
    { FirstName = "Jane"; LastName = "Smith"; Age = 49 }
    { FirstName = "Alice"; LastName = "Johnson"; Age = 55 }
]

// 9.Find the sum of all people's ages. (1 point)

let sumOfAges (people: Person list) = people |> List.sumBy (fun person -> person.Age)
printfn "Sum of ages: %d" (sumOfAges people)

// 10.Find all people above a given age
let findPeopleAbove (age: int) (people: Person list) = people |> List.filter (fun person -> person.Age > age)
printfn "People above age 50: %A" (findPeopleAbove 50 people)

// 11.Compute the average age in a given list of people
let findAverageAge (people: Person list) =
    let totalAge = float (sumOfAges people)
    let count = float (List.length people)
    totalAge / count
printfn "Average age: %.1f" (findAverageAge people)

// 12.Compute a new people list with first and last names interchanged
let swapname (people: Person list) =
    people
    |> List.map (fun person -> { person with FirstName = person.LastName; LastName = person.FirstName })
printfn "People with swapped first and last names: %A" (swapname people)

// 13.Implement a "marriage name change" algorithm that takes a list of (old last name, new last name) pairs and returns a new people list.
let applyMarriage (nameChanges: (string * string) list) (people: Person list) =
    let lastNameMap = Map.ofList nameChanges
    people
    |> List.map (fun person ->
        match Map.tryFind person.LastName lastNameMap with
        | Some newLastName -> { person with LastName = newLastName }
        | None -> person)

printfn "People after marriage name change: %A" (applyMarriage [("Smith", "Johnson"); ("Johnson", "Jones")] people)


// 12.Extend the original Person type with a gender and enhance ApplyMarriage to only rename females. (2 points)
type PersonGender =
    {
        FirstName: string
        LastName: string
        Age: int
        Gender: string
    }


let ApplyMarriage (nameChanges: (string * string) list) (people: PersonGender list) =
    let lastNameMap = Map.ofList nameChanges
    people
    |> List.map (fun person ->
        if person.Gender = "Female" then
            match Map.tryFind person.LastName lastNameMap with
            | Some newLastName -> { person with LastName = newLastName }
            | None -> person
        else
            person)
printfn "People after marriage name change with gender: %A" (ApplyMarriage [("Smith", "Johnson"); ("Johnson", "Jones")] [{ FirstName = "John"; LastName = "Doe"; Age = 30; Gender = "Male" }; { FirstName = "Jane"; LastName = "Smith"; Age = 45; Gender = "Female" }; { FirstName = "Alice"; LastName = "Johnson"; Age = 55; Gender = "Female" }])


// Test


