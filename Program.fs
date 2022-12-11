open System.IO
open System

let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList 

type Ref =
    | Old
//    | New
    | Num of int

type Op =
    | Plus
    | Mult
type Operation(op:Op,ref1:Ref,ref2:Ref) =
    member this.Exec (old:int) =
        match (op,ref1,ref2) with
        | Plus,Old,Old -> old+old
        | Plus,Old,Num n -> old+n
        | Plus,Num n,Old -> old+n
        | Plus,Num a,Num b -> a+b
        | Mult,Old,Old -> old*old
        | Mult,Old,Num n -> old*n
        | Mult,Num n,Old -> old*n
        | Mult,Num a,Num b -> a*b
    
    override this.ToString () = $"Operation({op} {ref1} {ref2})"

let (|Int|_|) (s:string) =
    match Int32.TryParse s with
    | true,value -> Some value
    | false,_ -> None
 
type Monkey(id:int,items:List<int>,op:Operation,divBy:int,trueMonkey:int,falseMonkey:int,inspections: int) =
    member this.Id = id
    member this.Items = items
    member this.Inspections = inspections
    
    member this.Round () : (int*(int list))*(int*(int list))*Monkey =
        let inspections = inspections + items.Length 
        let items = items |> List.map op.Exec
        let items = items |> List.map (fun n -> n / 3) // risk 1
        let trueItems = items |> List.filter (fun i -> i % divBy = 0)
        let falseItems = items |> List.filter (fun i -> i % divBy = 0 |> not)
        
        printfn $"{items}: true:{trueItems} false:{falseItems}"
        let monkey = Monkey(id,[],op,divBy,trueMonkey,falseMonkey,inspections)
        (trueMonkey,trueItems),(falseMonkey,falseItems),monkey
    member this.AddItems (more:int list) =
        Monkey(id,[items;more] |> List.concat,op,divBy,trueMonkey,falseMonkey,inspections)
        
    override this.ToString () = $"Monkey({id},{items},{op},{divBy},true->{trueMonkey},false->{falseMonkey},inspections={inspections})"

let toRef (ref:string) =
    match ref with
    | "old" -> Old
    | Int n -> Num n

let toOper (op:string) =
    match op with
    | "+" -> Plus
    | "*" -> Mult 
    

let parseMonkey (input : List<string>) =
    let id = ((input[0]).Split [|' ';':'|])[1] |> int
    let items = (((input[1]).Split [|':'|])[1]).Split [|','|] |> Array.toList |> List.map int
    let op = (((input[2]).Split [|'='|])[1]).Split ' ' |> Array.toList
    let op = Operation(op[2] |> toOper, op[1] |> toRef, op[3] |> toRef)
    let divBy = input[3].Split [|' '|][5] |> int
    let trueMonkey = input[4].Split [|' '|][9] |> int
    let falseMonkey = input[5].Split [|' '|][9] |> int
    Monkey(id,items,op,divBy,trueMonkey,falseMonkey,0)
    
let rec parse (input:List<string>) =
    match input with
    | [] -> []
    | ""::rest -> parse rest 
    | a::b::c::d::e::f::rest -> parseMonkey (a::b::c::d::e::f::[])::(parse rest)  

// "█"    
let monkeys = parse input
let monkeyMap = monkeys |> List.map (fun m -> m.Id,m) |> Map 

let round (monkeys:Map<int,Monkey>) =
    let keys = monkeys.Keys |> Seq.toList
    printfn $"keys={keys}"
    let rec exec (monkeys:Map<int,Monkey>) (mids:int list) : Map<int,Monkey> =
        match mids with
        | [] -> monkeys
        | i::rest ->
            let (monkey1,items1),(monkey2,items2),monkey = monkeys[i].Round ()
            let monkeys = monkeys.Add (monkey.Id,monkey) // monkey has cleared inventory
            let monkey1 = monkeys[monkey1].AddItems items1
            let monkey2 = monkeys[monkey2].AddItems items2
            let monkeys = monkeys.Add (monkey1.Id,monkey1)
            let monkeys = monkeys.Add (monkey2.Id,monkey2)
            exec monkeys rest 
    keys |> exec monkeys
    
let monkeyMap2 = round monkeyMap
printfn "AFTER: "
monkeyMap2.Values |> Seq.toList |> List.map (printfn "%A")

let rec execRounds (iter:int) (monkeys:Map<int,Monkey>) =
    printfn "ZZZ"
    if iter < 1 then monkeys
    else round monkeys |> execRounds (iter-1)
    
let afterMap = execRounds 20 monkeyMap
printfn $"afterMap = {afterMap}"
afterMap.Values |> Seq.toList |> List.map (printfn "%A")

let mostActive = afterMap.Values |> Seq.toList |> List.map (fun m -> m.Inspections) |> List.sort |> List.rev |> List.take 2

printfn $"{mostActive[0]*mostActive[1]}"