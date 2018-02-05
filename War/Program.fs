// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


type CardSuit=
|Spade
|Club
|Heart
|Diamond

type Card={Suit: CardSuit;Value: int}
let generateCard x= 
    let suit=
     match x%4 with
     |0-> Spade
     |1-> Club
     |2-> Heart
     |_-> Diamond
           
    let card = {Suit=suit;Value=x/4%13+2}
    card

let rng = new System.Random()

let swap (a: _[]) x y =
    let tmp = a.[x]
    a.[x] <- a.[y]
    a.[y] <- tmp

let shuffle a =
    Array.iteri (fun i _ -> swap a i (rng.Next(i, Array.length a))) a
    a


let generateCards=[|0..51|] |>Array.map generateCard|>shuffle|>Array.toList
let generateTrump=
    let trumpNr=rng.Next(3)
    match trumpNr with
              |0-> Spade
              |1-> Club
              |2-> Heart
              |_-> Diamond


let compareByValue (first: Card) (second: Card) (trump: CardSuit)=
        if (first.Suit=trump&&second.Suit=trump||second.Suit<>trump) then
            if(first.Value>second.Value) then
                true
                    else
                false
        else
            false 

let formPointStack (first:Card list) (second:Card list) (trump:CardSuit)=

        second 
        |>List.zip first 
        |>List.filter (fun (x, y) ->x.Suit=trump&&y.Suit<>trump||compareByValue x y trump)
        |>List.collect (fun (x, y) -> [x;y])
        |>List.append (second
         |> List.zip first
         |> List.filter (fun (x,y)->(x.Suit<>trump&&y.Suit<>trump||x.Suit=trump&&y.Suit=trump)&&x.Value=y.Value)
         |> List.map (fun (x,y)-> x))
         


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let trump=generateTrump
    printfn "Trump suit: %A" trump
    let (first,second)=generateCards |> List.splitAt 26
    printfn "First player pile: %A" first
    printfn "Second player pile: %A" second
    let firstPlayerPileAfterGame= formPointStack first second trump
    let secondPlayerPileAfterGame= formPointStack second first trump
    printfn "First player pile after the game: %A" firstPlayerPileAfterGame
    printfn "Second player pile after the game: %A" secondPlayerPileAfterGame
    printfn "Total score"
    printfn "P1: %d  P2:%d"firstPlayerPileAfterGame.Length secondPlayerPileAfterGame.Length
    System.Console.ReadKey() |> ignore
    
    0 // return an integer exit code

