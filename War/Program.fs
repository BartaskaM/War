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

let Shuffle (org:_[]) = 
    let arr = Array.copy org
    let max = (arr.Length - 1)
    let randomSwap (arr:_[]) i =
        let pos = rng.Next(max)
        let tmp = arr.[pos]
        arr.[pos] <- arr.[i]
        arr.[i] <- tmp
        arr
   
    [|0..max|] |> Array.fold randomSwap arr

let generateCards=[|0..51|] |>Array.map generateCard|>Shuffle|>Shuffle|>Shuffle
let generateTrump=
    let trumpNr=rng.Next(3)
    match trumpNr with
              |0-> Spade
              |1-> Club
              |2-> Heart
              |_-> Diamond


let formPointStack (first:Card[]) (second:Card[]) (trump:CardSuit)=

    let firstPlayerWonCards=
        second 
        |>Array.zip first 
        |>Array.filter (fun (x, y) ->x.Suit=trump&&y.Suit<>trump||(x.Value>y.Value&&((x.Suit<>trump&&y.Suit<>trump)||(x.Suit=trump&&y.Suit=trump))))
        |>Array.collect (fun (x, y) -> [|x;y|])


    let firstPlayerDrawCards=
         second
         |> Array.zip first
         |> Array.filter (fun (x,y)->(x.Suit<>trump&&y.Suit<>trump||x.Suit=trump&&y.Suit=trump)&&x.Value=y.Value)
         |> Array.map (fun (x,y)-> x)

    Array.append firstPlayerWonCards firstPlayerDrawCards

let formPointStack2 (first:Card[]) (second:Card[]) (trump:CardSuit)=

        second 
        |>Array.zip first 
        |>Array.filter (fun (x, y) ->x.Suit=trump&&y.Suit<>trump||(x.Value>y.Value&&((x.Suit<>trump&&y.Suit<>trump)||(x.Suit=trump&&y.Suit=trump))))
        |>Array.collect (fun (x, y) -> [|x;y|])
        |>Array.append (second
         |> Array.zip first
         |> Array.filter (fun (x,y)->(x.Suit<>trump&&y.Suit<>trump||x.Suit=trump&&y.Suit=trump)&&x.Value=y.Value)
         |> Array.map (fun (x,y)-> x))
         


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let trump=generateTrump
    printfn "Trump suit: %A" trump
    let (first,second)=generateCards |> Array.splitAt 26
    printfn "First player pile: %A" first
    printfn "Second player pile: %A" second
    let firstPlayerPileAfterGame= formPointStack2 first second trump
    let secondPlayerPileAfterGame= formPointStack second first trump
    printfn "First player pile after the game: %A" firstPlayerPileAfterGame
    printfn "Second player pile after the game: %A" secondPlayerPileAfterGame
    printfn "Total score"
    printfn "P1: %d  P2:%d"firstPlayerPileAfterGame.Length secondPlayerPileAfterGame.Length
    System.Console.ReadKey() |> ignore
    
    0 // return an integer exit code

