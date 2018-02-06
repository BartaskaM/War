// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


type CardSuit=
|Spade
|Club
|Heart
|Diamond


type Card(suit:CardSuit,value:int)=
    do
        if value>12 then
            invalidArg "value" "Invalid card value"
    
    member this.Suit=suit
    member this.Value=value
    override this.ToString()= sprintf "{Suit: %A; Value: %i;}" this.Suit this.Value

    
let generateCard x= 
    let suit=
     match x%4 with
     |0-> Spade
     |1-> Club
     |2-> Heart
     |_-> Diamond
           
    let card = Card(suit,x/4%13)
    card

let rng = new System.Random()

let generateCards=[0..51] |>List.map generateCard|>List.sortBy(fun _->rng.Next())
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
        |>List.choose(fun (x,y) ->
          if(x.Suit=trump&&y.Suit<>trump||compareByValue x y trump) 
          then Some [x;y]
          elif((x.Suit<>trump&&y.Suit<>trump||x.Suit=trump&&y.Suit=trump)&&x.Value=y.Value) 
          then Some [x] 
          else
            None
        )
        |>List.concat
        
         


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

