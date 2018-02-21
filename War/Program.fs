// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

//type Rank =
//        /// Represents the rank of cards 2 .. 10
//        | Value of int 
//        | Ace
//        | King
//        | Queen
//        | Jack

type Rank = 
    Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    member this.ToInt() =
        match this with
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5
        | Six -> 6
        | Seven -> 7
        | Eight -> 8
        | Nine -> 9
        | Ten -> 10
        | Jack -> 11
        | Queen -> 12
        | King -> 13
        | Ace -> 14

type CardSuit=
|Spade
|Club
|Heart
|Diamond

type Card={Suit:CardSuit;Value:Rank}
    
let generateCard x= 
    let suit=
     match x%4 with
     |0-> Spade
     |1-> Club
     |2-> Heart
     |_-> Diamond
     
    let value=
        match (x/4%13) with
        |0->Two
        |1->Three
        |2->Four
        |3->Five
        |4->Six
        |5->Seven
        |6->Eight
        |7->Nine
        |8->Ten
        |9->Jack
        |10->Queen
        |11->King
        |_->Ace
        
         
    let card = {Suit=suit;Value=value}
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


let isFirstCardValueGreater (first: Card) (second: Card) (trump: CardSuit)=
        if (first.Suit = trump && second.Suit = trump || second.Suit <> trump) then
            if(first.Value.ToInt()>second.Value.ToInt()) then
              true
             else
              false
        else
         false 

let areBothTrump (first: Card) (second: Card) (trump: CardSuit)=
    if(first.Suit = trump && second.Suit = trump) then
     true
    else
     false

let areBothNotTrump (first: Card) (second: Card) (trump: CardSuit)=
    if(first.Suit <> trump && second.Suit <> trump) then
     true
    else
     false

let isFirstCardWinningByTrump (first: Card) (second: Card) (trump: CardSuit)=
    if(first.Suit = trump && second.Suit <> trump) then
     true
    else
     false

let areCardValuesEqual (first: Card) (second: Card)=
    if(first.Value.ToInt() = second.Value.ToInt()) then
     true
    else
     false
let formRoundOutcome (first:Card) (second:Card) (trump:CardSuit)=
    if(isFirstCardWinningByTrump first second trump || isFirstCardValueGreater first second trump) 
                            then [first;second],[]
                            elif((areBothNotTrump first second trump || areBothTrump first second trump) && areCardValuesEqual first second) 
                            then [first],[second]
                            else
                            [],[first;second]

let formPointStack (first:Card list) (second:Card list) (trump:CardSuit)=
        let (firstPlayerPile,secondPlayerPile)=second
                                                |>List.zip first
                                                |>List.map(fun (x,y)->formRoundOutcome x y trump)
                                                |>List.unzip                                         

        (firstPlayerPile|>List.concat),(secondPlayerPile|>List.concat)
        
         

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let trump=generateTrump
    printfn "Trump suit: %A" trump
    let (first,second)=generateCards |> List.splitAt 26
    printfn "First player pile: %A" first
    printfn "Second player pile: %A" second
    let (firstPlayerPileAfterGame,secondPlayerPileAfterGame)= formPointStack first second trump
    printfn "First player pile after the game: %A" firstPlayerPileAfterGame
    printfn "Second player pile after the game: %A" secondPlayerPileAfterGame
    printfn "Total score"
    printfn "P1: %d  P2:%d"firstPlayerPileAfterGame.Length secondPlayerPileAfterGame.Length
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code

