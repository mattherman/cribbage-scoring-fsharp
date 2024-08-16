open System
open Microsoft.FSharp.Reflection

type Suit =
    | Spades
    | Clubs
    | Hearts
    | Diamonds

    override this.ToString() =
        match this with
        | Spades -> "\u2660"
        | Clubs -> "\u2663"
        | Diamonds -> "\u2666"
        | Hearts -> "\u2665"

type Rank =
    | Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King

    override this.ToString() =
        match this with
        | Ace -> "A"
        | Two -> "2"
        | Three -> "3"
        | Four -> "4"
        | Five -> "5"
        | Six -> "6"
        | Seven -> "7"
        | Eight -> "8"
        | Nine -> "9"
        | Ten -> "10"
        | Jack -> "J"
        | Queen -> "Q"
        | King -> "K"

type Card = Rank * Suit

let rankValue card =
    match card with
    | (Ace, _) -> 1
    | (Two, _) -> 2
    | (Three, _) -> 3
    | (Four, _) -> 4
    | (Five, _) -> 5
    | (Six, _) -> 6
    | (Seven, _) -> 7
    | (Eight, _) -> 8
    | (Nine, _) -> 9
    | _ -> 10

let rankOrdering (card: Card) =
    match card with
    | (Ace, _) -> 1
    | (Two, _) -> 2
    | (Three, _) -> 3
    | (Four, _) -> 4
    | (Five, _) -> 5
    | (Six, _) -> 6
    | (Seven, _) -> 7
    | (Eight, _) -> 8
    | (Nine, _) -> 9
    | (Ten, _) -> 10
    | (Jack, _) -> 11
    | (Queen, _) -> 12
    | (King, _) -> 13

let createDeck () : seq<Card> =
    let suits =
        FSharpType.GetUnionCases(typeof<Suit>)
        |> Array.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> Suit)

    let ranks =
        FSharpType.GetUnionCases(typeof<Rank>)
        |> Array.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> Rank)

    seq {
        for suit in suits do
            for rank in ranks do
                yield (rank, suit)
    }

let cardsToString (cards: seq<Card>) =
    cards
    |> Seq.toList
    |> List.sortBy rankOrdering
    |> List.map (fun (rank, suit) -> $"{rank}{suit}")
    |> String.concat " "

type Hand = { Starter: Card; Rest: Card[] }

type ScoreType =
    | Fifteen
    | Pair
    | Flush
    | RunOf of int
    | HisNobs
    | HisHeels

type ScoringCombination =
    { Cards: Card[]
      Points: int
      Type: ScoreType }

let rank (card: Card) = fst card
let suit (card: Card) = snd card

let scorePairs hand =
    let cards = Array.concat [ [| hand.Starter |]; hand.Rest ]
    let uniqueGroupings = Array.groupBy rank cards

    seq {
        for (_, cards) in uniqueGroupings do
            for i in 0 .. (cards.Length - 1) do
                for j in (i + 1) .. (cards.Length - 1) do
                    yield
                        { Cards = [| cards[i]; cards[j] |]
                          Points = 2
                          Type = Pair }
    }
    |> Seq.toList

let scoreFlush hand =
    let candidateSuit = suit hand.Rest[0]
    let handMatches = Array.TrueForAll(hand.Rest, (fun c -> (suit c) = candidateSuit))

    match handMatches with
    | true when (suit hand.Starter) = candidateSuit ->
        [ { Cards = Array.concat [ [| hand.Starter |]; hand.Rest ]
            Points = 5
            Type = Flush } ]
    | true ->
        [ { Cards = hand.Rest
            Points = 4
            Type = Flush } ]
    | _ -> []

let scoreHisNobs hand =
    let jackOfStarterSuit =
        hand.Rest
        |> Array.tryFind (fun c -> (rank c) = Jack && (suit c) = (suit hand.Starter))

    match jackOfStarterSuit with
    | Some c ->
        [ { Cards = [| c |]
            Points = 1
            Type = HisNobs } ]
    | _ -> []


let sequentialRanks first second =
    (rankOrdering second) - (rankOrdering first) = 1

let sequentialCards (cards: Card list) =
    cards
    |> List.sortBy rankOrdering
    |> List.pairwise
    |> List.exists (fun (first, second) -> not (sequentialRanks first second))
    |> not

(*
  [
    [(Ace); (Two); (Three)]; -> 1 1 1
    [(Ace); (Two)];          -> 0 1 1
    [(Ace); (Three)];        -> 1 0 1
    [(Ace)];                 -> 0 0 1
    [(Two); (Three)];        -> 1 1 0
    [(Two)];                 -> 0 1 0
    [(Three)];               -> 1 0 0
    []
  ]
*)
let rec generateCombos cards =
    match cards with
    | card :: rest -> // 1 :: [2; 3]
        let remainingCombos = generateCombos rest // [ [2; 3]; [2]; [3]; [] ]
        let withCard = List.map (fun combo -> card :: combo) remainingCombos // [ [1; 2; 3]; [1; 2]; [1; 3]; [1] ]
        withCard @ remainingCombos // [ [1; 2; 3]; [1; 2]; [1; 3]; [1]; [2; 3]; [2]; [3]; [] ]
    | [] -> [ [] ]

let scoreFifteens hand =
    let cards = Array.concat [ [| hand.Starter |]; hand.Rest ] |> Array.toList

    generateCombos cards
    |> List.choose (fun combo ->
        let total = combo |> List.sumBy rankValue

        if total = 15 then
            Some
                { Cards = combo |> List.toArray
                  Points = 2
                  Type = Fifteen }
        else
            None)

let scoreRuns hand =
    let cards = Array.concat [ [| hand.Starter |]; hand.Rest ] |> Array.toList

    generateCombos cards
    |> List.where (fun combo -> combo.Length >= 3)
    |> List.where sequentialCards
    |> List.map (fun combo ->
        { Cards = combo |> List.toArray
          Points = combo.Length
          Type = RunOf(combo.Length) })

let scoreHand hand =
    seq {
        yield! (scoreFifteens hand)
        yield! (scorePairs hand)
        yield! (scoreRuns hand)
        yield! (scoreFlush hand)
        yield! (scoreHisNobs hand)
    }
    |> Seq.toList

let printScore (combos: ScoringCombination list) =
    let comboText =
        combos
        |> List.map (fun combo -> $"{combo.Cards |> cardsToString} -> {combo.Points} ({combo.Type})")
        |> String.concat "\n"

    let total = combos |> List.sumBy (fun scoringCombo -> scoringCombo.Points)
    Console.WriteLine(comboText)
    Console.WriteLine($"Total: {total}")

{ Starter = (Three, Spades)
  Rest = [| (Jack, Spades); (Five, Clubs); (Ace, Diamonds); (Two, Hearts) |] }
|> scoreHand
|> printScore
