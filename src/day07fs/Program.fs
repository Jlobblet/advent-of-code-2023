open type System.Environment
open System.IO
open System

let inline uncurry f (a, b) = f a b

type Card =
    | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King | Ace
    static member Parse = function
        | '2' -> Two
        | '3' -> Three
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine
        | 'T' -> Ten
        | 'J' -> Jack
        | 'Q' -> Queen
        | 'K' -> King
        | 'A' -> Ace
        | _ -> failwith "Unknown card"

type JokerCard =
    | Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Queen | King | Ace
    static member ofCard = function
        | Card.Two -> Two
        | Card.Three -> Three
        | Card.Four -> Four
        | Card.Five -> Five
        | Card.Six -> Six
        | Card.Seven -> Seven
        | Card.Eight -> Eight
        | Card.Nine -> Nine
        | Card.Ten -> Ten
        | Card.Jack -> Joker
        | Card.Queen -> Queen
        | Card.King -> King
        | Card.Ace -> Ace

type HandType =
    | HighCard
    | OnePair
    | TwoPair
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind
    with
        static member ofCounts = function
            | 5 :: _ -> FiveOfAKind
            | 4 :: _ -> FourOfAKind
            | 3 :: 2 :: _ -> FullHouse
            | 3 :: _ -> ThreeOfAKind
            | 2 :: 2 :: _ -> TwoPair
            | 2 :: _ -> OnePair
            | _ -> HighCard

        static member ofCards (cards: Card list) =
            cards
            |> List.countBy id
            |> List.map snd
            |> List.sortDescending
            |> HandType.ofCounts

        static member ofJokerCards (cards: JokerCard list) =
            let counts = 
                cards
                |> List.countBy id

            let jokers =
                counts
                |> List.tryFind (fst >> ((=) Joker))
                |> Option.map snd
                |> Option.defaultValue 0

            counts
            |> List.filter (fst >> ((<>) Joker))
            |> List.map snd
            |> List.sortDescending
            |> function
                | h :: t -> h + jokers :: t
                | [] -> [jokers]
            |> HandType.ofCounts


[<CustomEquality; CustomComparison>]
type Hand =
    { Cards: IComparable list
      Type: HandType
      Bid: int64 }

    interface IEquatable<Hand> with
        member this.Equals other = this.Cards = other.Cards

    override this.Equals (other: obj) =
        match other with
        | :? Hand as h -> (this :> IEquatable<_>).Equals h
        | _ -> false

    override this.GetHashCode() = this.Cards.GetHashCode()

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Hand as h -> (this :> IComparable<_>).CompareTo h
            | _ -> invalidArg "other" "Cannot compare to non-Hand"

    interface IComparable<Hand> with
        member this.CompareTo other =
            match compare this.Type other.Type with
            | 0 ->
                (this.Cards, other.Cards)
                ||> Seq.zip 
                |> Seq.filter (uncurry (<>))
                |> Seq.map (uncurry compare)
                |> Seq.tryHead
                |> Option.defaultValue 0
            | i -> i

let parseLine cp ht (line: string) =
    let parts = line.Split(' ')
    let cards =
        parts[0]
        |> Seq.map cp
        |> Seq.toList

    let handType = ht cards

    let bid = int64 parts[1]

    { Cards = cards |> List.map (fun c -> c :> IComparable)
      Type = handType
      Bid = bid }

let part pl =
    Array.map pl
    >> Array.sort
    >> Array.mapi (fun i h -> int64 (i + 1) * h.Bid)
    >> Array.sum
    >> printfn "%d"

let raw = GetEnvironmentVariable("INPUT") |> File.ReadAllLines

part (parseLine Card.Parse HandType.ofCards) raw
part (parseLine (Card.Parse >> JokerCard.ofCard) HandType.ofJokerCards) raw
