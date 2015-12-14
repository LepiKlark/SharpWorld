#load @"..\packages\FsLab.0.3.11\FsLab.fsx"
open FSharp.Data
open FSharp.Charting
#load "Utility.fs"
#load "PlotExtension.fs"
open Utility

// Basic examples.
// Interactivity.
// Higher order functions and immutability.
//

let x = 2.
let y = x * x * x
let z = x ** 3.

y = z

// Functions.
//
let addOne x = x + 1
addOne 42

// Easy, now lists.
//
[1;2;3]
[1..10]
[1 .. 2 .. 10]

// Higher order functins.
//
let applyFive f = f 5
applyFive addOne

List.map addOne [1;2;3]

List.map addOne [1 .. 10]
List.sum [1;2;3]

// Pipe.
//
2 |> (~-)
[1;2;3] |> List.map addOne
[1;2;3] |> List.sum

let isEven x = x % 2 = 0
[1..10] |> List.filter isEven

// Anonymous function
//
2 |> fun x -> x + 1
[1..10] |> List.filter (fun x -> x % 2 = 0)

// Tuple
//
let t = (1,2,3)
let _,t2,_ = t

// f'(x) = lim (h->0) (f(a + h) - f(a)) / h)
//
let derivative f =
    let f' x = (f(x + 1E-10) - f(x)) / 1E-10
    f'

// Ok this is way easier to show with plotting.
//
open System

[1;2] |> Chart.Column

[1..10] |> Chart.Column

[-10. .. 0.1 .. 10.] |> List.map (Math.Sin) |> Chart.Line

let sin' = derivative Math.Sin

[
    [0. .. 0.01 .. (Math.PI * 6.)] |> List.map (sin') |> fun data -> Chart.Line(data, Name = "sin`")
    [0. .. 0.01 .. (Math.PI * 6.)] |> List.map (Math.Sin) |> fun data -> Chart.Line(data, Name = "sin")
    //[0. .. 0.01 .. (Math.PI * 6.)] |> List.map (Math.Cos) |> fun data -> Chart.Line(data, Name = "cos")
] |> Chart.Combine

// We know how to plot, what to do with data collections, how to write our own functions...
// ... World is our.
//

// Crimes
//

[<Literal>]
let fPath = @"C:\Users\atomic\Desktop\rec-crime-1898-2002.csv"
type Crimes = CsvProvider<fPath, Separators=",", IgnoreErrors=true, HasHeaders=true>
type CrimeYear = Crimes.Row

let crimeYear = Crimes.Load(fPath).Rows |> Seq.toArray

crimeYear |> Array.map (fun y -> y.Year, y.Blackmail)
|> Chart.Line |> Chart.WithTitle("Blackmail")

// Peaks during WWs
//
open Deedle
let ww2Frame = Frame.ReadCsv(fPath, hasHeaders=true) |> Frame.indexRowsInt "Year"

let ratioBetweenWWMeanAndTotalMean (series : Series<int, float>)=
    let totalMean = series |> Stats.mean
    let wwMean = Series.merge series.[1914..1918] series.[1939..1945] |> Stats.mean
    wwMean / totalMean

ww2Frame
|> Frame.getNumericCols
|> Series.mapValues ratioBetweenWWMeanAndTotalMean
|> Series.sort

[
    crimeYear 
    |> Array.map (fun y -> y.Year, y.Bigamy)
    |> fun d -> Chart.Line (d, Name = "Bigamy")
    crimeYear 
    |> Array.map (fun y -> y.Year, y.``Procuring                   illegal                 abortion``)
    |> fun d -> Chart.Line (d, Name = "illegal abortion")
] |> Chart.Combine |> Chart.WithLegend()

// Titanic
// 
[<Literal>]
let filePath = @"C:\users\atomic\desktop\train.csv"
type Titanic = CsvProvider<filePath, Separators=",", IgnoreErrors=true, HasHeaders=true>
type Passenger = Titanic.Row

let passengers = Titanic.Load(filePath).Rows |> Seq.toArray

let females = passengers |> Seq.where (fun p -> p.Sex = "female")
let femaleSurvivors = females |> Seq.where (fun p -> p.Survived)
let femaleSurvivorsPercentage = (float <| Seq.length femaleSurvivors) / (float <| Seq.length females)
let survived (p : Passenger) = p.Survived 

#load "DecisionTree.fs"
open DecisionTree
open Utility

let survivalRate criteria  passengers = 
    passengers |> Array.groupBy criteria 
    |> Array.map (fun (key,matching) -> 
        key, matching |> Array.percentage survived
    )

passengers |> survivalRate (fun p -> p.Age < 18.) |> Chart.Pie |> Chart.WithLegend()
passengers |> survivalRate (fun p -> p.Sex)
passengers |> survivalRate (fun p -> p.Sex)
passengers |> survivalRate (fun p -> p.Pclass, p.Sex)

passengers |> Array.where (fun p -> p.Age < 8.) |> survivalRate (fun p -> p.Sex, p.Pclass)

passengers |> Array.where (fun p -> p.Age < 8.) |> Array.countBy (fun p -> p.Pclass, p.Sex)

let labels = [|"sex"; "class"|]

let features (p:Passenger) : obj[] = 
    [|p.Sex; p.Pclass|]

let dataSet : obj[][] =
    [|for p in passengers ->
        [|yield! features p; 
          yield box (p.Survived = true)|] |]

let tree = DecisionTree.createTree(dataSet, labels)

// World Data Bank
//

let wb = WorldBankData.GetDataContext()
wb.Countries.Serbia.CapitalCity

query {
    for c in wb.Regions.``European Union``.Countries do
    select (c.Name, (c.Indicators.``Adolescent fertility rate (births per 1,000 women ages 15-19)``.[2010]))
}
|> Chart.Column |> Chart.WithTitle ("Births per 1k women ages 15-19")

let sample = query {
        for c in wb.Regions.``European Union``.Countries do
        where (c.Indicators.``Population, total``.[2000] > 10000000.)
        select c }

let countryNames = sample |> Seq.map (fun c -> c.Name)

let dataWB =
    [
        "Fertility_rate_per_1k", [ for c in sample -> c.Indicators.``Adolescent fertility rate (births per 1,000 women ages 15-19)``.[2000] ]
        "Youth_employment_%",    [ for c in sample -> c.Indicators.``Population ages 65 and above (% of total)``.[2000] ]
        "GDP_per_capita",        [ for c in sample -> c.Indicators.``GDP per capita growth (annual %)``.[2000] ]
        "Female_employers_%",    [ for c in sample -> c.Indicators.``Employers, female (% of employment)``.[2000] ]
    ]

(dataWB,countryNames) ||> PlotExtension.Chart.plotDataFrames

wb.Countries.Italy.Indicators.``Employers, female (% of employment)``
|> Chart.Line