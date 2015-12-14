module PlotExtension

open FSharp.Charting
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearRegression
open System

module Chart =
    let plotDataFrames df labels =  
        let rec getPairs =
            function
            | [] | [_] -> []
            | head::tail -> 
                [ for e in tail do yield head, e 
                  yield! getPairs tail ]
        let getNamedPairs data = 
            let names, data = data |> List.unzip
            let pairs = data |> getPairs
            let namePairs = names |> getPairs
            List.zip namePairs pairs
        let combineCharts (charts : ChartTypes.GenericChart list) =
            let list1, list2 = charts |> List.mapi (fun i x -> (i, x)) |> List.partition (fun (i, _) -> i % 2 = 0)
            [list1 |> Seq.map snd;list2 |> Seq.map snd] |> List.map (Chart.Columns) |> Chart.Rows
        getNamedPairs df
        |> List.map (fun ((n1, n2),(d1, d2)) -> 
            let name = n1 + " -- " + n2
            let data = Seq.zip d1 d2
            Chart.Point(data, name, name,XTitle=n1,YTitle=n2,Labels=labels,MarkerSize=10))
        |> combineCharts
    let plotLm (n:float,k:float) data =
        let min, max = (data  |> Seq.map fst |> Seq.min), (data |> Seq.map fst |> Seq.max)
        let line = [min;max] |> List.map (fun x -> x,(k*x + n)) |> Chart.Line
        [ data |> Chart.Point; line ] |> Chart.Combine
    let plotLmPure data =
        let fit = data |> SimpleRegression.Fit
        plotLm fit data
    let plotGroupByCount projection data =
        data
        |> Utility.Seq.groupByProj projection Seq.length
        |> Chart.Column
    let ShowChart (c : ChartTypes.GenericChart) = c.ShowChart()
    let combineCharts (charts : ChartTypes.GenericChart seq) =
        let list1, list2 = charts |> Seq.mapi (fun i x -> (i, x)) |> Seq.toList |> List.partition (fun (i, _) -> i % 2 = 0)
        [list1 |> Seq.map snd;list2 |> Seq.map snd] |> List.map (Chart.Columns) |> Chart.Rows
    let setName name (chart:ChartTypes.GenericChart) = chart.WithTitle(name)
    let plotPointsWithLabels (data:seq<string*(float*float)>) =
        let names,data = data |> Seq.toList |> List.unzip
        Chart.Point(data,Labels=names)
    /// <summary>
    /// Plot bubbles that reprsent time-event plot
    /// </summary>
    /// <param name="data">Seq of Seq of X, size, label</param>
    let plotBubbleTime (data : seq<seq<'X * 'S * string>>) =
        data 
        |> Seq.mapi (fun y d -> 
            let Xs, size, labels = d |> Seq.toList |> List.unzip3
            let Ys = Seq.init (Seq.length Xs) (fun _ -> y)
            (Xs,Ys,size) |||> Seq.zip3 |> fun d -> Chart.Bubble(d, Labels=labels,BubbleMaxSize=5,BubbleMinSize=1))
        |> Chart.Combine
