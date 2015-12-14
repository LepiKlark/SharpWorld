namespace Utility
open System.Linq
open System

module Seq =
    let groupByProj projection stats data =
        data 
        |> Seq.groupBy projection
        |> Seq.map (fun (g,d) -> g, stats d)

module Standard =
    open System.IO
        
    let (=~) input pattern =
        System.Text.RegularExpressions.Regex.IsMatch(input, pattern)

    let (==~) input pattern =
        let matches = System.Text.RegularExpressions.Regex.Match(input, pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
        [ for g in matches.Groups -> g.Value ]

    let memoize f = 
        let cache = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        fun x ->
          let ok, res = cache.TryGetValue(x)
          if ok then res
          else let res = f x
               cache.[x] <- res
               res

    let fileread f = File.ReadAllText(f)
    let filewrite f s = File.WriteAllText(f, s)
    let filereadlines f = File.ReadAllLines(f)
    let filewritelines f ar = File.WriteAllLines(f, ar)

    let safereadall f = 
        use fs = new FileStream(f, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use sr = new StreamReader(fs, System.Text.Encoding.Default)
        sr.ReadToEnd()
    let split sep (s:string) = System.Text.RegularExpressions.Regex.Split(s, sep)

    let roundUp (round : TimeSpan) (date : DateTime) = new DateTime (((date.Ticks + round.Ticks - 1L) / round.Ticks) * round.Ticks)

module StringUtil =
    let fContains (pattern:string) (x:string) = x.ToLower().Contains(pattern.ToLower())
    let fNotContains pattern str= not <| fContains pattern str
    let patternMatch (pattern:string) (x:string) =
        System.Text.RegularExpressions.Regex.IsMatch(x, pattern)
    let filterStrings funcs strs =
        strs 
        |> Seq.filter (fun s -> funcs |> Seq.fold (fun acc f -> acc && f s) true)
        |> Seq.toList

module Csv =
    open System.IO
    open System

    type Array =
        static member join delimeter xs =
            xs
            |> Array.map (fun x -> x.ToString())
            |> String.concat delimeter

    type Seq =
        static member write (path:string) (data:seq<seq<'a>>): unit =
            data
            |> Seq.map (fun row -> row |> Seq.map (fun v -> v.ToString()) |> String.concat ",")
            |> Seq.toArray
            |> Standard.filewritelines path
        static member write_to_temp (data:seq<seq<'a>>) =
            let f' = System.IO.Path.GetTempPath()
            let f = Path.Combine(f', Guid.NewGuid().ToString() + ".csv")            
            Seq.write f data
            f

[<AutoOpen>]
module Array =
    /// Tally up items that match specified criteria
    let tally criteria items = 
        items |> Array.filter criteria |> Seq.length
    /// Percentage of items that match specified criteria
    let percentage criteria items =
        let total = items |> Seq.length
        let count = items |> tally criteria
        float count * 100.0 / float total
    /// Where = filter
    let where f xs = Array.filter f xs
    /// F# interactive friendly groupBy
    let groupBy f xs =
        xs 
        |> Seq.groupBy f |> Seq.toArray 
        |> Array.map (fun (k,vs) -> k, vs |> Seq.toArray)
