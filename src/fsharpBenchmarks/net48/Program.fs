open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type FibonacciBenchmark() =

    [<Params(30)>]
    member val N = 0 with get, set

    [<Benchmark>]
    member this.NaiveFibonacci() =
        let rec fib n =
            if n <= 1 then n
            else fib (n - 1) + fib (n - 2)
        fib this.N

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<FibonacciBenchmark>() |> ignore
    0