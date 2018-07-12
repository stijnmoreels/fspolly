//namespace FsPolly

open System
open System.Threading
open System.Threading.Tasks

exception CountException of int

module Policy =

    let private onError f r =
        match r with
        | Error _ -> f ()
        | v -> v

    let handleWith<'ex when 'ex :> exn> (predicate : 'ex -> bool) f x =
        try f x with | :? 'ex as ex when predicate ex -> 
            Result.Error (ex :> exn)

    let handle<'ex when 'ex :> exn> f x =
        handleWith<'ex> (fun _ -> true) f x

    let handleAll f x = handle<exn> f x

    let retry n retrier timer f =
        if n <= 0 
        then invalidArg "n" "Retry count should be greater then zero"
        List.replicate (n - 1) retrier
        |> List.indexed
        |> List.map (fun (i, retrier) -> retrier (timer (i)))
        |> List.reduce (>>)
        |> fun g -> g f

    let retry1 f x =
        match f x with
        | Error _ -> printfn "retry"; f x
        | v -> v

    let internal retryNWith n retrier f =
        if n <= 0 
        then invalidArg "n" "Retry count should be greater then zero"
        List.replicate (n - 1) retrier
        |> List.reduce (>>)
        |> fun g -> g f

    let retryN n f = retryNWith n retry1 f
    let retry3 f = retryN 3 f
    let retry5 f = retryN 5 f

    let timeout time f x =
        if time < TimeSpan.Zero 
        then invalidArg "time" "Time-out representation should be greater then 0:00:00"
        use cts = new CancellationTokenSource ()
        try
            cts.CancelAfter (delay=time)
            Async.RunSynchronously (
                async { return f x }, 
                cancellationToken=cts.Token)
        with
        | _ when cts.IsCancellationRequested ->
            sprintf "Timed-out after: %A" time
            |> TimeoutException :> exn
            |> Result.Error
        | ex -> Result.Error ex

    let fallback orElse f x =
        match f x with
        | Error err -> Ok (orElse err)
        | v -> v

    let wait (time : TimeSpan) f x =
        Thread.Sleep (int time.TotalMilliseconds)
        printfn "wait %A" time
        f x

    let circuitBreaker amount backoff f x =
        let rec waitBefRetry () =
            wait backoff f x
            |> onError waitBefRetry
        
        retryN amount f x
        |> onError waitBefRetry

    let linBackoff1 time f x =
        match f x with
        | Error _ -> 
            printfn "retry"
            let r = wait time f x
            r
        | v -> v            

    let linBackoffN n (time : TimeSpan) f x =
        retryNWith n (linBackoff1 time) f x

    let expBackoffN n f =
        if n <= 0 
        then invalidArg "n" "Retry count should be greater then zero"
        
        let timer = pown 2 >> float >> TimeSpan.FromSeconds 
        let times = [ 1..(n - 1)] |> List.map (fun i -> timer i)
        List.iter (printfn "time: %A") times

        List.replicate (n - 1) (fun t -> linBackoff1 t)
        |> List.zip times
        |> List.map (fun (time, retrier) -> retrier time)
        |> List.reduce (>>)
        |> fun g -> g f

    let test : ((float * (obj -> obj)) list) = List.replicate 5 id |> List.indexed |> List.map (fun (i, f) -> 
        let r = pown 2 i |> float
        r,f)

    let run x f = f x

module APolicy =

    type AResult<'b> = Async<Result<'b, exn>>

    let handleWith<'ex when 'ex :> exn> predicate f x = async {
        try return! f x
        with :? 'ex as ex when predicate ex ->
            return Result.Error (ex :> exn) }

    let handle<'ex when 'ex :> exn> f x =
        handleWith<'ex> (fun _ -> true) f x

    let retry1 f x = async {
        let! r = f x
        match r with 
        | Error _ -> printfn "retry"; return! f x 
        | v -> return v }

    let retryN n f =
        Policy.retryNWith n retry1 f

    let timeout (time : TimeSpan) f x = async {
        let! child = Async.StartChild (f x, int time.TotalMilliseconds)
        try return! child
        with :? TimeoutException as ex -> return Result.Error (ex :> exn) }

    let wait (time : TimeSpan) f x = async {
        do! Async.Sleep (int time.TotalMilliseconds)
        printfn "wait: %A" time
        return! f x }

    // let linBackoff n (time : TimeSpan) f x =
    //     retryN n (retry1 >> wait time) f x
        

    let fallback orElse f x = async {
        let! r = f x 
        match r with
        | Error err -> return (Ok <| orElse err)
        | v -> return v }

    let run x f = f x

module Program =
    let count = ref -1

    let sut (xs : (unit -> int) list) _ =
        incr count
        printfn "run %A" !count
        xs.[!count]() |> Result.Ok

    let sutAsync (xs : (unit -> int) list) x = async {
        return sut xs x }

    let main =
            sut [
                // Initial
                fun () -> raise <| DivideByZeroException()
                // fun () -> Thread.Sleep 3000; 2

                // Retries
                fun () -> raise <| DivideByZeroException()
                fun () -> raise <| DivideByZeroException()
                fun () -> raise <| DivideByZeroException()
                fun () -> raise <| DivideByZeroException()
                fun () -> 1
            ]
            // Async
            //|> APolicy.handle<DivideByZeroException>
            //|> APolicy.timeout (TimeSpan.FromSeconds 1.)
            //|> APolicy.run 0
            //|> Async.RunSynchronously
            
            // Sync
            |> Policy.handleAll
            // |> Policy.linBackoffN 3 (TimeSpan.FromSeconds 1.)
            |> Policy.expBackoffN 5
            // |> Policy.retryN 3
            // |> Policy.timeout (TimeSpan.FromSeconds 1.)
            // |> Policy.circuitBreaker 3 (TimeSpan.FromSeconds 1.)
            // |> Policy.fallback (fun _ -> 4)
            |> Policy.run 0