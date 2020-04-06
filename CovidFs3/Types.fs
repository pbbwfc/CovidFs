namespace CovidFs
[<AutoOpen>]
module Types = 
    type XTvar(ages,pt) = 
        let mutable fn = fun _ -> 0.0
        let vl : float option [,] = Array2D.zeroCreate ages pt
        
        member tv.Item 
            with get (x,t) = 
                if vl.[x,t].IsSome then vl.[x,t].Value
                else 
                    let ans = fn (x,t)
                    vl.[x,t] <- Some(ans)
                    ans
        
        member tv.Init ifn = fn <- ifn
        member tv.Vals = 
            let ans = Array2D.zeroCreate ages pt
            for x = 0 to ages-1 do
                for t = 0 to pt-1 do
                    ans.[x,t] <- tv.[x,t]
            ans

    type Tvar(pt) = 
        let mutable fn = fun _ -> 0.0
        let vl : float option [] = Array.zeroCreate pt
        
        member tv.Item 
            with get (t) = 
                if vl.[t].IsSome then vl.[t].Value
                else 
                    let ans = fn (t)
                    vl.[t] <- Some(ans)
                    ans
        
        member tv.Init ifn = fn <- ifn
        member tv.Vals = [| 0..pt - 1 |] |> Array.map (fun t -> tv.[t])

