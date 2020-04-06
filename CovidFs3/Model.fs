namespace CovidFs
open System

module Model =

    let Calc(prop:float,oldR0:float,newR0:float) =
        let well = XTvar(9,1000)
        let latentarr = [|0..15|]|>Array.map(fun _ -> XTvar(9,1000))
        let preclinarr = [|0..5|]|>Array.map(fun _ -> XTvar(9,1000))
        let subclinarr = [|0..19|]|>Array.map(fun _ -> XTvar(9,1000))
        let clinarr = [|0..13|]|>Array.map(fun _ -> XTvar(9,1000))
        let hdths = XTvar(10,1000)

        //totals across ages
        let allwell = Tvar(1000)
        let latent = Tvar(1000)
        let preclin = Tvar(1000)
        let subclin = Tvar(1000)
        let clin = Tvar(1000)
        let contag = Tvar(1000)
        let newcases = Tvar(1000)
        let cases = Tvar(1000)
        let newhdths = Tvar(1000) 
        let allhdths = Tvar(1000)

        //functions
        let fnwell (x,t) =
            if t=0 then Data.pop.[x]
            else
                well.[x,t-1] - latentarr.[0].[x,t-1]

        let fnlatentarr i =
            if i=0 then
                fun(x,t) ->
                    //adjust key function based on inputs
                    let initial =
                        [|0..8|]|>Array.map(fun x -> Data.pop.[x] * prop /100.0)
                
                    if t<=15 then initial.[x]
                    elif t<Data.lockdownt then
                        contag.[t-1] * oldR0 * well.[x,t]/(20.0 * allwell.[0])
                        //reduce by proportion in well
                    else
                        contag.[t-1] * newR0 * well.[x,t]/(20.0 * allwell.[0])
                        //reduce by proportion in well
            else
                fun(x,t) ->
                    if t=0 then 0.0
                    else
                        latentarr.[i-1].[x,t-1]

        let fnpreclinarr i =
            if i=0 then
                fun(x,t) ->
                    if t=0 then 0.0
                    else
                        (1.0 - Data.subclin_prop) * latentarr.[15].[x,t-1]
            else
                fun(x,t) ->
                    if t=0 then 0.0
                    else
                        preclinarr.[i-1].[x,t-1]

        let fnsubclinarr i =
            if i=0 then
                fun(x,t) ->
                    if t=0 then 0.0
                    else
                        Data.subclin_prop * latentarr.[15].[x,t-1]
            else
                fun(x,t) ->
                    if t=0 then 0.0
                    else
                        subclinarr.[i-1].[x,t-1]

        let fnclinarr i =
            if i=0 then
                fun(x,t) ->
                    if t=0 then 0.0
                    else
                        preclinarr.[5].[x,t-1]
            else
                fun(x,t) ->
                    if t=0 then 0.0
                    else
                        clinarr.[i-1].[x,t-1]

        let fnhdths (x,t) =
            //report assumes 7 days to hospitalisation then either 8 or 10 days to death depending on whether put in ICU
            //I assume clin1 is after 1.5 days so use clin1 14.5 days ago
            //This is t-58
            if t<=58 then 0.0
            else clinarr.[0].[x,t-58]*Data.ifr.[x]

        //functions for all ages
        let fnlatent(t) =
            [0..8]|>List.map(fun i -> latentarr|>Array.map(fun l -> l.[i,t])|>Array.sum)|>List.sum

        let fnpreclin(t) =
            [0..8]|>List.map(fun i -> preclinarr|>Array.map(fun l -> l.[i,t])|>Array.sum)|>List.sum

        let fnsubclin(t) =
            [0..8]|>List.map(fun i -> subclinarr|>Array.map(fun l -> l.[i,t])|>Array.sum)|>List.sum

        let fnclin(t) =
            [0..8]|>List.map(fun i -> clinarr|>Array.map(fun l -> l.[i,t])|>Array.sum)|>List.sum 

        let fnnewcases(t) =
            [0..8]|>List.map(fun i -> clinarr.[0].[i,t])|>List.sum

        let fncases(t) =
            if t=0 then 0.0
            else cases.[t-1] + newcases.[t]
        
        let fncontag(t) =
            preclin.[t] + clin.[t] + subclin.[t] * Data.subclin_inf //half as contagious with no symptoms
        
        let fnallwell(t) =
            [0..8]|>List.map(fun i -> well.[i,t])|>List.sum

        let fnnewhdths(t) =
            [0..8]|>List.map(fun i -> hdths.[i,t])|>List.sum

        let fnallhdths(t) =
            if t=0 then 0.0
            else allhdths.[t-1] + newhdths.[t]
        
        //initialise
        well.Init fnwell
        latentarr|>Array.iteri(fun i l -> l.Init (fnlatentarr(i)))
        preclinarr|>Array.iteri(fun i l -> l.Init (fnpreclinarr(i)))
        subclinarr|>Array.iteri(fun i l -> l.Init (fnsubclinarr(i)))
        clinarr|>Array.iteri(fun i l -> l.Init (fnclinarr(i)))
        hdths.Init fnhdths

        //initialise for all ages
        allwell.Init fnallwell
        latent.Init fnlatent
        preclin.Init fnpreclin
        subclin.Init fnsubclin
        clin.Init fnclin
        contag.Init fncontag
        newcases.Init fnnewcases
        cases.Init fncases
        newhdths.Init fnnewhdths
        allhdths.Init fnallhdths

        //functions to generate daily results
        let sumday (tv:Tvar) =
            let rec accum ct (il:int list) vsum outl =
                if il.IsEmpty then outl|>List.rev
                else
                    let ci = il.Head
                    let cv = tv.[ci]
                    if ct=3 then
                        let noutl = (vsum + cv)::outl
                        accum 0 il.Tail 0.0 noutl
                    else
                        accum (ct+1) il.Tail (vsum+cv) outl
            accum 0 [0..999] 0.0 []
        let lstday (tv:Tvar) =
            let rec lst ct (il:int list) outl =
                if il.IsEmpty then outl|>List.rev
                else
                    let ci = il.Head
                    let cv = tv.[ci]
                    if ct=3 then
                        let noutl = (cv)::outl
                        lst 0 il.Tail noutl
                    else
                        lst (ct+1) il.Tail outl
            lst 0 [0..999] []

        //decide what to keep
        let resultxt = ["Males Current Well",well.Vals]|>dict
        let resultt = ["Total Contagious",contag|>lstday;"Total Well",allwell|>lstday;"Clinically Infectious",clin|>lstday;"Cases",cases|>lstday;"New Cases",newcases|>sumday;"Deaths",allhdths|>lstday;"New Deaths",newhdths|>sumday]|>dict

        resultt,resultxt
