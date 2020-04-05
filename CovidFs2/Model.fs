namespace CovidFs
open System

module Model =

    let CalcFit(prop:float,oldR0:float,newR0:float) =
        let well = XTvar(91,1000)
        let latent1 = XTvar(91,1000)
        let latent2 = XTvar(91,1000)
        let latent3 = XTvar(91,1000)
        let latent4 = XTvar(91,1000)
        let latent5 = XTvar(91,1000)
        let latent6 = XTvar(91,1000)
        let latent7 = XTvar(91,1000)
        let latent8 = XTvar(91,1000)
        let latent9 = XTvar(91,1000)
        let latent10 = XTvar(91,1000)
        let latent11 = XTvar(91,1000)
        let latent12 = XTvar(91,1000)
        let latent13 = XTvar(91,1000)
        let latent14 = XTvar(91,1000)
        let latent15 = XTvar(91,1000)
        let latent16 = XTvar(91,1000)
        let preclin1 = XTvar(91,1000)
        let preclin2 = XTvar(91,1000)
        let preclin3 = XTvar(91,1000)
        let preclin4 = XTvar(91,1000)
        let preclin5 = XTvar(91,1000)
        let preclin6 = XTvar(91,1000)
        let subclin1 = XTvar(91,1000)
        let subclin2 = XTvar(91,1000)
        let subclin3 = XTvar(91,1000)
        let subclin4 = XTvar(91,1000)
        let subclin5 = XTvar(91,1000)
        let subclin6 = XTvar(91,1000)
        let subclin7 = XTvar(91,1000)
        let subclin8 = XTvar(91,1000)
        let subclin9 = XTvar(91,1000)
        let subclin10 = XTvar(91,1000)
        let subclin11 = XTvar(91,1000)
        let subclin12 = XTvar(91,1000)
        let subclin13 = XTvar(91,1000)
        let subclin14 = XTvar(91,1000)
        let subclin15 = XTvar(91,1000)
        let subclin16 = XTvar(91,1000)
        let subclin17 = XTvar(91,1000)
        let subclin18 = XTvar(91,1000)
        let subclin19 = XTvar(91,1000)
        let subclin20 = XTvar(91,1000)
        let clin1 = XTvar(91,1000)
        let clin2 = XTvar(91,1000)
        let clin3 = XTvar(91,1000)
        let clin4 = XTvar(91,1000)
        let clin5 = XTvar(91,1000)
        let clin6 = XTvar(91,1000)
        let clin7 = XTvar(91,1000)
        let clin8 = XTvar(91,1000)
        let clin9 = XTvar(91,1000)
        let clin10 = XTvar(91,1000)
        let clin11 = XTvar(91,1000)
        let clin12 = XTvar(91,1000)
        let clin13 = XTvar(91,1000)
        let clin14 = XTvar(91,1000)
        let hdths = XTvar(91,1000)

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
            if t=0 then Data.males.[x] + Data.females.[x]
            else
                well.[x,t-1] - latent1.[x,t-1]

        let fnlatent2 (x,t) =
            if t=0 then 0.0
            else
                latent1.[x,t-1]

        let fnlatent3 (x,t) =
            if t=0 then 0.0
            else
                latent2.[x,t-1]

        let fnlatent4 (x,t) =
            if t=0 then 0.0
            else
                latent3.[x,t-1]

        let fnlatent5 (x,t) =
            if t=0 then 0.0
            else
                latent4.[x,t-1]

        let fnlatent6 (x,t) =
            if t=0 then 0.0
            else
                latent5.[x,t-1]

        let fnlatent7 (x,t) =
            if t=0 then 0.0
            else
                latent6.[x,t-1]

        let fnlatent8 (x,t) =
            if t=0 then 0.0
            else
                latent7.[x,t-1]

        let fnlatent9 (x,t) =
            if t=0 then 0.0
            else
                latent8.[x,t-1]

        let fnlatent10 (x,t) =
            if t=0 then 0.0
            else
                latent9.[x,t-1]

        let fnlatent11 (x,t) =
            if t=0 then 0.0
            else
                latent10.[x,t-1]

        let fnlatent12 (x,t) =
            if t=0 then 0.0
            else
                latent11.[x,t-1]

        let fnlatent13 (x,t) =
            if t=0 then 0.0
            else
                latent12.[x,t-1]

        let fnlatent14 (x,t) =
            if t=0 then 0.0
            else
                latent13.[x,t-1]

        let fnlatent15 (x,t) =
            if t=0 then 0.0
            else
                latent14.[x,t-1]

        let fnlatent16 (x,t) =
            if t=0 then 0.0
            else
                latent15.[x,t-1]

        let fnpreclin1 (x,t) =
            if t=0 then 0.0
            else
                (1.0 - Data.subclin_prop) * latent16.[x,t-1]

        let fnpreclin2 (x,t) =
            if t=0 then 0.0
            else
                preclin1.[x,t-1]

        let fnpreclin3 (x,t) =
            if t=0 then 0.0
            else
                preclin2.[x,t-1]

        let fnpreclin4 (x,t) =
            if t=0 then 0.0
            else
                preclin3.[x,t-1]

        let fnpreclin5 (x,t) =
            if t=0 then 0.0
            else
                preclin4.[x,t-1]

        let fnpreclin6 (x,t) =
            if t=0 then 0.0
            else
                preclin5.[x,t-1]

        let fnsubclin1 (x,t) =
            if t=0 then 0.0
            else
                Data.subclin_prop * latent16.[x,t-1]

        let fnsubclin2 (x,t) =
            if t=0 then 0.0
            else
                subclin1.[x,t-1]

        let fnsubclin3 (x,t) =
            if t=0 then 0.0
            else
                subclin2.[x,t-1]

        let fnsubclin4 (x,t) =
            if t=0 then 0.0
            else
                subclin3.[x,t-1]

        let fnsubclin5 (x,t) =
            if t=0 then 0.0
            else
                subclin4.[x,t-1]

        let fnsubclin6 (x,t) =
            if t=0 then 0.0
            else
                subclin5.[x,t-1]

        let fnsubclin7 (x,t) =
            if t=0 then 0.0
            else
                subclin6.[x,t-1]

        let fnsubclin8 (x,t) =
            if t=0 then 0.0
            else
                subclin7.[x,t-1]

        let fnsubclin9 (x,t) =
            if t=0 then 0.0
            else
                subclin8.[x,t-1]

        let fnsubclin10 (x,t) =
            if t=0 then 0.0
            else
                subclin9.[x,t-1]

        let fnsubclin11 (x,t) =
            if t=0 then 0.0
            else
                subclin10.[x,t-1]

        let fnsubclin12 (x,t) =
            if t=0 then 0.0
            else
                subclin11.[x,t-1]

        let fnsubclin13 (x,t) =
            if t=0 then 0.0
            else
                subclin12.[x,t-1]

        let fnsubclin14 (x,t) =
            if t=0 then 0.0
            else
                subclin13.[x,t-1]

        let fnsubclin15 (x,t) =
            if t=0 then 0.0
            else
                subclin14.[x,t-1]

        let fnsubclin16 (x,t) =
            if t=0 then 0.0
            else
                subclin15.[x,t-1]

        let fnsubclin17 (x,t) =
            if t=0 then 0.0
            else
                subclin16.[x,t-1]

        let fnsubclin18 (x,t) =
            if t=0 then 0.0
            else
                subclin17.[x,t-1]

        let fnsubclin19 (x,t) =
            if t=0 then 0.0
            else
                subclin18.[x,t-1]

        let fnsubclin20 (x,t) =
            if t=0 then 0.0
            else
                subclin19.[x,t-1]

        let fnclin1 (x,t) =
            if t=0 then 0.0
            else
                preclin6.[x,t-1]

        let fnclin2 (x,t) =
            if t=0 then 0.0
            else
                clin1.[x,t-1]

        let fnclin3 (x,t) =
            if t=0 then 0.0
            else
                clin2.[x,t-1]

        let fnclin4 (x,t) =
            if t=0 then 0.0
            else
                clin3.[x,t-1]

        let fnclin5 (x,t) =
            if t=0 then 0.0
            else
                clin4.[x,t-1]

        let fnclin6 (x,t) =
            if t=0 then 0.0
            else
                clin5.[x,t-1]

        let fnclin7 (x,t) =
            if t=0 then 0.0
            else
                clin6.[x,t-1]

        let fnclin8 (x,t) =
            if t=0 then 0.0
            else
                clin7.[x,t-1]

        let fnclin9 (x,t) =
            if t=0 then 0.0
            else
                clin8.[x,t-1]

        let fnclin10 (x,t) =
            if t=0 then 0.0
            else
                clin9.[x,t-1]

        let fnclin11 (x,t) =
            if t=0 then 0.0
            else
                clin10.[x,t-1]

        let fnclin12 (x,t) =
            if t=0 then 0.0
            else
                clin11.[x,t-1]

        let fnclin13 (x,t) =
            if t=0 then 0.0
            else
                clin12.[x,t-1]

        let fnclin14 (x,t) =
            if t=0 then 0.0
            else
                clin13.[x,t-1]

        //let fnrecov (x,t) =
        //    if t=0 then 0.0
        //    else
        //        recov.[x,t-1] + inf9.[x,t-1] - dths.[x,t]

        let fnhdths (x,t) =
            //report assumes 7 days to hospitalisation then either 8 or 10 days to death depending on whether put in ICU
            //I assume clin1 is after 1.5 days so use clin1 14.5 days ago
            //This is t-58
            
            if t<=58 then 0.0
            elif x<Data.age_grp_bot.[1] then clin1.[x,t-58]*Data.ifr.[0]
            elif x<Data.age_grp_bot.[2] then clin1.[x,t-58]*Data.ifr.[1]
            elif x<Data.age_grp_bot.[3] then clin1.[x,t-58]*Data.ifr.[2]
            elif x<Data.age_grp_bot.[4] then clin1.[x,t-58]*Data.ifr.[3]
            elif x<Data.age_grp_bot.[5] then clin1.[x,t-58]*Data.ifr.[4]
            elif x<Data.age_grp_bot.[6] then clin1.[x,t-58]*Data.ifr.[5]
            elif x<Data.age_grp_bot.[7] then clin1.[x,t-58]*Data.ifr.[6]
            elif x<Data.age_grp_bot.[8] then clin1.[x,t-58]*Data.ifr.[7]
            else clin1.[x,t-58]*Data.ifr.[8]

        //functions for all ages
        let fnlatent(t) =
            [0..90]|>List.map(fun i -> latent1.[i,t] + latent2.[i,t] + latent3.[i,t] + latent4.[i,t] 
                                        + latent5.[i,t] + latent6.[i,t] + latent7.[i,t] + latent8.[i,t]
                                        + latent9.[i,t] + latent10.[i,t] + latent11.[i,t] + latent12.[i,t]
                                        + latent13.[i,t] + latent14.[i,t] + latent15.[i,t] + latent16.[i,t]
                                        )|>List.sum

        let fnpreclin(t) =
            [0..90]|>List.map(fun i -> preclin1.[i,t] + preclin2.[i,t] + preclin3.[i,t] + preclin4.[i,t] 
                                        + preclin5.[i,t] + preclin6.[i,t]
                                        )|>List.sum

        let fnsubclin(t) =
            [0..90]|>List.map(fun i -> subclin1.[i,t] + subclin2.[i,t] + subclin3.[i,t] + subclin4.[i,t] 
                                        + subclin5.[i,t] + subclin6.[i,t] + subclin7.[i,t] + subclin8.[i,t]
                                        + subclin9.[i,t] + subclin10.[i,t] + subclin11.[i,t] + subclin12.[i,t]
                                        + subclin13.[i,t] + subclin14.[i,t] + subclin15.[i,t] + subclin16.[i,t]
                                        + subclin17.[i,t] + subclin18.[i,t] + subclin19.[i,t] + subclin20.[i,t]
                                        )|>List.sum

        let fnclin(t) =
            [0..90]|>List.map(fun i -> clin1.[i,t] + clin2.[i,t] + clin3.[i,t] + clin4.[i,t] 
                                        + clin5.[i,t] + clin6.[i,t] + clin7.[i,t] + clin8.[i,t]
                                        + clin9.[i,t] + clin10.[i,t] + clin11.[i,t] + clin12.[i,t]
                                        + clin13.[i,t] + clin14.[i,t]
                                        )|>List.sum
        let fnnewcases(t) =
            [0..90]|>List.map(fun i -> clin1.[i,t])|>List.sum

        let fncases(t) =
            if t=0 then 0.0
            else cases.[t-1] + newcases.[t]
        
        let fncontag(t) =
            preclin.[t] + clin.[t] + subclin.[t] * Data.subclin_inf //half as contagious with no symptoms
        
        let fnallwell(t) =
            [0..90]|>List.map(fun i -> well.[i,t])|>List.sum

        let fnnewhdths(t) =
            [0..90]|>List.map(fun i -> hdths.[i,t])|>List.sum

        let fnallhdths(t) =
            if t=0 then 0.0
            else allhdths.[t-1] + newhdths.[t]
        
        //adjust key function based on inputs
        let initial =
            let emp = Array.zeroCreate 91
            emp|>Array.map(fun x -> (Data.males.[x] + Data.females.[x]) * prop /100.0)
        
        let fnlatent1 (x,t) =
            //set replacement R0
            let repR0 = if newR0= -1.0 then Data.R0 else newR0
            
            if t<=15 then initial.[x]
            elif t<Data.lockdownt then
                contag.[t-1] * oldR0 * well.[x,t]/(20.0 * allwell.[0])
                //reduce by proportion in well
            else
                contag.[t-1] * repR0 * well.[x,t]/(20.0 * allwell.[0])
                //reduce by proportion in well
        
        //initialise
        well.Init fnwell
        latent1.Init fnlatent1
        latent2.Init fnlatent2
        latent3.Init fnlatent3
        latent4.Init fnlatent4
        latent5.Init fnlatent5
        latent6.Init fnlatent6
        latent7.Init fnlatent7
        latent8.Init fnlatent8
        latent9.Init fnlatent9
        latent10.Init fnlatent10
        latent11.Init fnlatent11
        latent12.Init fnlatent12
        latent13.Init fnlatent13
        latent14.Init fnlatent14
        latent15.Init fnlatent15
        latent16.Init fnlatent16
        preclin1.Init fnpreclin1
        preclin2.Init fnpreclin2
        preclin3.Init fnpreclin3
        preclin4.Init fnpreclin4
        preclin5.Init fnpreclin5
        preclin6.Init fnpreclin6
        subclin1.Init fnsubclin1
        subclin2.Init fnsubclin2
        subclin3.Init fnsubclin3
        subclin4.Init fnsubclin4
        subclin5.Init fnsubclin5
        subclin6.Init fnsubclin6
        subclin7.Init fnsubclin7
        subclin8.Init fnsubclin8
        subclin9.Init fnsubclin9
        subclin10.Init fnsubclin10
        subclin11.Init fnsubclin11
        subclin12.Init fnsubclin12
        subclin13.Init fnsubclin13
        subclin14.Init fnsubclin14
        subclin15.Init fnsubclin15
        subclin16.Init fnsubclin16
        subclin17.Init fnsubclin17
        subclin18.Init fnsubclin18
        subclin19.Init fnsubclin19
        subclin20.Init fnsubclin20
        clin1.Init fnclin1
        clin2.Init fnclin2
        clin3.Init fnclin3
        clin4.Init fnclin4
        clin5.Init fnclin5
        clin6.Init fnclin6
        clin7.Init fnclin7
        clin8.Init fnclin8
        clin9.Init fnclin9
        clin10.Init fnclin10
        clin11.Init fnclin11
        clin12.Init fnclin12
        clin13.Init fnclin13
        clin14.Init fnclin14
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

        //debug vars
        let well020 = [|0..20|]|>Array.map(fun i -> allwell.[i])
        let seed = initial|>Array.sum
        let lat020 = [|0..20|]|>Array.map(fun i -> latent.[i])
        let clin040 = [|0..40|]|>Array.map(fun i -> clin.[i]) 
        
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

    let Calc(prop:float,newR0:float) = CalcFit(prop,Data.R0,newR0)