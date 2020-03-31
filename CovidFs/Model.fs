namespace CovidFs
open System

module Model =

    let Calc(prop:float,newR0:float) =
        let well = XTvar(91,200)
        let inf0 = XTvar(91,200)
        let inf1 = XTvar(91,200)
        let inf2 = XTvar(91,200)
        let inf3 = XTvar(91,200)
        let inf4 = XTvar(91,200)
        let inf5 = XTvar(91,200)
        let inf6 = XTvar(91,200)
        let inf7 = XTvar(91,200)
        let inf8 = XTvar(91,200)
        let inf9 = XTvar(91,200)
        let recov = XTvar(91,200)
        let dths = XTvar(91,200)
        
        //totals across ages
        let notinfect = Tvar(200)
        let infect = Tvar(200)
        let sick = Tvar(200)
        let allwell = Tvar(200)
        let allrecov = Tvar(200)
        let allnewrecov = Tvar(200)
        let alldths = Tvar(200)
        let cumdths = Tvar(200)

        //functions
        let fnwell (x,t) =
            if t=0 then Data.males.[x] + Data.females.[x]
            else
                well.[x,t-1] - inf0.[x,t-1]

        let fninf1 (x,t) =
            if t=0 then 0.0
            else
                inf0.[x,t-1]
        
        let fninf2 (x,t) =
            if t=0 then 0.0
            else
                inf1.[x,t-1]

        let fninf3 (x,t) =
            if t=0 then 0.0
            else
                inf2.[x,t-1]

        let fninf4 (x,t) =
            if t=0 then 0.0
            else
                inf3.[x,t-1]

        let fninf5 (x,t) =
            if t=0 then 0.0
            else
                inf4.[x,t-1]

        let fninf6 (x,t) =
            if t=0 then 0.0
            else
                inf5.[x,t-1]

        let fninf7 (x,t) =
            if t=0 then 0.0
            else
                inf6.[x,t-1]

        let fninf8 (x,t) =
            if t=0 then 0.0
            else
                inf7.[x,t-1]

        let fninf9 (x,t) =
            if t=0 then 0.0
            else
                inf8.[x,t-1]

        let fnrecov (x,t) =
            if t=0 then 0.0
            else
                recov.[x,t-1] + inf9.[x,t-1] - dths.[x,t]

        let fndths (x,t) =
            if t=0 then 0.0
            elif x<Data.age_grp_bot.[1] then inf9.[x,t-1]*Data.ifr.[0]
            elif x<Data.age_grp_bot.[2] then inf9.[x,t-1]*Data.ifr.[1]
            elif x<Data.age_grp_bot.[3] then inf9.[x,t-1]*Data.ifr.[2]
            elif x<Data.age_grp_bot.[4] then inf9.[x,t-1]*Data.ifr.[3]
            elif x<Data.age_grp_bot.[5] then inf9.[x,t-1]*Data.ifr.[4]
            elif x<Data.age_grp_bot.[6] then inf9.[x,t-1]*Data.ifr.[5]
            elif x<Data.age_grp_bot.[7] then inf9.[x,t-1]*Data.ifr.[6]
            elif x<Data.age_grp_bot.[8] then inf9.[x,t-1]*Data.ifr.[7]
            else inf9.[x,t-1]*Data.ifr.[8]

        //functions for all ages
        let fnnotinfect(t) =
            [0..90]|>List.map(fun i -> inf0.[i,t] + inf1.[i,t] + inf2.[i,t] + inf3.[i,t] + inf4.[i,t])|>List.sum
        
        let fninfect(t) =
            [0..90]|>List.map(fun i -> inf5.[i,t] + inf6.[i,t] + inf7.[i,t] + inf8.[i,t] + inf9.[i,t])|>List.sum

        let fnsick(t) = notinfect.[t] + infect.[t]

        let fnallwell(t) =
            [0..90]|>List.map(fun i -> well.[i,t])|>List.sum

        let fnallnewrecov(t) =
            [0..90]|>List.map(fun i -> inf9.[i,t]-dths.[i,t])|>List.sum

        let fnallrecov(t) =
            [0..90]|>List.map(fun i -> recov.[i,t])|>List.sum

        let fnalldths(t) =
            [0..90]|>List.map(fun i -> dths.[i,t])|>List.sum

        let fncumdths(t) =
            if t=0 then 0.0
            else
                cumdths.[t-1] + alldths.[t]

        
        //adjust key function based on inputs
        let initial =
            let emp = Array.zeroCreate 91
            emp|>Array.map(fun x -> (Data.males.[x] + Data.females.[x]) * prop /100.0)
        
        let fninf0 (x,t) =
            if t=0 then initial.[x]
            elif t<Data.lockdownt then
                infect.[t-1] * Data.R0 * well.[x,t]/(5.0 * allwell.[0])
                //reduce by proportion in well
            else
                infect.[t-1] * newR0 * well.[x,t]/(5.0 * allwell.[0])
                //reduce by proportion in well
        
        //initialise
        well.Init fnwell
        inf0.Init fninf0
        inf1.Init fninf1
        inf2.Init fninf2
        inf3.Init fninf3
        inf4.Init fninf4
        inf5.Init fninf5
        inf6.Init fninf6
        inf7.Init fninf7
        inf8.Init fninf8
        inf9.Init fninf9
        recov.Init fnrecov
        dths.Init fndths
        //initialise for all ages
        notinfect.Init fnnotinfect
        infect.Init fninfect
        sick.Init fnsick
        allwell.Init fnallwell
        allrecov.Init fnallrecov
        allnewrecov.Init fnallnewrecov
        alldths.Init fnalldths
        cumdths.Init fncumdths
        //decide what to keep
        let resultxt = ["Males Current Well",well.Vals;"New Recov",inf9.Vals;"Deaths",dths.Vals]|>dict
        let resultt = ["Total Sick",sick.Vals;"Total Recov",allrecov.Vals;"Total New Recov",allnewrecov.Vals;"Total Deaths",alldths.Vals;"Cumulative Deaths",cumdths.Vals]|>dict

        resultt,resultxt