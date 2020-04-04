﻿// This is updated based on assumptions used in
// https://cmmid.github.io/topics/covid19/control-measures/report/uk_scenario_modelling_preprint_2020_04_01.pdf

open CovidFs
open System
open System.IO

[<EntryPoint>]
let main argv =
    let rest,res = Model.Calc(0.000002,4.0)
    //lockdown date figures/reconcile
    //let dthsld = res.["Deaths"].[*,82]
    //let nrecld = res.["New Recov"].[*,82]
    //let cumdthsld = rest.["Cumulative Deaths"].[82]
    //let ratio = cumdthsld/Data.actcumdthsld
    ////export CSV
    let fol = @"D:\GitHub\CovidFs\Results2"
    let csv = Path.Combine(fol,"modeled.csv")
    if File.Exists(csv) then File.Delete(csv)
    let nl = Environment.NewLine
    File.AppendAllText(csv,"Date,NewCase,CumCases")
    File.AppendAllText(csv,nl)
    for t=0 to 200 do
        let dt = t|>Data.currdy
        File.AppendAllText(csv,dt.ToShortDateString() + "," + rest.["New Cases"].[t].ToString() + "," + rest.["Cases"].[t].ToString())
        File.AppendAllText(csv,nl)
    ////repeat but with fix on R0
    //let rest1,res1 = Model.Calc(0.0000000085,1.0)
    //let csv1 = Path.Combine(fol,"modeled1.csv")
    //if File.Exists(csv1) then File.Delete(csv1)
    //File.AppendAllText(csv1,"Date,Cumulative Deaths,Total Recovered")
    //File.AppendAllText(csv1,nl)
    //for t=65 to 100 do
    //    let dt = t|>Data.currdt
    //    File.AppendAllText(csv1,dt.ToShortDateString() + "," + rest1.["Cumulative Deaths"].[t].ToString() + "," + rest1.["Total Recov"].[t].ToString())
    //    File.AppendAllText(csv1,nl)



    0 // return an integer exit code
