// This is updated based on assumptions used in
// https://cmmid.github.io/topics/covid19/control-measures/report/uk_scenario_modelling_preprint_2020_04_01.pdf

open CovidFs
open System
open System.IO

[<EntryPoint>]
let main argv =
    //let p,r,s = Fit.result
    //results of fit: p=9E-06, r=4.4


    let rest,res = Model.Calc(9E-06,4.4)
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
    File.AppendAllText(csv,"Date,NewCase,CumCases,NewDeaths,Deaths")
    File.AppendAllText(csv,nl)
    for t=0 to 200 do
        let dt = t|>Data.currdy
        File.AppendAllText(csv,dt.ToShortDateString() + "," + rest.["New Cases"].[t].ToString() + "," + rest.["Cases"].[t].ToString() + "," + rest.["New Deaths"].[t].ToString() + "," + rest.["Deaths"].[t].ToString())
        File.AppendAllText(csv,nl)
    ////repeat but with fix on R0
    let rest1,res1 = Model.Calc(9E-06,1.0)
    let csv1 = Path.Combine(fol,"modeled1.csv")
    if File.Exists(csv1) then File.Delete(csv1)
    File.AppendAllText(csv1,"Date,NewCase,CumCases,NewDeaths,Deaths")
    File.AppendAllText(csv1,nl)
    for t=0 to 200 do
        let dt = t|>Data.currdy
        File.AppendAllText(csv1,dt.ToShortDateString() + "," + rest1.["New Cases"].[t].ToString() + "," + rest1.["Cases"].[t].ToString() + "," + rest1.["New Deaths"].[t].ToString() + "," + rest1.["Deaths"].[t].ToString())
        File.AppendAllText(csv1,nl)



    0 // return an integer exit code
