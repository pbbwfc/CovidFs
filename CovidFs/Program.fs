open CovidFs
open System
open System.IO

[<EntryPoint>]
let main argv =
    //let p,r,sumsqs = Fit.result
    //results of fit: p=1.14e-08, r=5.3

    let rest,res = Model.Calc(1.14e-08,5.3)
    //lockdown date figures/reconcile
    let dthsld = res.["Deaths"].[*,82]
    let nrecld = res.["New Recov"].[*,82]
    let cumdthsld = rest.["Cumulative Deaths"].[82]
    let ratio = cumdthsld/Data.actcumdthsld
    //export CSV
    let fol = @"D:\GitHub\CovidFs\Results"
    let csv = Path.Combine(fol,"modeled.csv")
    if File.Exists(csv) then File.Delete(csv)
    let nl = Environment.NewLine
    File.AppendAllText(csv,"Date,Cumulative Deaths,Total Recovered")
    File.AppendAllText(csv,nl)
    for t=65 to 100 do
        let dt = t|>Data.currdt
        File.AppendAllText(csv,dt.ToShortDateString() + "," + rest.["Cumulative Deaths"].[t].ToString() + "," + rest.["Total Recov"].[t].ToString())
        File.AppendAllText(csv,nl)
    //repeat but with fix on R0
    let rest1,res1 = Model.Calc(1.14e-08,1.0)
    let csv1 = Path.Combine(fol,"modeled1.csv")
    if File.Exists(csv1) then File.Delete(csv1)
    File.AppendAllText(csv1,"Date,Cumulative Deaths,Total Recovered")
    File.AppendAllText(csv1,nl)
    for t=65 to 100 do
        let dt = t|>Data.currdt
        File.AppendAllText(csv1,dt.ToShortDateString() + "," + rest1.["Cumulative Deaths"].[t].ToString() + "," + rest1.["Total Recov"].[t].ToString())
        File.AppendAllText(csv1,nl)



    0 // return an integer exit code
