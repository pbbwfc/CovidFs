#load @"D:\GitHub\CovidFs\CovidFs\Types.fs"
#load @"D:\GitHub\CovidFs\CovidFs\Data.fs"
#load @"D:\GitHub\CovidFs\CovidFs\Model.fs"

open CovidFs

let rest,res = Model.Calc()

res.["Males Current Well"].[0,1]

res.["Males Current Well"].[1,2]

res.["Males Current Well"].[*,2]

res.["Males Current Well"].[40,*]

rest.["Total Sick"]