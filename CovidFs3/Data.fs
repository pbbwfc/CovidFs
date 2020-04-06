namespace CovidFs
open System
module Data =
    //from https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/ukmidyearestimates20182019ladcodes.xls
    let pop = [|8052552.0; 7528144.0; 8711750.0; 8835591.0; 8500792.0; 8968055.0; 7069544.0; 5487167.0; 3281955.0|]

    let start = DateTime(2020,1,25)
    let currdt t = start.AddHours(float(t*6))
    let currdy d = start.AddDays(float(d))
    let lockdowndate = DateTime(2020,3,23)
    let lockdownt = 232
    let subclin_prop = 0.5
    let subclin_inf = 0.5
 
    let ifr = [|0.00002;0.0009;0.001;0.0012;0.0023;0.0068;0.0187;0.0414;0.0768|]


    let test = (pop|>Array.sum)*9E-06/100.0

