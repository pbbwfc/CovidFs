namespace CovidFs
open System
module Data =
    //from https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data/ukmidyearestimates20182019ladcodes.xls
    let males = [|382332.0;395273.0;408684.0;408882.0;412553.0;421934.0;434333.0;427809.0;419161.0;414994.0;418348.0;405604.0;396531.0;381466.0;375311.0;365807.0;360771.0;371195.0;384777.0;401688.0;411122.0;426309.0;432658.0;433554.0;447422.0;447101.0;457870.0;470965.0;462696.0;454070.0;455376.0;439884.0;448165.0;447012.0;433650.0;435220.0;433790.0;438225.0;438053.0;422038.0;393518.0;387736.0;393134.0;400211.0;407039.0;425447.0;443536.0;454593.0;444402.0;455038.0;455264.0;463020.0;459687.0;463433.0;458773.0;449077.0;439605.0;424184.0;405958.0;395941.0;386958.0;371615.0;357493.0;342516.0;341152.0;335697.0;323224.0;323633.0;327472.0;334709.0;349268.0;375924.0;286543.0;273933.0;270187.0;246316.0;215728.0;189863.0;192839.0;186251.0;175626.0;160475.0;146314.0;132941.0;116050.0;103669.0;93155.0;81174.0;68110.0;55652.0;183486.0|]
    let females =[|362931.0;375341.0;387630.0;388301.0;392101.0;401270.0;414348.0;408199.0;400663.0;395813.0;398640.0;384526.0;377837.0;363458.0;357173.0;346926.0;341812.0;353628.0;362219.0;380427.0;388931.0;402450.0;405362.0;411037.0;425730.0;431409.0;450115.0;456180.0;447914.0;448855.0;455763.0;448522.0;447555.0;447937.0;439493.0;443562.0;444015.0;446006.0;445383.0;425942.0;397469.0;390564.0;400468.0;407912.0;415341.0;433399.0;452204.0;470036.0;458544.0;470201.0;470090.0;474274.0;476094.0;479358.0;474242.0;463241.0;451605.0;436233.0;418441.0;409535.0;400174.0;385286.0;370916.0;357872.0;359127.0;352953.0;343286.0;345174.0;351835.0;358452.0;376645.0;405199.0;312740.0;302019.0;299138.0;278921.0;249132.0;223746.0;228622.0;224153.0;214793.0;202210.0;189402.0;175985.0;159667.0;147771.0;138433.0;126713.0;113032.0;96759.0;400538.0|]
    // actuals from https://github.com/emmadoughty/Daily_COVID-19/blob/master/Data
    let actcumdthsld = 335.0
    let actcumrepsld = 6650.0

    let start = DateTime(2020,1,25)
    let currdt t = start.AddHours(float(t*6))
    let currdy d = start.AddDays(float(d))
    let lockdowndate = DateTime(2020,3,23)
    let lockdownt = 232
    let latent_per = 16//4 daya
    let preclin_per = 6//1.5 days
    let clin_per = 14//3.5 days
    let subclin_prop = 0.5
    let subclin_inf = 0.5
    let subclin_per = 20//5 days
    let prehosp_per = 28//7 days
    let icu_hosp_per = 32//8 days
    let non_icu_hosp_per = 40//10 days
    let icu_prop = 0.3
    let non_hosp_per_to_death = 88//22 days
    //from https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf
    let age_grp_bot = [|0;10;20;30;40;50;60;70;80|]
    let ifr = [|0.00002;0.0009;0.001;0.0012;0.0023;0.0068;0.0187;0.0414;0.0768|]
    let hosp_prop = ifr|>Array.map(fun i -> i * 8.04)

    let R0 = 4.0//adjust to fit

    //cross check ifr
    let agegroups =
        let ans = Array.zeroCreate 9
        let addval i v =
            if i<10 then ans.[0]<-ans.[0]+v
            elif i<20 then ans.[1]<-ans.[1]+v
            elif i<30 then ans.[2]<-ans.[2]+v
            elif i<40 then ans.[3]<-ans.[3]+v
            elif i<50 then ans.[4]<-ans.[4]+v
            elif i<60 then ans.[5]<-ans.[5]+v
            elif i<70 then ans.[6]<-ans.[6]+v
            elif i<80 then ans.[7]<-ans.[7]+v
            else ans.[8]<-ans.[8]+v
        males|>Array.iteri addval
        females|>Array.iteri addval
        ans

    let totages = agegroups|>Array.sum

    let calcifr =
        let agrat = Array.zip agegroups ifr
        agrat|>Array.map(fun(a,b)-> a*b)

    let totcalcifr = calcifr|>Array.sum

    let estifr = totcalcifr/totages //1.2% - seem a bit high