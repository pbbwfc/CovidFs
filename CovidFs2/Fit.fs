namespace CovidFs
open System

module Fit =
    //deaths from this model t=0 at 1/1/2020
    let act = [0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;1.0;2.0;2.0;3.0;6.0;6.0;8.0;10.0;21.0;35.0;55.0;71.0;103.0;144.0;177.0;233.0;281.0;335.0;422.0;463.0;578.0;759.0;1019.0;1228.0;1408.0;1789.0;2352.0;2921.0;3605.0;4313.0;4934.0]
    //range 0 to 71
    let rs = [3.0; 3.1; 3.2; 3.3; 3.4; 3.5; 3.6; 3.7; 3.8; 3.9; 4.0; 4.1; 4.2; 4.3; 4.4; 4.5; 4.6; 4.7; 4.8; 4.9; 5.0; 5.1; 5.2; 5.3; 5.4; 5.5; 5.6; 5.7; 5.8; 5.9; 6.0]
    let ps = [8e-06; 8.2e-06; 8.4e-06; 8.6e-06; 8.8e-06; 9e-06; 9.2e-06; 9.4e-06; 9.6e-06;
                9.8e-06; 1e-05; 1.02e-05; 1.04e-05; 1.06e-05; 1.08e-05; 1.1e-05; 1.12e-05;
                1.14e-05; 1.16e-05; 1.18e-05; 1.2e-05; 1.22e-05; 1.24e-05; 1.26e-05;
                1.28e-05; 1.3e-05; 1.32e-05; 1.34e-05; 1.36e-05; 1.38e-05; 1.4e-05;
                1.42e-05; 1.44e-05; 1.46e-05; 1.48e-05; 1.5e-05; 1.52e-05; 1.54e-05;
                1.56e-05; 1.58e-05; 1.6e-05]
    let fitrange = [48..71]
    let getdths p r = 
        let rest,res = Model.CalcFit(p,r,r)
        let alldths = rest.["Deaths"]
        //let dths = fitrange|>List.map(fun i -> alldths.[i])
        //let acts = fitrange|>List.map(fun i -> act.[i])
        let sqs = fitrange|>List.map(fun i -> (act.[i] - alldths.[i])**2.0) 
        let sumsqs = sqs|>List.sum
        p,r,sumsqs
    let dofit() =
        [for p in ps do
            for r in rs do
                yield getdths p r]

    //let result  = 
    //    let all = dofit()
    //    all|>List.minBy(fun (p,r,s) -> s)

