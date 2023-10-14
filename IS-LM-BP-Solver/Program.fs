open FSharp.Data

type IS = {
    A: double // Автономные внутренние расходы (без учета автономного экспорта)
    Xn_a: double // Автономный экспорт
    I_r: double // Чувствительность инвестиций к ставке процента
    Xn_e: double // Чувствительность чистого экспорта к реальному валютному курсу
    MLR: double  // Предельная норма изъятий
}

type LM = {
    MS: double // Номинальное предложение денег
    P: double // Уровень цен
    Md_Y: double // Чувствительность реального спроса на деньги к ВВП
    Md_r: double // Чувствительность реального спроса на деньги к ставке процента
}
type Xn = {
    Xn_a: double // Автономный чистый экспорт
    Xn_e: double // Чувствительность чистого экспорта к реальному валютному курсу
    mpm: double // предельная склонность к импорту
}
type KA = {
    KA_a: double // Автономная часть СДК
    r_f: double // Мировая ставка процента
    m: double  // Степень мобильности капитала
}
type BP = {
    Xn: Xn
    KA: KA
}

type FloatingModel = {
    IS: IS
    LM: LM
    BP: BP
}

type FixedModel = {
    e: double
    IS: IS
    BP: BP
}

type Model =
    |FixedExchangeRate of FixedModel
    |FloatingExchangeRate of FloatingModel

type Result = {
    Y: double
    r: double
    e: double
}

let calculateFloating (model: FloatingModel) =
    let is = model.IS
    let lm = model.LM
    let bp = model.BP
    let y = ((is.A-bp.KA.KA_a+bp.KA.m*bp.KA.r_f)*lm.Md_r+(lm.MS/lm.P)*(is.I_r+bp.KA.m))/((is.MLR-bp.Xn.mpm)*lm.Md_r+(is.I_r+bp.KA.m)*lm.Md_Y)
    let r = (lm.Md_Y*y-(lm.MS/lm.P))/lm.Md_r
    let e = (is.Xn_a-y*bp.Xn.mpm+bp.KA.KA_a+bp.KA.m*(r-bp.KA.r_f))/is.Xn_e
    {
        Y=y
        r=r
        e=e
    }
    
let calculateFixed (model: FixedModel) =
    let e = model.e
    let is = model.IS
    let bp = model.BP
    let r = (bp.Xn.mpm*(is.A-bp.Xn.Xn_e*e) - is.MLR*(bp.Xn.Xn_a-bp.Xn.Xn_e*e+bp.KA.KA_a-bp.KA.m*bp.KA.r_f)) / (bp.KA.m*is.MLR+bp.Xn.mpm*is.I_r)
    let y = is.A-is.I_r*r-is.Xn_e*e
    {
        Y=y
        r=r
        e = e 
    }
let calculate (model:Model) =
    match model with
    | FloatingExchangeRate model -> calculateFloating model
    | FixedExchangeRate model -> calculateFixed model
    
let printResult result =
    printfn $"Y: %.2f{result.Y} r: %.2f{result.r} e: %.2f{result.e}" 
    
let configPath = __SOURCE_DIRECTORY__+ @"/params.json"
type configProvider = JsonProvider<"./params.json">

let config = configProvider.Load(configPath)

let is = {
    A=config.A
    Xn_a = config.XnA
    I_r = config.IR
    Xn_e = config.XnE
    MLR = config.Mlr |> double
}
let lm = {
    MS=config.Ms
    P = config.P
    Md_Y = config.MdY |> double
    Md_r = config.MdR
}

let bp = {
    Xn = {
        Xn_a = config.XnA
        Xn_e = config.XnE
        mpm = config.Mpm |>double
    }
    KA = {
        KA_a = config.KaA
        r_f = config.RF
        m = config.M
        
    }
}

let model =
    match config.Mode with
    | "Floating" ->
       (FloatingExchangeRate {
            IS=is
            LM=lm
            BP = bp 
        })
    | "Fixed" ->
        (FixedExchangeRate {
            e=config.E |> double
            IS=is
            BP=bp
        })
    | unknown -> failwith $"Unknown mode: '{unknown}'. Expecting either 'Floating' or 'Fixed'"
 
let result = calculate model

printResult result
    