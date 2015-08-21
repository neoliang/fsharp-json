// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open System.Collections.Generic
open fjson
open fjson.parser
open fjson.parser.Formmater
open fjson.parser.Helper
[<EntryPoint>]
let main argv = 

    let ``test read`` () =
        let o = new JObject()
        o.Add("hello",Str "world")
        o.Add("neo",Number 47.0)
        o.Add("nil",Null)
        let ls = new List<JValue>()
        ls.Add(Number 51.0)
        ls.Add(Str "five")
        ls.Add(Bool false)
        ls.Add(Bool true)
        ls.Add(Str "apple")
        ls.Add(Null)
        let o1 = new JObject()
        o1.Add("ooo",Number 100.0)
        ls.Add(Obj o1)
        ls.Add(Str "end")
        let ls1 = new List<JValue>()
        ls1.Add(Number 55.0)
        ls1.Add(Bool false)
        ls.Add(Arr (new List<JValue>()))
        ls1.Add(Obj (new JObject()))
        ls.Add(Arr ls1)
        o.Add("ls",Arr ls)
        let ss = render.pretty (Obj o) 2
        let xx = read ss
        let yy = render.pretty xx 4
        //let xx =  readBool (List.ofSeq "true") //|> should  equal (Some (Bool true ,List.ofSeq "") )
        printfn "%s \n %s" ss  yy
        let builder = new parser.parserHelperBuilder()
        let parserHex  xs = builder.hexaDecimal (List.ofSeq xs)
        let y = parserHex "1234fx"
        let i = builder.char 'h' (List.ofSeq "xhello")
        let builder = new parser.parserHelperBuilder()
        let x = builder.Return(PError (List.ofSeq "hello")) "xx"
        printfn "%A\n%A\n%A" y i x

    ``test read``()
    0
