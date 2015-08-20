

namespace fjson
open System
open System.Collections.Generic
open fjson.parser.Helper
module Main =
    let main() =
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
//        let flip f x y = f y x
//        let renders = [render.simple]
//        let results = Seq.map (fun r -> r o ) renders
//        Seq.iter (fun str -> printfn "%s" str) results
        //let x = render.pretty (Obj o) 2
        //parser.Formmater.read(x) |> render.simple  |> printf "%s"
//        printfn "%s" y
//        let strs = [
//            "hello\\\"\" world";
//            "\"\"xxxyy";
//            "\"hello 123world !!\"123";
//        ]
//        let printPaser x = printfn "%A\n" x
//        Seq.iter (fun str -> printPaser (smartStr (List.ofSeq str)) )strs