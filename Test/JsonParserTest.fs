

namespace Test
open System
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open fjson
open fjson.parser
open fjson.parser.Helper
open fjson.parser.Formmater
type JsonParserTest() = 

    let  _c r ls = r (List.ofSeq ls)
    [<Test>]
    member x. `` read array `` () = 
        let ra = _c readArray
        ra "[]" |> should equal ( PR (Arr (new List<JValue>()),List.ofSeq ""))
    [<Test>]
    member x. ``read simple value`` () =
        let rb = _c readBool 
        rb "true" |> should  equal (PR (Bool true ,List.ofSeq "") )
        rb "false" |> should equal (PR (Bool false ,List.ofSeq ""))
        rb "True" |> isNone |> should be True
        rb "aaa" |> isNone |> should be True
        let rs = _c readString
        rs "  \"hello\"" |> should equal (PR (Str "hello" ,List.ofSeq ""))
        rs "\"hello\"," |> should equal (PR (Str "hello" ,List.ofSeq ","))
    [<Test>]
    member x.``test read`` () =
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
        let ss = render.simple (Obj o)
        read ss |> should be (sameAs (Obj o))