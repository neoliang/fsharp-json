

namespace fjson
open System
open System.Collections.Generic


module json = 
    let rec toString (v:JValue) =

        let arrToString (arr:List<JValue>) =
            let rec _toString (arr:JValue list) (str:string) = 
                match arr with
                | [] -> str
                | x :: y :: xs ->
                    let str1 = str + toString(x) + "," 
                    _toString (y::xs) str1
                | x :: xs -> 
                    let str1 = str + toString(x)
                    _toString xs str1
            "[" + ( _toString (List.ofSeq arr) "") + "]"
        
        let rec objToString (obj:JObject) =
            let keyToString (k:string) = "\"" + k + "\""
            let objstrs = Seq.map (fun (KeyValue(k,v)) -> keyToString k + ":" + toString v) obj
            let content = Seq.fold (fun (acc:string) str -> if acc.Equals("") then str else acc + "," + str) "" objstrs
            "{" + content + "}"
        match v with
        | Number(x) -> x.ToString()
        | Str(x) -> "\"" + x + "\""
        | Null -> "null"
        | Arr(x) -> arrToString x
        | Obj(x) -> objToString x
        | Bool(true) -> "true"
        | Bool(false) -> "false"

