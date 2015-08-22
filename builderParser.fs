

namespace fjson.parser
open System
open System.Collections.Generic
open fjson


open Helper
module formmater =
    let list2str (ls: char list) = String.Concat ls
    let _b = new parserHelperBuilder()
    let rec readObject = _b{
            let! _ = _b.token (_b.char '{')
            let! kvs = readKeyValues
            let! _ = _b.token ( _b.char '}')
            return (JValue.createObj kvs)  
        }
    and  readKeyValues = many (_b{
            let! kv = readkv
            let! _ = _b.select (_b.token (_b.char ',')) (_b.Return ',')
            return kv
        })
    and readkv = _b{
            let! k = _b.token _b.smartStr
            let! _ = _b.token (_b.char ':')
            let! v = _b.token readValue
            return (list2str k,v)
        }
    and readValue = _b.selects [readObject;readArray;readBool;readNumber;readString;readNull]
    and readArray = _b{
            let! _ = _b.token (_b.char '[')
            let! vs = many(_b{
                let! v = readValue
                let! _ = _b.select (_b.token (_b.chars ',')) _b.whiteSpace
                return v

            })
            let! _ = _b.token (_b.char ']')
            return (JValue.createArr vs)
        }
    and readBool = _b{
            let! r = select (token (str "true")) (token (str "false"))
            if r.Equals(List.ofSeq "true") then
                return (Bool true)
            else
                return (Bool false)
        }
    and readNumber = _b{
            let! n = _b.token _b.numbers
            return (Number (double (list2str n)))
        }
    and readString = _b{
            let! s = _b.token _b.quoteStr
            return (Str (list2str s))
        }
    and readNull = _b{
            let! _ = _b.token (_b.str "null")
            return Null
        }
    let read (str:string) =
        let r = readObject (List.ofSeq str)
        match r with
        | PR(obj,_) -> obj
        | PError e -> raise (ParserException (list2str e))