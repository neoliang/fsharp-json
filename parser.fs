

namespace fjson.parser
open System
open System.Collections.Generic
open fjson

exception ParserException of string

open Helper
module Formmater =
    let rec readObject = 
        let parserObject = cons (token (char '{')) (fun _->
                cons readKeyValues (fun kvs->
                    cons (token (char '}')) (fun _->
                        ret kvs
                    )
                )
            )
        cons parserObject (fun kvs ->
            let addKV (obj:JObject) (k,v) = 
                    obj.Add(k,v)
                    obj
            ret (Obj (Seq.fold addKV (new JObject()) kvs))
        )
    and  readKeyValues = 
        let readkv = cons (token smartStr)(fun k->
            cons (token (char ':')) (fun _->
                cons (token (readValue)) (fun v->
                    ret (string(k),v)
                )
            )
        )
        many (cons readkv (fun kv ->
            cons (select (token (char ',')) (ret ',')) (fun _->
                ret kv
            )
        ))
    and readValue = selects [readObject;readArray;readBool;readNumber;readString;readNull]
    and readArray = 
        let parserArray = cons (token (char '[')) (fun _ ->
            many (cons readValue (fun v ->
                cons (select (token (chars ',')) space) (fun _ ->
                        ret v
                    )

                )
            )
        )
        cons parserArray (fun vs ->
            ret (Arr (List vs))
        )
    and readBool = cons (select (token (str "true")) (token (str "false"))) (fun r->
            if r.Equals("true") then
                ret (Bool true)
            else
                ret (Bool false)
        )
    and readNumber = cons (token numbers) (fun n ->
            ret (Number (double (string n)))
        )
    and readString = cons (token quoteStr) (fun s->
            ret (Str (string s))
        )
    and readNull = cons (token (str "null")) (fun _ ->
            ret Null
        )
    let read (str:string) =
        let r = readObject (List.ofSeq str)
        match r with
        | Some(obj,_) -> obj
        | _ -> raise (ParserException("parser error"))
module simple = 
    let private createParserExceptoin str = ParserException(str)
    let private matchCh (ch:char) = fun x -> 
        if x = ch then 
            ()
        else
            raise (createParserExceptoin("expect " + ch.ToString()))
    let rec spacesMatch str =
        match str with
        | [] -> str
        | x::xs ->
            if x = ' ' || x = '\n' then
                spacesMatch xs
            else
                x::xs

    let private matchStr target str =
        let rec _matchStr str xs =
            match str with
            |[] -> ((),xs)
            |s::str1 ->
                match xs with
                | x::xs1 ->
                    matchCh x s
                    _matchStr str1 xs1
                | _-> raise (createParserExceptoin("expect" + s.ToString()))
        let (_,remain) = _matchStr target str
        (target,remain)
    let rec private readObject (stro:char list) =
        let str = spacesMatch stro
        match str with
        |x::y::xs ->
            matchCh '{' x
            if y = '}' then
                (new JObject(),xs)
            else
                let (kvs,r::rStr) = readKeyValues (y::xs)
                matchCh '}',r
                let addKV (obj:JObject) (k,v) = 
                    obj.Add(k,v)
                    obj
                let obj = Seq.fold addKV (new JObject()) kvs
                (obj,rStr)
        |_ -> raise (createParserExceptoin("parser object error"))
    and private readKeyValue str =
        let (k,ch::rStr) = readString str
        matchCh ':' ch
        let (v,rStr1) = readValue rStr
        ((k,v),rStr1)
    and readKeyValues str =
        let rec _readKeyValues str (kvs: (string*JValue) list) =
            let ((k,v),ch::rStr) = readKeyValue str
            if ch = ',' then
                _readKeyValues rStr (kvs @ [(k,v)])
            else
                (kvs @ [(k,v)],ch::rStr)
        _readKeyValues str []
    and private readValue stro =
        let str = spacesMatch stro
        match str with
        | [] -> raise (createParserExceptoin("read value error"))
        | x::xs ->
            match x with
            | '{' ->
                let (obj,rStr) = readObject str
                (Obj obj,rStr)
            | '[' ->readArray str
            | '\"'->
                let (r,rStr) = readString str
                (Str r, rStr)
            | '-' ->readNumber str
            | 't'
            | 'f'-> readBool str
            | 'n' -> readNull str
            | _ ->
                if Char.IsNumber x then
                    readNumber str
                else
                    raise (createParserExceptoin("read value error"))
    and private readArray str =
        match str with
        | x::y::xs ->
            matchCh '[' x
            if y = ']' then
                (Arr (new List<JValue>()),xs)
            else
                let rec _readArray str arr =
                    let (v,ch::rStr) = readValue str
                    if ch = ',' then
                        _readArray rStr (arr @[v])
                    else
                        (arr @[v],ch::rStr)
                let (vs,ch1::rStr1) = _readArray (y::xs) []
                matchCh ']' ch1
                (Arr (List(vs)),rStr1)
        |_ -> raise (createParserExceptoin("paser arry error"))
    and private readString str =
        let rec _parserString str (r:string) =
            match str with
                |[] -> raise (createParserExceptoin("paser string error"))
                | x::xs -> 
                    if Char.IsLetterOrDigit x then
                        _parserString xs (r + x.ToString())
                    else
                        (r,str)
        match str with
            |[] -> raise (createParserExceptoin("parser string error"))
            | x::xs ->
                matchCh '\"' x
                let (r,ch::rStr) = _parserString xs ""
                matchCh '\"' ch
                (string(r),rStr)
    and private readNull str =
        let (_,rStr) = matchStr (List.ofSeq("null")) str
        (Null,rStr)
    and private readNumber str =
        let rec _paserNumber str (r:string) =
            match str with
            | []-> raise (createParserExceptoin("paser number error"))
            | x::xs ->
                if Char.IsDigit x then
                    _paserNumber xs (r + x.ToString())
                else
                    (r,x::xs)
        let (r,rStr) = _paserNumber str ""
        (Number (double (string(r))),rStr)
    and private readBool str =
        try
            let (_,rStr) = matchStr (List.ofSeq("true")) str
            (Bool true,rStr)
        with 
            |ParserException e ->
                let (_,rStr) = matchStr (List.ofSeq("false")) str
                (Bool false,rStr)

    let read (str:string) =
        let (obj,_) = readObject (List.ofSeq str)
        obj