﻿

namespace fjson.parser
open System

module Helper =
    let item inp =
        match inp with
        |[] -> None
        | x::xs -> Some (x,xs)
    let ret x = fun inp -> Some(x,inp)
    let failure = fun inp -> None
    let cons x y = fun inp ->
        match x inp with
        |None -> None
        |Some(a,out) ->  (y a) out
    let sat f = 
        cons item (fun x ->
            if f x then
                ret x
            else
                failure
        ) 
    let char ch = sat (fun x -> x = ch)
    let chars ch = fun inp ->
        let r = char ch inp
        match r with
        |Some(x,xs) -> Some([x],xs)
        |None -> None
    let str ss =
        let rec _str ss =
            match ss with
            | [] -> ret []
            | x::xs ->
                cons (char x) (fun _ ->
                    cons (_str xs) (fun _ ->
                        ret (x::xs)
                    )
                )
        _str (List.ofSeq ss)
    let select x y = fun inp ->
        match x inp with
        | None -> y inp
        | Some(v,out) -> Some(v,out)
    let rec selects fs = fun (inp:char list) ->
        match fs with
        | [] -> None
        | x::xs ->
            match x inp with
            |Some(v,out) -> Some(v,out)
            |None -> selects xs inp
    
    let repeat n f =
        let rec _repeat n f rs =
            if n <= 0 then
                ret rs
            else
                cons f (fun v->
                    _repeat (n-1) f (rs@[v])
                )
        _repeat n f []
    let rec many f =
        select (many1 f) (ret [])
    and many1 f =
        cons f (fun v ->
            cons (many f) (fun vs ->
                ret (v::vs)
            )
        )
    let space = many (sat (fun ch -> ch = ' ' || ch = '\n'))
    let digits = many1 (sat Char.IsNumber)
    let digit1_9 = sat (fun ch -> (Char.IsNumber ch) && ch <> '0')
    let part1NE =
        let minusSign = select (chars '-') (ret [])
        cons minusSign (fun ch ->
            let part11 = select (chars '0') (cons digit1_9 (fun dig ->
                    cons digits (fun digs1 ->
                        ret ([dig] @ digs1)
                    )
                )
            )
            cons part11 (fun p11 ->
                ret (ch @ p11)
            )
        )
    let part2NE =
        let dotDigits = 
            cons (char '.') (fun ch ->
                cons digits (fun num ->
                    ret (ch::num)
                )
            )
        select dotDigits (ret [])
    let part3NE =
        let eExpr = select (char 'e') (char 'E')
        let signExp = select (char '+') (char '-')
        let eParts = cons eExpr (fun e->
            cons signExp (fun s ->
                cons digits (fun digs ->
                    ret (e :: s :: digs)    
                )
            )
        )
        select eParts (ret [])
    let numbers = cons part1NE (fun p1s ->
        cons part2NE (fun p2s ->
            cons part3NE (fun p3s ->
                ret (p1s @ p2s @ p3s)
            )
        )
    )
    let token p = cons space (fun _ ->
            cons p (fun r ->
                    cons space (fun _ ->
                        ret r
                    )
            )
        )
    let hexaDecimal = 
        let isHex ch = (Char.IsNumber ch ) || ( ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F') )
        repeat 4 (sat isHex)
    let quoteHexDecimal =
        cons (char 'u') (fun ch ->
            cons hexaDecimal (fun hexs ->
                ret ([ch] @ hexs)
            )
        )
    let unQuoteStr =
        let pp = [chars '"'; chars '\\'; chars 'f'; chars 'b'; chars 'f'; chars 'n'; chars 'r'; chars 't'; quoteHexDecimal]
        let specialStr =
            cons (char '\\') (fun ch ->
                    cons (selects pp) (fun cs->
                        ret ([ch] @ cs)
                    )
            )
        let oneChars = cons (sat (fun ch-> ch <> '"' && ch <> '\\')) (fun ch ->
                ret [ch]
            )
        cons (many1 (select specialStr oneChars)) (fun rs ->
            ret (List.ofSeq (Seq.concat rs))
        )
    let quoteStr = 
        cons (char '"') (fun _ ->
            cons (select unQuoteStr (ret [])) (fun xs->
                cons (char '"') (fun _ ->
                    ret xs
                )
            )
        )
    let smartStr = select quoteStr unQuoteStr