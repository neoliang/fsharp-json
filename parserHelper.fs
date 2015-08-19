

namespace fjson
open System

module parserHelper =
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
    let rec str ss =
        match ss with
        | [] -> ret []
        | x::xs ->
            cons (char x) (fun _ ->
                cons (str xs) (fun _ ->
                    ret (x::xs)
                )
            )
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
    let private digits = many1 (sat Char.IsNumber)
    let private digits1_9 = many1 (sat (fun ch -> (Char.IsNumber ch) && ch <> '0'))
    let private part1NE =
        let minusSign = select (chars '-') (ret [])
        cons minusSign (fun ch ->
            let signZero = cons (chars '0') (fun z -> ret (ch @ z))
            select signZero (cons digits1_9 (fun digs ->
                    cons digits (fun digs1 ->
                        ret (ch @ digs @ digs1)
                    )
                )
            )
        )
    let private part2NE =
        let dotDigits = 
            cons (char '.') (fun ch ->
                cons digits (fun num ->
                    ret (ch::num)
                )
            )
        select dotDigits (ret [])
    let private part3NE =
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
        let isHex ch = (Char.IsNumber ch ) && ( ('a' <= ch && ch <= 'F') || ('A' <= ch && ch <= 'F') )
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
        many1 (select specialStr (many1 (sat (fun ch-> ch <> '"' && ch <> '\\'))))
    let quoteStr = 
        cons (char '"') (fun _ ->
            cons (select unQuoteStr (ret [])) (fun xs->
                cons (char '"') (fun _ ->
                    ret xs
                )
            )
        )
    let smartStr = select quoteStr unQuoteStr