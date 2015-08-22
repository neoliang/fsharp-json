
namespace fjson.parser
open System



type parserHelperBuilder() = 
    member this.Delay(f) =
        f()
    member this.Failure() = fun inp -> 
        PError inp
    member this.Zero() = 
        this.Failure()
    member this.ReturnFrom(x) = x
    member this.Return(x) =
        fun inp -> PR(x,inp)
    member this.Bind(ma,amb) = fun inp ->
        match ma inp with
        |PError e -> PError e
        |PR (v,out) -> (amb v) out
    member this.item = fun inp ->
        match inp with
        |[] -> PError inp
        | x::xs -> PR (x,xs)
    member this.sat f = 
        this{
            let! x = this.item
            if f x then 
                return x 
            else 
                return! this.Failure()
        }
    member this.char ch = this.sat (fun x -> x = ch)
    member this.chars ch = fun inp ->
        let r = this.char ch inp
        match r with
        |PR(x,xs) -> PR([x],xs)
        |PError e -> PError e
    member this.selects fs = 
        let rec _selects fs = fun (inp:char list) ->
            match fs with
            | [] -> PError inp
            | x::xs ->
                match x inp with
                |PR(v,out) -> PR(v,out)
                |PError _ -> _selects xs inp
        _selects fs
    member this.select x y = this.selects [x;y]
    member this.many1 f = 
        let rec _many1 f =
            this{
                let! v = f
                let! vs = _many f
                return (v::vs)
            }
        and _many f =
            this.select (_many1 f) (this.Return [])
        _many1 f
    member this.many f =
        this.select (this.many1 f) (this.Return [])
    member this.whiteSpace = 
        this.many (this.sat (fun ch -> ch = ' ' || ch = '\n' || ch = '\t'))
    member this.digits = this.many1 (this.sat Char.IsNumber)
    member this.digit1_9 = this.sat (fun ch -> (Char.IsNumber ch) && ch <> '0')
    member this.numbers =
        let minusSign = this.select (this.chars '-') (this.Return [])
        let part1 = this{
            let! ch = minusSign
            let! p11 = this.select (this.chars '0') (this{
                let! dig = this.digit1_9
                let! digs1 = this.digits
                return ([dig] @ digs1)
                })
            return (ch@p11)
        }
        let part2 = 
            let dotDigits = this{
                let! ch = this.char '.'
                let! num = this.digits
                return (ch::num)
            }
            this.select dotDigits (this.Return [])
        let part3 =
            let eExpr = this.select (this.char 'e') (this.char 'E')
            let signExp = this.select (this.char '+') (this.char '-')
            let eParts = this{
                let! e = eExpr
                let! s = signExp
                let! digs = this.digits
                return (e :: s :: digs)   
            }
            this.select eParts (this.Return [])
        this{
            let! p1s = part1
            let! p2s = part2
            let! p3s = part3
            return (p1s @ p2s @ p3s)
        }
    member this.token p =
        this{
            let! _ = this.whiteSpace
            let! r = p
            let! _ = this.whiteSpace
            return r
        }
    member this.str ss =
        let rec _str ss = this{
            match ss with
            |[] -> return []
            |x::xs ->
            let! _ = this.char x
            let!_ = _str xs
            return (x::xs)
        }
        _str (List.ofSeq ss)    
    member this.repeat n f =
        let rec _repeat n f rs =
            this{
                if n<=0 then
                    return rs
                else
                    let! v = f
                    let! rs1 = _repeat (n-1) f (rs@[v])
                    return rs1
            }
        _repeat n f []
    member this.hexaDecimal = 
        let isHex ch = (Char.IsNumber ch ) || ( ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F') )
        this.repeat 4 (this.sat isHex)
    member this.quoteHexDecimal =
        this{
            let! ch = this.char 'u'
            let! hexs = this.hexaDecimal
            return ([ch] @ hexs)
        }
    member this.unQuoteStr =
        let pp = [this.chars '"'; this.chars '\\'; this.chars 'f'; this.chars 'b'; this.chars 'f'; this.chars 'n'; this.chars 'r'; this.chars 't'; this.quoteHexDecimal]
        let specialStr = this{
            let! ch = this.char '\\'
            let! cs = this.selects pp
            return ([ch] @ cs)
        }
        let oneChars = this{
            let! ch = this.sat (fun ch-> ch <> '"' && ch <> '\\')
            return [ch]
        }
        this{
            let! rs = this.many1 (this.select specialStr oneChars)
            return (List.ofSeq (Seq.concat rs))
        }
    member this.quoteStr = 
        this{
            let! _ = this.char '"'
            let! xs = this.select this.unQuoteStr (this.Return [])
            let! _ = this.char '"'
            return xs 
        }

    member this.smartStr = this.select this.quoteStr this.unQuoteStr