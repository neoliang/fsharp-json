

namespace Test
open System
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open fjson
open fjson.parser
open fjson.parser.Helper

type ParserHelperBuilderTest() = 
    let mySome a b = PR (List.ofSeq a, List.ofSeq b)
    let equalSome a = equal (PR a)
    let builder = new parserHelperBuilder()
    [<Test>]
    member x.testItem() = 
        builder.item (List.ofSeq "hello") |> should equal (PR('h',List.ofSeq "ello"))
        builder.item (List.ofSeq "") |> isNone |> should be True
        builder.item (List.ofSeq " ") |> should equal (PR(' ',List.ofSeq ""))
        builder.str (List.ofSeq "world") (List.ofSeq "hello") |> isNone |> should be True
    [<Test>] 
    member x.testStringParser() =
        let helloStr x = builder.str (List.ofSeq "hello") (List.ofSeq x)
        helloStr "hello" |> should equal (mySome "hello" "")
        helloStr "hello world" |> should equal (mySome "hello" " world")
        helloStr "" |> isNone|> should be True
        helloStr "hell" |> isNone|> should be True
    [<Test>]
    member x.``test smart string `` () =
        let parserStr xs = builder.smartStr (List.ofSeq xs)
        parserStr "helloStr" |> should equal (mySome "helloStr" "")
        parserStr "hello Str" |> should equal (mySome "hello Str" "")
        parserStr "\"xxxxyyy\"" |> should equal (mySome "xxxxyyy" "")
        parserStr "\u1234" |> should equal (mySome "\u1234" "") 
    [<Test>]
    member x. ``test token`` () =
        let pt xs = builder.token (str "true") (List.ofSeq xs)
        pt " true" |> should equal (mySome "true" "")
        pt "true " |> should equal (mySome "true" "")
        pt " true "|> should equal (mySome "true" "")
        pt "true \n,"|> should equal (mySome "true" ",")
    [<Test>]
    member x.``test numbers`` () =
        let parserDigits xs = builder.digits (List.ofSeq xs)
        parserDigits "123456" |> should equal (mySome "123456" "")
        parserDigits "xf12" |> isNone |> should be True
        parserDigits "012" |> should equal (mySome "012" "")
        parserDigits "0.124"|> should equal (mySome "0" ".124")
        let paserNumber xs = builder.numbers (List.ofSeq xs)
        paserNumber "12345" |> should equal (mySome "12345" "")
        paserNumber "-0.10" |> should equal (mySome "-0.10" "")
        paserNumber "-10" |> should equal (mySome "-10" "")
        paserNumber "-" |> isNone |> should be True
        paserNumber "0" |> should equal (mySome "0" "")
        paserNumber "-0" |> should equal (mySome "-0" "")
        paserNumber "-012" |> should equal (mySome "-0" "12")
        paserNumber "-51.33" |> should equal (mySome "-51.33" "")
        paserNumber "12e-3" |> should equal (mySome "12e-3" "")
        paserNumber "12e+03" |> should equal (mySome "12e+03" "")
        paserNumber "12e+03," |> should equal (mySome "12e+03" ",")
        paserNumber "xz12e+03"|> isNone |> should be True
        paserNumber "ff+03"|> isNone |> should be True
        paserNumber "11+03" |> should equal (mySome "11" "+03")
    [<Test>] 
    member x.``test space`` () =
        let parserSpace xs = builder.whiteSpace (List.ofSeq xs)
        parserSpace "" |> should equal (mySome "" "")
        parserSpace "    " |> should equal (mySome "    " "")
        parserSpace "  \n,abcd" |> should equal (mySome "  \n" ",abcd")
        parserSpace "    aa"|> should equal (mySome "    " "aa")
    [<Test>]
    member x.``test many``() =
        let parserMany xs = builder.many (char 'x') (List.ofSeq xs)
        let parserChar xs = builder.char 'x' (List.ofSeq xs)
        parserChar "xy" |> should equal (PR ('x', List.ofSeq "y"))
        parserMany "y" |> should equal (mySome "" "y")
        parserMany "xxxyyy" |> should equal (mySome "xxx" "yyy")
        parserMany "hello" |> should equal (mySome "" "hello")
        parserMany "xxxx" |> should equal (mySome "xxxx" "")
        parserMany "" |> should equal (mySome "" "")

    [<Test>]
    member x.``test hexaDecimal``() =
        let parserHex  xs = builder.hexaDecimal (List.ofSeq xs)
        parserHex "1234fx" |> should equal (mySome "1234" "fx")
        parserHex "ffeeff" |> should equal (mySome "ffee" "ff")
        parserHex "AaC2kd22" |> should equal (mySome "AaC2" "kd22")
        parserHex "h12wee" |> isNone |> should be True 
    [<Test>]
    member x.``test Select and Selects`` () =
        let f = builder.Return []
        (builder.select f failure) (List.ofSeq "") |> should equal (mySome [] "")
        (builder.select failure f) (List.ofSeq "hee") |> should equal (mySome [] "hee")
        let selectMany = builder.selects [chars '"'; chars '\\'; chars 'f'; chars 'b'; chars 'f'; chars 'n'; chars 'r'; builder.chars 't'; builder.quoteHexDecimal]
        let mParser xs = selectMany  (List.ofSeq xs)
        mParser "\"abcd" |> should equal (mySome "\"" "abcd")
        mParser "\\" |> should equal (mySome "\\" "")
        mParser "u12" |> isNone |> should be True
        mParser "b"   |> should equal (mySome "b" "")
        mParser "f"    |> should equal (mySome "f" "")
        mParser "n" |> should equal (mySome "n" "")
        mParser "r" |> should equal (mySome "r" "")
        mParser "t" |> should equal (mySome "t" "")
        mParser "u1234" |> should equal (mySome "u1234" "")
        mParser "uff38aa" |> should equal (mySome "uff38" "aa")
        mParser "u12Fe" |> should equal (mySome "u12Fe" "")
        mParser "uhelloStr" |> isNone |> should be True
        mParser "1234" |> isNone |> should be True
        mParser "uh1234" |> isNone |> should be True