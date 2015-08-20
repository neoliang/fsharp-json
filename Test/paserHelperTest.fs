namespace Test

open System
open System.Collections.Generic
open NUnit.Framework
open FsUnit
open fjson
open fjson.parser
open fjson.parser.Helper

type paserHelperTest() = 
    
    let mySome a b = PR (List.ofSeq a, List.ofSeq b)
    let equalSome a = equal (PR a)

    [<Test>]
    member x.testItem() = 
        item (List.ofSeq "hello") |> should equal (PR('h',List.ofSeq "ello"))
        item (List.ofSeq "") |> isNone |> should be True
        item (List.ofSeq " ") |> should equal (PR(' ',List.ofSeq ""))
    [<Test>] 
    member x.testStringParser() =
        let helloStr x = str (List.ofSeq "hello") (List.ofSeq x)
        helloStr "hello" |> should equal (mySome "hello" "")
        helloStr "hello world" |> should equal (mySome "hello" " world")
        helloStr "" |> isNone|> should be True
        helloStr "hell" |> isNone|> should be True
    [<Test>]
    member x.``test smart string `` () =
        let parserStr xs = smartStr (List.ofSeq xs)
        parserStr "helloStr" |> should equal (mySome "helloStr" "")
        parserStr "hello Str" |> should equal (mySome "hello Str" "")
        parserStr "\"xxxxyyy\"" |> should equal (mySome "xxxxyyy" "")
        parserStr "\u1234" |> should equal (mySome "\u1234" "") 
    [<Test>]
    member x. ``test token`` () =
        let pt xs = token (str "true") (List.ofSeq xs)
        pt " true" |> should equal (mySome "true" "")
        pt "true " |> should equal (mySome "true" "")
        pt " true "|> should equal (mySome "true" "")
        pt "true \n,"|> should equal (mySome "true" ",")
    [<Test>]
    member x.``test numbers`` () =
        let parserDigits xs = digits (List.ofSeq xs)
        parserDigits "123456" |> should equal (mySome "123456" "")
        parserDigits "xf12" |> isNone |> should be True
        parserDigits "012" |> should equal (mySome "012" "")
        parserDigits "0.124"|> should equal (mySome "0" ".124")
        let paserPart1 xs = part1NE (List.ofSeq xs)
        paserPart1 "-" |> isNone |> should be True
        paserPart1 "-0"|> should equal (mySome "-0" "")
        paserPart1 "0"|> should equal (mySome "0" "")
        paserPart1 "-01" |> should equal (mySome "-0" "1")
        //paserPart1 "-12345" |> should equal (mySome "-12345" "")
        //paserPart1 "-12032" |> should equal (mySome "-12032" "")
        paserPart1 "0" |> should equal (mySome "0" "")
        paserPart1 "123" |> should equal (mySome "123" "")
        paserPart1 "123." |> should equal (mySome "123" ".")
        paserPart1 "0.12" |> should equal (mySome "0" ".12")
        paserPart1 "-0.23" |> should equal (mySome "-0" ".23")
        let paserPart2 xs = part2NE (List.ofSeq xs)
        paserPart2 "" |> should equal (mySome "" "")
        paserPart2 "." |> should equal (mySome "" ".")
        paserPart2 ".1" |> should equal (mySome ".1" "")
        paserPart2 ".0" |> should equal (mySome ".0" "")
        paserPart2 ".12345e" |> should equal (mySome ".12345" "e")
        let paserPart3 xs = part3NE (List.ofSeq xs)
        paserPart3 "" |> should equal (mySome "" "")
        paserPart3 "e" |> should equal (mySome "" "e")
        paserPart3 "E" |> should equal (mySome "" "E")
        paserPart3 "E+" |> should equal (mySome "" "E+") 
        paserPart3 "E+123"|> should equal (mySome "E+123" "")
        paserPart3 "e+0" |> should equal (mySome "e+0" "")
        paserPart3 "E-01"|> should equal (mySome "E-01" "")

        let paserNumber xs = numbers (List.ofSeq xs)
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
        let parserSpace xs = space (List.ofSeq xs)
        parserSpace "" |> should equal (mySome "" "")
        parserSpace "    " |> should equal (mySome "    " "")
        parserSpace "  \n,abcd" |> should equal (mySome "  \n" ",abcd")
        parserSpace "    aa"|> should equal (mySome "    " "aa")
    [<Test>]
    member x.``test many``() =
        let parserMany xs = many (char 'x') (List.ofSeq xs)
        let parserChar xs = char 'x' (List.ofSeq xs)
        parserChar "xy" |> should equal (PR ('x', List.ofSeq "y"))
        parserMany "y" |> should equal (mySome "" "y")
        parserMany "xxxyyy" |> should equal (mySome "xxx" "yyy")
        parserMany "hello" |> should equal (mySome "" "hello")
        parserMany "xxxx" |> should equal (mySome "xxxx" "")
        parserMany "" |> should equal (mySome "" "")

    [<Test>]
    member x.``test hexaDecimal``() =
        let parserHex  xs = hexaDecimal (List.ofSeq xs)
        parserHex "1234fx" |> should equal (mySome "1234" "fx")
        parserHex "ffeeff" |> should equal (mySome "ffee" "ff")
        parserHex "AaC2kd22" |> should equal (mySome "AaC2" "kd22")
        parserHex "h12wee" |> isNone |> should be True 
    [<Test>]
    member x.``test Select and Selects`` () =
        let f = ret []
        (select f failure) (List.ofSeq "") |> should equal (mySome [] "")
        (select failure f) (List.ofSeq "hee") |> should equal (mySome [] "hee")
        let selectMany = selects [chars '"'; chars '\\'; chars 'f'; chars 'b'; chars 'f'; chars 'n'; chars 'r'; chars 't'; quoteHexDecimal]
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