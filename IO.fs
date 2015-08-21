

namespace fjson
open System
open System.IO
open fjson.parser
open fjson.render
module simpleIO =
    let readFile (f:string) = 
        use sr = new StreamReader(f)
        let content = sr.ReadToEnd()
        formmater.read content
    let writeFile (v:JValue) (f:string) =
        let str = simple v
        use sw = new StreamWriter(f)
        sw.Write str
