# fsharp-json
a small json parser and formatter for F# 

#read json from string

```F#
open fjson.parser
let jobj = formmater.read   "{\"hello\": \"world\",\"neo\": 47,\"nil\": null,\"ls\": [51,\"five\",false,true,\"apple\",null,{\"ooo\": 100},\"end\",[],[55,false,{}]]}"
printfn "%A" jobj
```


#render json to string

```F#
open fjson
let o = new JObject()
o.Add("hello",Str "world")
o.Add("neo",Number 47.0)
o.Add("nil",Null)
let ss = render.pretty (Obj o) 2
let ss1 = render.simple (Obj o)
printfn "%s\n%s" ss1 ss
```


#read json from file

```F#
open fjson
let jobj = simpleIO.readFile "./test1.json"
printfn "%A" jobj
```


#write json to file

```
open fjson
let o = new JObject()
o.Add("hello",Str "world")
o.Add("neo",Number 47.0)
o.Add("nil",Null)
simpleIO.writeFile (Obj o) "./test2.json"
```
