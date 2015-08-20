

namespace fjson
open System
open System.Collections.Generic

type RenderTemplate =
    abstract sep : unit -> string
    abstract needNewLine: unit->bool
    abstract accNumber : double -> string
    abstract accString : string -> string
    abstract accBool : bool -> string
    abstract accNull: unit->string
    abstract accKey : string -> string
    abstract beginList : unit -> string
    abstract endList : unit -> string
    abstract beginObject: unit -> string
    abstract endObject: unit -> string

module private RenderHelper =
    let  private concatStr (strSeq : seq<string>) (c:string) (needNewLine:bool) = 
        let endSep = if needNewLine then "\n" else ""
        let mixStr = fun acc s -> 
            if acc.Equals("") then 
                s 
            else 
                acc + c + endSep  + s
        let mixedStr = Seq.fold mixStr "" strSeq
        if not (mixedStr.Equals("")) then
            mixedStr + endSep
        else
            mixedStr
    let rec  render_ (this:JValue) (accept:RenderTemplate)  = 
      match this with
        | Number v -> accept.accNumber v
        | Str v -> accept.accString v
        | Null  -> accept.accNull() 
        | Bool v -> accept.accBool v
        | Arr ls ->
            let bStr = accept.beginList()
            let lr = Seq.map (fun (v:JValue) ->  render_ v accept) ls
            bStr + (concatStr lr "," (accept.needNewLine())) + accept.endList()
        | Obj o ->
            let renderKV (KeyValue(k,v:JValue)) = 
                let kStr = accept.accKey k 
                let vStr = render_ v accept
                kStr + ": " + vStr
            let bStr = accept.beginObject()
            let lo = Seq.map renderKV o
            let lostr = concatStr lo (accept.sep()) (accept.needNewLine())
            bStr + lostr + accept.endObject()
    
type SimpleRender() =
    interface RenderTemplate with
        member this.accNumber x = string(x)
        member this.accBool x = if x then "true" else "false"
        member this.accString x = "\"" + x + "\""
        member this.accNull() = "null"
        member this.accKey key = "\"" + key + "\""
        member this.beginList() = "["
        member this.endList() = "]"
        member this.beginObject() = "{"
        member this.endObject() = "}"
        member this.sep() = ","
        member this.needNewLine() = false


type private RenderState =
    | RS_Array
    | RS_Object
type PrettyRender(indent:int) =
    let _simpleRender:RenderTemplate = new SimpleRender() :> RenderTemplate
    let mutable _currentPos = 0
    let _renderState = new Stack<RenderState>()
    let pushState s = _renderState.Push(s)
    let popState() =
        _renderState.Pop() |> ignore

    let getRenderState() = _renderState.Peek()
    let _indent = indent
    do
        _renderState.Push(RS_Object)
    
    member private this.currentSpace =
        let mutable str = ""
        for i in 1 .. _currentPos do
            str <- str + " " 
        str
    member private this.addIndent f =
        match getRenderState() with
            | RS_Array -> this.currentSpace + f()
            | _ -> f()
    
    interface RenderTemplate with
        member this.accNumber x =
            this.addIndent (fun() -> _simpleRender.accNumber x)
        member this.accBool x = 
            this.addIndent (fun() -> _simpleRender.accBool x)
        member this.accString x =
            this.addIndent (fun()-> _simpleRender.accString x)
        member this.accNull() = 
            this.addIndent (fun()-> _simpleRender.accNull() )
        member this.accKey key = this.currentSpace + _simpleRender.accKey key
        member this.beginList() =
            let r = this.addIndent(fun() -> _simpleRender.beginList() + "\n")
            pushState RS_Array
            _currentPos <- _currentPos + _indent
            r
        member this.endList() =
            _currentPos <- _currentPos - _indent
            popState() 
            this.currentSpace + _simpleRender.endList()

        member this.beginObject() = 
            let r = this.addIndent(fun()-> _simpleRender.beginObject() + "\n")
            pushState RS_Object
            _currentPos <- _currentPos + _indent
            r
        member this.endObject() = 
            _currentPos <- _currentPos - _indent
            popState()  
            this.currentSpace +  _simpleRender.endObject()
        member this.sep() = ","
        member this.needNewLine() = true

module render =
    let custom o (template:RenderTemplate) = RenderHelper.render_  o template
    let simple o = custom o (new SimpleRender())
    let pretty o indent = custom o (new PrettyRender(indent))