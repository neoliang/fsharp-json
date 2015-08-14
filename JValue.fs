#light
namespace fjson
open System
open System.Collections.Generic

exception JValueConvertException of string


type JValue =
  | Number of double
  | Str of string
  | Bool of bool
  | Obj of JObject
  | Arr of List<JValue>
  | Null

  member private v.getValueException (t:string) = 
    raise (JValueConvertException("can't convert " + v.ToString() + "to " + t))

  //type check
  member v.isNull() =
    match v with
    | Null -> true
    | _ -> false
  member v.isArray() =
    match v with
    | Arr _ -> true
    | _ -> false
  member v.isObject() =
    match v with
    | Obj _ -> true
    | _ -> false
  member v.isString() =
    match v with
    | Str _ -> true
    | _ -> false
  member v.isNumber() =
    match v with
    | Number _ -> true
    | _ -> false
  member v.isBool() = 
    match v with
    | Bool _ -> true
    | _ -> false

  //get values
  member v.asFloat()  =
    match v with
    | Number(x) -> float(x)
    | _ -> raise (v.getValueException "float")
  member v.asDouble() =
    match v with
    | Number(x) -> double(x)
    | _ -> raise (v.getValueException "double")
  member v.asBool() =
    match v with
    | Bool(x) -> x
    | _ -> raise (v.getValueException "bool")
  member v.asString() = 
    match v with
    | Str(x) -> x
    | _ -> raise (v.getValueException "string")
  member v.asInt() =
    match v with
    | Number(x) -> int(x)
    | _ -> raise (v.getValueException "int")
  member v.asList() =
    match v with
    | Arr x -> x
    | _ -> raise (v.getValueException "list")
  member v.asObject() =
    match v with
    | Obj x -> x
    | _ -> raise (v.getValueException "Object")
  
and JObject = Dictionary<string,JValue>