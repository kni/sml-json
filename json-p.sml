(* Парсер на основе списка токенов *)

datatype token = StartObj | EndObj | StartArr | EndArr | Colon | String of string

datatype jValue = jObject of (string * jValue) list 
               | jArray of jValue list
               | jString of string
               | jNumber of IEEEReal.decimal_approx
               | jTrue
               | jFalse 
               | jNull


val ts = [StartObj, String "a", Colon, String "b", EndObj, StartObj, String "c", Colon, String "d", EndObj]
val ts = [StartObj, String "a", String "b", EndObj, StartObj, String "c", String "d", EndObj]


exception Json

fun ht []      = raise Json
  | ht (x::xs) = (x, xs)


fun parse (ts:token list) : (jValue * token list) =
let
  val (k, ts) = ht ts
in
  case k of
       StartObj => let val (v, ts) = parseObj ts in ( (jObject v), ts) end
     | StartArr => let val (v, ts) = parseArr ts in ( (jArray v),  ts) end
     | String s => ( (jString s), ts)
     | _        => raise Json
end


and parseObj (ts:token list) : ((string * jValue) list * token list) =
let
  val (k, ts) = ht ts
in
  if k = EndObj then ([], ts) else
  let 
    val k = case k of String s => s | _ =>  raise Json 
    val (v, ts) = parse ts
  in
    if null ts
    then ((k, v)::[], ts)
    else let val (z, ts) = parseObj(ts) in ((k, v)::z, ts) end
  end
end


and parseArr (ts:token list) : (jValue list * token list) =
let
  val (v, _) = ht ts
in
  if v = EndArr then ([], ts) else
  let
    val (v, ts) = parse ts
  in
    if null ts
    then (v::[], [])
    else let val (z, ts) = parseArr(ts) in (v::z, ts) end
  end
end



val (j, ts) = parse ts
val (j, ts) = parse ts

val _ = print "The End\n"
