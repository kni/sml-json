structure JSON =
struct

  exception Json

  datatype Value = Object of (string * Value) list 
                 | Array of Value list
                 | String of string
                 (*
                 | Number of IEEEReal.decimal_approx
                 | Int of int
                 | Real of real
                 | True
                 | False 
                 | Null
                 ToDo *)

  structure Lexer =
  struct
    datatype Token = StartObj
                   | EndObj
                   | StartArr
                   | EndArr
                   | String  of string
                   (*
                   | Number of IEEEReal.decimal_approx
                   | True
                   | False 
                   | Null
                   ToDo *)
  end

  structure Parser =
  struct
    structure L = Lexer
    local

      fun ht []      = raise Json
        | ht (x::xs) = (x, xs)
      
      
      fun parse (ts:L.Token list) : (Value * L.Token list) =
        let
          val (k, ts) = ht ts
        in
          case k of
               L.StartObj => let val (v, ts) = parseObj ts in ( (Object v), ts) end
             | L.StartArr => let val (v, ts) = parseArr ts in ( (Array v),  ts) end
             | L.String s => ( (String s), ts)
             | _        => raise Json
        end
      
      
      and parseObj (ts:L.Token list) : ((string * Value) list * L.Token list) =
        let
          val (k, ts) = ht ts
        in
          if k = L.EndObj then ([], ts) else
          let 
            val k = case k of L.String s => s | _ =>  raise Json 
            val (v, ts) = parse ts
          in
            if null ts
            then ((k, v)::[], ts)
            else let val (z, ts) = parseObj(ts) in ((k, v)::z, ts) end
          end
        end
      
      
      and parseArr (ts:L.Token list) : (Value list * L.Token list) =
        let
          val (v, _) = ht ts
        in
          if v = L.EndArr then ([], ts) else
          let
            val (v, ts) = parse ts
          in
            if null ts
            then (v::[], [])
            else let val (z, ts) = parseArr(ts) in (v::z, ts) end
          end
        end

    in
      val parse = parse
    end
  end

end


open JSON

val s = String "s"
val l = Lexer.String "s"

local open Lexer in
val ts = [StartObj, String "a", String "b", EndObj, StartObj, String "c", String "d", EndObj]
end

open Parser
val (j, ts) = parse ts
val (j, ts) = parse ts

val _ = print "The End\n"
