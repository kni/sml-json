use "json.sml";


open JSON

local open Lexer in
  val ts = [StartObj, String "a", String "b", EndObj, StartObj, String "c", String "d", EndObj]
end

open Parser

fun printJSON j = print ((show j) ^ "\n")

val (j, ts) = parse ts
val _ = printJSON j

val (j, ts) = parse ts
val _ = printJSON j


val j = Object [ ("ab\"c", Array [String "A1", String "A2"] ) ]
val _ = printJSON j



fun showLexerResult r = case r of NONE => () | SOME (ts, t) => ( print (Lexer.show ts); print ("\nTAIL: "  ^ t ^ "\n\n"))

(*   { "a" : "A", "b" : { "b1" : "B1", "b2" : "B2"}, "c" : ["c1", "c2"], "d" : "D" }   *)
val js = "{ \"a\" : \"A\", \"b\" : { \"b1\" : \"B1\", \"b2\" : \"B2\"}, \"c\" : [\"c1\", \"c2\"], \"d\" : \"D\" }"
val _ = showLexerResult (Lexer.lex js)
val jl = case Lexer.lex js of NONE => raise Json | SOME (jl, t) => jl

(* val (j, _)  = parse jl *)
