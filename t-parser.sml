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

val _ = print "\n"

(*   { "a" : "A", "b" : { "b1" : "B1", "b2" : "B2" }, "c" : [ "c1", "c2" ], "d" : "D" }   *)
val s = "{ \"a\" : \"A\", \"b\" : { \"b1\" : \"B1\", \"b2\" : \"B2\" }, \"c\" : [ \"c1\", \"c2\" ], \"d\" : \"D\" }"
(*
val s = "{ \"b\" : { \"b1\" : \"B1\"}, \"c\" : [\"c1\"] }"
val s = "{ \"b\" : [\"c1\"] }"
*)
val ts = case Lexer.lex s of 
              NONE => ( print "NONE\n"; raise Json "lexer")
            | SOME (ts, t) => ( print (Lexer.show ts); print ("\nTAIL: "  ^ t ^ "\n\n"); ts )

val (j, _)  = parse ts
val _ = print (s ^ "\n")
val _ = printJSON j
