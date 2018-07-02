use "json.sml";

open JSON

local open Lexer in
  val ts = [StartObj, String "a", String "b", EndObj, StartObj, String "c", String "d", EndObj]
end

open Parser

fun printJSON j = print ((encode j) ^ "\n")

val (j, ts) = parse ts
val _ = printJSON j

val (j, ts) = parse ts
val _ = printJSON j
val _ = print "\n"

val s = "{ \"a\" : \"A\", \"b\" : { \"b1\" : \"B1\", \"b2\" : \"B2\" }, \"c\" : [ \"c1\", \"c2\" ], \"d\" : \"D\" }"
val _ = print (s ^ "\n")
val j = decode s
val _ = printJSON j


val s = "[ { \"a\" : \"A\" }, { \"b\" : { \"b1\" : \"B1\", \"b2\" : \"B2\" }, \"c\" : [ \"c1\", \"c2\" ], \"d\" : \"D\" } ]"
val _ = print (s ^ "\n")
val j = decode s
val _ = printJSON j
val _ = print "\n"


val s = "{ \"a\" : \"A\", \"b\" : { \"b1\" : \"B1\" }, \"c\" : [ \"c1\" ] }"
val _ = print (s ^ "\n")
val _ = print (show (decode s))
val _ = print "\n"

val j = Object [("a", String "A"), ("b", Object [("b1", String "B1")]), ("c", Array [String "c1"])]
val _ = printJSON j
val _ = print "\n"
