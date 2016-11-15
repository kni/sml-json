use "json.sml";


open JSON

local open Lexer in
  val ts = [StartObj, String "a", String "b", EndObj, StartObj, String "c", String "d", EndObj]
end

open Parser
val (j, ts) = parse ts
val (j, ts) = parse ts

fun printJSON j = print ((show j) ^ "\n")

val j = Object [ ("ab\"c", Array [String "A1", String "A2"] ) ]
val _ = printJSON j
