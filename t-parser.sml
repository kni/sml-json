use "json.sml";

open JSON

local open Lexer in
  val ts = [StartObj, String "a", String "b", EndObj, StartObj, String "c", String "d", EndObj]
end


fun is g e name =
  if String.compare (g, e) = General.EQUAL
  then print ("OK: " ^ name ^ "\n")
  else print ("NOT OK: " ^ name ^ " (got: '" ^ g ^ "', expected: '" ^ e ^ "')\n")


open Parser


val (j, ts) = parse ts
val str = encode j
val _ = is str "{ \"a\" : \"b\" }" "parse first"


val (j, ts) = parse ts
val str = encode j
val _ = is str "{ \"c\" : \"d\" }" "parse second"

val _ = print "\n"


val decodeAndEncode = encode o decode


val s = "{ \"a\" : \"A\", \"b\" : { \"b1\" : \"B1\", \"b2\" : \"B2\" }, \"c\" : [ \"c1\", \"c2\" ], \"d\" : \"D\" }"
val _ = is (decodeAndEncode s) s "decodeAndEncode"


val s = "[ { \"a\" : \"A\" }, { \"b\" : { \"b1\" : \"B1\", \"b2\" : \"B2\" }, \"c\" : [ \"c1\", \"c2\" ], \"d\" : \"D\" } ]"
val _ = is (decodeAndEncode s) s "decodeAndEncode"


val s = "{ \"a\" : \"A\", \"b\" : { \"b1\" : \"B1\" }, \"c\" : [ \"c1\" ] }"
val _ = is (decodeAndEncode s) s "decodeAndEncode"

val j = Object [("a", String "A"), ("b", Object [("b1", String "B1")]), ("c", Array [String "c1"])]
val _ = is (encode j) s "encode"

val _ = print "\n"


val s = "{ \"num\" : 0, \"bt\" : true, \"bf\" : false, \"nl\" : null }"
val e = "{ \"num\" : 0.0, \"bt\" : true, \"bf\" : false, \"nl\" : null }"
val _ = is (decodeAndEncode s) e "num, bool, null"

val s = "{ \"num\" : 1.2, \"bt\" : true, \"bf\" : false, \"nl\" : null }"
val e = "{ \"num\" : 0.12E1, \"bt\" : true, \"bf\" : false, \"nl\" : null }"
val _ = is (decodeAndEncode s) e "num, bool, null"

(* val _ = print (decodeAndEncode s) *)
