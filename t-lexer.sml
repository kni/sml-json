use "json.sml";

open JSON.Lexer

fun showLexerResult r = case r of NONE => "" | SOME (ts, t) => ( (show ts) ^ "; TAIL: " ^ t)

fun printLexerResult r = print (showLexerResult r ^ "\n")

fun is g e name =
  if String.compare (g, e) = General.EQUAL
  then print ("OK: " ^ name ^ "\n")
  else print ("NOT OK: " ^ name ^ " (got: '" ^ g ^ "', expected: '" ^ e ^ "')\n")


val s1 = " {\"ab\\\"c\" : [\"A"
val s2 = "1\", \"A2\"]}"
val s = s1 ^ s2


val r = lex s1
val _ = is (showLexerResult r) "StartObj, String \"ab\\\"c\", StartArr; TAIL: \"A" "lex head"


val _ = case r of NONE => () | SOME (_, t) =>
let
  val r = lex (t ^ s2)
in
  is (showLexerResult r) "String \"A1\", String \"A2\", EndArr, EndObj; TAIL: " "lex tail"
end


val r = lex s
val _ = is (showLexerResult r)  "StartObj, String \"ab\\\"c\", StartArr, String \"A1\", String \"A2\", EndArr, EndObj; TAIL: " "lex all"
(* val _ = printLexerResult r *)
