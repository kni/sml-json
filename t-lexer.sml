use "json.sml";

open JSON.Lexer

fun showLexerResult r = case r of NONE => () | SOME (ts, t) => ( print (show ts); print ("\nTAIL: "  ^ t ^ "\n\n"))


val s1 = " {\"ab\\\"c\" : [\"A"
val s2 = "1\", \"A2\"]}"
val s = s1 ^ s2

(* val _ = print (s1 ^ "\n" ^ s2 ^ "\n\n" ^ s ^ "\n\n-----\n\n") *)
val r = lex s1
val _ = showLexerResult r
val _ = case r of NONE => () | SOME (_, t) => let val r = lex (t ^ s2) in showLexerResult r end

val _ = showLexerResult (lex s)
