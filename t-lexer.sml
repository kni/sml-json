use "json.sml";

open JSON.Lexer

fun showLexerResult r =
  let
    fun showToken StartObj   = print "StartObj"
      | showToken EndObj     = print "EndObj"
      | showToken StartArr   = print "StartArr"
      | showToken EndArr     = print "EndArr"
      | showToken (String s) = print ("String \"" ^ s ^ "\"")

    fun showTokens [] = ()
      | showTokens (t::ts) = ( showToken t ; print ", "; showTokens ts )
  in
    case r of NONE => () | SOME (ts, t) => ( showTokens ts ; print ("TAIL: "  ^ t ^ "\n"))
  end


val s1 = " {\"ab\\\"c\" : [\"A"
val s2 = "1\", \"A2\"]}"
val s = s1 ^ s2

(* val _ = print (s1 ^ "\n" ^ s2 ^ "\n\n" ^ s ^ "\n\n-----\n\n") *)
val r = lexer s1
val _ = showLexerResult r
val _ = case r of NONE => () | SOME (_, t) => let val r = lexer (t ^ s2) in print "\n" ; showLexerResult r end

val _ = showLexerResult (lexer s)
