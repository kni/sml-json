(* Принимает строку. Возвращает список токенов и хвост.  *)

val s1 = " {\"ab\\\"c\" : [\"A"
val s2 = "1\", \"A2\"]}"
val s = s1 ^ s2

(* val _ = print (s1 ^ "\n" ^ s2 ^ "\n\n" ^ s ^ "\n\n-----\n\n") *)


datatype token = StartObj | EndObj | StartArr | EndArr | Colon | String of string

fun showToken StartObj   = print "StartObj\n"
  | showToken EndObj     = print "EndObj\n"
  | showToken StartArr   = print "StartArr\n"
  | showToken EndArr     = print "EndArr\n"
  | showToken Colon      = print "Colon\n"
  | showToken (String s) = print ("String " ^ s ^ "\n")

fun showTokens [] = ()
  | showTokens (t::ts) = ( showToken t ; showTokens ts )

fun showLexerResult r = case r of NONE => () | SOME (ts, t) => ( showTokens ts ; print ("TAIL: "  ^ t ^ "\n"))

fun showChar c = print (Char.toString c)

fun showTail getc strm =
let
  fun loop cs strm = case getc strm of
      NONE           => let val t = String.implode(List.rev cs) in print ("---: " ^ t ^ "\n"); SOME (t, strm) end
    | SOME (c, strm) => loop (c::cs) strm
in
    loop [] strm
end

fun lexer s =
let

  fun tail tokens getc strm =
  let
    fun loop cs strm = case getc strm of
        NONE           => SOME (((List.rev tokens), String.implode(List.rev cs)), strm)
      | SOME (c, strm) => loop (c::cs) strm
  in
      loop [] strm
  end

  fun pure tokens strm = SOME (((List.rev tokens), ""), strm)

  fun scanString getc strm =
  let
    fun loop cs strm = 
      case getc strm of NONE => NONE | SOME (c, strm) =>
      case c of
           #"\\" => (case getc strm of NONE => NONE | SOME (c', strm) => loop (c'::c::cs) strm)
         | #"\"" => let val s = String.implode(List.rev cs) in SOME (s, strm) end
         | _     => loop (c::cs) strm
  in
    loop [] strm
  end


  val scanNumber = IEEEReal.scan


  fun scan tokens getc strm =
  let
    val strm = StringCvt.skipWS getc strm
  in
    case getc strm of NONE => pure tokens strm | SOME (c, strm_n) =>
    case c of
         #"{"  => scan (StartObj::tokens) getc strm_n
       | #"}"  => scan (EndObj::tokens) getc strm_n
       | #"["  => scan (StartArr::tokens) getc strm_n
       | #"]"  => scan (EndArr::tokens) getc strm_n
       | #":"  => scan (Colon::tokens) getc strm_n
       | #"\"" => (case scanString getc strm_n of NONE => tail tokens getc strm | SOME (s, strm_n) => scan ((String s)::tokens) getc strm_n)
       | #","  => scan tokens getc strm_n
       | _     => tail tokens getc strm
  end

in
  StringCvt.scanString (scan []) s
end


(*
val r = lexer s1
val _ = showLexerResult r
val _ = case r of NONE => () | SOME (_, t) => let val r = lexer (t ^ s2) in print "\n" ; showLexerResult r end
*)


(* val _ = showLexerResult (lexer s) *)


(* ******************************************************** *)

datatype jValue = jObject of (string * jValue) list 
               | jArray of jValue list
               | jString of string
               | jNumber of IEEEReal.decimal_approx
               | jTrue
               | jFalse 
               | jNull


fun showJSON (jObject l) = "{ " ^ (String.concatWith ", " (List.map (fn(k,v) => ("\"" ^ String.toCString k) ^ "\"" ^ " : " ^ (showJSON v) ) l)) ^ " }"
  | showJSON (jArray  l) = "[ " ^ (String.concatWith ", " (List.map showJSON l)) ^ " ]"
  | showJSON (jString s) = "\"" ^ String.toCString s ^ "\""
  | showJSON (jNumber n) = IEEEReal.toString n
  | showJSON (jTrue    ) = "True"
  | showJSON (jFalse   ) = "False"
  | showJSON (jNull    ) = "Null"

fun printJSON j = print ((showJSON j) ^ "\n")

(*
val j = jObject [ ("ab\"c", jArray [jString "A1", jString "A2"] ) ]
val _ = printJSON j
*)
