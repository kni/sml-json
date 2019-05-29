signature JSON =
sig
  exception Json of string

  structure Lexer:
  sig
    datatype Token = EndArr
                   | EndObj
                   | StartArr
                   | StartObj
                   | String of string
                   | Number of IEEEReal.decimal_approx
                   | Bool   of bool
                   | Null

    val lex:  string -> (Token list * string) option
    val show: Token list -> string
  end

  datatype Value = Array  of Value list
                 | Object of (string * Value) list
                 | String of string
                 | Number of IEEEReal.decimal_approx
                 | Bool   of bool
                 | Null

  structure Parser:
  sig
    val parse: Lexer.Token list -> Value * Lexer.Token list
  end

  val decode: string -> Value
  val encode: Value -> string
  val show:   Value -> string (* produce SML code *)
end

structure JSON :> JSON =
struct

  exception Json of string

  datatype Value = Object of (string * Value) list
                 | Array  of Value list
                 | String of string
                 | Number of IEEEReal.decimal_approx
                 | Bool   of bool
                 | Null

  local
      fun dodigits [] = ""
        | dodigits (a::b) = Int.toString a ^ dodigits b

      fun dozero n = "" ^ CharVector.tabulate (n, fn _ => #"0")
  in
      fun numberToString {class, sign=true, digits, exp} = (* Sign bit set *)
              "~" ^ numberToString {class=class, sign=false, digits=digits, exp=exp}
        | numberToString {class=IEEEReal.NAN, ...} = "nan"
        | numberToString {class=IEEEReal.INF, ...} = "inf"
        | numberToString {class=IEEEReal.ZERO, ...} = "0"
        | numberToString (n as {digits, exp, ...}) = (* NORMAL or SUBNORMAL *)
           let
            val dl = List.length digits
          in
            if dl = exp then dodigits digits else
            if dl < exp then dodigits digits ^ dozero (exp - dl) else
            "0." ^ dodigits digits ^ (if exp = 0 then "" else "E"^(Int.toString exp))
          end
  end

  structure Lexer =
  struct
    datatype Token = StartObj
                   | EndObj
                   | StartArr
                   | EndArr
                   | String of string
                   | Number of IEEEReal.decimal_approx
                   | Bool   of bool
                   | Null

    (* Accepts a string and  returns a tokens list and a tail. *)
    fun lex s =
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


        (* compare and return if match. It is copy from Scancom *)
        fun compare s []     getc strm = SOME (s, strm)
          | compare s (h::t) getc strm =
              case getc strm of
                   NONE           => NONE
                 | SOME (c, strm) =>
                     if c = h
                     then compare s t getc strm
                     else NONE

        fun takeStr s = let val e = String.explode s in compare s e end


        fun scan tokens getc strm =
          let
            val strm = StringCvt.skipWS getc strm
          in
            case getc strm of NONE => pure tokens strm | SOME (c, strm_n) =>
            case c of
                 #"{"  => scan (StartObj::tokens) getc strm_n
               | #"}"  => scan (EndObj::tokens)   getc strm_n
               | #"["  => scan (StartArr::tokens) getc strm_n
               | #"]"  => scan (EndArr::tokens)   getc strm_n
               | #"\"" => (case scanString getc strm_n of SOME (s, strm_n) => scan ((String s)::tokens) getc strm_n | NONE => tail tokens getc strm)
               | #":"  => scan tokens getc strm_n
               | #","  => scan tokens getc strm_n
               | _     => (
                 case IEEEReal.scan   getc strm of SOME (n, strm) => scan ((Number n)::tokens)   getc strm | NONE =>
                 case takeStr "true"  getc strm of SOME (n, strm) => scan ((Bool true)::tokens)  getc strm | NONE =>
                 case takeStr "false" getc strm of SOME (n, strm) => scan ((Bool false)::tokens) getc strm | NONE =>
                 case takeStr "null"  getc strm of SOME (n, strm) => scan (Null::tokens)         getc strm | NONE =>
                 tail tokens getc strm
               )
          end

      in
        StringCvt.scanString (scan []) s
      end

    fun show ts =
      let
        fun show' StartObj     = "StartObj"
          | show' EndObj       = "EndObj"
          | show' StartArr     = "StartArr"
          | show' EndArr       = "EndArr"
          | show' (String s)   = "String \"" ^ s ^ "\""
          | show' (Number n)   = numberToString n
          | show' (Bool true)  = "True"
          | show' (Bool false) = "False"
          | show' Null         = "Null"
      in
        String.concatWith ", " (List.map show' ts)
      end
  end


  structure Parser =
  struct
    structure L = Lexer
    local

      fun ht []      = raise Json "parse ht"
        | ht (x::xs) = (x, xs)

      fun parse (ts:L.Token list) : (Value * L.Token list) =
        let
          val (k, ts) = ht ts
        in
          case k of
               L.StartObj => let val (v, ts) = parseObj ts in ((Object v), ts) end
             | L.StartArr => let val (v, ts) = parseArr ts in ((Array v),  ts) end
             | L.String s => ((String s), ts)
             | L.Number n => (Number n, ts)
             | L.Bool b   => (Bool b, ts)
             | L.Null     => (Null, ts)
             | _          => raise Json "parse"
        end

      and parseObj (ts:L.Token list) : ((string * Value) list * L.Token list) =
        let
          val (k, ts) = ht ts
        in
          if k = L.EndObj then ([], ts) else
          let
            val k = case k of L.String s => s | _ => raise Json "parse Obj"
            val (v, ts) = parse ts
          in
            if null ts
            then ((k, v)::[], ts)
            else let val (z, ts) = parseObj(ts) in ((k, v)::z, ts) end
          end
        end

      and parseArr (ts:L.Token list) : (Value list * L.Token list) =
        let
          val (v, ts') = ht ts
        in
          if v = L.EndArr then ([], ts') else
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


  fun decode s = case Lexer.lex s of
                      NONE         => raise Json "lexer"
                    | SOME (ts, _) => let val (j, _) = Parser.parse ts in j end


  fun encode (Object l) = "{ " ^ (String.concatWith ", " (List.map (fn (k, v) => ("\"" ^ String.toCString k) ^ "\"" ^ " : " ^ (encode v) ) l)) ^ " }"
    | encode (Array  l) = "[ " ^ (String.concatWith ", " (List.map encode l)) ^ " ]"
    | encode (String s) = "\"" ^ String.toCString s ^ "\""
    | encode (Number n) = numberToString n
    | encode (Bool true)  = "true"
    | encode (Bool false) = "false"
    | encode Null         = "null"


  fun show (Object l) = "Object [" ^ (String.concatWith ", " (List.map (fn (k, v) => ("(\"" ^ String.toCString k) ^ "\"" ^ ", " ^ (show v) ^ ")" ) l)) ^ "]"
    | show (Array  l) = "Array [" ^ (String.concatWith ", " (List.map show l)) ^ "]"
    | show (String s) = "String \"" ^ String.toCString s ^ "\""
    | show (Number n) = "Number (valOf (IEEEReal.fromString \"" ^ numberToString n ^ "\"))"
    | show (Bool true)  = "Bool true"
    | show (Bool false) = "Bool false"
    | show Null         = "Null"

end
