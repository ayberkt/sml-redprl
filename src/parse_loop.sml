structure ParseLoop =
struct

  fun stringreader s =
    let val pos = ref 0
        val remainder = ref (String.size s)
        fun min(a, b) = if a < b then a else b
    in
      fn n =>
         let
             val m = min(n, !remainder)
             val s = String.substring(s, !pos, m)
             val () = pos := !pos + m
             val () = remainder := !remainder - m
         in
             s
         end
  end

  fun error (s, pos, pos') = raise ExprParser.ParseError

  fun parse text =
    let
        val lexer = ExprParser.makeLexer (stringreader text)
        val (res,_) = ExprParser.parse(1, lexer, error, ())
    in
        SOME res
    end


  fun loop f =
    let val dummyEOF = ExprLrVals.Tokens.EOF(0, 0)
        val input = valOf ( TextIO.output(TextIO.stdOut, "> ")
                          ; TextIO.flushOut(TextIO.stdOut)
                          ; TextIO.inputLine TextIO.stdIn)
        val result : RedPrlAst.ast option = f input
    in
        case result of
             SOME term => print (RedPrlAst.toString term ^ "\n")
           | NONE => ();
        loop f
    end

    val _ = loop parse

end
