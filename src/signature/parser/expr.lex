type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token

fun getString s =
  let
    val n = size s
  in
    substring (s, 1, n-2)
  end

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos, !pos)

exception LexerError of pos

%%
%header (functor ExprLexFun (structure Tokens : Expr_TOKENS));
alpha = [A-Za-z];
digit = [0-9];
any = [@a-zA-Z0-9];
whitespace = [\ \t];
%%

\n                 => (pos := !pos + 1; lex ());
{whitespace}+      => (lex ());
{alpha}{any}*      => (Tokens.IDENT (yytext, !pos, !pos));

"fun"              => (Tokens.FUN (!pos, !pos));
"dfun"             => (Tokens.DFUN (!pos, !pos));
"lam"              => (Tokens.LAM (!pos, !pos));
"ap"               => (Tokens.AP (!pos, !pos));
"ifeq"             => (Tokens.IFEQ (!pos, !pos));
"member"           => (Tokens.MEMBER (!pos, !pos));
"nu"               => (Tokens.NU (!pos, !pos));
"disect"           => (Tokens.DISECT (!pos, !pos));
"cons"             => (Tokens.CONS (!pos, !pos));

"Ax"               => (Tokens.AX (!pos, !pos));
"Atom"             => (Tokens.ATOM (!pos, !pos));
"Squash"           => (Tokens.SQUASH (!pos, !pos));
"Univ"             => (Tokens.UNIV (!pos, !pos));
"Base"             => (Tokens.BASE (!pos, !pos));

"("                => (Tokens.LPAREN (!pos, !pos));
")"                => (Tokens.RPAREN (!pos, !pos));
"["                => (Tokens.LSQUARE (!pos, !pos));
"]"                => (Tokens.RSQUARE (!pos, !pos));
"#"                => (Tokens.OCTOTHORPE (!pos, !pos));
"="                => (Tokens.EQUALS (!pos, !pos));
":>"               => (Tokens.COLONLT (!pos, !pos));
"<="               => (Tokens.LTEQUALS (!pos, !pos));
"@"                => (Tokens.AT (!pos, !pos));
"|"                => (Tokens.BAR (!pos, !pos));
"."                => (Tokens.DOT (!pos, !pos));
"~"                => (Tokens.TILDE (!pos, !pos));
"\'"               => (Tokens.APOSTROPHE (!pos, !pos));
