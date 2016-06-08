structure ExprLrVals = ExprLrValsFun (structure Token = LrParser.Token)

structure ExprLex = ExprLexFun (structure Tokens = ExprLrVals.Tokens)

structure ExprParser = Join(structure LrParser = LrParser
                            structure ParserData = ExprLrVals.ParserData
                            structure Lex = ExprLex)
