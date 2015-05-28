module Test.Lexer

open NUnit.Framework
open FsUnit
open LexValues

[<TestFixture>]
type ``All simple Lex input should be handled correctly`` ()=
    [<Test>] member int.
     ``simple int`` ()=
            Lexer.lex "1"
             |> should equal [{lineNumber = 0;charPosition = 0;lexValue = TLexValue.Int "1"}]



