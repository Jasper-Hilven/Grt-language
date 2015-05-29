module Compiler

open Lexer
open LexValueToIdMapper
open ParserHierarchyBuilder
open Parser
open SymbolLinker

let compile code = 
  let lexed : LexValues.TPositionedLexValue list = Lexer.lex code
  let (lexmap,lexID) = mapLexValuesToParseValue lexed
  0
