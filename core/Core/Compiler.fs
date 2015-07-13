module Compiler

open Lexer
open LexDefinitions
open LexValueToIdMapper
open LexErrorSpaceHandler
open ParserHierarchyBuilder
open Parser
open SymbolLinker
open System.Collections;
let compile code = 
  let lexed : LexValues.TPositionedLexValue list = Lexer.lex code
  let (lexmap,lexID) = mapLexValuesToParseValue lexed
  let cleanLexID = List.toArray(filterErrorAndSpace(lexID))
  let hierarchy = ParserHierarchyBuilder.BuildHierarchy(cleanLexID)
  let parsed = parseBuiltHierarchy(match hierarchy with | TBHierarchy.TPC(parsedHierarchy,_) -> parsedHierarchy)

  
