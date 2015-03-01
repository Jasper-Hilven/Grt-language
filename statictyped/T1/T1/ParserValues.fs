﻿module ParserValues
open LexDefinitions
//Values
  type TParsCharValue =      | LexID of TLexCharID
  type TParsFloatValue =     | LexID of TLexFloatID
  type TParsIntValue =       | LexID of TLexIntID
  type TParsRefereeSValue =  | LexID of TLexRefID
  type TParsReferenceValue = | LexID of TLexRefID
  type TParsStringValue =    | LexID of TLexStringID
  type TParsLetValue =       | LexID of TLexLetID
  type TParsFunctionValue =  | LexID of TLexFnID
  type TParseError = 
  | EmptyFunctionCall
  | Bracket
  | NakedFunctionIdentifier of TLexFnID
  | NakedLetIdentifier of TLexLetID
  | NotEndingParenthesesLike of TLexLParenthesisLikeID 
  | WrongEndingParenthesisLike of TLexLParenthesisLikeID * TLexRParenthesisLikeID
  | EndOfParsingUnexpected
  | UnexpectedParenthesisLike of TLexRParenthesisLikeID
  type TStringParseSymbolValue =
  | Let of TParsLetValue * list<TParsRefereeSValue * TStringParseSymbolValue> * TStringParseSymbolValue //
  | FN of TParsFunctionValue * TParsRefereeSValue * list<TParsRefereeSValue> * TStringParseSymbolValue
  | FNCall of TParsReferenceValue * list<TStringParseSymbolValue>
  | Float of TParsFloatValue
  | Int of TParsIntValue
  | Char of TParsCharValue
  | String of TParsStringValue
  | Reference of TParsReferenceValue
  | Referee of TParsRefereeSValue
  | Error of TParseError
  | Array of List<TStringParseSymbolValue> 
  