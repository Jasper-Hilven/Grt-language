module ParserValues
open LexDefinitions
//Values
  type TParsCharValue =      | LexID of TLexCharID
  type TParsError =          | EndOfFileError
  type TParsFloatValue =     | LexID of TLexFloatID
  type TParsIntValue =       | LexID of TLexIntID
  type TParsRefereeSValue =  | LexID of TLexID
  type TParsReferenceValue = | LexID of TLexID
  type TParsStringValue =    | LexID of TLexStringID
  type TParsLetValue =       | LexID of TLexLetID
  type TValuableValue =   | Rest of TRestID
                          | Referee of TRefereeID
  type TStringParseSymbolValue =
  | Let of list<TParsRefereeSValue * TValuableID > * TValuableID //
  | FN of TRefereeID * list<TRefereeID> * TValuableID
  | FNCall of TReferringID * list<TParseSymbolID>
  | Float of TParsFloatValue
  | Int of TParsIntValue
  | Char of TParsCharValue
  | String of TParsStringValue
  | Reference of TParsReferenceValue
  | Referee of TParsRefereeSValue
  | Error of TParsError
  