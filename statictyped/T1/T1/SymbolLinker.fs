
module SymbolLinker
  open ParserDefinitions
//After the symbol linking
  type TLinkedParseSymbolValue = 
  | Let of List<TReferringID * TValuableID > * TValuableID //
  | FN of TRefereeID * list<TRefereeID> * TValuableID
  | FNCall of TReferringID * List<TParseSymbolID>
  | Float of TFloatID
  | Int of TIntID
  | Char of TCharID
  | String of TStringID
  | Reference of TReferringID * TRefereeID
  | Referee of TRefereeID