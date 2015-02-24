
module SymbolLinker
  open Parser
//After the symbol linking
  type TLinkedParseSymbolValue = 
  | Let of List<Parser.TReferringID * Parser.TValuableID > * Parser.TValuableID //
  | FN of Parser.TRefereeID * list<Parser.TRefereeID> * Parser.TValuableID
  | FNCall of Parser.TReferringID * List<Parser.TParseSymbolID>
  | Float of Parser.TFloatID
  | Int of Parser.TIntID
  | Char of Parser.TCharID
  | String of Parser.TStringID
  | Reference of Parser.TReferringID * Parser.TRefereeID
  | Referee of Parser.TRefereeID
