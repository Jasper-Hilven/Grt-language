module LinkedSymbolDefinitions
  //The LinkedSymbolDefinitions build further on the parser definitions, due to the limited amount of difference between both
  open ParserDefinitions
  type TLinkedParseSymbolID = 
  | Let of List<TReferringID * TLinkedParseSymbolID > * TLinkedParseSymbolID //
  | FN of TRefereeID * list<TRefereeID> * TLinkedParseSymbolID
  | FNCall of TFnCallID * List<TLinkedParseSymbolID>
  | Float of TFloatID
  | Int of TIntID
  | Char of TCharID
  | String of TStringID
  | Reference of TReferringID * TRefereeID
  | Referee of TRefereeID * TReferMatchingID