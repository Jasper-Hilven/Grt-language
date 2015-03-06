module LinkedSymbolDefinitions
  //The LinkedSymbolDefinitions build further on the parser definitions, due to the limited amount of difference between both
  open ParserDefinitions
  type TLinkedParseSymbolID = 
  | Let of ((TRefereeID * TLinkedParseSymbolID ) list) * TLinkedParseSymbolID //
  | FN of TRefereeID * (TRefereeID list) * TLinkedParseSymbolID
  | FNCall of TFnCallID * (TLinkedParseSymbolID list)
  | Float of TFloatID
  | Int of TIntID
  | Char of TCharID
  | String of TStringID
  | Reference of TReferringID * TRefereeID
  | Referee of TRefereeID * TReferMatchingID