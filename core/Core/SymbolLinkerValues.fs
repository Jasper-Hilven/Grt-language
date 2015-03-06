module SymbolLinkerValues
  open ParserDefinitions
  
  //type TSLReferenceID 
  type TLinkedValue = 
  | Let of TLetID * List<TRefereeID * TLinkedValue > * TLinkedValue //
  | FN of  TFnID* TRefereeID * list<TRefereeID> * TLinkedValue
  | FNCall of TFnCallID * List<TLinkedValue>
  | Float of TFloatID
  | Int of TIntID
  | Char of TCharID
  | String of TStringID
  | Reference of TRefereeID
  | Referee of TRefereeID    
  | Array of TArrayID * list<TLinkedValue>
  | Associative of TAssociativeID * list<TLinkedValue*TLinkedValue>
  | Error of LinkError
  and LinkError = 
  | NameNotFound
  | DuplicateName
  | UnexpectedReferee
  | UnexpectedToComeHere

