module SymbolLinkerValues
  open ParserDefinitions
  
  //type TSLReferenceID 
  type TLinkedValue = 
  | Let of List<TReferringID * TLinkedValue > * TLinkedValue //
  | FN of TRefereeID * list<TRefereeID> * TLinkedValue
  | FNCall of TFnCallID * List<TLinkedValue>
  | Float of TFloatID
  | Int of TIntID
  | Char of TCharID
  | String of TStringID
  | Reference of TReferringID * TRefereeID
  | Referee of TRefereeID * TReferMatchingID    
  | Array of TArrayID * list<TLinkedValue>
  | Associative of TAssociativeID * list<TLinkedValue*TLinkedValue>
  | Error of LinkError
  and LinkError = 
  | NameNotFound
  | DuplicateName
  | NotYetImplemented

