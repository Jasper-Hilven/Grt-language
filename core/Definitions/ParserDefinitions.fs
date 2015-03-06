module ParserDefinitions
  //IDs
  type PID = {id : int}

  type TReferMatchingID= | ID of PID 
  type TReferringID =    | ID of PID
  type TRestID =         | ID of PID
  type TRefereeID =      | ID of PID
  type TFloatID =        | ID of PID
  type TIntID =          | ID of PID
  type TCharID  =        | ID of PID
  type TStringID =       | ID of PID
  type TLetID     =      | ID of PID
  type TFnID      =      | ID of PID
  type TFnCallID =       | ID of PID
  type TArrayID   =      | ID of PID
  type TAssociativeID =  | ID of PID

  type TRefereeCID   = {refereeID : TRefereeID    ; matchingID : TReferMatchingID}
  type TReferringCID = {referringID : TReferringID; matchingID : TReferMatchingID} 
  type TParseID =
  | Let of TLetID * list<TRefereeCID * TParseID> * TParseID
  | FN of TFnID * TRefereeCID * list<TRefereeCID> * TParseID
  | FNCall of  TFnCallID * list<TParseID>
  | Float of TFloatID
  | Int of TIntID
  | Char of TCharID
  | String of TStringID 
  | Referring of TReferringCID 
  | Referee of TRefereeCID    
  | Array of TArrayID * list<TParseID>
  | Associative of TAssociativeID * list<TParseID * TParseID>