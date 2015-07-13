module ParserDefinitions
  //IDs
  type PID = {id : int}
  type TPIDProvider(curInt :int) = 
    member x.currentID = {id = curInt}
    member x.nextProvider : unit ->TPIDProvider = 
      let increment = curInt + 1 in (fun () -> TPIDProvider increment)

  type TReferMatchingID= | ID of PID 
  type TReferringID =    | ID of PID
  type TRefereeID =      | ID of PID
  type TFloatID =        | ID of PID
  type TIntID =          | ID of PID
  type TCharID  =        | ID of PID
  type TStringID =       | ID of PID

  type TRefereeCID   = {refereeID : TRefereeID    ; matchingID : TReferMatchingID}
  type TReferringCID = {referringID : TReferringID; matchingID : TReferMatchingID} 
  type TParseID =
  | Let of list<TRefereeCID * TParseID> * TParseID
  | Letrec of list<TRefereeCID * TParseID> * TParseID
  | FN of TRefereeCID * list<TRefereeCID> * TParseID
  | FNCall of list<TParseID>
  | Float of TFloatID
  | Int of TIntID
  | Char of TCharID
  | String of TStringID 
  | Referring of TReferringCID 
  | Referee of TRefereeCID    
  | Array of list<TParseID>
  | Associative of  list<TParseID * TParseID>