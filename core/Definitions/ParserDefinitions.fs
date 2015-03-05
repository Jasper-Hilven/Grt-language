module ParserDefinitions
open LexDefinitions
  //IDs
  type PID = {id : int}

  type TReferringID =  | ID of PID
  type TRestID =       | ID of PID
  type TRefereeID =    | ID of PID
  type TFloatID =      | ID of PID
  type TIntID =        | ID of PID
  type TCharID  =      | ID of PID
  type TStringID =     | ID of PID
  type TValuableID =   | RestID of TRestID
                       | RefereeID of TRefereeID
  type TParseSymbolID =  | ReferringID of TReferringID
                         | RefereeID of TRefereeID
                         | RestID of TRestID