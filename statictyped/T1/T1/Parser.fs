module Parser
  open System.Collections.Generic
  open Lexer   
  //IDs
  type PID = {id : int}

  type TReferringID =
  | ID of PID
  type TRestID = 
  | ID of PID
  type TRefereeID = 
  | ID of PID
  type TFloatID = 
  | ID of PID
  type TIntID = 
  | ID of PID
  type TCharID  = 
  | ID of PID
  type TStringID = 
  | ID of PID
  type TValuableID =
  | RestID of TRestID
  | RefereeID of TRefereeID

  type TParseSymbolID =
  | ReferringID of TReferringID
  | RefereeID of TRefereeID
  | RestID of TRestID
  //Values
  type TParsCharValue = 
  | LexID of Lexer.TLexCharID
  type TParsError = 
  | EndOfFileError
  type TParsFloatValue = 
  | LexID of Lexer.TLexFloatID
  type TParsIntValue = 
  | LexID of Lexer.TLexIntID
  type TParsRefereeSValue = 
  | LexID of Lexer.TLexID
  type TParsRefereeLValue = 
  | RefereeID of TRefereeID
  type TParsRefereeValue = 
  | StringValue of TParsRefereeSValue
  | LinkedValue of TParsRefereeLValue
  type TParsReferenceValue = 
  | LexID of Lexer.TLexID
  type TParsStringValue = 
  | LexID of Lexer.TLexStringID
  type TParsLetValue = 
  | LexID of Lexer.TLexLetID

//After lexing, before symbol linking
  type TStringParseSymbolValue =
  | Let of List<TRefereeID * TValuableID > * TValuableID //
  | FN of TRefereeID * list<TRefereeID> * TValuableID
  | FNCall of TReferringID * List<TParseSymbolID>
  | Float of TParsFloatValue
  | Int of TParsIntValue
  | Char of TParsCharValue
  | String of TParsStringValue
  | Reference of TParsReferenceValue
  | Referee of TParsRefereeSValue
  | Error of TParsError
  
  (*
  let rec parse (lexed: IList<Lexer.TLexID>, count : int) = 
    if(count >= lexed.Count) then TStringParseSymbolValue.Error TParsError.EndOfFileError
    else 
    let curSymbol = lexed.Item(count)
    match curSymbol with
    | Lexer.TLexID.Char schar     -> TStringParseSymbolValue.Char      (TParsCharValue.LexID schar)
    | Lexer.TLexID.Float sfloat   -> TStringParseSymbolValue.Float     (TParsFloatValue.LexID sfloat)
    | Lexer.TLexID.Int sint       -> TStringParseSymbolValue.Int       (TParsIntValue.LexID sint)
    | Lexer.TLexID.Let slet       -> calculateLetClosing(lexed,count)
    | Lexer.TLexID.String sstring -> TStringParseSymbolValue.String    (TParsStringValue.LexID sstring)
    //| Lexer.TLexID.FN sfn         -> TStringParseSymbolValue.FN        (TPars)
  and calculateLetClosing (lexed,count) = *)