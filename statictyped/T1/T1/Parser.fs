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

//After parsing, before symbol linking
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

//After hierarchy building, before final parse
  type TParenthesisHierarchy = 
  | Float of TLexFloatID
  | Int of TLexIntID
  | Char of TLexCharID
  | String of TLexStringID
  | Let of TLexLetID
  | Fn of TLexFnID
  | Ref of TLexRefID
  | Parenthesis of TLexLParenthesisID * List<TParenthesisHierarchy> * TLexRParenthesisID
  | Brackets of TLexLBracketID * List<TParenthesisHierarchy> * TLexRBracketID
  | CBrackets of TLexLCBracketID * List<TParenthesisHierarchy> * TLexRCBracketID
  | Error of TParenthesisError
  and TParenthesisError = 
  | NotEndingParenthesisLike of TLexLParenthesisLikeID * List<TParenthesisHierarchy>
  | WrongEndingParenthesisLike of TLexLParenthesisLikeID * List<TParenthesisHierarchy> * TLexRParenthesisLikeID
  | EndOfParsing
  | ErrorEndingParenthesisLike of TLexLParenthesisLikeID * List<TParenthesisHierarchy> * TParenthesisError

  and ListBuilderResult = 
  | Succes of List<TParenthesisHierarchy> * TLexRParenthesisLikeID
  | Fail of List<TParenthesisHierarchy> * TParenthesisError 

  let rec BuildHierarchy(lexed : List<Lexer.TLexID>, count : int) : TParenthesisHierarchy =
    if(count >= lexed.Count) then  Error EndOfParsing
    else
    let curSymbol = lexed.Item(count)
    match curSymbol with
    | Lexer.TLexID.Char schar     ->  TParenthesisHierarchy.Char schar
    | Lexer.TLexID.Float sfloat   ->  TParenthesisHierarchy.Float sfloat
    | Lexer.TLexID.Int sint       ->  TParenthesisHierarchy.Int sint
    | Lexer.TLexID.String sstring ->  TParenthesisHierarchy.String sstring
    | Lexer.TLexID.Let  slet      ->  TParenthesisHierarchy.Let slet 
    | Lexer.TLexID.Fn  sfn        ->  TParenthesisHierarchy.Fn sfn 
    | Lexer.TLexID.Ref  sref      ->  TParenthesisHierarchy.Ref sref
    | Lexer.TLexID.LBracket sbrid ->  
      match ListBuilder(lexed, count + 1) with 
      | Succes list endpar -> 
        match endpar with
        | Parenthesis -> 
        | Bracket     -> 
        | CBracket    -> 
      | Fail list error    -> Error (TParenthesisError.ErrorEndingParenthesisLike sbrid list error) 
     
  and ListBuilder(lexed : List<Lexer.TLexID>, count : int): ListBuilderResult =  
     
    
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