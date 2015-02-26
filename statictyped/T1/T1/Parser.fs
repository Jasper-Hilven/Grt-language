module Parser
  open System.Collections.Generic
  open Lexer   
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
  //Values
  type TParsCharValue =      | LexID of Lexer.TLexCharID
  type TParsError =          | EndOfFileError
  type TParsFloatValue =     | LexID of Lexer.TLexFloatID
  type TParsIntValue =       | LexID of Lexer.TLexIntID
  type TParsRefereeSValue =  | LexID of Lexer.TLexID
  type TParsRefereeLValue =  | RefereeID of TRefereeID
  type TParsRefereeValue =   | StringValue of TParsRefereeSValue
                             | LinkedValue of TParsRefereeLValue
  type TParsReferenceValue = | LexID of Lexer.TLexID
  type TParsStringValue =    | LexID of Lexer.TLexStringID
  type TParsLetValue =       | LexID of Lexer.TLexLetID

//After parsing, before symbol linking
  type TStringParseSymbolValue =
  | Let of list<TRefereeID * TValuableID > * TValuableID //
  | FN of TRefereeID * list<TRefereeID> * TValuableID
  | FNCall of TReferringID * list<TParseSymbolID>
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
  | Parenthesis of TLexLParenthesisID * list<TParenthesisHierarchy> * TLexRParenthesisID
  | Brackets of TLexLBracketID * list<TParenthesisHierarchy> * TLexRBracketID
  | CBrackets of TLexLCBracketID * list<TParenthesisHierarchy> * TLexRCBracketID
  | Error of TParenthesisError
  and TParenthesisError = 
  | NotEndingParenthesisLike of TLexLParenthesisLikeID * list<TParenthesisHierarchy>
  | WrongEndingParenthesisLike of TLexLParenthesisLikeID * list<TParenthesisHierarchy> * TLexRParenthesisLikeID
  | EndOfParsing
  | UnexpectedParenthesisLike of TLexRParenthesisLikeID
  and ListBuilderResult = 
  | Succes of list<TParenthesisHierarchy> * TLexRParenthesisLikeID * int
  | FailLBNotEndingParenthesisLike of list<TParenthesisHierarchy> * int
    
  type TBHierarchy = 
  | TPC of TParenthesisHierarchy * int

  let rec BuildHierarchy(lexed : List<Lexer.TLexID>, count : int) : TBHierarchy =
    let countinc = count + 1
    if(count >= lexed.Count) then  TPC(Error(EndOfParsing),countinc)
    else
    let curSymbol = lexed.Item(count)
    match curSymbol with
    | Lexer.TLexID.Char schar       ->  TPC(TParenthesisHierarchy.Char(schar),countinc)
    | Lexer.TLexID.Float sfloat     ->  TPC(TParenthesisHierarchy.Float sfloat,countinc)
    | Lexer.TLexID.Int sint         ->  TPC(TParenthesisHierarchy.Int sint,countinc)
    | Lexer.TLexID.String sstring   ->  TPC(TParenthesisHierarchy.String sstring,countinc)
    | Lexer.TLexID.Let  slet        ->  TPC(TParenthesisHierarchy.Let slet ,countinc)
    | Lexer.TLexID.Fn  sfn          ->  TPC(TParenthesisHierarchy.Fn sfn ,countinc)
    | Lexer.TLexID.Ref  sref        ->  TPC(TParenthesisHierarchy.Ref sref,countinc)
    | Lexer.TLexID.RBracket     srb ->  TPC(TParenthesisHierarchy.Error(TParenthesisError.UnexpectedParenthesisLike(TLexRParenthesisLikeID.Bracket(srb))),countinc)
    | Lexer.TLexID.RParenthesis srb ->  TPC(TParenthesisHierarchy.Error(TParenthesisError.UnexpectedParenthesisLike(TLexRParenthesisLikeID.Parenthesis(srb))),countinc)
    | Lexer.TLexID.RCBracket    srb ->  TPC(TParenthesisHierarchy.Error(TParenthesisError.UnexpectedParenthesisLike(TLexRParenthesisLikeID.CBracket(srb))),countinc)
    | Lexer.TLexID.LParenthesis sbrid ->  
      match ListBuilder(lexed, count + 1) with 
      | Succes (tlist, endpar,nc) -> match endpar with
                                     | TLexRParenthesisLikeID.Parenthesis p-> TPC(TParenthesisHierarchy.Parenthesis(sbrid,tlist,p),nc)
                                     | TLexRParenthesisLikeID.Bracket     b-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.Parenthesis(sbrid), tlist, TLexRParenthesisLikeID.Bracket(b))),nc)
                                     | TLexRParenthesisLikeID.CBracket    b-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.Parenthesis(sbrid), tlist, TLexRParenthesisLikeID.CBracket(b))),nc)
      | FailLBNotEndingParenthesisLike (list,nc) -> TPC(Error(TParenthesisError.NotEndingParenthesisLike( TLexLParenthesisLikeID.Parenthesis(sbrid), list)),nc)
    | Lexer.TLexID.LBracket sbrid ->  
      match ListBuilder(lexed, count + 1) with 
      | Succes (list, endpar,nc) -> match endpar with
                                    | TLexRParenthesisLikeID.Parenthesis p-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.Bracket(sbrid), list, TLexRParenthesisLikeID.Parenthesis(p))),nc)
                                    | TLexRParenthesisLikeID.Bracket     b-> TPC(TParenthesisHierarchy.Brackets(sbrid, list, b),nc)
                                    | TLexRParenthesisLikeID.CBracket    b-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.Bracket(sbrid), list, TLexRParenthesisLikeID.CBracket(b))),nc)
      | FailLBNotEndingParenthesisLike (list,nc) -> TPC(Error(TParenthesisError.NotEndingParenthesisLike( TLexLParenthesisLikeID.Bracket(sbrid), list)),nc)
    | Lexer.TLexID.LCBracket sbrid ->  
      match ListBuilder(lexed, count + 1) with 
      | Succes (list, endpar,nc) -> match endpar with
                                    | TLexRParenthesisLikeID.Parenthesis p-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.CBracket(sbrid), list, TLexRParenthesisLikeID.Parenthesis(p))),nc)
                                    | TLexRParenthesisLikeID.Bracket     b-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.CBracket(sbrid), list, TLexRParenthesisLikeID.Bracket(b))),nc)
                                    | TLexRParenthesisLikeID.CBracket    b-> TPC(TParenthesisHierarchy.CBrackets(sbrid, list, b),nc)
      | FailLBNotEndingParenthesisLike (list,nc) -> TPC(Error(TParenthesisError.NotEndingParenthesisLike( TLexLParenthesisLikeID.CBracket(sbrid), list)),nc)
  and ListBuilder(lexed : List<Lexer.TLexID>, ocount : int): ListBuilderResult = 
    let nextRes = BuildHierarchy(lexed,ocount)
    let appendAndContinue(elem,lexed,count) =
      match ListBuilder(lexed,count + 1) with
                     | Succes (list,endpar,nc) -> Succes(elem::list,endpar,nc)
                     | FailLBNotEndingParenthesisLike (list,nc)   -> FailLBNotEndingParenthesisLike(elem::list,nc)
    match nextRes with | TPC(tph,count) -> match tph with  
                                           | Float(fid)   -> appendAndContinue(TParenthesisHierarchy.Float(fid),lexed,count)
                                           | Int (fint)   -> appendAndContinue(TParenthesisHierarchy.Int(fint),lexed,count)
                                           | Char (fchar) -> appendAndContinue(TParenthesisHierarchy.Char(fchar),lexed,count)
                                           | String (fstring) -> appendAndContinue(TParenthesisHierarchy.String(fstring),lexed,count)
                                           | Let (flet)  ->appendAndContinue(TParenthesisHierarchy.Let(flet),lexed,count)
                                           | Fn (fFn)   ->appendAndContinue(TParenthesisHierarchy.Fn(fFn),lexed,count)
                                           | Ref (fRef) -> appendAndContinue(TParenthesisHierarchy.Ref(fRef),lexed,count)
                                           | Brackets(id,flist,rbr) ->appendAndContinue(TParenthesisHierarchy.Brackets(id,flist,rbr),lexed,count)
                                           | CBrackets(id,flist,rbr) ->appendAndContinue(TParenthesisHierarchy.CBrackets(id,flist,rbr),lexed,count)
                                           | Parenthesis(id,flist,rbr) ->appendAndContinue(TParenthesisHierarchy.Parenthesis(id,flist,rbr),lexed,count)
                                           | Error lerror  ->
                                             match lerror with
                                             | NotEndingParenthesisLike(lid, elist)                -> FailLBNotEndingParenthesisLike(TParenthesisHierarchy.Error(NotEndingParenthesisLike(lid, elist))::[],count)                         
                                             | EndOfParsing                                        -> FailLBNotEndingParenthesisLike([],count)
                                             | WrongEndingParenthesisLike (l,m,r)                  -> appendAndContinue(Error(WrongEndingParenthesisLike(l,m,r)),lexed,count)
                                             | UnexpectedParenthesisLike(tlexparid)                -> Succes([],tlexparid,count) 
      
