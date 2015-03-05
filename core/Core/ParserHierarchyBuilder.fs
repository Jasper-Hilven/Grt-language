module ParserHierarchyBuilder
  open System.Collections.Generic
  open LexDefinitions
  
  //Intermediate representation: After hierarchy building, before final parse
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
  
  //Intermediate types
  type ListBuilderResult = 
  | Succes of list<TParenthesisHierarchy> * TLexRParenthesisLikeID * int
  | FailLBNotEndingParenthesisLike of list<TParenthesisHierarchy> * int  
  type TBHierarchy = 
  | TPC of TParenthesisHierarchy * int
  
  //Lets build the hierarchy
  let rec BuildHierarchy(lexed : List<TLexID>, count : int) : TBHierarchy =
    let countinc = count + 1
    if(count >= lexed.Count) then  TPC(Error(EndOfParsing),countinc)
    else
    let curSymbol = lexed.Item(count)
    match curSymbol with
    | TLexID.Char schar       ->  TPC(TParenthesisHierarchy.Char(schar),countinc)
    | TLexID.Float sfloat     ->  TPC(TParenthesisHierarchy.Float sfloat,countinc)
    | TLexID.Int sint         ->  TPC(TParenthesisHierarchy.Int sint,countinc)
    | TLexID.String sstring   ->  TPC(TParenthesisHierarchy.String sstring,countinc)
    | TLexID.Let  slet        ->  TPC(TParenthesisHierarchy.Let slet ,countinc)
    | TLexID.Fn  sfn          ->  TPC(TParenthesisHierarchy.Fn sfn ,countinc)
    | TLexID.Ref  sref        ->  TPC(TParenthesisHierarchy.Ref sref,countinc)
    | TLexID.RBracket     srb ->  TPC(TParenthesisHierarchy.Error(TParenthesisError.UnexpectedParenthesisLike(TLexRParenthesisLikeID.Bracket(srb))),countinc)
    | TLexID.RParenthesis srb ->  TPC(TParenthesisHierarchy.Error(TParenthesisError.UnexpectedParenthesisLike(TLexRParenthesisLikeID.Parenthesis(srb))),countinc)
    | TLexID.RCBracket    srb ->  TPC(TParenthesisHierarchy.Error(TParenthesisError.UnexpectedParenthesisLike(TLexRParenthesisLikeID.CBracket(srb))),countinc)
    | TLexID.LParenthesis sbrid ->  
      match ListBuilder(lexed, count + 1) with 
      | Succes (tlist, endpar,nc) -> match endpar with
                                     | TLexRParenthesisLikeID.Parenthesis p-> TPC(TParenthesisHierarchy.Parenthesis(sbrid,tlist,p),nc)
                                     | TLexRParenthesisLikeID.Bracket     b-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.Parenthesis(sbrid), tlist, TLexRParenthesisLikeID.Bracket(b))),nc)
                                     | TLexRParenthesisLikeID.CBracket    b-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.Parenthesis(sbrid), tlist, TLexRParenthesisLikeID.CBracket(b))),nc)
      | FailLBNotEndingParenthesisLike (list,nc) -> TPC(Error(TParenthesisError.NotEndingParenthesisLike( TLexLParenthesisLikeID.Parenthesis(sbrid), list)),nc)
    | TLexID.LBracket sbrid ->  
      match ListBuilder(lexed, count + 1) with 
      | Succes (list, endpar,nc) -> match endpar with
                                    | TLexRParenthesisLikeID.Parenthesis p-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.Bracket(sbrid), list, TLexRParenthesisLikeID.Parenthesis(p))),nc)
                                    | TLexRParenthesisLikeID.Bracket     b-> TPC(TParenthesisHierarchy.Brackets(sbrid, list, b),nc)
                                    | TLexRParenthesisLikeID.CBracket    b-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.Bracket(sbrid), list, TLexRParenthesisLikeID.CBracket(b))),nc)
      | FailLBNotEndingParenthesisLike (list,nc) -> TPC(Error(TParenthesisError.NotEndingParenthesisLike( TLexLParenthesisLikeID.Bracket(sbrid), list)),nc)
    | TLexID.LCBracket sbrid ->  
      match ListBuilder(lexed, count + 1) with 
      | Succes (list, endpar,nc) -> match endpar with
                                    | TLexRParenthesisLikeID.Parenthesis p-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.CBracket(sbrid), list, TLexRParenthesisLikeID.Parenthesis(p))),nc)
                                    | TLexRParenthesisLikeID.Bracket     b-> TPC(TParenthesisHierarchy.Error(WrongEndingParenthesisLike(TLexLParenthesisLikeID.CBracket(sbrid), list, TLexRParenthesisLikeID.Bracket(b))),nc)
                                    | TLexRParenthesisLikeID.CBracket    b-> TPC(TParenthesisHierarchy.CBrackets(sbrid, list, b),nc)
      | FailLBNotEndingParenthesisLike (list,nc) -> TPC(Error(TParenthesisError.NotEndingParenthesisLike( TLexLParenthesisLikeID.CBracket(sbrid), list)),nc)
  
  //Lets build a list of expressions
  and ListBuilder(lexed : List<TLexID>, ocount : int): ListBuilderResult = 
    let nextRes = BuildHierarchy(lexed,ocount)
    let appendAndContinue(elem,lexed,count) =
      match ListBuilder(lexed,count + 1) with
                     | Succes (list,endpar,nc) -> Succes(elem::list,endpar,nc)
                     | FailLBNotEndingParenthesisLike (list,nc)   -> FailLBNotEndingParenthesisLike(elem::list,nc)
    match nextRes with 
    | TPC(tph,count) -> match tph with  
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
                          | NotEndingParenthesisLike(lid, elist)         -> FailLBNotEndingParenthesisLike(TParenthesisHierarchy.Error(NotEndingParenthesisLike(lid, elist))::[],count)
                          | EndOfParsing                                 -> FailLBNotEndingParenthesisLike([],count)
                          | WrongEndingParenthesisLike (l,m,r)           -> appendAndContinue(Error(WrongEndingParenthesisLike(l,m,r)),lexed,count)
                          | UnexpectedParenthesisLike(tlexparid)         -> Succes([],tlexparid,count)