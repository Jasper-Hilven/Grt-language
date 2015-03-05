module Parser
  open System.Collections.Generic
  open LexDefinitions
  open ParserHierarchyBuilder     
  open ParserValues
  open System.Linq


  let castToParseError(hierarchyError : TParenthesisError) :TParseError = 
    match hierarchyError with
    | NotEndingParenthesisLike(parenthesisLike,plist) -> TParseError.NotEndingParenthesesLike parenthesisLike
    | TParenthesisError.WrongEndingParenthesisLike(left,plist,right)  -> TParseError.WrongEndingParenthesisLike(left ,right)
    | TParenthesisError.EndOfParsing                                  -> TParseError.EndOfParsingUnexpected
    | TParenthesisError.UnexpectedParenthesisLike unexpected          -> TParseError.UnexpectedParenthesisLike(unexpected)
  type TLetArgumentValidation  = 
  | ValidLetArgument of list<TParsRefereeSValue * TStringParseSymbolValue>
  | UnEvenNumberOfArguments
  | ArgumentNotID of TStringParseSymbolValue
  type TAssociativeArgumentValidation = 
  | Succes of list<TStringParseSymbolValue*TStringParseSymbolValue>
  | UnEvenNumberOfParameters
  


  let rec parseBuiltHierarchy(hierarchy : TParenthesisHierarchy) : TStringParseSymbolValue = 
    let validateFunctionCall(elements : (TParenthesisHierarchy list)) : TStringParseSymbolValue = 
      TStringParseSymbolValue.FNCall(List.map parseBuiltHierarchy elements)
    let validateArray(elemList) : TStringParseSymbolValue = 
      TStringParseSymbolValue.Array(List.map parseBuiltHierarchy elemList)
    match hierarchy with
    | TParenthesisHierarchy.Parenthesis(lbracket,elemList,rBracket) -> 
      match elemList with
      | [] -> Error(EmptyFunctionCall)
      | firstElem::restElems ->
        match firstElem with  
        | TParenthesisHierarchy.Let lexLetId-> validateLet(lexLetId ,restElems)   
        | TParenthesisHierarchy.Fn lexFnId -> validateFunction(lexFnId,restElems)
        | _ -> validateFunctionCall(elemList) 
    | TParenthesisHierarchy.Brackets(lbracket,elemList,rBracket) -> validateArray(elemList)
    | TParenthesisHierarchy.CBrackets(lcbracket,elemList,rcBracket) -> validateAssociative(elemList)
    | TParenthesisHierarchy.Char(char)            -> Char(ParserValues.TParsCharValue.LexID(char))
    | TParenthesisHierarchy.Error(hierarchyError) -> Error(castToParseError(hierarchyError))
    | TParenthesisHierarchy.Float(f)              -> Float(TParsFloatValue.LexID f)
    | TParenthesisHierarchy.Fn(fid)               -> Error(TParseError.NakedFunctionIdentifier fid)
    | TParenthesisHierarchy.Int(iid)              -> Int(TParsIntValue.LexID iid)
    | TParenthesisHierarchy.Let(lid)              -> Error(TParseError.NakedLetIdentifier lid)
    | TParenthesisHierarchy.Ref(rid)              -> Reference(TParsReferenceValue.LexID rid)
    | TParenthesisHierarchy.String(sid)           -> String(TParsStringValue.LexID sid)
  


  and  validateLet(lexLetId: TLexLetID,elements : (TParenthesisHierarchy list)) : TStringParseSymbolValue = 
      let rec validateLetArguments(letArguments : (TStringParseSymbolValue list)) : TLetArgumentValidation = 
        match letArguments with 
        | first::second::rest -> 
          match first with 
          | TStringParseSymbolValue.Reference rVal -> 
            match rVal with 
            | TParsReferenceValue.LexID lID -> 
              match validateLetArguments(rest) with
              | ValidLetArgument currentValid -> ValidLetArgument((TParsRefereeSValue.LexID(lID),second)::currentValid)
              | UnEvenNumberOfArguments -> UnEvenNumberOfArguments
              | ArgumentNotID nonid ->  ArgumentNotID nonid 
          | _ -> ArgumentNotID first
        | first::rest -> UnEvenNumberOfArguments
        | _ -> ValidLetArgument []
      if(elements.Length <> 2) then Error(LetWrongAmountOfArguments(TParsLetValue.LexID(lexLetId)))
      else 
      let firstElement = parseBuiltHierarchy(elements.Head)
      let secondElement = parseBuiltHierarchy(elements.Tail.Head)
      match firstElement with
      | Array letElements -> 
        match validateLetArguments(letElements) with  
        | ValidLetArgument validTupels -> TStringParseSymbolValue.Let(TParsLetValue.LexID lexLetId,validTupels,secondElement)
        | UnEvenNumberOfArguments -> TStringParseSymbolValue.Error(LetWrongAmountOfArguments(TParsLetValue.LexID lexLetId))
        | ArgumentNotID nonId -> TStringParseSymbolValue.Error(LetArgumentNoId(nonId))
      | _ -> Error(LetFirstArgumentNotValidArray(TParsLetValue.LexID(lexLetId)))




  and validateAssociative(elemList: (TParenthesisHierarchy list)) : TStringParseSymbolValue =
      match elemList with
      | first::second::rest -> 
        match validateAssociative(rest) with
        |  Error(AssociativeUnevenAmountOfParameters) -> Error(AssociativeUnevenAmountOfParameters)
        |  Associative childlist-> Associative((parseBuiltHierarchy(first),parseBuiltHierarchy(second))::childlist)
        | whatever -> whatever 
      | first::rest  -> Error(AssociativeUnevenAmountOfParameters)
      | [] -> Associative []  




  and validateFunction(lexFnId: TLexFnID,elements :(TParenthesisHierarchy list) ) : TStringParseSymbolValue = 
      let rec validateParameters(parameters: TStringParseSymbolValue list) : (bool* (TParsRefereeSValue list))        = 
        match parameters with
        |[] -> (true,[])
        | elem::rest -> 
          match elem with
          | Reference tpref -> 
            match tpref with 
            | TParsReferenceValue.LexID tpreflid -> 
               match validateParameters(rest) with
               | (true,relems) -> (true,TParsRefereeSValue.LexID(tpreflid)::relems)
               | (false,_)     -> (false, [])
          | noreference -> (false, []) 
      if(elements.Length <> 3) then Error(FunctionWrongAmountOfArguments(TParsFunctionValue.LexID(lexFnId))) 
      else 
        match elements with 
        | name::parameters::value::[] -> 
          let parsedParams = parseBuiltHierarchy(parameters)
          let parsedValue = parseBuiltHierarchy(value)
          match name with
          | TParenthesisHierarchy.Ref lexNameRefId ->
            match parsedParams with
            |  Array slist -> 
              match validateParameters(slist) with
              | (true, referees) -> TStringParseSymbolValue.FN(TParsFunctionValue.LexID lexFnId,
                                                               TParsRefereeSValue.LexID lexNameRefId,
                                                               referees, parsedValue)
              | (false,referees) -> Error(FunctionInvalidParameters(TParsFunctionValue.LexID(lexFnId)))
            | _ -> Error(FunctionParamsArrayInvalid(TParsFunctionValue.LexID(lexFnId)))
          | _ -> Error(FunctionFirstArgumentName(TParsFunctionValue.LexID(lexFnId),parseBuiltHierarchy(name)))
        | _ ->Error(FunctionWrongAmountOfArguments(TParsFunctionValue.LexID(lexFnId)))