module Parser
  open System.Collections.Generic
  open LexDefinitions
  open ParserHierarchyBuilder     
  open ParserValues
  
  let castToParseError(hierarchyError : TParenthesisError) :TParseError = 
    match hierarchyError with
    | NotEndingParenthesisLike(parenthesisLike,plist) -> TParseError.NotEndingParenthesesLike parenthesisLike
    | TParenthesisError.WrongEndingParenthesisLike(left,plist,right)  -> TParseError.WrongEndingParenthesisLike(left ,right)
    | TParenthesisError.EndOfParsing                                  -> TParseError.EndOfParsingUnexpected
    | TParenthesisError.UnexpectedParenthesisLike unexpected          -> TParseError.UnexpectedParenthesisLike(unexpected)
      
  let rec parseBuiltHierarchy(hierarchy : TParenthesisHierarchy) : TStringParseSymbolValue = 
    let validateLet(lexLetId,elements) : TStringParseSymbolValue = Error(EmptyFunctionCall)
    let validateFunction(lexFnId,elements) : TStringParseSymbolValue = Error(EmptyFunctionCall)  
    let validateFunctionCall(elements) : TStringParseSymbolValue = Error(EmptyFunctionCall)  
    let validateArray(elemList) : TStringParseSymbolValue = Error(EmptyFunctionCall)
    let validateAssociative(elemList) : TStringParseSymbolValue = Error(EmptyFunctionCall)
    match hierarchy with
    | TParenthesisHierarchy.Parenthesis(lbracket,elemList,rBracket) -> 
      match elemList with
      | [] -> Error(EmptyFunctionCall)
      | firstElem::restElems ->
        match firstElem with  
        | TParenthesisHierarchy.Let lexLetId-> validateLet(lexLetId,restElems)   
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
