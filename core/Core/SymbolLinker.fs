module SymbolLinker
  open ParserDefinitions
  open SymbolLinkerValues
  
  type SymbolTable = Map<TReferMatchingID,TRefereeID>

  let rec LinkSymbols(parsedExpression : TParseID, symbolTable : SymbolTable) :TLinkedValue = 
    let mapChildrenWithCurrentTable(elemList) = List.map (fun elem -> LinkSymbols(elem,symbolTable)) elemList 
    let mapChildrenTupledWithCurrentTable(elemList: ((TParseID * TParseID) list)): ((TLinkedValue * TLinkedValue) list) = List.map (fun elem -> match elem with | (first,second) -> (LinkSymbols(first,symbolTable),LinkSymbols(second,symbolTable))) elemList 
    match parsedExpression with
    | TParseID.Float f        -> TLinkedValue.Float f
    | TParseID.Char c         -> TLinkedValue.Char c 
    | TParseID.String s       -> TLinkedValue.String s
    | TParseID.Int i          -> TLinkedValue.Int i
    | TParseID.Referee(r,s)   -> TLinkedValue.Referee(r,s)
    | TParseID.Referring(r,s) -> 
     match symbolTable.TryFind(s) with
      |Some s -> TLinkedValue.Reference(r,s)
      |None   -> TLinkedValue.Error (LinkError.NameNotFound)
    | TParseID.Array(a,elemList)-> TLinkedValue.Array(a,mapChildrenWithCurrentTable(elemList))
    | TParseID.Associative(aid,pairs) -> TLinkedValue.Associative(aid,mapChildrenTupledWithCurrentTable(pairs))
    | TParseID.FN(fnId,referenceId,parameters,result)  -> TLinkedValue.Error NotYetImplemented //TODO ADD SYMBOL TABLE HERE
    | TParseID.Let(letId,assignments,result)  -> TLinkedValue.Error NotYetImplemented //TODO ADD SYMBOL TABLE HERE
    | TParseID.FNCall(id,arguments)  -> TLinkedValue.FNCall(id, mapChildrenWithCurrentTable(arguments)) 