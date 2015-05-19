module SymbolLinker
  open ParserDefinitions
  open SymbolLinkerValues
  
  
  
  type SymbolTable = 
    | Empty
    | Segment of SymbolTable * Map<TReferMatchingID,TRefereeID>
  let rec findInTable(table : SymbolTable, symbolName : TReferMatchingID) : (TRefereeID option) = 
    match table with
    | Empty -> None
    | Segment(parent,locals) -> match locals.TryFind(symbolName) with
                                | Some s -> Some(s)
                                | None   -> findInTable(parent,symbolName)
  
  type TableExpandResult = | DuplicatedMatchingIDs | Succes of SymbolTable 
  let expandTable(originalTable : SymbolTable, ids : (TRefereeCID list)) : TableExpandResult = 
    let uniqueIDMap : Map<TReferMatchingID,TRefereeID> = 
      List.fold  (fun (acc: Map<TReferMatchingID,TRefereeID>) (id : TRefereeCID) -> 
                    if(acc.ContainsKey(id.matchingID)) then acc else acc.Add(id.matchingID,id.refereeID)) 
                 Map.empty
                 ids
    if(uniqueIDMap.Count < ids.Length) then DuplicatedMatchingIDs
    else  Succes(Segment(originalTable,uniqueIDMap))
  
  
  
  
  let rec LinkSymbols(parsedExpression : TParseID, symbolTable : SymbolTable) :TLinkedValue = 
    let mapChildrenWithCurrentTable(elemList) = List.map (fun elem -> LinkSymbols(elem,symbolTable)) elemList 
    let mapChildrenTupledWithCurrentTable(elemList: ((TParseID * TParseID) list)): ((TLinkedValue * TLinkedValue) list) = 
      List.map (fun elem -> match elem with | (first,second) -> (LinkSymbols(first,symbolTable),LinkSymbols(second,symbolTable))) elemList 
    match parsedExpression with
    | TParseID.Float f        -> TLinkedValue.Float f
    | TParseID.Char c         -> TLinkedValue.Char c 
    | TParseID.String s       -> TLinkedValue.String s
    | TParseID.Int i          -> TLinkedValue.Int i
    | TParseID.Referee r      -> Error(UnexpectedReferee) 
    | TParseID.Referring(referring) -> 
     let symbol = referring.matchingID
     let r =referring.referringID
     match findInTable(symbolTable, symbol) with
      |Some s -> TLinkedValue.Reference(s)
      |None   -> TLinkedValue.Error (LinkError.NameNotFound)
    | TParseID.Array(a,elemList)-> TLinkedValue.Array(a,mapChildrenWithCurrentTable(elemList))
    | TParseID.Associative(aid,pairs) -> TLinkedValue.Associative(aid,mapChildrenTupledWithCurrentTable(pairs))
    | TParseID.FN(fnId,refereeId,parameters,result)  -> 
        match expandTable(symbolTable,[refereeId]) with
        | Succes fnInTable -> 
          match expandTable(fnInTable,parameters) with
          |  Succes updatedTable -> TLinkedValue.FN(fnId, 
                                                    refereeId.refereeID,
                                                    List.map (fun c->c.refereeID) parameters,
                                                    LinkSymbols(result,updatedTable))
          | DuplicatedMatchingIDs -> Error DuplicateName
        | DuplicatedMatchingIDs -> Error UnexpectedToComeHere
    | TParseID.Letrec(letrecId,assignments,result)  -> 
      let updatedTable = expandTable(symbolTable, List.map (fun (rcid: TRefereeCID,valueid: TParseID) -> rcid) assignments)
      match updatedTable with
      | DuplicatedMatchingIDs -> Error DuplicateName
      | Succes updatedTableSucces -> 
        let parsedArguments : ((TRefereeID * TLinkedValue) list) = List.map (fun (refID,parsID) -> (refID.refereeID, LinkSymbols(parsID,updatedTableSucces))) assignments
        TLinkedValue.Letrec(letrecId, parsedArguments,LinkSymbols(result,updatedTableSucces)) 
    | TParseID.Let(letId,assignments,result)  -> 
      let updatedTable = expandTable(symbolTable, List.map (fun (rcid: TRefereeCID,valueid: TParseID) -> rcid) assignments)
      match updatedTable with
      | DuplicatedMatchingIDs -> Error DuplicateName
      | Succes updatedTableSucces -> 
        let parsedArguments : ((TRefereeID * TLinkedValue) list) = List.map (fun (refID,parsID) -> (refID.refereeID, LinkSymbols(parsID,updatedTableSucces))) assignments
        TLinkedValue.Let(letId, parsedArguments,LinkSymbols(result,updatedTableSucces)) //TODO: LET and LETREC are linked in the same way, change definitions 
    | TParseID.FNCall(id,arguments)  -> TLinkedValue.FNCall(id, mapChildrenWithCurrentTable(arguments)) 