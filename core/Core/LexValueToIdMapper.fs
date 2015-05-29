module LexValueToIdMapper
open LexValues
open LexDefinitions

let simpleValueToID value tlID =
    match value.lexValue with
    | TLexValue.Error _ ->TLexID.Error (TLexErrorID.ID tlID)
    | TLexValue.Spacing _-> TLexID.Spacing (TLexSpacingID.ID tlID)
    | TLexValue.Char _ -> TLexID.Char (TLexCharID.ID tlID)
    | TLexValue.Float _ ->TLexID.Float (TLexFloatID.ID tlID)
    | TLexValue.Fn _ ->   TLexID.Fn (TLexFnID.ID tlID)
    | TLexValue.Id _ ->   TLexID.Id (TLexIdID.ID tlID)
    | TLexValue.Int _ ->  TLexID.Int (TLexIntID.ID tlID)
    | TLexValue.Let _ ->  TLexID.Let (TLexLetID.ID tlID)
    | TLexValue.LBracket _ -> TLexID.LBracket(TLexLBracketID.ID tlID)
    | TLexValue.RBracket _->   TLexID.RBracket(TLexRBracketID.ID tlID)
    | TLexValue.LCBracket _->   TLexID.LCBracket(TLexLCBracketID.ID tlID)
    | TLexValue.RCBracket _->   TLexID.RCBracket(TLexRCBracketID.ID tlID)
    | TLexValue.LParenthesis _-> TLexID.LParenthesis(TLexLParenthesisID.ID tlID)
    | TLexValue.RParenthesis _->  TLexID.RParenthesis(TLexRParenthesisID.ID tlID)
    | TLexValue.Ref _-> TLexID.Ref(TLexRefID.ID tlID)
    | TLexValue.String _-> TLexID.String(TLexStringID.ID tlID)

let mapLexValuesToParseValue (lexed : TPositionedLexValue list) :  (Map<TLexID,TPositionedLexValue>* (TLexID list))   =

  let rec foldrec lexed   (provider : TLIDProvider) : (TLexID * TPositionedLexValue) list  = 
    match lexed with 
    | first::tail -> 
      let id = simpleValueToID  first provider.currentID
      let rest = provider.nextProvider() |> foldrec tail  // TODO stack inefficienty problem, bad tail recursion
      let res = (id,first)::rest in res
    | [] -> []
  let idBuilder = new TLIDProvider(0)
  let seqList = (foldrec lexed idBuilder)
  let retMap  = Map.ofList seqList
  (retMap, List.map (fun (a,b) -> a) seqList )
  
 
 