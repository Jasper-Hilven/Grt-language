module ParserValuesToIDMapper

open ParserDefinitions
open ParserValues
open LexDefinitions

let MapParseValuesToIDs (parsed : TStringParseSymbolValue) :  (Map<TParseID,TLexID>* TParseID)   =

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
  
