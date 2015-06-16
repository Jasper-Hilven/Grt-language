module LexErrorSpaceHandler
open LexDefinitions

let rec filterErrorAndSpace(lexed: TLexID list) : TCorrectLexID list= 
  match lexed with
  | [] -> []
  | first::rest ->
    match first with
    | TLexID.Float f -> TCorrectLexID.Float(f)::filterErrorAndSpace(rest)
    | TLexID.Char c -> TCorrectLexID.Char(c)::filterErrorAndSpace(rest)
    | TLexID.Error e-> filterErrorAndSpace(rest)
    | TLexID.Fn f-> TCorrectLexID.Fn(f)::filterErrorAndSpace(rest) 
    | TLexID.Id d-> TCorrectLexID.Id(d)::filterErrorAndSpace(rest)
    | TLexID.Int i-> TCorrectLexID.Int(i)::filterErrorAndSpace(rest)
    | TLexID.LBracket lb-> TCorrectLexID.LBracket(lb)::filterErrorAndSpace(rest)
    | TLexID.LCBracket lc-> TCorrectLexID.LCBracket(lc)::filterErrorAndSpace(rest)
    | TLexID.LParenthesis lp-> TCorrectLexID.LParenthesis(lp)::filterErrorAndSpace(rest)
    | TLexID.Let lt -> TCorrectLexID.Let(lt)::filterErrorAndSpace(rest)
    | TLexID.Letrec lt -> TCorrectLexID.Letrec(lt)::filterErrorAndSpace(rest)
    | TLexID.RBracket rb -> TCorrectLexID.RBracket(rb)::filterErrorAndSpace(rest)
    | TLexID.RCBracket rc -> TCorrectLexID.RCBracket(rc)::filterErrorAndSpace(rest)
    | TLexID.RParenthesis rp -> TCorrectLexID.RParenthesis(rp)::filterErrorAndSpace(rest)
    | TLexID.Ref r -> TCorrectLexID.Ref(r)::filterErrorAndSpace(rest)
    | TLexID.Spacing _ -> filterErrorAndSpace(rest)
    | TLexID.String s -> TCorrectLexID.String(s)::filterErrorAndSpace(rest) //TODO Invert logic for proper tail call
  

