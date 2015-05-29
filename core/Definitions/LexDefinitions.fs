module LexDefinitions

//ID
  type TLID = {ID : int}
  type TLexFloatID =  | ID of TLID
  type TLexIntID =    | ID of TLID
  type TLexCharID =   | ID of TLID
  type TLexStringID = | ID of TLID
  type TLexLetID =    | ID of TLID
  type TLexLetrecID = | ID of TLID
  type TLexFnID =     | ID of TLID
  type TLexRefID =    | ID of TLID
  type TLexLParenthesisID = | ID of TLID
  type TLexRParenthesisID = | ID of TLID
  type TLexLBracketID =     | ID of TLID
  type TLexRBracketID =     | ID of TLID
  type TLexLCBracketID =    | ID of TLID
  type TLexRCBracketID =    | ID of TLID
  let functionText = "fn"
  let letText = "let"

  type TLexLParenthesisLikeID =  
  | Parenthesis of TLexLParenthesisID
  | Bracket of TLexLBracketID
  | CBracket of TLexLCBracketID

  type TLexRParenthesisLikeID = 
  | Parenthesis of TLexRParenthesisID
  | Bracket of TLexRBracketID
  | CBracket of TLexRCBracketID

  type TLexID = 
  | Float of TLexFloatID
  | Int of TLexIntID
  | Char of TLexCharID
  | String of TLexStringID
  | Let of TLexLetID
  | Letrec of TLexLetrecID
  | Fn of TLexFnID
  | Ref of TLexRefID  
  | LParenthesis of TLexLParenthesisID
  | RParenthesis of TLexRParenthesisID
  | LCBracket    of TLexLCBracketID
  | RCBracket    of TLexRCBracketID    
  | RBracket     of TLexRBracketID
  | LBracket     of TLexLBracketID
  
  