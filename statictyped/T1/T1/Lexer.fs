module Lexer
  
  //ID
  type TLID = {ID : int}
  type TLexFloatID =  
  | ID of TLID
  type TLexIntID =  
  | ID of TLID
  type TLexCharID =  
  | ID of TLID
  type TLexStringID =  
  | ID of TLID
  type TLexLetID =
  | ID of TLID
  type TLexFnID =
  | ID of TLID
  
  type TLexID = 
  | Float of TLexFloatID
  | Int of TLexIntID
  | Char of TLexCharID
  | String of TLexStringID
  | Let of TLexLetID
  | Fn of TLexFnID
  //VALUE
  type TLexValue =
  | Float of string
  | Int of string 
  | String of string 
  | Char of string
  | Id of string
  | Let
  | Fn
  | LParenthesis
  | RParenthesis
  | LCBracket
  | RCBracket
  | RBracket
  | LBracket