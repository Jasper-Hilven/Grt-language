module LexValues
//VALUE
  type TLexValue =
  | Float of string
  | Int  of string
  | String  of string
  | Char  of string
  | Id  of string
  | Let 
  | Fn  of string
  | Ref of string
  | LParenthesis 
  | RParenthesis
  | LCBracket
  | RCBracket
  | RBracket
  | LBracket
  | Spacing of string
  | Error of TLexError
  and TLexError = 
  | UnknownCharacter
  | UndefinedFloatDot
  | BadCharacterEnding
  | BadFloatEnding 
  | BadIDEnding
  | BadIntEnding


  type PositionedTLexValue = { lineNumber : int; charPosition:  int; lexValue : TLexValue}
