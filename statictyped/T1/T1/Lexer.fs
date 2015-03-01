module Lexer
  open LexValues
  open CharDefinitions
  
  type TParseState = 
  | Char | EndChar | Id | Initial | Int | FloatDotDirect | Float | Space
  
  let rec lex(text: string, currentLineNumber: int, currentState : TParseState, 
              currentIndex :int, currentTextValue : string,
              startingIndex :int, acc : (PositionedTLexValue list)) 
          : (PositionedTLexValue list) = 
    if(text.Length < currentIndex) 
    then
      let currentCharString = text.Chars(currentIndex).ToString()
      let smatch(charGroup: string) = charGroup.Contains(currentCharString)
      let updatedString = currentTextValue + currentCharString
      let updatedIndex = currentIndex + 1
      
      let finishAndContinue (token : TLexValue) = 
        lex(text, currentLineNumber, TParseState.Initial, updatedIndex,"",updatedIndex,
            {charPosition = currentIndex;lineNumber = currentLineNumber;lexValue = token}::acc )
      let errorOut(token : TLexValue,nextStart) = 
        lex(text,currentLineNumber, TParseState.Initial, nextStart, "", nextStart, 
            {charPosition = currentIndex;lineNumber = currentLineNumber; lexValue = token}::acc)
      let appendAndContinue() = 
        lex(text,currentLineNumber, currentState, updatedIndex, 
            currentTextValue + currentCharString,startingIndex,acc)
      let appendAndNext(nextstate : TParseState) = 
        lex(text,currentLineNumber, nextstate, updatedIndex, currentTextValue + currentCharString,
            startingIndex,acc)
      let finishAndNext(token : TLexValue, nextState: TParseState,nextIndex)  = 
        lex(text, currentLineNumber, nextState, nextIndex, "",nextIndex, 
            {charPosition = startingIndex;lineNumber = currentLineNumber; lexValue =  token }::acc)
      
      match currentState with
      | Char -> if(smatch(validChar)) then appendAndNext(TParseState.EndChar)
                else errorOut(Error(UnknownCharacter), updatedIndex)
      | EndChar ->  if(smatch(charChar)) then finishAndNext(TLexValue.Char(updatedString),Initial,updatedIndex)
                    else errorOut(Error(BadCharacterEnding), updatedIndex)
      | Id -> if(smatch(TextNumberChar)) then appendAndContinue()
              elif (smatch(EndOtherChar)) then finishAndNext(TLexValue.Id(currentTextValue),TParseState.Initial,currentIndex)
              else errorOut(Error(BadIDEnding),currentIndex)
      | Initial -> if(smatch(numbers)) then appendAndNext(TParseState.Int)
                   elif(smatch(textChar)) then appendAndNext(TParseState.Id)
                   elif (smatch(spaceChar)) then appendAndNext(TParseState.Space)
                   elif (smatch(OpenPChar)) then finishAndContinue(TLexValue.LParenthesis)    
                   elif (smatch(ClosePChar)) then finishAndContinue(TLexValue.RParenthesis)    
                   elif (smatch(OpenBChar)) then finishAndContinue(TLexValue.LBracket)    
                   elif (smatch(CloseBChar)) then finishAndContinue(TLexValue.RBracket)    
                   elif (smatch(OpenCBChar)) then finishAndContinue(TLexValue.LCBracket)    
                   elif (smatch(CloseCBChar)) then finishAndContinue(TLexValue.RCBracket)    
                   elif (smatch(dotChar)) then appendAndNext(TParseState.FloatDotDirect)
                   else  errorOut(Error(UnknownCharacter),updatedIndex)
      | Int -> if(smatch(numbers)) then appendAndContinue()
               elif(smatch(dotChar)) then appendAndNext(TParseState.Float)
               elif(smatch(EndOtherChar)) then finishAndNext(TLexValue.Int(currentCharString),Initial,currentIndex)
               else errorOut(Error(BadIntEnding),currentIndex) 
      | FloatDotDirect -> if(smatch(numbers)) then appendAndNext(TParseState.Float)
                          elif(smatch(EndOtherChar)) then errorOut(Error(UndefinedFloatDot),currentIndex)
                          else errorOut(Error(BadFloatEnding),currentIndex)
      | Float -> if(smatch(numbers)) then appendAndContinue()
                 elif(smatch(EndOtherChar)) then finishAndNext(TLexValue.Float(currentCharString),Initial,currentIndex)
                 else errorOut(Error(BadFloatEnding),currentIndex)
      | Space -> if(smatch(spaceChar)) then appendAndContinue()
                 else finishAndNext(TLexValue.Spacing(currentCharString),TParseState.Initial,currentIndex)
                   
    else 
      let AddAndRevert(token : TLexValue) = 
        List.rev({lineNumber = currentLineNumber; charPosition = startingIndex; 
                  lexValue = TLexValue.Error(TLexError.BadCharacterEnding)}::acc)
      match currentState with
      | Char -> AddAndRevert(Error(TLexError.BadCharacterEnding))
      | EndChar -> AddAndRevert(Error(TLexError.BadCharacterEnding))
      | Id ->   AddAndRevert(TLexValue.String(currentTextValue))
      | Initial -> List.rev(acc)
      | Int -> AddAndRevert(TLexValue.Int(currentTextValue))
      | FloatDotDirect -> AddAndRevert(Error(UndefinedFloatDot))
      | Float -> AddAndRevert(TLexValue.Float(currentTextValue))
      | Space -> AddAndRevert(Spacing(currentTextValue))