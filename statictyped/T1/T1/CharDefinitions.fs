module CharDefinitions


  let numbers        = "01234456789"
  let textChar       = "<>+-?!_:qwertyuiopasdfghjklmzxcvbnAZERTYUIOPQSDFGHKLMWXCVBN"
  let spaceChar      = " "
  let OpenPChar      = "(" 
  let ClosePChar     = ")"
  let OpenBChar      = "["
  let CloseBChar     = "]" 
  let OpenCBChar     = "{"
  let CloseCBChar    = "}"
  let dotChar        = "."
  let OpenChar       = OpenBChar + OpenCBChar + OpenPChar
  let CloseChar      = CloseBChar + CloseCBChar + ClosePChar 
  let EndOtherChar   = spaceChar + OpenBChar + CloseChar
  let TextNumberChar =  textChar + numbers
  let charChar       = "'"
  let validChar      = numbers + textChar + spaceChar + OpenChar
