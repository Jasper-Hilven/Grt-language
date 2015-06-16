module TypeInferer


type TypeID = int


//Just demo types, these will be replaced with valid types
type InferredType = 
| Function of (InferredType list) * InferredType
| Int
| String
| FreeType of TypeID

type TypeRule = 
| Equality of InferredType * InferredType

type ValidateTypeResult =
| Valid of (TypeRule list)
| InValid

let applyRuleSetOnOther validatedList typeRule = [Equality(InferredType.Int,InferredType.Int)]
let breakDownRuleSet typeRules : TypeRule list = []
let checkValidRules rules = true

let validateTypes(validatedList,typeRule) : ValidateTypeResult = 
  let updatedTypeRules : TypeRule list = applyRuleSetOnOther validatedList typeRule
  let bareTypeRules : TypeRule list = breakDownRuleSet updatedTypeRules
  match (checkValidRules bareTypeRules) with
  | false -> ValidateTypeResult.InValid   
  | true -> 
    let updatedRuleSet = applyRuleSetOnOther bareTypeRules validatedList
    ValidateTypeResult.Valid updatedRuleSet
  

