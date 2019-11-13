data SimpleType = Integer | Cross SimpleType SimpleType | Func SimpleType SimpleType

type IntInt = Func Integer Integer

data Term = Numeral Integer | Variable String | Paren Term Term | FuncApply Term Term | Lambda Variable SimpleType Term

type t = Lambda "x" IntInt "x"

