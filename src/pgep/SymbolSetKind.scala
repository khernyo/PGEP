package pgep

sealed abstract class SymbolSetKind
case object Func extends SymbolSetKind
case object Var extends SymbolSetKind
case object Const extends SymbolSetKind
