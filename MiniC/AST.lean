namespace MiniC

-- 通过递归定义算术表达式
inductive AExpr where
  | const : Int → AExpr
  | var   : String → AExpr
  | add   : AExpr → AExpr → AExpr
  | sub   : AExpr → AExpr → AExpr
  | mul   : AExpr → AExpr → AExpr
deriving Repr, BEq


inductive BExpr where
  | eq   : AExpr → AExpr → BExpr
  | less : AExpr → AExpr → BExpr
  | and  : BExpr → BExpr → BExpr
  | not  : BExpr → BExpr
deriving Repr, BEq

end MiniC


-- Test
#eval MiniC.AExpr.add
  (MiniC.AExpr.const 1)
  (MiniC.AExpr.const 2)
