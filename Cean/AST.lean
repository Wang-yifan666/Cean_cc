namespace Cean

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

end Cean


/- ============================================================
   自测：这些 #guard 在 lake build 期间执行，断言失败会让构建失败
   ============================================================ -/

open Cean

-- 结构相同则相等
#guard
  AExpr.add (.const 1) (.const 2)
    == AExpr.add (.const 1) (.const 2)

-- 结构不同则不等（参数顺序也算结构）
#guard
  AExpr.add (.const 1) (.const 2)
    != AExpr.add (.const 2) (.const 1)

-- 构造子不同则不等
#guard
  BExpr.eq (.const 1) (.const 1)
    != BExpr.not (BExpr.eq (.const 1) (.const 1))

-- 嵌套结构按递归比较
#guard
  BExpr.and
    (BExpr.less (.const 1) (.const 2))
    (BExpr.not (BExpr.eq (.var "x") (.const 0)))
  ==
  BExpr.and
    (BExpr.less (.const 1) (.const 2))
    (BExpr.not (BExpr.eq (.var "x") (.const 0)))
