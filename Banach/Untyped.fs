namespace Banach

type UIdent = UIdent of string list

type UHole = UHole of string

type UExpr =
| UExprIdent of UIdent
| UExprApp of UExpr * UExpr
| UExprArr of UExpr * UExpr
| UExprNamed of UIdent * UExpr
| UExprHole of UHole
| UExprMatch of UExpr * (UIdent * UIdent list * UExpr) list

type UValueDef =
    {
        Recursive : bool
        Name : UIdent
        Parameters : (UIdent * UExpr) list
        ReturnType : UExpr
        InnerDefinitions : UDef list
        Body : UExpr
    }

and UTypeDef =
    {
        Name : UIdent
        TType : UExpr
        Constructors : (UIdent * UExpr) list
    }

and UDef =
| UValueDef of UValueDef
| UTypeDef of UTypeDef
