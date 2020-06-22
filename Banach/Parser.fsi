namespace Banach

open Parsy

[<RequireQualifiedAccess>]
module Parser =

    val identifier : UIdent Parser

    val inlineExpression : UExpr Parser

    val expression : indent:int -> UExpr Parser

    val valueDefinition : indent:int -> UValueDef Parser

    val typeDefinition : indent:int -> UTypeDef Parser

    val definition : indent:int -> UDef Parser

    val definitions : indent:int -> UDef list Parser
