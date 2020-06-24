﻿namespace Banach

type Ident = Ident of string list

type Hole = Hole of string

type ExprBody<'annot> =
| ExprIdent of Ident
| ExprApp of Expr<'annot> * Expr<'annot>
| ExprArr of Expr<'annot> * Expr<'annot>
| ExprNamed of Ident * Expr<'annot>
| ExprHole of Hole
| ExprMatch of Expr<'annot> * (Ident * Ident list * Expr<'annot>) list

and Expr<'annot> =
    {
        Body : ExprBody<'annot>
        Annotation : 'annot
    }

type ValueDef<'annot> =
    {
        Recursive : bool
        Name : Ident
        Parameters : (Ident * Expr<'annot>) list
        ReturnType : Expr<'annot>
        InnerDefinitions : Def<'annot> list
        Body : Expr<'annot>
        Annotation : 'annot
    }

and TypeDef<'annot> =
    {
        Name : Ident
        TType : Expr<'annot>
        Constructors : (Ident * Expr<'annot>) list
        Annotation : 'annot
    }

and Def<'annot> =
| ValueDef of ValueDef<'annot>
| TypeDef of TypeDef<'annot>
