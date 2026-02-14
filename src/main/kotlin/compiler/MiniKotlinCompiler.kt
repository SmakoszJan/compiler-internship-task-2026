package org.example.compiler

import MiniKotlinBaseVisitor
import MiniKotlinParser

// arg is the argument id of the continuation
// this is more intuition than formality
sealed class CpsFunction(val arg: Int) {

    abstract fun emit(asExpr: Boolean): String

    abstract fun then(func: CpsFunction): CpsFunction

    abstract fun subCont(k: Lambda): CpsFunction

    abstract fun subArg(x: Int, l: CpsFunction): CpsFunction
}

abstract class Term(arg: Int) : CpsFunction(arg) {
    override fun emit(asExpr: Boolean): String {
        require(asExpr)
        return emit()
    }

    abstract fun emit(): String
}

class Lambda(arg: Int, val body: CpsFunction) : CpsFunction(arg) {
    override fun emit(asExpr: Boolean): String {
        require(asExpr)
        return "arg$arg -> ${body.emit(true)}"
    }

    override fun then(func: CpsFunction) = Lambda(arg, body.then(func))

    override fun subCont(k: Lambda) = Lambda(arg, body.subCont(k))

    override fun subArg(x: Int, l: CpsFunction) = if (x == arg) {
        body.subArg(x, l)
    } else {
        Lambda(arg, body.subArg(x, l))
    }
}

// NoOp doesn't have any continuation. It's effectively a program termination
// which means it's malformed as a cps function. Expect special treatment in various places
class NoOp(arg: Int) : CpsFunction(arg) {
    // Pretty sure this should never be called outside of debugging purposes
    override fun emit(asExpr: Boolean): String = "{}"

    override fun then(func: CpsFunction) = func

    override fun subCont(k: Lambda) = k

    override fun subArg(x: Int, l: CpsFunction) = this
}


class Arg(arg: Int) : Term(arg) {
    override fun emit(): String = "arg$arg"

    override fun then(func: CpsFunction) = func

    override fun subCont(k: Lambda) = if (k.arg == arg) {
        k
    } else {
        k.subArg(k.arg, this)
    }

    override fun subArg(x: Int, l: CpsFunction) = if (x == arg) l else this
}

class Atom(
    id: Int,
    val content: String,
) : Term(id) {
    override fun emit(): String = content

    override fun then(func: CpsFunction) = func

    override fun subCont(k: Lambda) = k.subArg(k.arg, this)

    override fun subArg(x: Int, l: CpsFunction) = this
}

class If(
    arg: Int,
    val cond: Term,
    val then: CpsFunction,
    val otherwise: CpsFunction,
) : CpsFunction(arg) {
    override fun emit(asExpr: Boolean): String {
        val ret = """
            if (${cond.emit()}) {
                ${then.emit(false)}
            } else {
                ${otherwise.emit(false)}
            }
        """.trimIndent()

        return if (asExpr) "{$ret}" else ret
    }

    override fun then(func: CpsFunction) = If(arg, cond, then.then(func), otherwise.then(func))

    override fun subCont(k: Lambda) = If(arg, cond, then.subCont(k), otherwise.subCont(k))

    override fun subArg(x: Int, l: CpsFunction): CpsFunction {
        val cond = cond.subArg(x, l)
        val then = then.subArg(x, l)
        val otherwise = otherwise.subArg(x, l)

        return if (cond is Term) {
            If(arg, cond, then, otherwise)
        } else {
            cond.subCont(Lambda(cond.arg, If(arg, Arg(cond.arg), then, otherwise)))
        }
    }
}

class Return(arg: Int, val ret: Term) : CpsFunction(arg) {
    override fun emit(asExpr: Boolean): String = "k.accept(${ret.emit()})" + if (asExpr) {
        ""
    } else {
        ";"
    }

    override fun then(func: CpsFunction) = this

    override fun subCont(k: Lambda) = this

    override fun subArg(x: Int, l: CpsFunction): CpsFunction {
        val ret = ret.subArg(x, l)

        return if (ret is Term) {
            Return(arg, ret)
        } else {
            ret.subCont(Lambda(ret.arg, Return(arg, Arg(ret.arg))))
        }
    }
}

class Call(arg: Int, val name: String, val params: List<Term>, val then: CpsFunction) : CpsFunction(arg) {
    override fun emit(asExpr: Boolean): String {
        val paramsString = params.joinToString("") {
            it.emit() + ", "
        }
        val name = if (name == "println") "Prelude.$name" else name

        return "$name($paramsString arg$arg -> ${then.emit(true)})" + if (asExpr) "" else ";"
    }

    override fun then(func: CpsFunction) = Call(arg, name, params, then.then(func))

    override fun subCont(k: Lambda) = if (then is NoOp) {
        Call(arg, name, params, k.body.subArg(k.arg, Arg(arg)))
    } else {
        Call(arg, name, params, then.subCont(k))
    }

    override fun subArg(x: Int, l: CpsFunction): CpsFunction {
        val paramFunctions = mutableListOf<CpsFunction>()
        val newParams = params.map {
            val param = it.subArg(x, l)
            if (param is Term) {
                param
            } else {
                paramFunctions.add(param)
                Arg(param.arg)
            }
        }

        return paramFunctions.foldRight(Call(arg, name, newParams, then.subArg(x, l))) { param, base: CpsFunction ->
            param.subCont(Lambda(param.arg, base))
        }
    }
}

class BinOp(arg: Int, val lhs: Term, val op: String, val rhs: Term) : Term(arg) {
    override fun emit(): String = "(${lhs.emit()} $op ${rhs.emit()})"

    override fun then(func: CpsFunction) = func

    override fun subCont(k: Lambda) = k.subArg(k.arg, this)

    override fun subArg(x: Int, l: CpsFunction): CpsFunction {
        val lhs = lhs.subArg(x, l)
        val rhs = rhs.subArg(x, l)

        return if (lhs is Term) {
            if (rhs is Term) {
                BinOp(arg, lhs, op, rhs)
            } else {
                rhs.subCont(Lambda(rhs.arg, BinOp(arg, lhs, op, Arg(rhs.arg))))
            }
        } else {
            if (rhs is Term) {
                lhs.subCont(Lambda(lhs.arg, BinOp(arg, Arg(lhs.arg), op, rhs)))
            } else {
                lhs.subCont(
                    Lambda(
                        lhs.arg, rhs.subCont(
                            Lambda(
                                rhs.arg, BinOp(arg, Arg(lhs.arg), op, Arg(rhs.arg))
                            )
                        )
                    )
                )
            }
        }
    }
}

class UnOp(arg: Int, val op: String, val operand: Term) : Term(arg) {
    override fun emit(): String = "($op${operand.emit()})"

    override fun then(func: CpsFunction) = func

    override fun subCont(k: Lambda) = k.subArg(k.arg, this)

    override fun subArg(x: Int, l: CpsFunction): CpsFunction {
        val operand = operand.subArg(x, l)

        return if (operand is Term) {
            UnOp(arg, op, operand)
        } else {
            operand.subCont(Lambda(operand.arg, UnOp(arg, op, Arg(operand.arg))))
        }
    }
}

data class Binding(val type: String?, val name: String, val value: Term)

// I'm not really sure if it's legal in CPS, but let bindings and assignments get treated the same
// and are allowed in sequence
class LetAssign(arg: Int, val bindings: List<Binding>, val then: CpsFunction) : CpsFunction(arg) {
    override fun emit(asExpr: Boolean): String {
        val body = """
            ${
            bindings.joinToString("") {
                it.type.orEmpty() + " ${it.name} = ${it.value.emit()};"
            }
        }
            ${then.emit(false)}
        """.trimIndent()

        return if (asExpr) "{$body}" else body
    }

    override fun then(func: CpsFunction) =
        if (func is LetAssign && then is NoOp) {
            LetAssign(arg, bindings + func.bindings, func.then)
        } else {
            LetAssign(arg, bindings, then.then(func))
        }

    override fun subCont(k: Lambda) = LetAssign(arg, bindings, then.subCont(k))

    override fun subArg(x: Int, l: CpsFunction): CpsFunction {
        val bindingFunctions = mutableListOf<CpsFunction>()
        val newBindings = bindings.map {
            val value = it.value.subArg(x, l)
            if (value is Term) {
                Binding(it.type, it.name, value)
            } else {
                bindingFunctions.add(value)
                Binding(it.type, it.name, Arg(value.arg))
            }
        }

        val base = LetAssign(arg, newBindings, then.subArg(x, l))
        return bindingFunctions.foldRight(base) { value, base: CpsFunction ->
            value.subCont(Lambda(value.arg, base))
        }
    }
}

// Helper for while loops
class Continue(arg: Int) : CpsFunction(arg) {
    override fun emit(asExpr: Boolean) =
        "((Continuation<Object>) loopCon$arg).accept(loopCon$arg)" + if (asExpr) "" else ";"

    override fun then(func: CpsFunction) = this

    override fun subCont(k: Lambda) = this

    override fun subArg(x: Int, l: CpsFunction) = this
}

class While(arg: Int, val cond: CpsFunction, val repeat: CpsFunction, val then: CpsFunction) : CpsFunction(arg) {
    val loop = cond.subCont(Lambda(cond.arg, If(arg, Arg(cond.arg), repeat.then(Continue(arg)), then)))

    override fun emit(asExpr: Boolean): String {
        val body = """
            Continuation<Object> loop$arg = loopCon$arg -> ${loop.emit(true)};
            loop$arg.accept(loop$arg);
        """.trimIndent()

        return if (asExpr) "{$body}" else body
    }

    override fun then(func: CpsFunction) = While(arg, cond, repeat, then.then(func))

    override fun subCont(k: Lambda) = While(arg, cond, repeat, then.subCont(k))

    override fun subArg(x: Int, l: CpsFunction) = While(arg, cond.subArg(x, l), repeat.subArg(x, l), then.subArg(x, l))
}

class MiniKotlinCompiler : MiniKotlinBaseVisitor<CpsFunction>() {
    var expr: Int = 0


    fun compile(program: MiniKotlinParser.ProgramContext, className: String = "MiniProgram"): String {
        val functions = program.functionDeclaration().joinToString("") { func ->
            val name = func.IDENTIFIER().text
            val type = convertType(func.type())
            val params = func.parameterList()?.parameter().orEmpty()
                .joinToString("") { param -> "${convertType(param.type())} var_${param.IDENTIFIER().text}, " }
            val body = visit(func.block())
                .then(Return(expr++, Atom(expr++, "null")))
                .emit(false)
            """
                static void $name($params Continuation<$type> k) {
                    $body
                }
                
            """.trimIndent()
        }
        return """
            public class $className {
                static class Compiled$className {
                    $functions
                }
                
                public static void main(String[] args) {
                    Compiled$className.main(v -> {});
                }
            }
        """.trimIndent()
    }

    fun convertType(type: MiniKotlinParser.TypeContext) = when {
        type.BOOLEAN_TYPE() != null -> "Boolean"
        type.INT_TYPE() != null -> "Integer"
        type.UNIT_TYPE() != null -> "Void"
        type.STRING_TYPE() != null -> "String"
        else -> throw Error("Unknown type ${type.text}")
    }

    fun unOp(op: String, operand: CpsFunction) =
        operand.subCont(Lambda(operand.arg, UnOp(expr++, op, Arg(operand.arg))))

    fun binOp(lhs: CpsFunction, op: String, rhs: CpsFunction) =
        lhs.subCont(
            Lambda(
                lhs.arg, rhs.subCont(
                    Lambda(rhs.arg, BinOp(expr++, Arg(lhs.arg), op, Arg(rhs.arg)))
                )
            )
        )

    override fun defaultResult() = NoOp(expr++)

    override fun aggregateResult(aggregate: CpsFunction, nextResult: CpsFunction) = aggregate.then(nextResult)

    override fun visitVariableDeclaration(ctx: MiniKotlinParser.VariableDeclarationContext): CpsFunction {
        val value = visit(ctx.expression())
        val name = "var_${ctx.IDENTIFIER().text}"

        return value.subCont(
            Lambda(
                value.arg,
                LetAssign(
                    expr++,
                    listOf(Binding(convertType(ctx.type()), name, Arg(value.arg))),
                    NoOp(expr++)
                )
            )
        )
    }

    override fun visitVariableAssignment(ctx: MiniKotlinParser.VariableAssignmentContext): CpsFunction {
        val value = visit(ctx.expression())
        val name = "var_${ctx.IDENTIFIER().text}"

        return value.subCont(
            Lambda(
                value.arg,
                LetAssign(expr++, listOf(Binding(null, name, Arg(value.arg))), NoOp(expr++))
            )
        )
    }

    override fun visitIfStatement(ctx: MiniKotlinParser.IfStatementContext): CpsFunction {
        val cond = visit(ctx.expression())
        return cond.subCont(
            Lambda(
                cond.arg, If(
                    expr++,
                    Arg(cond.arg),
                    visit(ctx.block(0)),
                    ctx.block(1)?.let { visit(it) } ?: NoOp(expr++)
                )))
    }

    override fun visitWhileStatement(ctx: MiniKotlinParser.WhileStatementContext) =
        While(expr++, visit(ctx.expression()), visit(ctx.block()), NoOp(expr++))

    override fun visitReturnStatement(ctx: MiniKotlinParser.ReturnStatementContext): CpsFunction {
        val ret = visit(ctx.expression())

        return ret.subCont(Lambda(ret.arg, Return(expr++, Arg(ret.arg))))
    }

    override fun visitAndExpr(ctx: MiniKotlinParser.AndExprContext): CpsFunction {
        val lhs = visit(ctx.expression(0))
        val rhs = visit(ctx.expression(1))

        return lhs.subCont(
            Lambda(
                lhs.arg,
                LetAssign(
                    expr++,
                    listOf(Binding("Boolean", "tmp${lhs.arg}", Arg(lhs.arg))),
                    If(expr++, Atom(expr++, "tmp${lhs.arg}"), rhs, Atom(expr++, "tmp${lhs.arg}"))
                )
            )
        )
    }

    override fun visitFunctionCallExpr(ctx: MiniKotlinParser.FunctionCallExprContext): CpsFunction {
        val paramFunctions = mutableListOf<CpsFunction>()
        val params = ctx.argumentList()?.expression().orEmpty().map {
            val v = visit(it)
            paramFunctions.add(v)
            Arg(v.arg)
        }

        val base = Call(expr++, ctx.IDENTIFIER().text, params, NoOp(expr++))
        return paramFunctions.foldRight(base) { param, base: CpsFunction ->
            param.subCont(Lambda(param.arg, base))
        }
    }

    override fun visitMulDivExpr(ctx: MiniKotlinParser.MulDivExprContext): CpsFunction {
        val lhs = visit(ctx.expression(0))
        val rhs = visit(ctx.expression(1))
        val op = when {
            ctx.MULT() != null -> "*"
            ctx.DIV() != null -> "/"
            ctx.MOD() != null -> "%"
            else -> error("Unknown muldiv operator ${ctx.expression()}")
        }

        return binOp(lhs, op, rhs)
    }

    override fun visitOrExpr(ctx: MiniKotlinParser.OrExprContext): CpsFunction {
        val lhs = visit(ctx.expression(0))
        val rhs = visit(ctx.expression(1))

        return lhs.subCont(
            Lambda(
                lhs.arg,
                LetAssign(
                    expr++,
                    listOf(Binding("Boolean", "tmp${lhs.arg}", Arg(lhs.arg))),
                    If(expr++, Atom(expr++, "tmp${lhs.arg}"), Atom(expr++, "tmp${lhs.arg}"), rhs)
                )
            )
        )
    }

    override fun visitEqualityExpr(ctx: MiniKotlinParser.EqualityExprContext): CpsFunction {
        val lhs = visit(ctx.expression(0))
        val rhs = visit(ctx.expression(1))
        val op = when {
            ctx.EQ() != null -> "=="
            ctx.NEQ() != null -> "!="
            else -> error("Unknown equality operator ${ctx.expression()}")
        }

        return binOp(lhs, op, rhs)
    }

    override fun visitComparisonExpr(ctx: MiniKotlinParser.ComparisonExprContext): CpsFunction {
        val lhs = visit(ctx.expression(0))
        val rhs = visit(ctx.expression(1))
        val op = when {
            ctx.GE() != null -> ">="
            ctx.GT() != null -> ">"
            ctx.LT() != null -> "<"
            ctx.LE() != null -> "<="
            else -> error("Unknown comparison operator ${ctx.expression()}")
        }

        return binOp(lhs, op, rhs)
    }

    override fun visitAddSubExpr(ctx: MiniKotlinParser.AddSubExprContext): CpsFunction {
        val lhs = visit(ctx.expression(0))
        val rhs = visit(ctx.expression(1))
        val op = when {
            ctx.PLUS() != null -> "+"
            ctx.MINUS() != null -> "-"
            else -> error("Unknown addsub operator ${ctx.expression()}")
        }

        return binOp(lhs, op, rhs)
    }

    override fun visitNotExpr(ctx: MiniKotlinParser.NotExprContext): CpsFunction {
        val operand = visit(ctx.expression())

        return unOp("!", operand)
    }

    override fun visitIntLiteral(ctx: MiniKotlinParser.IntLiteralContext) = Atom(expr++, ctx.text)

    override fun visitBoolLiteral(ctx: MiniKotlinParser.BoolLiteralContext) = Atom(expr++, ctx.text)

    override fun visitStringLiteral(ctx: MiniKotlinParser.StringLiteralContext) = Atom(expr++, ctx.text)

    override fun visitIdentifierExpr(ctx: MiniKotlinParser.IdentifierExprContext) = Atom(expr++, "var_${ctx.text}")
}
