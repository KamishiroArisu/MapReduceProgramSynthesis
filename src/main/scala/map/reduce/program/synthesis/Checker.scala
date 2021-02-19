package map.reduce.program.synthesis

import z3.scala.Z3Context

object Checker {
    private def mkSort(t: Type): String = t match {
        case IntType | StringType => t.toString
        case ListType(e) =>
            val es = mkSort(e)
            s"(Seq $es)"
    }

    private def mkFreshVariable(id: String, t: Type): String = t match {
        case IntType | StringType =>
            s"(declare-const $id $t)"
        case ListType(e) =>
            val es = mkSort(e)
            s"(declare-const $id (Seq $es))"
    }

    private def encode(term: Term, map: Map[String, String]): String = {
        def fun(t: Term): String = t match {
            case Function(ps, component) if component.isInstanceOf[Component0[_]] =>
                component.encode(List())
            case Function(ps, component) =>
                val ers = ps.map(p => fun(p))
                component.encode(ers)
            case v: Variable => map(v.toString)
        }

        fun(term)
    }

    def checkCommutative(term: Term): Boolean = term match {
        case Lambda(var1, Lambda(var2, e)) if var1.t == var2.t =>
            val t = var1.t
            val ctx = new Z3Context(Map.empty[String, String])
            val v1 = var1.toString
            val v2 = var2.toString
            val x = mkFreshVariable("x", t)
            val y = mkFreshVariable("y", t)

            // o1 = x op y
            val o1 = encode(e, Map(v1 -> "x", v2 -> "y"))
            // o2 = y op x
            val o2 = encode(e, Map(v1 -> "y", v2 -> "x"))
            // Commutative is true iff assertion(o1 != o2) is unsolvable
            val constraints = s"(assert (not (= $o1 $o2)))"
            val s = s"""
                            |$x
                            |$y
                            |$constraints
                            |""".stripMargin
            val ast = ctx.parseSMTLIB2String(s)

            val solver = ctx.mkSolver()
            solver.assertCnstr(ast)
            solver.check().contains(false)
        case _ => false
    }

    def checkAssociative(term: Term): Boolean = term match {
        case Lambda(var1, Lambda(var2, e)) if var1.t == var2.t =>
            val t = var1.t
            val ctx = new Z3Context(Map.empty[String, String])
            val v1 = var1.toString
            val v2 = var2.toString
            val x = mkFreshVariable("x", t)
            val y = mkFreshVariable("y", t)
            val z = mkFreshVariable("z", t)

            // o1 = ((x op y) op z)
            val o1 = encode(e, Map(v1 -> encode(e, Map(v1 -> "x", v2 -> "y")), v2 -> "z"))
            // o2 = (x op (y op z))
            val o2 = encode(e, Map(v1 -> "x", v2 -> encode(e, Map(v1 -> "y", v2 -> "z"))))
            // Associative is true iff assertion(o1 != o2) is unsolvable
            val constraints = s"(assert (not (= $o1 $o2)))"
            val s = s"""
                       |$x
                       |$y
                       |$z
                       |$constraints
                       |""".stripMargin
            val ast = ctx.parseSMTLIB2String(s)

            val solver = ctx.mkSolver()
            solver.assertCnstr(ast)
            solver.check().contains(false)
        case _ => false
    }

    def checkCommutativeSemiGroup(term: Term): Boolean =
        checkCommutative(term) && checkAssociative(term)
}