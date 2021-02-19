package map.reduce.program.synthesis

sealed trait Term {
    val getType: Type
    val weight: Int
}

case class Wildcard(t: Type, context: Set[Variable]) extends Term {
    override val getType: Type = t
    override val weight: Int = 0

    override def toString: String = s"〇: $t"
}

class Variable(val sub: Int, val t: Type) extends Term {
    override val getType: Type = t
    override val weight: Int = 1

    override def toString: String = s"v$sub"

    def updateType(nt: Type): Variable =
        new Variable(sub, nt)

    override def hashCode(): Int = t.hashCode() + sub

    override def equals(obj: Any): Boolean = obj match {
        case Variable(sub2, t2) => sub2 == sub && t2 == t
        case _ => false
    }
}

case class Function(ps: List[Term], component: Component) extends Term {
    override val getType: Type = component.getType
    override val weight: Int = 4 + ps.map(_.weight).sum

    override def toString: String = {
        val parameters = ps.mkString("(", ", ", ")")
        s"${component.getName}$parameters"
    }
}

case class Lambda(v: Variable, e: Term) extends Term {
    lazy val getType: Type = FunctionType(List(v.getType), e.getType)
    override val weight: Int = v.weight + e.weight

    override def toString: String = s"$v: ${v.getType} => $e"
}

case object Unit extends Term {
    override val getType: Type = UnitType
    override val weight: Int = 0

    override def toString: String = ""
}

object Variable {
    var subscript: Int = 0

    def apply(t: Type): Variable = {
        subscript += 1
        new Variable(subscript, t)
    }

    def unapply(arg: Variable): Option[(Int, Type)] = Some((arg.sub, arg.t))
}

object Term {
    // explicit give the type of variable, since expr x => const0() will not type check
    // need to rewrite as (x: Any) => const0() even though x is dropped when eval
    def convertVariableType(v: Variable): String = v.getType match {
        case GenericType(_) => "Any"
        case _ => v.getType.toString
    }

    def convert(term: Term): String = term match {
        case lambda: Lambda =>
            val (vs, e) = uncurry(lambda)
            val parameters = vs.map(v => s"${v.toString}: ${convertVariableType(v)}").mkString("(", ", ", ")")
            val expr = convert(e)
            s"$parameters => $expr"
        case Function(ps, component) =>
            val name = component.getName.toUpperCase
            val parameters = ps.map(t => convert(t)).mkString("", ", ", "")
            s"map.reduce.program.synthesis.Component.$name.eval($parameters)"
        case Unit | Variable(_, _) => term.toString
        case Wildcard(_, _) => throw new RuntimeException("can not convert a incomplete program.")
    }

    def uncurry(lambda: Lambda): (List[Variable], Term) = lambda match {
        case Lambda(v, e: Lambda) =>
            val (vs, expr) = uncurry(e)
            (v :: vs, expr)
        case Lambda(v, e) => (List(v), e)
    }
}