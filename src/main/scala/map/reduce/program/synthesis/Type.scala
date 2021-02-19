package map.reduce.program.synthesis

trait Type
trait BaseType extends Type

case object IntType extends BaseType {
    override def toString: String = "Int"
}

case object StringType extends BaseType {
    override def toString: String = "String"
}

case class ListType(t: Type) extends Type {
    override def toString: String = s"List[$t]"
}

case class FunctionType(its: List[Type], ot: Type) extends Type {
    override def toString: String = {
        val prefix = its.mkString("", " => ", " => ")
        s"$prefix$ot"
    }
}

case class GenericType(t: String) extends Type {
    override def toString: String = s"'$t"
}

case object UnitType extends BaseType {
    override def toString: String = "()"
}
