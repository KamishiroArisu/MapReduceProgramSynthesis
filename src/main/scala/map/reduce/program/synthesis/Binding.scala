package map.reduce.program.synthesis

class Binding(content: DisjointSet[Type]) {
    private def isGeneric(t: Type): Boolean =
        t.isInstanceOf[GenericType]

    // t1 <-> t2 violates the binding iff t1 <-> x and t2 <-> y when x and y are 2 diff concrete types
    def isViolate(t1: Type, t2: Type): Boolean = (content.find(t1), content.find(t2)) match {
        case (Some(x), Some(y)) if !x.isInstanceOf[GenericType] && !y.isInstanceOf[GenericType] && x != y => true
        case (Some(x), None) if !x.isInstanceOf[GenericType] && !t2.isInstanceOf[GenericType] && x != t2 => true
        case (None, Some(y)) if !y.isInstanceOf[GenericType] && !t1.isInstanceOf[GenericType] && t1 != y => true
        case _ => false
    }

    def updated(t1: Type, t2: Type): Binding =
        new Binding(content.union(t1, t2))

    def getReprType(t: Type): Option[Type] =
        content.find(t)

    def same(t1: Type, t2: Type): Boolean =
        !(!isGeneric(t1) && !isGeneric(t2) && t1 != t2) && content.same(t1, t2)

    override def toString: String = content.toString
}

object Binding {
    // use compare function to insure that the repr type of any set is a concrete type if such a concrete type exist
    private def cmp(t1: Type, t2: Type): Boolean = (t1, t2) match {
        case (gt1: GenericType, gt2: GenericType) => (gt1.t compare gt2.t) < 0
        case (_: GenericType, _: Type) => false
        case (_: Type, _: GenericType) => true
        case _ => true
    }

    def apply(): Binding = new Binding(DisjointSet.empty[Type]((t1, t2) => cmp(t1, t2)))
}
