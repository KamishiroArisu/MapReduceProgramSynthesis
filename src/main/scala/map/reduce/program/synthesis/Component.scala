package map.reduce.program.synthesis

import z3.scala.{Z3AST, Z3Context, Z3Solver}

trait Component {
    def getType: FunctionType
    def getName: String
    def encode(ps: List[String]): String
}

abstract class Component0[A](name: String, ot: Type, val eval: () => A) extends Component {
    lazy val getType: FunctionType = FunctionType(List(UnitType), ot)

    override def toString: String = s"$name: $getType"

    override def getName: String = name
}

abstract class Component1[A,B](name: String, it: Type, ot: Type, val eval: A => B) extends Component {
    lazy val getType: FunctionType = FunctionType(List(it), ot)

    override def toString: String = s"$name: $getType"

    override def getName: String = name
}

abstract class Component2[A,B,C](name: String, it1: Type, it2: Type, ot: Type, val eval: (A, B) => C) extends Component {
    lazy val getType: FunctionType = FunctionType(List(it1, it2), ot)

    override def toString: String = s"$name: $getType"

    override def getName: String = name
}

object Component {
    // name of predefined components should be UpperCase of name field inside each component
    // Reflect use such invariant to reference the functions inside components
    val CONST0: Component0[Int] = new Component0[Int]("const0",
        IntType, () => 0) {
        override def encode(ps: List[String]): String = "0"
    }

    val CONST1: Component0[Int] = new Component0[Int]("const1",
        IntType, () => 1) {
        override def encode(ps: List[String]): String = "1"
    }

    val LENGTH: Component1[String, Int] = new Component1[String, Int]("length",
        StringType, IntType, (s: String) => s.length) {
        override def encode(ps: List[String]): String = s"(seq.len (${ps.head}))"
    }

    val PLUS1: Component1[Int, Int] = new Component1[Int, Int]("plus1",
        IntType, IntType, i => i + 1) {
        override def encode(ps: List[String]): String = s"(+ ${ps(0)} 1)"
    }

    val MAX: Component2[Int, Int, Int] = new Component2[Int, Int, Int]("max",
        IntType, IntType, IntType, (i1: Int, i2: Int) => i1 max i2) {
        override def encode(ps: List[String]): String = s"(ite (> ${ps(0)} ${ps(1)}) ${ps(0)} ${ps(1)})"
    }

    val ADD: Component2[Int, Int, Int] = new Component2[Int, Int, Int]("add",
        IntType, IntType, IntType, (i1: Int, i2: Int) => i1 + i2) {
        override def encode(ps: List[String]): String = s"(+ ${ps(0)} ${ps(1)})"
    }

    val EXTEND: Component1[Int, List[Int]] = new Component1[Int, List[Int]]("extend",
        IntType, ListType(IntType), i => List(i, i + 1, i + 2)) {
        override def encode(ps: List[String]): String = s"(seq.++ (seq.unit ${ps(0)}) (seq.unit (+ ${ps(0)} 1)) (seq.unit (+ ${ps(0)} 2)))"
    }

    def default(): List[Component] = {
        List(
            // Component0
            CONST0, CONST1,
            // Component1
            LENGTH, PLUS1,
            // Component2
            MAX, ADD
        )
    }
}