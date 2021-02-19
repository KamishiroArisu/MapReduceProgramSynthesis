package map.reduce.program.synthesis

import org.scalatest.funsuite.AnyFunSuite

class CheckerTest extends AnyFunSuite {
    test("checkCommutative true") {
        val t = IntType
        val v1 = Variable(t)
        val v2 = Variable(t)
        val add = Component.ADD

        val term = Lambda(v1, Lambda(v2, Function(List(v1, v2), add)))
        val isAssociative = Checker.checkCommutative(term)
        assert(isAssociative)
    }

    test("checkCommutative false") {
        val t = IntType
        val v1 = Variable(t)
        val v2 = Variable(t)
        val first = new Component2[Int,Int,Int]("first", t, t, t, (i1, _) => i1) {
            override def encode(ps: List[String]): String = ps.head
        }

        val term = Lambda(v1, Lambda(v2, Function(List(v1, v2), first)))
        val isAssociative = Checker.checkCommutative(term)
        assert(!isAssociative)
    }

    test("String") {
        val t = StringType
        val v1 = Variable(t)
        val v2 = Variable(t)
        val concat = new Component2[String,String,String]("concat", t, t, t, (s1, s2) => s1 + s2) {
            override def encode(ps: List[String]): String = s"(str.++ ${ps(0)} ${ps(1)})"
        }

        val term = Lambda(v1, Lambda(v2, Function(List(v1, v2), concat)))
        val isAssociative = Checker.checkCommutative(term)
        assert(!isAssociative)
    }

    test("List[Int]") {
        val t = ListType(IntType)
        val v1 = Variable(t)
        val v2 = Variable(t)
        val maxSize = new Component2[List[_],List[_],List[_]]("maxSize", t, t, t, (l1, l2) => List(l1.size max l2.size)) {
            override def encode(ps: List[String]): String =
                s"(ite (> (seq.len ${ps(0)}) (seq.len ${ps(1)})) (seq.unit (seq.len ${ps(0)})) (seq.unit (seq.len ${ps(1)})))"
        }

        val term = Lambda(v1, Lambda(v2, Function(List(v1, v2), maxSize)))
        val isAssociative = Checker.checkCommutative(term)
        assert(isAssociative)
    }

    test("List[List[Int]]") {
        val t = ListType(IntType)
        val v1 = Variable(t)
        val v2 = Variable(t)
        val maxSize2 = new Component2[List[_],List[_],List[_]]("maxSize2", t, t, t, (l1, l2) => List(List(l1.size max l2.size))) {
            override def encode(ps: List[String]): String =
                s"(ite (> (seq.len ${ps(0)}) (seq.len ${ps(1)})) (seq.unit (seq.unit (seq.len ${ps(0)}))) (seq.unit (seq.unit (seq.len ${ps(1)}))))"
        }

        val term = Lambda(v1, Lambda(v2, Function(List(v1, v2), maxSize2)))
        val isAssociative = Checker.checkCommutative(term)
        assert(isAssociative)
    }

    test("checkAssociative true") {
        val t = StringType
        val v1 = Variable(t)
        val v2 = Variable(t)
        val concat = new Component2[String,String,String]("concat", t, t, t, (s1, s2) => s1 + s2) {
            override def encode(ps: List[String]): String = s"(str.++ ${ps(0)} ${ps(1)})"
        }

        val term = Lambda(v1, Lambda(v2, Function(List(v1, v2), concat)))
        val isAssociative = Checker.checkAssociative(term)
        assert(isAssociative)
    }

    test("checkAssociative false") {
        val t = IntType
        val v1 = Variable(t)
        val v2 = Variable(t)
        val mean = new Component2[Int,Int,Int]("mean", t, t, t, (i1, i2) => (i1 + i2) / 2) {
            override def encode(ps: List[String]): String = s"(div (+ ${ps(0)} ${ps(1)}) 2)"
        }

        val term = Lambda(v1, Lambda(v2, Function(List(v1, v2), mean)))
        val isAssociative = Checker.checkAssociative(term)
        assert(!isAssociative)
    }
}
