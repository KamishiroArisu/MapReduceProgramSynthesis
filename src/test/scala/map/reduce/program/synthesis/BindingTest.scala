package map.reduce.program.synthesis

import org.scalatest.funsuite.AnyFunSuite

class BindingTest extends AnyFunSuite {
    test("BindingTest") {
        val binding0 = Binding()
        assert(!binding0.isViolate(GenericType("a"), StringType))
        assert(!binding0.isViolate(IntType, GenericType("a")))
        assert(!binding0.isViolate(GenericType("a"), GenericType("a")))
        assert(!binding0.isViolate(GenericType("a"), GenericType("b")))
        assert(binding0.getReprType(GenericType("a")).isEmpty)

        val binding1 = binding0.updated(GenericType("a"), GenericType("b"))
        assert(binding1.same(GenericType("a"), GenericType("b")))
        assert(!binding1.isViolate(GenericType("a"), GenericType("b")))

        val binding2 = binding1.updated(GenericType("c"), StringType)
        assert(binding2.same(StringType, GenericType("c")))
        assert(!binding2.isViolate(GenericType("b"), GenericType("c")))
        assert(!binding2.isViolate(GenericType("b"), StringType))
        assert(binding2.getReprType(GenericType("c")).contains(StringType))

        val binding3 = binding2.updated(GenericType("a"), IntType)
        assert(binding3.getReprType(GenericType("a")).contains(IntType))
        assert(binding3.getReprType(GenericType("b")).contains(IntType))

        assert(binding3.isViolate(GenericType("a"), GenericType("c")))
        assert(binding3.isViolate(GenericType("a"), UnitType))
    }
}
