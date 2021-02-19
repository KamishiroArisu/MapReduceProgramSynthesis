package map.reduce.program.synthesis

import org.scalatest.funsuite.AnyFunSuite

class DisjointSetTest extends AnyFunSuite {
    test("DisjointSetTest") {
        val ds0 = DisjointSet.empty[Int]((x, y) => x < y)
        val ds1 = ds0.make(0)
        val ds2 = ds1.make(1)
        val ds3 = ds2.make(2)
        val ds4 = ds3.make(3)
        val ds5 = ds4.make(4)

        for {
            i <- (0 to 4)
            j <- (0 to 4)
            if i != j
        } assert(!ds5.same(i, j))

        val ds6 = ds5.union(1, 2).union(3, 4)

        assert(ds6.same(1, 2))
        assert(ds6.same(3, 4))
        assert(ds6.find(1).contains(1))
        assert(ds6.find(2).contains(1))

        assert(!ds3.same(1, 2))
    }
}
