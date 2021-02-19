package map.reduce.program.synthesis

import org.scalatest.funsuite.AnyFunSuite

class ProducerTest extends AnyFunSuite {
    test("produce") {
        val terms = Producer.produce(FunctionType(List(StringType), IntType), Component.default())
        val list = terms.take(200).toList
        assert(list.map(term => term.toString).distinct.size == 200)
    }
}
