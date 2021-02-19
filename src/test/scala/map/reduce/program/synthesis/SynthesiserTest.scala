package map.reduce.program.synthesis

import org.scalatest.funsuite.AnyFunSuite

class SynthesiserTest extends AnyFunSuite {
    test("MapReduce") {
        val input = """List("abc", "de", "f")"""
        val output = 6
        val result = Synthesiser.synthesis(input, StringType, output, IntType, Component.default(), verbose = false)
        assert(result.nonEmpty)
        assert(result.get.isInstanceOf[MapReduceProgram])
        val program = result.get.asInstanceOf[MapReduceProgram]
        assert(program.m.toString.contains("LENGTH"))
        assert(program.r.toString.contains("ADD"))
    }

    test("MapReduceEval") {
        val input = """List("abc", "de", "f")"""
        val output = 7
        val result = Synthesiser.synthesis(input, StringType, output, IntType, Component.default(), verbose = false)
        assert(result.nonEmpty)
        assert(result.get.isInstanceOf[MapReduceEvalProgram])
        val program = result.get.asInstanceOf[MapReduceEvalProgram]
        assert(program.m.toString.contains("LENGTH"))
        assert(program.r.toString.contains("ADD"))
        assert(program.e.toString.contains("PLUS1"))
    }

    test("FlatMapReduce") {
        val input = """List(12, 5, 1)"""
        // 12 + 13 + 14 + 5 + 6 + 7 + 1 + 2 + 3 = 63
        val output = 63
        val components = List(Component.ADD, Component.PLUS1, Component.LENGTH, Component.MAX, Component.EXTEND)
        val result = Synthesiser.synthesis(input, IntType, output, IntType, components, verbose = false)
        assert(result.nonEmpty)
        assert(result.get.isInstanceOf[FlatMapReduceProgram])
        val program = result.get.asInstanceOf[FlatMapReduceProgram]
        assert(program.fm.toString.contains("EXTEND"))
        assert(program.r.toString.contains("ADD"))
    }

    test("FlatMapReduceEval") {
        val input = """List(12, 5, 1)"""
        // 12 + 13 + 14 + 5 + 6 + 7 + 1 + 2 + 3 = 63
        val output = 64
        val components = List(Component.CONST0, Component.ADD, Component.PLUS1, Component.MAX, Component.EXTEND)
        val result = Synthesiser.synthesis(input, IntType, output, IntType, components, verbose = false)
        assert(result.nonEmpty)
        assert(result.get.isInstanceOf[FlatMapReduceEvalProgram])
        val program = result.get.asInstanceOf[FlatMapReduceEvalProgram]
        assert(program.fm.toString.contains("EXTEND"))
        assert(program.r.toString.contains("ADD"))
        assert(program.e.toString.contains("PLUS1"))
    }
}
