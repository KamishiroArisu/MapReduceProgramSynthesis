package map.reduce.program.synthesis

sealed trait Snippet {
    def code: String
    def term: Term

    override def toString: String = code
}

case class MapSnippet(code: String, term: Term) extends Snippet
case class FlatMapSnippet(code: String, term: Term) extends Snippet
case class ReduceSnippet(code: String, term: Term) extends Snippet
case class EvalSnippet(code: String, term: Term) extends Snippet

sealed trait Program {
    val weight: Int
}

case class MapReduceProgram(m: MapSnippet, r: ReduceSnippet) extends Program {
    override def toString: String = s"input${m}${r}"
    override val weight: Int = m.term.weight + r.term.weight
}

case class MapReduceEvalProgram(m: MapSnippet, r: ReduceSnippet, e: EvalSnippet) extends Program {
    override def toString: String = s"input${m}${r}.eval${e}"
    override val weight: Int = m.term.weight + r.term.weight + e.term.weight
}

case class FlatMapReduceProgram(fm: FlatMapSnippet, r: ReduceSnippet) extends Program {
    override def toString: String = s"input${fm}${r}"
    override val weight: Int = fm.term.weight + r.term.weight
}

case class FlatMapReduceEvalProgram(fm: FlatMapSnippet, r: ReduceSnippet, e: EvalSnippet) extends Program {
    override def toString: String = s"input${fm}${r}.eval${e}"
    override val weight: Int = fm.term.weight + r.term.weight + e.term.weight
}

abstract class Sketch(it: Type, ot: Type, val wildcards: List[Type]) {
    def combine(terms: List[Term]): Program
}

class MapReduceSketch(it: Type, ot: Type, override val wildcards: List[Type]) extends Sketch(it, ot, wildcards) {
    override def combine(terms: List[Term]): Program = {
        val m = Term.convert(terms(0))
        val r = Term.convert(terms(1))
        MapReduceProgram(
            MapSnippet(s".map($m)", terms(0)),
            ReduceSnippet(s".reduce($r)", terms(1)))
    }

    override def toString: String = s"MapReduceSketch: ${ListType(it)}.map(${wildcards(0)}).reduce(${wildcards(1)})"
}

class MapReduceEvalSketch(it: Type, ot: Type, override val wildcards: List[Type]) extends Sketch(it, ot, wildcards) {
    override def combine(terms: List[Term]): Program = {
        val m = Term.convert(terms(0))
        val r = Term.convert(terms(1))
        val e = Term.convert(terms(2))
        MapReduceEvalProgram(
            MapSnippet(s".map($m)", terms(0)),
            ReduceSnippet(s".reduce($r)", terms(1)),
            EvalSnippet(s"(${e})", terms(2)))
    }

    override def toString: String = s"MapReduceEvalSketch: ${ListType(it)}.map(${wildcards(0)}).reduce(${wildcards(1)}).eval(${wildcards(2)})"
}

class FlatMapReduceSketch(it: Type, ot: Type, override val wildcards: List[Type]) extends Sketch(it, ot, wildcards) {
    override def combine(terms: List[Term]): Program = {
        val m = Term.convert(terms(0))
        val r = Term.convert(terms(1))
        FlatMapReduceProgram(
            FlatMapSnippet(s".flatMap($m)", terms(0)),
            ReduceSnippet(s".reduce($r)", terms(1)))
    }

    override def toString: String = s"FlatMapReduceSketch: ${ListType(it)}.flatMap(${wildcards(0)}).reduce(${wildcards(1)})"
}

class FlatMapReduceEvalSketch(it: Type, ot: Type, override val wildcards: List[Type]) extends Sketch(it, ot, wildcards) {
    override def combine(terms: List[Term]): Program = {
        val m = Term.convert(terms(0))
        val r = Term.convert(terms(1))
        val e = Term.convert(terms(2))
        FlatMapReduceEvalProgram(
            FlatMapSnippet(s".flatMap($m)", terms(0)),
            ReduceSnippet(s".reduce($r)", terms(1)),
            EvalSnippet(s"(${e})", terms(2)))
    }

    override def toString: String = s"FlatMapReduceEvalSketch: ${ListType(it)}.flatMap(${wildcards(0)}).reduce(${wildcards(1)}).eval(${wildcards(2)})"
}

object Sketch {
    def generate(it: Type, ot: Type): List[Sketch] = List(
        new MapReduceSketch(it, ot,
            List(FunctionType(List(it), ot),
                FunctionType(List(ot, ot), ot))),
        new MapReduceEvalSketch(it, ot,
            List(FunctionType(List(it), GenericType("a")),
                FunctionType(List(GenericType("a"), GenericType("a")), GenericType("a")),
                FunctionType(List(GenericType("a")), ot))),
        new FlatMapReduceSketch(it, ot,
            List(FunctionType(List(it), ListType(ot)),
                FunctionType(List(ot, ot), ot))),
        new FlatMapReduceEvalSketch(it, ot,
            List(FunctionType(List(it), ListType(GenericType("a"))),
                FunctionType(List(GenericType("a"), GenericType("a")), GenericType("a")),
                FunctionType(List(GenericType("a")), ot)))
    )
}
