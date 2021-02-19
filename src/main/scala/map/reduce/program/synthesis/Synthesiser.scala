package map.reduce.program.synthesis

import scala.collection.mutable

object Synthesiser {
    def synthesis(input: String, inputType: Type, output: Any, outputType: Type, components: List[Component],
                  limit: Int = 5000, verbose: Boolean = true): Option[Program] = {
        val sketches = Sketch.generate(inputType, outputType)
        if (verbose) {
            println("Sketches:")
            sketches.foreach(println)
            println()
        }

        if (verbose) {
            println("Components:")
            components.foreach(println)
            println()
        }

        val wildcardTypes: Set[Type] = sketches.flatMap(s => s.wildcards).toSet
        if (verbose) {
            println("Create producers with following types:")
            wildcardTypes.foreach(println)
            println()
        }

        // note that the ".toSeq" can't be remove since wildcardTypes will be a HashSet when we have more than 5 types
        // in such case, the LazyList will be exhausted to calculate the hash which is impossible for our infinite list
        val producers: Map[Type, LazyList[Term]] = (for {
            t <- wildcardTypes.toSeq
        } yield (t, Producer.produce(t, components))).toMap

        val programStreams = sketches.map(sketch => Consumer.consume(sketch, sketch.wildcards.map(t => producers(t))))

        if (verbose) {
            println(s"Input = $input")
            println(s"Output = $output")
            println()
        }

        val solution = combineProgramStreams(programStreams).take(limit).find(p => {
            // a solution to given synthesis task requires that
            // 1. generates given output under given input
            // 2. generate the same output under any permutation of input
            // 3. the op in reduce form a commutative semi-group
            run(p, input).contains(output) && checkUnderPermutations(p, input) && checkCommutativeSemiGroup(p)
        })

        if (verbose) {
            println("Synthesis result = ")
            if (solution.nonEmpty) {
                println(s"${solution.get}")
            } else {
                println(s"unsolvable in $limit steps.")
            }
        }

        solution
    }

    private def run(p: Program, input: String): Option[Any] = p match {
        case MapReduceProgram(m, r) =>
            for {
                r1 <- Reflect.run(input, m)
                r2 <- Reflect.run(convertToString(r1), r)
            } yield r2
        case MapReduceEvalProgram(m, r, e) =>
            for {
                r1 <- Reflect.run(input, m)
                r2 <- Reflect.run(convertToString(r1), r)
                r3 <- Reflect.run(convertToString(r2), e)
            } yield r3
        case FlatMapReduceProgram(fm, r) =>
            for {
                r1 <- Reflect.run(input, fm)
                r2 <- Reflect.run(convertToString(r1), r)
            } yield r2
        case FlatMapReduceEvalProgram(fm, r, e) =>
            for {
                r1 <- Reflect.run(input, fm)
                r2 <- Reflect.run(convertToString(r1), r)
                r3 <- Reflect.run(convertToString(r2), e)
            } yield r3
    }

    private def checkUnderPermutations(p: Program, input: String): Boolean = p match {
        case MapReduceProgram(m, r) =>
            val intermediate = Reflect.run(input, m)
            intermediate.nonEmpty && Reflect.checkReduceUnderPermutation(convertToString(intermediate.get), r)
        case MapReduceEvalProgram(m, r, _) =>
            val intermediate = Reflect.run(input, m)
            intermediate.nonEmpty && Reflect.checkReduceUnderPermutation(convertToString(intermediate.get), r)
        case FlatMapReduceProgram(fm, r) =>
            val intermediate = Reflect.run(input, fm)
            intermediate.nonEmpty && Reflect.checkReduceUnderPermutation(convertToString(intermediate.get), r)
        case FlatMapReduceEvalProgram(fm, r, _) =>
            val intermediate = Reflect.run(input, fm)
            intermediate.nonEmpty && Reflect.checkReduceUnderPermutation(convertToString(intermediate.get), r)
    }

    private def checkCommutativeSemiGroup(p: Program): Boolean = p match {
        case MapReduceProgram(_, r) => Checker.checkCommutativeSemiGroup(r.term)
        case MapReduceEvalProgram(_, r, _) => Checker.checkCommutativeSemiGroup(r.term)
        case FlatMapReduceProgram(_, r) => Checker.checkCommutativeSemiGroup(r.term)
        case FlatMapReduceEvalProgram(_, r, _) => Checker.checkCommutativeSemiGroup(r.term)
    }

    private def convertToString(x: Any): String = x match {
        case Nil => "List()"
        case (h: List[_]) :: t => x.asInstanceOf[List[_]].map(l => convertToString(l)).toString()
        case (h: String) :: t => x.asInstanceOf[List[String]].mkString("List(\"", "\", \"", "\")")
        case s: String => "\"" + s + "\""
        case _ => x.toString
    }

    private def combineProgramStreams(streams: List[LazyList[Program]]): LazyList[Program] = {
        // (Int, Int, Int, Program) stands for (weight, from, index, program)
        val heap = mutable.PriorityQueue.empty[(Int, Int, Int, Program)](Ordering.by((_: (Int, Int, Int, Program))._1).reverse)
        for ((s, i) <- streams.zipWithIndex if s.nonEmpty) {
            val p = s.head
            heap.enqueue((p.weight, i, 0, p))
        }

        LazyList.from(new Iterator[Program] {
            override def hasNext: Boolean = heap.nonEmpty
            override def next(): Program = {
                val (_, from, index, program) = heap.dequeue()
                val insert = streams(from)(index + 1)
                heap.enqueue((insert.weight, from, index + 1, insert))
                program
            }
        })
    }
}
