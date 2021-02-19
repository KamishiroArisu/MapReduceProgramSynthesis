package map.reduce.program.synthesis

import scala.collection.mutable

object Consumer {
    type IndexCombination = List[Int]
    type TermCombination = List[Term]

    def consume(sketch: Sketch, terms: List[LazyList[Term]]): LazyList[Program] = {
        // pick TermCombination from input $terms, indexes[k] is the index in terms[k]
        def pick(indexCombination: IndexCombination): Option[TermCombination] = {
            val zipped = indexCombination.zipWithIndex
            // need to check whether index is valid since the LazyList can be finite or even empty.
            if (zipped.forall { case (termIndex, listIndex) => terms(listIndex).isDefinedAt(termIndex) }) {
                Some(zipped.map {
                    case (termIndex, listIndex) => terms(listIndex)(termIndex)
                })
            } else None
        }

        def weight(termCombination: TermCombination): Int =
            termCombination.map(_.weight).sum

        // get all combinations which move one step in any position in IndexCombination
        // (a, b, c) -> [(a + 1, b, c), (a, b + 1, c), (a, b, c + 1)]
        def nextIndexCombinations(indexCombination: IndexCombination): Set[IndexCombination] = {
            (for {
                i <- indexCombination.indices
            } yield indexCombination.updated(i, indexCombination(i) + 1)).toSet
        }

        // pick terms from n LazyLists which contains completed code snippets with given type signature
        // starts from (0, ..., 0), use a minimum heap to insure that return the completed program in ascending order on weight
        val initIndexes: IndexCombination = List.fill(sketch.wildcards.size)(0)
        val initTermsOpt: Option[TermCombination] = pick(initIndexes)

        // avoid adding duplicated combination of indexes
        val added: mutable.Set[IndexCombination] = mutable.HashSet(initIndexes)
        val heap = mutable.PriorityQueue.empty[(Int, IndexCombination, TermCombination)](Ordering.by((_: (Int, IndexCombination, TermCombination))._1).reverse)

        if (initTermsOpt.nonEmpty) {
            val initTerms = initTermsOpt.get
            val initElement = (weight(initTerms), initIndexes, initTerms)
            heap.addOne(initElement)
        }

        def nextProgram(): Program = {
            val (_, indexCombination, termCombination) = heap.dequeue()
            val addIndexes = nextIndexCombinations(indexCombination).diff(added)
            // try to pick elements at addIndexes, add the nonempty ones to the heap
            val addElements = addIndexes.map(c => (c, pick(c))).filter(t => t._2.nonEmpty)
                .map(x => (weight(x._2.get), x._1, x._2.get))
            heap.addAll(addElements)
            added.addAll(addIndexes)
            sketch.combine(termCombination)
        }

        LazyList.from(new Iterator[Program] {
            override def hasNext: Boolean = heap.nonEmpty
            override def next(): Program = nextProgram()
        })
    }
}
