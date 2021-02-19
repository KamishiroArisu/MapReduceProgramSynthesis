package map.reduce.program.synthesis

import scala.collection.mutable

object Producer {
    def expand(term: Term, components: List[Component], binding: Binding): List[(Term, Binding)] = term match {
        case Wildcard(UnitType, _) =>
            List((Unit, binding))
        case Wildcard(ft: FunctionType, context) =>
            // Wildcard(T1 => T2), expand as v1: T1 => Wildcard(T2)
            // Wildcard((T1, ..., Tn) => Tk), expand as v1: T1 => Wildcard((T2, ..., Tn) => Tk)
            if (ft.its.size == 1) {
                val introduce = Variable(ft.its.head)
                List((Lambda(introduce, Wildcard(ft.ot, context + introduce)), binding))
            } else {
                val introduce = Variable(ft.its.head)
                List((Lambda(introduce, Wildcard(FunctionType(ft.its.tail, ft.ot), context + introduce)), binding))
            }
        case Wildcard(gt: GenericType, context) =>
            // Wildcard('a), expand as Variable(v1): T1 if 'a <-> v1 doesn't violate existing binding
            // Wildcard('a), expand as Function(w1, ..., wk, fun) where fun is a component s.t. fun has arity k and return T1 if 'a <-> v1 doesn't violate existing binding
            val matchVariables = context.filterNot(v => binding.isViolate(v.getType, gt))
            val variables = matchVariables.map(v => (v, binding.updated(v.getType, gt)))
            val matchComponents = components.filterNot(c => binding.isViolate(c.getType.ot, gt))
            val functions = matchComponents.map(c => {
                val b = binding.updated(c.getType.ot, gt)
                (Function(c.getType.its.map(pt => Wildcard(pt, context)), c), b)
            })
            functions ++ variables
        case Wildcard(t: ListType, context) if isGeneric(t) =>
            val (layer, gt) = extract(t)
            val matchVariables = context.filter(v => {
                val peeled = peel(v.getType, layer)
                peeled.nonEmpty && !binding.isViolate(peeled.get, gt)
            })
            val variables = matchVariables.map(v => (v, binding.updated(peel(v.getType, layer).get, gt)))
            val matchComponents = components.filter(c => {
                val peeled = peel(c.getType.ot, layer)
                peeled.nonEmpty && !binding.isViolate(peeled.get, gt)
            })
            val functions = matchComponents.map(c => {
                val b = binding.updated(peel(c.getType.ot, layer).get, gt)
                (Function(c.getType.its.map(pt => Wildcard(pt, context)), c), b)
            })
            functions ++ variables
        case Wildcard(t: Type, context) =>
            // Wildcard(T1), expand as Variable(v1): T1 where v1 in context
            // Wildcard(T1), expand as Function(w1, ..., wk, fun) where fun is a component s.t. fun has arity k and return T1
            val matchVariables = context.filter(v => v.getType == t || binding.same(v.getType, t))
            val variables = matchVariables.toList
            val matchComponents = components.filter(c => c.getType.ot == t)
            val functions = matchComponents.map(c => Function(c.getType.its.map(pt => Wildcard(pt, context)), c))
            val withoutNewBinding = (functions ++ variables).map(t => (t, binding))
            // bind $t to a existing generic type variable
            // if such binding doesn't violate current bindings and the two types has not been bound yet
            val bindVariables = context.filter(v => v.getType.isInstanceOf[GenericType] && !binding.isViolate(v.getType, t) && !binding.same(v.getType, t))
            val withNewBinding = bindVariables.map(v => (v.updateType(t), binding.updated(v.getType, t)))
            (withNewBinding ++ withoutNewBinding).toList
        case Variable(_) | Unit =>
            // Variable or Unit can not be further expanded
            List((term, binding))
        case Lambda(v, e) =>
            // Lambda(v, e), expand as Lambda(v, expand(e))
            val expanded = expand(e, components, binding)
            expanded.map {
                case (t, b) =>
                    val reprOpt = b.getReprType(v.getType)
                    // if following binding restrict v to be a specific type, just update v's type
                    if (reprOpt.isDefined)
                        (Lambda(v.updateType(b.getReprType(v.getType).get), t), b)
                    else
                        (Lambda(v, t), b)
            }
        case Function(ps, component) =>
            val index = ps.indexWhere(t => !isCompleted(t))
            val expanded = expand(ps(index), components, binding)
            expanded.map {
                // replace term at index of parameters, update binding from expand result
                case (t, b) => (Function(ps.slice(0, index) ::: (t :: ps.slice(index + 1, ps.length)), component), b)
            }
    }

    def produce(t: Type, components: List[Component]): LazyList[Term] = {
        val init = Wildcard(t, Set())
        val heap = mutable.PriorityQueue.empty[(Int, Term, Binding)](Ordering.by((_: (Int, Term, Binding))._1).reverse)
        heap.addOne((init.weight, init, Binding()))

        def prepare(): Unit = {
            while (heap.nonEmpty && !isCompleted(heap.head._2)) {
                val (_, headTerm, binding) = heap.dequeue()
                val expanded = expand(headTerm, components, binding).map(t => (t._1.weight, t._1, t._2))
                heap.addAll(expanded)
            }
        }

        def nextTerm(): Term = {
            val next = heap.dequeue()._2
            prepare()
            next
        }

        prepare()

        LazyList.from(new Iterator[Term] {
            override def hasNext: Boolean = heap.nonEmpty
            override def next(): Term = nextTerm()
        })
    }

    def isCompleted(t: Term): Boolean = t match {
        case Wildcard(_, _) => false
        case Function(ps, _) => ps.forall(p => isCompleted(p))
        case Lambda(_, e) => isCompleted(e)
        case Variable(_) | Unit => true
    }

    def isGeneric(t: Type): Boolean = t match {
        case ListType(e) => isGeneric(e)
        case _: BaseType => false
        case GenericType(_) => true
    }

    def extract(t: Type): (Int, Type) = t match {
        case ListType(e) =>
            val (layer, inner) = extract(e)
            (layer + 1, inner)
        case _ => (0, t)
    }

    def peel(t: Type, n: Int): Option[Type] = {
        if (n == 0)
            Some(t)
        else {
            t match {
                case ListType(e) => peel(e, n - 1)
                case _ => None
            }
        }
    }
}
