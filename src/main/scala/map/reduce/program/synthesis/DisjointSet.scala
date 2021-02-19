package map.reduce.program.synthesis

class DisjointSet[T](content: Map[T, T], cmp: (T, T) => Boolean) {
    def make(t: T): DisjointSet[T] = {
        if (content.contains(t))
            throw new IllegalArgumentException(s"$t can not be make as an new set.")
        else
            new DisjointSet[T](content + {t -> t}, cmp)
    }

    def find(t: T): Option[T] = {
        if (content.contains(t)) {
            val p = content(t)
            if (p == t) Some(t) else find(p)
        } else None
    }

    def union(l: T, r: T): DisjointSet[T] = {
        val fl = find(l)
        val fr = find(r)

        (fl, fr) match {
            case (Some(x), Some(y)) =>
                if (cmp(x, y))
                    new DisjointSet[T](content.updated(y, x), cmp)
                else
                    new DisjointSet[T](content.updated(x, y), cmp)
            case (None, _) => make(l).union(l, r)
            case (_, None) => make(r).union(l, r)
        }
    }

    def same(l: T, r: T): Boolean = {
        val fl = find(l)
        val fr = find(r)
        fl.isDefined && fr.isDefined && fl.get == fr.get
    }

    override def toString: String = {
        val types = content.keys
        types.groupBy(t => find(t).get).map {
            case (t, ts) => s"$t -> ${ts.mkString("{", ", ", "}")}"
        }.mkString("DisjointSet(", ", ", ")")
    }
}

object DisjointSet {
    def empty[T](cmp: (T, T) => Boolean): DisjointSet[T] = new DisjointSet(Map(), cmp)
}


