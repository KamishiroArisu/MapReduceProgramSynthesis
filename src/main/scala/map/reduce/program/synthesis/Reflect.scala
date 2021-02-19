package map.reduce.program.synthesis

import scala.collection.mutable
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.runtimeMirror
import scala.tools.reflect.ToolBox

object Reflect {
    val Toolbox: ToolBox[universe.type] = runtimeMirror(getClass.getClassLoader).mkToolBox()
    val Memoization: mutable.HashMap[(String, Snippet), Option[Any]] = mutable.HashMap()

    def run(input: String, snippet: Snippet): Option[Any] = {
        Memoization.getOrElseUpdate((input, snippet), {
            val code = snippet match {
                case MapSnippet(_, _) | FlatMapSnippet(_, _) | ReduceSnippet(_, _) =>
                    s"$input${snippet.code}"
                case EvalSnippet(_, _) =>
                    s"${snippet.code}.apply($input)"
            }
            val tree = Toolbox.parse(code)
            val checked = Toolbox.typecheck(tree, silent = true)
            if (checked.nonEmpty)
                Some(Toolbox.eval(tree))
            else None
        })
    }

    def checkReduceUnderPermutation(input: String, snippet: ReduceSnippet): Boolean = {
        val code = s"$input.permutations.toList.map(permutation => permutation${snippet.code}).distinct.size == 1"
        Toolbox.eval(Toolbox.parse(code)).asInstanceOf[Boolean]
    }
}
