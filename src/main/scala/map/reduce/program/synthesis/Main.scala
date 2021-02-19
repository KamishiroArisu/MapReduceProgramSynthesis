package map.reduce.program.synthesis

object Main {
    def main(args: Array[String]): Unit = {
        val input = """List("abc", "de", "f")"""
        val output = 6
        Synthesiser.synthesis(input, StringType, output, IntType, Component.default())
    }
}
