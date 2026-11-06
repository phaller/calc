import java.io.InputStreamReader

@main def Calc() =
    val scanner = Scanner(InputStreamReader(System.in))

    val parser = Parser(scanner)
    val tree = parser.prog()
    println("Success")
    println(s"AST: $tree")
