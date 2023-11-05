import java.io.InputStreamReader

@main def Calc() =
    val scanner = Scanner(InputStreamReader(System.in))

    val parser = Parser(scanner)
    val expr = parser.prog()
    println("Parsing successful")
    println(s"Tree: $expr")
