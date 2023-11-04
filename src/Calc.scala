import java.io.InputStreamReader

@main def Calc() =
    val scanner = Scanner(InputStreamReader(System.in))

    val parser = Parser(scanner)
    parser.prog()
    println("Parsing successful")
