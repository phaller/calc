import java.io.InputStreamReader

@main def Calc() =
    val scanner = Scanner(InputStreamReader(System.in))

    while scanner.hasNext() do
        val tok = scanner.next()
        println(tok)
