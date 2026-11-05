import java.io.InputStreamReader

class Scanner(reader: InputStreamReader) extends Iterator[Token] {
    private var atEOF = false
    private var currentChar: Char = Character.MIN_VALUE
    private var currentToken: Token = null

    nextChar()

    def hasNext(): Boolean =
        if currentToken == null then currentToken = nextToken()
        currentToken != Token(EOF)
    
    def next(): Token =
        if currentToken == null then currentToken = nextToken()
        val result = currentToken
        currentToken = nextToken()
        result
    
    def nextChar(): Unit =
        if atEOF then
            System.err.println("Error: end of input reached")
            sys.exit(1)
        val ch: Int = reader.read()
        atEOF = (ch == -1)
        currentChar = ch.toChar
    
    def nextToken(): Token =
        if atEOF then Token(EOF)
        else if Character.isDigit(currentChar) then
            if currentChar == '0' then
                nextChar()
                IntToken(0)
            else
                var k = 0
                while Character.isDigit(currentChar) do
                    k = k * 10 + currentChar.toString().toInt
                    nextChar()
                IntToken(k)
        else if currentChar == '+' then
            nextChar()
            Token(PLUS)
        else if currentChar == '-' then
            nextChar()
            Token(MINUS)
        else if currentChar == '*' then
            nextChar()
            Token(TIMES)
        else if currentChar == '/' then
            nextChar()
            Token(DIV)
        else if currentChar == '^' then
            nextChar()
            Token(EXP)
        else if currentChar == '(' then
            nextChar()
            Token(LPAREN)
        else if currentChar == ')' then
            nextChar()
            Token(RPAREN)
        else if currentChar == ' ' || currentChar == '\t' || currentChar == '\n' || currentChar == '\r' then
            nextChar()
            nextToken()
        else
            nextChar()
            Token(BAD)

}
