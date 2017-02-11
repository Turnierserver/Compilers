import compilers._
import java.io.File

object Main extends App {
    compile(Compile(Java(), new File("")), (l => new File("")), (l => println(l)))
}