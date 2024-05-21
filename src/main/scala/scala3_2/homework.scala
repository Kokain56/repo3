package scala3_2





object homework1 {
  extension (x: String)
    def +++(y: String): Int = {
      x.toInt + y.toInt
    }

    @main def part1Ex(rest:String*): Unit = {
      val p: String = ""
      println("1" +++ "33")
    }
}

object homework2 {
  enum CompletionArg:
    case FromString(s: String)
    case FromInt(f: Int)
    case FromFloat(code: Float)

  object CompletionArg {
    given fromString: Conversion[String, CompletionArg] = CompletionArg.FromString(_)

    given fromInt: Conversion[Int, CompletionArg] = CompletionArg.FromInt(_)

    given fromFloat: Conversion[Float, CompletionArg] = CompletionArg.FromFloat(_)
  }
  import CompletionArg.*

  object Completions {

    def complete(arg: CompletionArg): String = arg match {
      case CompletionArg.FromString(s) => s"string: $s"
      case CompletionArg.FromInt(d) => s"int: $d"
      case CompletionArg.FromFloat(f) => s"status: $f"
    }
  }

  @main def part2Ex(): Unit ={
    println(Completions.complete("String"))
    println(Completions.complete(1))
    println(Completions.complete(7f))
  }
}


object homework3 {
  opaque type Logarithm = Double

  object Logarithm{
    //см приведенную ссылку
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if d > 0.0 then Some(math.log(d)) else None
  }

  extension (x: Logarithm)
    def toDouble = math.exp(x)
    def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def * (y: Logarithm): Logarithm = x + y


  @main def part3Ex(): Unit ={

    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2

    println(l4)
  }
}