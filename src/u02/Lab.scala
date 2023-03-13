package u02

object Lab extends App:

  private val valPositive: Int => Boolean = _ match
    case n if n >= 0 => true
    case _ => false

  private def positive(x: Int): Boolean = x match
    case n if n >= 0 => true
    case _ => false

  println(positive(2))
  println(positive(-2))

  private val valNeg: (String => Boolean) => (String => Boolean) =
    p => !p(_)

  private def defNeg(predicate: String => Boolean): String => Boolean =
    !predicate(_)

  val emptyString: String => Boolean = _ == ""

  val notEmptyString = defNeg(emptyString)

  println(notEmptyString("ciao"))
  println(notEmptyString(""))
  println(emptyString("ciao"))

  private def neg[A](predicate: A => Boolean): A => Boolean =
    !predicate(_)

  val even: Int => Boolean = _ % 2 == 0
  val odd = neg(even)

  println(odd(3))
  println(odd(4))




