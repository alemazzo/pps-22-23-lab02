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

  // 4. Currying

  println("Currying")

  val p1: (Int, Int, Boolean) => Boolean = (x, y, b) => b match
    case true => x <= y
    case false => x > y

  val p2: (Int) => (Int) => (Boolean => Boolean) =
    x => y => p1(x, y, _)

  def p3(x: Int, y: Int, b: Boolean): Boolean = p1(x, y, b)

  def p4(x: Int)(y: Int)(b: Boolean): Boolean = p1(x, y, b)

  // test p1 to p4
  println(p1(1, 2, true))
  println(p2(1)(2)(true))
  println(p3(1, 2, true))
  println(p4(1)(2)(true))

  // false tests
  println(p1(1, 2, false))
  println(p2(1)(2)(false))
  println(p3(1, 2, false))
  println(p4(1)(2)(false))


