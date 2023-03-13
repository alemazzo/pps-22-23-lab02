package u02

import scala.annotation.tailrec

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

  // 5. Functional composition

  println("Functional composition")

  def intCompose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))
  // f(g(_))

  // 6. GCD

  println("GCD")

  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

  println(gcd(2, 4))
  println(gcd(30, 50))

  // 7. Shapes

  enum Shape:
    case Rectangle(width: Double, height: Double)
    case Circle(radius: Double)
    case Square(side: Double)

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(w, h) => (w + h) * 2
      case Circle(r) => 2 * math.Pi * r
      case Square(s) => s * 4

    def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
      case Rectangle(w, h) => point._1 < w && point._2 < h
      case Circle(r) => point._1 < r && point._2 < r
      case Square(s) => point._1 < s && point._2 < s




