package u02

import java.lang.Math.abs
import scala.annotation.tailrec

object Lab extends App:

  // 3a. Positive val and def

  private val valPositive: Int => Boolean = _ match
    case n if n >= 0 => true
    case _ => false

  private def positive(x: Int): Boolean = x match
    case n if n >= 0 => true
    case _ => false

  // 3b. Neg val and def

  private val valNeg: (String => Boolean) => (String => Boolean) =
    p => !p(_)

  private def defNeg(p: String => Boolean): String => Boolean =
    !p(_)

  // 3c. Generic neg

  private def neg[A](p: A => Boolean): A => Boolean =
    !p(_)

  // 4. Currying

  val p1: (Int, Int, Int) => Boolean = (x, y, b) =>
    x match
      case x if x <= y && y == b => true
      case _ => false

  val p2: (Int) => (Int) => (Int => Boolean) =
    x => y => p1(x, y, _)

  def p3(x: Int, y: Int, b: Int): Boolean = p1(x, y, b)

  def p4(x: Int)(y: Int)(b: Int): Boolean = p1(x, y, b)

  // 5. Functional composition

  def intCompose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  def compose[A, B, C](f: B => C, g: A => B): A => C = x => f(g(x))

  // 6. GCD

  @tailrec
  private def gcd(a: Int, b: Int): Int = b match
    case b if b == 0 => a
    case _ => gcd(b, a % b)

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

    def contains(shape: Shape, point: (Double, Double)): Boolean = (shape, point) match
      case (Rectangle(w, h), (x, y)) => abs(x) <= w / 2 && abs(y) <= h / 2
      case (Circle(r), (x, y)) => abs(x) <= r && abs(y) <= r
      case (Square(s), (x, y)) => abs(x) <= s / 2 && abs(y) <= s / 2

  // 8. Optional
  
  enum Option[A]:
    case Some(a: A)
    case None()

  object Option:

    // FILTER
    def filter[A](o: Option[A])(p: A => Boolean): Option[A] = o match
      case Some(x) if p(x) => Some(x)
      case _ => None()

    // MAP
    def map[A, B](o: Option[A])(m: A => B): Option[B] = o match
      case Some(x) => Some(m(x))
      case _ => None()

    // FOLD
    def fold[A, B](o: Option[A])(d: B)(m: A => B): B = o match
      case Some(x) => m(x)
      case _ => d
