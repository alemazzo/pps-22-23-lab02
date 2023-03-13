package u02

object Aula extends App:

  enum List:
    case Cons(head: Int, tail: List)
    case Nil

  object List:
    def size(list: List): Int = list match
      case Nil => 0
      case Cons(_, tail) => 1 + size(tail)

  import List.*

  val list = Cons(10, Cons(20, Nil))
  println(size(list))

  case class Pair[A, B](fst: A, snd: B)

  object Pair:
    def reverse[A, B](p: Pair[A, B]): Pair[B, A] = p match
      case Pair(f, s) => Pair(s, f)

  import Pair.*

  println(reverse(Pair(10, 20)))