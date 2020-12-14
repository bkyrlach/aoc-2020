import cats._
import cats.implicits._

def thing[F[_], G[_], A, B](bleh: F[(A,G[B])])(implicit ev: Traverse[F], ev2: Monad[G]): G[F[(A, B)]] =
  bleh.traverse(p => p._2.map(b => p._1 -> b))

val xs: List[(Int, Option[String])] = List(1 -> Some("a"), 2 -> Some("b"), 3 -> Some("c"))

println(thing(xs))