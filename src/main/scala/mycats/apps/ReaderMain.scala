package mycats.apps

import mycats.transformers.Reader

object ReaderMain extends App {

  println("\n-----")

  type StringReader[R] = Reader[String, R]

  val rev: StringReader[String] = Reader { str => str.reverse }

  val revup = rev map {_.toUpperCase}
  println(revup.run("Hello World"))

  val upper: StringReader[String] = Reader { str => str.toUpperCase }
  val uprev = upper map {_.reverse}
  println(uprev.run("Hello World"))

  val uprev2 = upper map rev.run
  println(uprev2.run("Hello World"))

  val res0: Reader[String, String] = for {
    msg <- Reader.ask[String]
    revMsg = msg.reverse
  } yield revMsg
  println(res0.run("Hello World"))

  val len = Reader[String, Int](_.length)
  val inc = Reader[ Int, Int](_ + 1)

  println((len andThen inc).run("Hello"))
  println((inc compose len).run("Hello"))

  println("-----\n")
}
