package mycats.apps

import mycats.transformers.Reader1

object Reader1Main extends App {

  println("\n-----")

  type StringReader1[R] = Reader1[String, R]

  val rev: StringReader1[String] = Reader1 { str => str.reverse }

  val revup = rev map {_.toUpperCase}
  println(revup.run("Hello World"))

  val upper: StringReader1[String] = Reader1 { str => str.toUpperCase }
  val uprev = upper map {_.reverse}
  println(uprev.run("Hello World"))

  val uprev2 = upper map rev.run
  println(uprev2.run("Hello World"))

  val res0: Reader1[String, String] = for {
    msg <- Reader1.ask[String]
    revMsg = msg.reverse
  } yield revMsg
  println(res0.run("Hello World"))

  val len = Reader1[String, Int](_.length)
  val inc = Reader1[ Int, Int](_ + 1)

  println((len andThen inc).run("Hello"))
  println((inc compose len).run("Hello"))

  println("-----\n")
}
