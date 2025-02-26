package evaluator

import evaluator.objects.{BuiltinFunctionObject, NullObject, StringObject}
import scala.io.StdIn

package object builtin {

  val fns: Seq[BuiltinFunctionObject] = Seq(
    PrintFn,
    PrintlnFn,
    InputFn
  )

  object InputFn extends BuiltinFunctionObject {
    override val name = "input"
    override def executor: Seq[Anything] => Option[Anything] =
      (arguments: Seq[Anything]) => {
        print(
          arguments
            .map {
              case arg: StringObject => arg.value
              case arg: Anything     => arg.inspect
            }
            .mkString(" ")
        )
        StdIn.readLine() match {
          case s: String => Some(StringObject(value = s))
        }
      }
  }

  object PrintlnFn extends BuiltinFunctionObject {
    override val name = "println"
    override def executor: Seq[Anything] => Option[Anything] =
      (arguments: Seq[Anything]) => {
        println(
          arguments
            .map {
              case arg: StringObject => arg.value
              case arg: Anything     => arg.inspect
            }
            .mkString(" ")
        )
        Some(NullObject)
      }
  }

  object PrintFn extends BuiltinFunctionObject {
    override val name = "print"
    override def executor: Seq[Anything] => Option[Anything] =
      (arguments: Seq[Anything]) => {
        print(arguments.map(_.inspect).mkString(" "))
        Some(NullObject)
      }
  }

}
