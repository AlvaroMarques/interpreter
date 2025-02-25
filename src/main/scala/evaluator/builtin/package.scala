package evaluator

import evaluator.objects.{BuiltinFunctionObject, NullObject}


package object builtin {

  object PrintFn extends BuiltinFunctionObject {
    override val name = "print"
    override def executor: Seq[Anything] => Option[Anything] = (arguments: Seq[Anything]) => {
      print(arguments.map(_.inspect).mkString(" "))
      Some(NullObject)
    }

  }


}
