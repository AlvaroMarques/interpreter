package evaluator

import evaluator.objects.{BuiltinFunctionObject, NullObject}

case class Environment(
    builtin: Seq[BuiltinFunctionObject],
    initialContext: String
) {
  var store: Map[String, Anything] = builtin.map { fn =>
    variableName(initialContext, fn.name) -> fn
  }.toMap

  def addObject(
      context: String,
      variable: String,
      value: Anything
  ): Option[Anything] = {
    store = store ++ Map(variableName(context, variable) -> value)
    Some(NullObject)
  }

  private[evaluator] def variableName(
      context: String,
      variable: String
  ): String = s"$variable.$context"

  def deleteContext(context: String): Unit = {
    store = store.filter {
      case (key, _) => key.endsWith(context)
    }
  }

  def getObject(context: String, variable: String): Option[Anything] = {
    if (context == "") {
      None
    } else {
      store.get(variableName(context = context, variable = variable)) match {
        case Some(obj) => Some(obj)
        case None =>
          context.split("[.]") match {
            case nonEmptyArray: Array[String] if nonEmptyArray.nonEmpty =>
              getObject(nonEmptyArray.tail.mkString("."), variable)
            case _ =>
              None
          }
      }
    }
  }
}
