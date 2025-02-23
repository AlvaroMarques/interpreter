package evaluator

import evaluator.objects.NullObject

class Environment {
  var store: Map[String, Anything] = Map.empty[String, Anything]

  def addObject(context: String, variable: String, value: Anything): Option[Anything] = {
    store = store ++ Map(variableName(context, variable) -> value)
    Some(NullObject)
  }

  private[evaluator] def variableName(context: String, variable: String): String = s"$variable.$context"

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
