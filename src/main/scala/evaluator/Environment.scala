package evaluator

import evaluator.objects.NullObject

class Environment {
  var store: Map[String, Anything] = Map.empty[String, Anything]

  def addObject(context: String, variable: String, Object: Anything): Option[Anything] = {
    store = store ++ Map(variableName(context, variable) -> Object)
    Some(NullObject)
  }

  private[evaluator] def variableName(context: String, variable: String): String = s"$context.$variable"

  def getObject(context: String, variable: String): Option[Anything] = {
    store.get(variableName(context = context, variable = variable))
  }
}
