package cc.cu.tplex.caseyaml

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

/**
 * Date: 13.07.13
 * Time: 19:34
 *
 * @author Vladimir Matveev
 */
package object model {
  case class NamedField(name: String, fieldName: String)

  implicit class NamedField2EntryWrapper(val namedField: NamedField) extends AnyVal {
    def -->[T: TypeTag : ClassTag](entity: YEntity): YEntry[T] = {
      val fsym = typeOf[T].declaration(namedField.fieldName: TermName).asTerm
      def fieldGetter(obj: T) = {
        val im = typeTag[T].mirror.reflect(obj)
        im.reflectField(fsym)
      }
      YFieldEntry(namedField.name, fieldGetter, entity)
    }
  }

  implicit def string2entryWrapper(fieldName: String) =
    new NamedField2EntryWrapper(NamedField(fieldName, fieldName))

  implicit class String2NamedFieldWrapper(val fieldName: String) extends AnyVal {
    def ~>(name: String) = NamedField(name, fieldName)
  }

  def skipField[T] = SkipField[T]()
}
