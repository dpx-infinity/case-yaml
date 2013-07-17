package cc.cu.tplex.caseyaml.test

import scala.reflect.ClassTag
import org.scalatest.matchers.{BePropertyMatchResult, BePropertyMatcher}

/**
 * Date: 15.07.13
 * Time: 12:57
 */
trait CustomMatchers {
  def anInstanceOf[T: ClassTag]: BePropertyMatcher[T] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    new BePropertyMatcher[T] {
      def apply(objectWithProperty: T) =
        BePropertyMatchResult(isAssignable(clazz, objectWithProperty.getClass), "an instance of " + clazz.getName)

      private[this] def isAssignable(dst: Class[_], src: Class[_]): Boolean =
        dst.isAssignableFrom(src) || CustomMatchers.primitiveClasses.get(dst).exists(_.isAssignableFrom(src))
    }
  }
}

object CustomMatchers {
  private val primitiveClasses: Map[Class[_], Class[_]] = Map(
    java.lang.Byte.TYPE      -> classOf[java.lang.Byte],
    java.lang.Short.TYPE     -> classOf[java.lang.Short],
    java.lang.Character.TYPE -> classOf[java.lang.Character],
    java.lang.Integer.TYPE   -> classOf[java.lang.Integer],
    java.lang.Long.TYPE      -> classOf[java.lang.Long],
    java.lang.Float.TYPE     -> classOf[java.lang.Float],
    java.lang.Double.TYPE    -> classOf[java.lang.Double],
    java.lang.Boolean.TYPE   -> classOf[java.lang.Boolean],
    java.lang.Void.TYPE      -> classOf[java.lang.Void]
  )
}