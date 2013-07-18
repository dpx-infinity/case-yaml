/*******************************************************************************
 * Copyright 2013 Vladimir Matveev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

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