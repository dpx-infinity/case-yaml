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
  case class NamedField(name: String, fieldName: String) {
    def createEntry[T, Obj, Yml](entity: YEntity[Obj, Yml], mirror: Mirror, tpe: Type): YEntry[T, Obj, Yml] = {
      val fsym = tpe.declaration(fieldName: TermName).asTerm
      def fieldGetter(obj: T) = {
        val im = mirror.reflect(obj)(ClassTag(obj.getClass))
        im.reflectField(fsym)
      }
      YFieldEntry(name, fieldGetter, entity)
    }

    def createEntry[T: TypeTag, Obj, Yml](entity: YEntity[Obj, Yml]): YEntry[T, Obj, Yml] =
      this.createEntry(entity, typeTag[T].mirror, typeOf[T])
  }

  object NamedField {
    def apply(name: String) = NamedField(name, name)
  }

  implicit class NamedField2EntryWrapper(val namedField: NamedField) extends AnyVal {
    def -->[T: TypeTag, Obj, Yml](entity: YEntity[Obj, Yml]): YEntry[T, Obj, Yml] = {
      namedField.createEntry(entity)
    }
  }

  implicit def string2entryWrapper(fieldName: String) =
    new NamedField2EntryWrapper(NamedField(fieldName, fieldName))

  implicit class String2NamedFieldWrapper(val fieldName: String) extends AnyVal {
    def ~>(name: String) = NamedField(name, fieldName)
  }

  implicit class AsEntityWrapper(val obj: Any) extends AnyVal {
    def asYmlOf[Obj, Yml](entity: YEntity[Obj, Yml]): Yml = obj.asInstanceOf[Yml]
    def asObjOf[Obj, Yml](entity: YEntity[Obj, Yml]): Obj = obj.asInstanceOf[Obj]
  }
}
