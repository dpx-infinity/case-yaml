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

import cc.cu.tplex.caseyaml.model.YEntity
import cc.cu.tplex.caseyaml.model.generators.ReflectiveEntityTreeGenerator
import scala.reflect.runtime.universe._

/** Library entry point, contain methods which return intermediate objects which perform [de]serialization. */
object CaseYaml {
  /** Same as [[cc.cu.tplex.caseyaml.CaseYaml#apply]],
    * returns an intermediate object, [[cc.cu.tplex.caseyaml.Converter]], which provides very small DSL for
    * [de]serialization.
    *
    * @param entity a description of the object which will be [de]serialized via the converter
    * @tparam Obj type of the object
    * @tparam Yml type of the YAML representation (usually [[java.util.Map]][String, Any] for classes
    * @return a converter for `entity` descriptor
    */
  def forEntity[Obj, Yml](entity: YEntity[Obj, Yml]): Converter[Obj, Yml] = new DefaultConverter(entity)

  /** Same as [[cc.cu.tplex.caseyaml.CaseYaml#forEntity]],
    * returns an intermediate object, [[cc.cu.tplex.caseyaml.Converter]], which provides very small DSL for
    * [de]serialization.
    *
    * @param entity a description of the object which will be [de]serialized via the converter
    * @tparam Obj type of the object
    * @tparam Yml type of the YAML representation (usually [[java.util.Map]][String, Any] for classes
    * @return a converter for `entity` descriptor
    */
  def apply[Obj, Yml](entity: YEntity[Obj, Yml]): Converter[Obj, Yml] = new DefaultConverter(entity)

  def forMultipleClasses: AccumulatingClassConverter = new MultiConverter

  def reflectiveEntityGeneratorFor[Obj: TypeTag] =
    new ReflectiveEntityTreeGenerator[Obj]
}
