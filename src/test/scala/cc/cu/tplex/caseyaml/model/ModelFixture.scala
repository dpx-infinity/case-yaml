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

package cc.cu.tplex.caseyaml.model

import cc.cu.tplex.caseyaml.CaseYaml

/**
 * Date: 14.07.13
 * Time: 14:27
 *
 * @author Vladimir Matveev
 */
case class ModelId(id: String)

case class ProjectModel(id: ModelId,
                        name: String,
                        enabled: Boolean,
                        count: Int,
                        fraction: Double,
                        plugins: Map[String, PluginModel])

case class PluginModel(id: ModelId,
                       name: String,
                       data: Option[BigDecimal],
                       dependencies: Seq[ModelId])

object ModelFixture {
  val modelId = YStringConverted[ModelId](_.id, ModelId)

  val manualEntity = YClassMap[ProjectModel](
    "id"         --> modelId,
    "name"       --> YString,
    "enabled"    --> YBoolean,
    "count"      --> YIntCompatible.YInt,
    "fraction"   --> YFloatCompatible.YDouble,
    "plugins"    --> YMap(YClassMap[PluginModel](
      "id"                    --> modelId,
      "name" ~> "pluginName"  --> YString,
      "data"                  --> YOptional(YFloatCompatible.YBigDecimal),
      "dependencies"          --> YList(modelId)
    ))
  )

  val reflectiveEntity =
    CaseYaml.reflectiveEntityGeneratorFor[ProjectModel]
      .withStringConvertedField[ModelId](_.id, ModelId)
      .generateClass


  val model = ProjectModel(
    ModelId("test"),
    "name",
    true,
    10,
    12.2,
    Map(
      "plugin1" -> PluginModel(
        ModelId("id"),
        "name",
        None,
        Seq(ModelId("id1"), ModelId("id2"))
      ),
      "plugin2" -> PluginModel(
        ModelId("id2"),
        "name2",
        Some(123.45),
        Seq.empty
      )
    )
  )

}
