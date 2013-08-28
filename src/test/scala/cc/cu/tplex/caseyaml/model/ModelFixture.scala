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
                        shape: Shape,
                        plugins: Map[String, PluginModel])

case class PluginModel(id: ModelId,
                       name: String,
                       data: Option[BigDecimal],
                       status: String = "active",
                       dependencies: Seq[ModelId])

sealed trait Shape
case class Rectangle(x1: Double, y1: Double, x2: Double, y2: Double) extends Shape
case class Circle(x: Double, y: Double, r: Double) extends Shape

object ModelFixture {
  val modelId = YStringConverted[ModelId](_.id, ModelId)

  val manualEntity = YClassMap[ProjectModel](
    "id"         --> modelId,
    "name"       --> YString,
    "enabled"    --> YBoolean,
    "count"      --> YIntCompatible.YInt,
    "fraction"   --> YFloatCompatible.YDouble,
    "shape"      --> YSealedTrait[Shape](
      YClassMap[Rectangle](
        "x1" --> YFloatCompatible.YDouble,
        "y1" --> YFloatCompatible.YDouble,
        "x2" --> YFloatCompatible.YDouble,
        "y2" --> YFloatCompatible.YDouble
      ),
      YClassMap[Circle](
        "x" --> YFloatCompatible.YDouble,
        "y" --> YFloatCompatible.YDouble,
        "r" --> YFloatCompatible.YDouble
      )
    ),
    "plugins"    --> YMap(YClassMap[PluginModel](
      "id"                    --> modelId,
      "name" ~> "pluginName"  --> YString,
      "data"                  --> YOptional(YFloatCompatible.YBigDecimal),
      "status"                --> YDefault(YString, "active"),
      "dependencies"          --> YList(modelId)
    ))
  )

  val reflectiveEntity =
    CaseYaml.reflectiveEntityGeneratorFor[ProjectModel]
      .withField[ModelId]((_: ModelId).id, ModelId)
      .withField[Shape](YSealedTrait.construct[Shape])
      .generateClass

  val model = ProjectModel(
    ModelId("test"),
    "name",
    true,
    10,
    12.2,
    Circle(0, 0, 0),
    Map(
      "plugin1" -> PluginModel(
        ModelId("id"),
        "name",
        None,
        "inactive",
        Seq(ModelId("id1"), ModelId("id2"))
      ),
      "plugin2" -> PluginModel(
        ModelId("id2"),
        "name2",
        Some(123.45),
        "active",
        Seq.empty
      )
    )
  )
}

object ShapeFixture {
  val manualEntity = YSealedTrait[Shape](
     YClassMap[Rectangle](
       "x1" --> YFloatCompatible.YDouble,
       "y1" --> YFloatCompatible.YDouble,
       "x2" --> YFloatCompatible.YDouble,
       "y2" --> YFloatCompatible.YDouble
     ),
     YClassMap[Circle](
       "x" --> YFloatCompatible.YDouble,
       "y" --> YFloatCompatible.YDouble,
       "r" --> YFloatCompatible.YDouble
     )
  )

  val reflectiveEntity = YSealedTrait.construct[Shape]

  val shapeRect = Rectangle(1, 2, 3, 4)
  val shapeCirc = Circle(5, 6, 7)
}
