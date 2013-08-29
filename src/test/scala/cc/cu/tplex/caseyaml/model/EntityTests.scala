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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import cc.cu.tplex.caseyaml.test.CustomMatchers

class EntityTests extends FlatSpec with ShouldMatchers with CustomMatchers {
  import ModelFixture.model

  def testEntity(entity: YClassMap[ProjectModel]) {
    val Seq(id, name, enabled, count, fraction, shape, plugins) = entity.entries

    id.field(model).get           should equal (ModelId("test"))
    name.field(model).get         should equal ("name")
    enabled.field(model).get      should equal (true)
    count.field(model).get        should equal (10)
    shape.field(model).get        should equal (Circle(0, 0, 0))
    fraction.field(model).get     should equal (12.2)

    {
      val pluginsMap = plugins.field(model).get.asInstanceOf[Map[String, PluginModel]]
      pluginsMap should have size 2

      val Seq(id, name, data, status, dependencies) =
        plugins.entity.asInstanceOf[YMap[PluginModel, java.util.Map[String, Any]]]
        .valueEntity.asInstanceOf[YClassMap[PluginModel]].entries

      {
        val plugin = pluginsMap("plugin1")
        id.field(plugin).get           should equal (ModelId("id"))
        name.field(plugin).get         should equal ("name")
        data.field(plugin).get         should equal (None)
        status.field(plugin).get       should equal ("inactive")
        dependencies.field(plugin).get should equal (Seq(ModelId("id1"), ModelId("id2")))
      }

      {
        val plugin = pluginsMap("plugin2")
        id.field(plugin).get           should equal (ModelId("id2"))
        name.field(plugin).get         should equal ("name2")
        data.field(plugin).get         should equal (Some(BigDecimal(123.45)))
        status.field(plugin).get       should equal ("active")
        dependencies.field(plugin).get should equal (Seq.empty)
      }
    }
  }

  "Manually constructed entity" should "allow access to object fields" in {
    testEntity(ModelFixture.manualEntity)
  }

  "Reflectively generated entity" should "allow access to object fields" in {
    testEntity(ModelFixture.reflectiveEntity)
  }

  def testSealedTrait(entity: YSealedTrait[Shape]) {
    val YSealedTrait(rectEntity, circEntity) = entity

    rectEntity.clazz should be theSameInstanceAs classOf[Rectangle]
    circEntity.clazz should be theSameInstanceAs classOf[Circle]

    val re = rectEntity.asInstanceOf[YClassMap[Rectangle]]
    val ce = circEntity.asInstanceOf[YClassMap[Circle]]

    import ShapeFixture.{shapeRect, shapeCirc}

    val Seq(x1, y1, x2, y2) = re.entries
    x1.field(shapeRect).get should equal (1.0)
    y1.field(shapeRect).get should equal (2.0)
    x2.field(shapeRect).get should equal (3.0)
    y2.field(shapeRect).get should equal (4.0)

    val Seq(x, y, r) = ce.entries
    x.field(shapeCirc).get should equal (5.0)
    y.field(shapeCirc).get should equal (6.0)
    r.field(shapeCirc).get should equal (7.0)
  }

  "Manually constructed YSealedTrait" should "allow extracting object fields" in {
    testSealedTrait(ShapeFixture.manualEntity)
  }

  "Macrogenerated YSealedTrait" should "allow extracting object fields too" ignore {
    testSealedTrait(ShapeFixture.reflectiveEntity)
  }
}
