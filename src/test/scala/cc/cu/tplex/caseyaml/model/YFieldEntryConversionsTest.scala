package cc.cu.tplex.caseyaml.model

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class YFieldEntryConversionsTest extends FlatSpec with ShouldMatchers {
  import ModelFixture.model

  "YFieldEntry implicit conversions" should "allow access to object fields" in {
    val Seq(id, name, enabled, count, fraction, plugins) =
      ModelFixture.yentity.entries

    id.field(model).get           should equal (ModelId("test"))
    name.field(model).get         should equal ("name")
    enabled.field(model).get      should equal (true)
    count.field(model).get        should equal (10)
    fraction.field(model).get     should equal (12.2)

    {
      val pluginsMap = plugins.field(model).get.asInstanceOf[Map[String, PluginModel]]
      pluginsMap should have size 2

      val Seq(id, name, data, dependencies) =
        plugins.entity.asInstanceOf[YMap[PluginModel, java.util.Map[String, Any]]]
          .valueEntity.asInstanceOf[YClassMap[PluginModel]].entries

      {
        val plugin = pluginsMap("plugin1")
        id.field(plugin).get           should equal (ModelId("id"))
        name.field(plugin).get         should equal ("name")
        data.field(plugin).get         should equal (None)
        dependencies.field(plugin).get should equal (Seq(ModelId("id1"), ModelId("id2")))
      }

      {
        val plugin = pluginsMap("plugin2")
        id.field(plugin).get           should equal (ModelId("id2"))
        name.field(plugin).get         should equal ("name2")
        data.field(plugin).get         should equal (Some(BigDecimal(123.45)))
        dependencies.field(plugin).get should equal (Seq.empty)
      }
    }
  }
}
