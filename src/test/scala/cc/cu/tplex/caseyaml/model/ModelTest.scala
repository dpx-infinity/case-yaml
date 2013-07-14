package cc.cu.tplex.caseyaml.model

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ModelTest extends FlatSpec with ShouldMatchers {
  import ModelFixture.model

  "YFieldEntry implicit conversion" should "allow access to object fields" in {
    val Seq(id, skip, groupId, artifactId, version, enabled, count, fraction, plugins) =
      ModelFixture.yentity.entries

    id.field(model).get           should equal (ModelId("test"))
      skip                          should equal (SkipField[ProjectModel]())
      groupId.field(model).get      should equal ("groupId")
      artifactId.field(model).get   should equal ("artifactId")
      version.field(model).get      should equal ("version")
      enabled.field(model).get      should equal (true)
      count.field(model).get        should equal (10)
      fraction.field(model).get     should equal (12.2)

    {
      val pluginsMap = plugins.field(model).get.asInstanceOf[Map[String, PluginModel]]
      pluginsMap should have size 1

      val Seq(id, name, groupId, artifactId, version, dependencies) =
        plugins.entity.asInstanceOf[YMap].valueEntity.asInstanceOf[YClassMap[PluginModel]].entries

      val plugin = pluginsMap("plugin1")
      id.field(plugin).get           should equal (ModelId("id"))
      name.field(plugin).get         should equal ("name")
      groupId.field(plugin).get      should equal ("groupId")
      artifactId.field(plugin).get   should equal ("artifactId")
      version.field(plugin).get      should equal ("version")
      dependencies.field(plugin).get should equal (Seq(ModelId("id1"), ModelId("id2")))
    }
  }
}
