package cc.cu.tplex.caseyaml.model

import org.scalatest.FlatSpec
import org.expecty.Expecty

class ModelTest extends FlatSpec {
  val model = ProjectModel(
    ModelId("test"),
    "name",
    "groupId",
    "artifactId",
    "version",
    true,
    10,
    12.2,
    Map(
      "plugin1" -> PluginModel(
        ModelId("id"),
        "name",
        "groupId",
        "artifactId",
        "version",
        Seq(ModelId("id1"), ModelId("id2"))
      )
    )
  )


  "YFieldEntry implicit conversion" should "allow access to object fields" in {
    val Seq(id, skip, groupId, artifactId, version, enabled, count, fraction, plugins) =
      ModelFixture.yentity.entries

    val expect = new Expecty()

    expect {
      id.field(model).get           == ModelId("test")
      skip                          == SkipField[ProjectModel]()
      groupId.field(model).get      == "groupId"
      artifactId.field(model).get   == "artifactId"
      version.field(model).get      == "version"
      enabled.field(model).get      == true
      count.field(model).get        == 10
      fraction.field(model).get     == 12.2
    }

    {
      val pluginsMap = plugins.field(model).get.asInstanceOf[Map[String, PluginModel]]
      expect(pluginsMap.size == 1)

      val Seq(id, name, groupId, artifactId, version, dependencies) =
        plugins.entity.asInstanceOf[YMap].valueEntity.asInstanceOf[YClassMap[PluginModel]].entries

      val plugin = pluginsMap("plugin1")
      expect {
        id.field(plugin).get           == ModelId("id")
        name.field(plugin).get         == "name"
        groupId.field(plugin).get      == "groupId"
        artifactId.field(plugin).get   == "artifactId"
        version.field(plugin).get      == "version"
        dependencies.field(plugin).get == Seq(ModelId("id1"), ModelId("id2"))
      }
    }
  }
}
