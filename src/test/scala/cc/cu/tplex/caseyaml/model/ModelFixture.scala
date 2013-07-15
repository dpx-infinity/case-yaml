package cc.cu.tplex.caseyaml.model

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
                       dependencies: Seq[ModelId])

object ModelFixture {
  val modelId = YStringConverted[ModelId](_.id, ModelId)

  val yentity = YClassMap[ProjectModel](
    "id" --> modelId,
    skipField,
    "enabled"    --> YBoolean,
    "count"      --> YIntCompatible.YInt,
    "fraction"   --> YFloatCompatible.YDouble,
    "plugins"    --> YMap(YClassMap[PluginModel](
      "id"                    --> modelId,
      "name" ~> "pluginName"  --> YString,
      "dependencies"          --> YList(modelId)
    ))
  )

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
        Seq(ModelId("id1"), ModelId("id2"))
      )
    )
  )

}
