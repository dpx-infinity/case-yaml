package cc.cu.tplex.caseyaml

import org.scalatest.FlatSpec
import org.expecty.Expecty
import cc.cu.tplex.caseyaml.model.ModelFixture

/**
 * Date: 14.07.13
 * Time: 14:22
 *
 * @author Vladimir Matveev
 */
class MapConverterToMapTest extends FlatSpec {
  "MapConverter.ToMap" should "serialize an object to a java.util.Map/ tree using YEntity tree" in {
    val m = MapConverter.ToMap
            .convert(ModelFixture.yentity, ModelFixture.model)
            .asInstanceOf[java.util.Map[String, Any]]

    val expect = new Expecty()
    expect {
      m.get("id") == "test"
      m.get("name") == null
      m.get("groupId") == "groupId"
      m.get("artifactId") == "artifactId"
      m.get("version") == "version"
      m.get("enabled") == true
      m.get("count") == 10
      m.get("fraction") == 12.2
    }

    val plugins = m.get("plugins").asInstanceOf[java.util.Map[String, java.util.Map[String, Any]]]
    expect(plugins.size == 1)

    val p = plugins.get("plugin1")
    expect {
      p.get("id") == "id"
      p.get("pluginName") == "name"
      p.get("groupId") == "groupId"
      p.get("artifactId") == "artifactId"
      p.get("version") == "version"
    }

    val d = p.get("dependencies").asInstanceOf[java.util.List[String]]
    expect {
      d.size == 2
      d.get(0) == "id1"
      d.get(1) == "id2"
    }
  }
}
