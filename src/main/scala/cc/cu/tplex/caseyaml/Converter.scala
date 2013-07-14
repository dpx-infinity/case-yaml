package cc.cu.tplex.caseyaml

import cc.cu.tplex.caseyaml.model.YEntity
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor

/**
 * Date: 13.07.13
 * Time: 21:36
 *
 * @author Vladimir Matveev
 */
class Converter private[caseyaml] (entity: YEntity, yaml: Yaml = new Yaml(new SafeConstructor)) {
  def withYaml(yaml: Yaml) = new Converter(entity, yaml)

  def serialize[T](obj: T) = {

  }
}
