package cc.cu.tplex.caseyaml

import cc.cu.tplex.caseyaml.model.YEntity

/**
 * Date: 13.07.13
 * Time: 21:34
 *
 * @author Vladimir Matveev
 */
object CaseYaml {
  def forEntity(entity: YEntity) = new Converter(entity)
}
