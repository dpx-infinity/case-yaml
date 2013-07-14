package cc.cu.tplex.caseyaml

/**
 * Date: 13.07.13
 * Time: 22:25
 *
 * @author Vladimir Matveev
 */
case class CaseYamlException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
