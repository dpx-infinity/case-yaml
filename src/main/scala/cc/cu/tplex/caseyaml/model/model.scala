package cc.cu.tplex.caseyaml.model

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import java.math.BigInteger

trait YEntity

trait YEntry[T] {
  def name: String
  def field: T => FieldMirror
  def entity: YEntity
}

case class YFieldEntry[T](name: String, field: T => FieldMirror, entity: YEntity) extends YEntry[T]
case class SkipField[T](name: String = null) extends YEntry[T] {
  val field = null
  val entity = null
}

case class YClassMap[T: TypeTag : ClassTag](entries: YEntry[T]*) extends YEntity {
  val clazz = typeTag[T].mirror runtimeClass typeOf[T]
  val ctormirror = {
    val classm = typeTag[T].mirror reflectClass typeOf[T].typeSymbol.asClass
    classm reflectConstructor typeOf[T].declaration(nme.CONSTRUCTOR).asMethod
  }
}

case class YMap(valueEntity: YEntity) extends YEntity
case class YList(entity: YEntity) extends YEntity

case class YStringConverted[T](toStr: T => String, fromStr: String => T) extends YEntity

case object YString extends YEntity
case object YBoolean extends YEntity

case class YIntCompatible[T : ClassTag]() extends YEntity {
  val clazz = implicitly[ClassTag[T]].runtimeClass
  require(YIntCompatible.isValid(clazz), "Required int-compatible class, got " + clazz)
}
object YIntCompatible {
  val intCompatibles = Seq(
    classOf[Byte],
    classOf[Short],
    classOf[Int],
    classOf[Long],
    classOf[BigInt]
  )

  def isValid(clazz: Class[_]) = intCompatibles contains clazz
}

case class YFloatCompatible[T: ClassTag]() extends YEntity {
  val clazz = implicitly[ClassTag[T]].runtimeClass
  require(YFloatCompatible.isValid(clazz), "Required float-compatible class, got " + clazz)
}

object YFloatCompatible {
  val floatCompatibles = Seq(
    classOf[Float],
    classOf[Double],
    classOf[BigDecimal]
  )

  def isValid(clazz: Class[_]) = floatCompatibles contains clazz
}
