package cc.cu.tplex.caseyaml.model

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag

trait YEntity[+T]

trait YEntry[T, +S] {
  def name: String
  def field: T => FieldMirror
  def entity: YEntity[S]
}

case class YFieldEntry[T, +S](name: String, field: T => FieldMirror, entity: YEntity[S]) extends YEntry[T, S]
case class SkipField[T](name: String = null) extends YEntry[T, Nothing] {
  val field = null
  val entity = null
}

case class YClassMap[T: TypeTag : ClassTag](entries: YEntry[T, _]*) extends YEntity[T] {
  val clazz = typeTag[T].mirror runtimeClass typeOf[T]
  val ctormirror = {
    val classm = typeTag[T].mirror reflectClass typeOf[T].typeSymbol.asClass
    classm reflectConstructor typeOf[T].declaration(nme.CONSTRUCTOR).asMethod
  }
}

case class YMap(valueEntity: YEntity) extends YEntity[Map[String, _]]
case class YList(entity: YEntity) extends YEntity[Seq[_]]

case class YNullable[T <: AnyRef](entity: YEntity[T]) extends YEntity[T]

case class YStringConverted[T](toStr: T => String, fromStr: String => T) extends YEntity[T]

case object YString extends YEntity[String]
case object YBoolean extends YEntity[String]

case class YIntCompatible[T : ClassTag]() extends YEntity[T] {
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

case class YFloatCompatible[T: ClassTag]() extends YEntity[T] {
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
