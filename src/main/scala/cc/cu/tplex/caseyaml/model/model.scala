package cc.cu.tplex.caseyaml.model

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import java.math.BigInteger

sealed trait YEntity[+T] {
  def of(obj: Any): T = obj.asInstanceOf[T]
  def objReprName: String
  def mapReprName: String
}

sealed trait YEntry[T, +S] {
  def name: String
  def field: T => FieldMirror
  def entity: YEntity[S]
}
case class YFieldEntry[T, +S](name: String, field: T => FieldMirror, entity: YEntity[S]) extends YEntry[T, S]
case class SkipField[T](name: String = null) extends YEntry[T, Nothing] {
  val field = null
  val entity = null
}

case class YClassMap[T: TypeTag](entries: YEntry[T, _]*) extends YEntity[T] {
  val clazz = typeTag[T].mirror runtimeClass typeOf[T]
  val ctormirror = {
    val classm = typeTag[T].mirror reflectClass typeOf[T].typeSymbol.asClass
    classm reflectConstructor typeOf[T].declaration(nme.CONSTRUCTOR).asMethod
  }
  val objReprName = clazz.getName
  val mapReprName = "map for " + clazz.getName
}

case class YMap[T](valueEntity: YEntity[T]) extends YEntity[Map[String, T]] {
  val objReprName = s"map from string to ${valueEntity.objReprName}"
  val mapReprName = s"java.util.Map from string to ${valueEntity.mapReprName}"
}
case class YList[T](valueEntity: YEntity[T]) extends YEntity[Seq[T]] {
  val objReprName = s"list of ${valueEntity.objReprName}"
  val mapReprName = s"java.util.List of ${valueEntity.mapReprName}"
}

case class YNullable[T <: AnyRef](entity: YEntity[T]) extends YEntity[T] {
  val objReprName = s"nullable ${entity.objReprName}"
  val mapReprName = s"nullable ${entity.mapReprName}"
}

case class YOptional[T](entity: YEntity[T]) extends YEntity[Option[T]] {
  val objReprName = s"optional ${entity.objReprName}"
  val mapReprName = s"optional ${entity.mapReprName}"
}

case class YStringConverted[T: ClassTag](toStr: T => String, fromStr: String => T) extends YEntity[T] {
  val objReprName = s"string for ${implicitly[ClassTag[T]].runtimeClass.getName}"
  val mapReprName = objReprName
}

case object YString extends YEntity[String] {
  val objReprName = "string"
  val mapReprName = objReprName
}
case object YBoolean extends YEntity[Boolean] {
  val objReprName = "boolean"
  val mapReprName = objReprName
}

sealed trait YIntCompatible[T] extends YEntity[T] {
  def fromInt(int: Int): T
  def fromLong(long: Long): T
  def fromBigInt(bigint: java.math.BigInteger): T

  val objReprName = "int-compatible type"
  val mapReprName = "int, long or java.math.BigInteger"
}
object YIntCompatible {
  case object YByte extends YIntCompatible[Byte] {
    def fromInt(int: Int) = int.toByte
    def fromLong(long: Long) = long.toByte
    def fromBigInt(bigint: BigInteger) = bigint.byteValue()
  }

  case object YShort extends YIntCompatible[Short] {
    def fromInt(int: Int) = int.toShort
    def fromLong(long: Long) = long.toShort
    def fromBigInt(bigint: BigInteger) = bigint.shortValue()
  }

  case object YInt extends YIntCompatible[Int] {
    def fromInt(int: Int) = int
    def fromLong(long: Long) = long.toInt
    def fromBigInt(bigint: BigInteger) = bigint.intValue()
  }

  case object YLong extends YIntCompatible[Long] {
    def fromInt(int: Int) = int.toLong
    def fromLong(long: Long) = long
    def fromBigInt(bigint: BigInteger) = bigint.longValue()
  }

  case object YBigInt extends YIntCompatible[BigInt] {
    def fromInt(int: Int) = BigInt(int)
    def fromLong(long: Long) = BigInt(long)
    def fromBigInt(bigint: BigInteger) = BigInt(bigint)
  }
}

sealed trait YFloatCompatible[T] extends YEntity[T] {
  def fromDouble(double: Double): T

  val objReprName = "float-compatible type"
  val mapReprName = "double"
}
object YFloatCompatible {
  case object YFloat extends YFloatCompatible[Float] {
    def fromDouble(double: Double) = double.toFloat
  }

  case object YDouble extends YFloatCompatible[Double] {
    def fromDouble(double: Double) = double
  }

  case object YBigDecimal extends YFloatCompatible[BigDecimal] {
    def fromDouble(double: Double) = BigDecimal(double)
  }
}
