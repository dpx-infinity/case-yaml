package cc.cu.tplex.caseyaml.model

import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import java.math.BigInteger

sealed trait YEntity[+T]

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

case class YClassMap[T: TypeTag : ClassTag](entries: YEntry[T, _]*) extends YEntity[T] {
  val clazz = typeTag[T].mirror runtimeClass typeOf[T]
  val ctormirror = {
    val classm = typeTag[T].mirror reflectClass typeOf[T].typeSymbol.asClass
    classm reflectConstructor typeOf[T].declaration(nme.CONSTRUCTOR).asMethod
  }
}

case class YMap[T](valueEntity: YEntity[T]) extends YEntity[Map[String, T]]
case class YList[T](entity: YEntity[T]) extends YEntity[Seq[T]]

case class YNullable[T <: AnyRef](entity: YEntity[T]) extends YEntity[T]

case class YStringConverted[T](toStr: T => String, fromStr: String => T) extends YEntity[T]

case object YString extends YEntity[String]
case object YBoolean extends YEntity[Boolean]

sealed trait YIntCompatible[T] extends YEntity[T] {
  def fromInt(int: Int): T
  def fromLong(long: Long): T
  def fromBigInt(bigint: java.math.BigInteger): T
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
