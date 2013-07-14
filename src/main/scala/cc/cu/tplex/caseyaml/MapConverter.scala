package cc.cu.tplex.caseyaml

import cc.cu.tplex.caseyaml.model._
import scala.collection.JavaConverters

/**
 * Date: 13.07.13
 * Time: 22:11
 *
 * @author Vladimir Matveev
 */
private[caseyaml] object MapConverter {
  import JavaConverters._

  object ToMap {
    def convert[T](entity: YEntity, obj: T): Any = entity match {
      case YString => convertString(obj)
      case YBoolean => obj.asInstanceOf[Boolean]
      case YIntCompatible() => convertInt(obj)
      case YFloatCompatible() => convertFloat(obj)
      case YMap(valueEntity) => convertMap(valueEntity, obj)
      case YList(valueEntity) => convertList(valueEntity, obj)
      case YStringConverted(to, _) => to.asInstanceOf[(T) => String](obj)
      case cm @ YClassMap(entries @ _*) =>
        if (cm.clazz.isInstance(obj)) convertClassMap(entries, obj)
        else throw CaseYamlException(s"Requested ${cm.clazz}, got ${obj.getClass}")
    }

    def convertString[T](obj: T): String = obj match {
      case s: String => s
      case _ => throw CaseYamlException("Requested string, got " + obj.getClass)
    }

    def convertBoolean[T](obj: T): Boolean = obj match {
      case b: Boolean => b
      case _ => throw CaseYamlException("Requested boolean, got " + obj.getClass)
    }

    def convertMap[T](valueEntity: YEntity, obj: T): java.util.Map[String, Any] = obj match {
      case m: Map[String, Any] => m.mapValues(convert(valueEntity, _)).asJava
      case _ => throw CaseYamlException("Requested map-compatible type, got " + obj.getClass)
    }

    def convertList[T](valueEntity: YEntity, obj: T): java.util.List[Any] = obj match {
      case s: Seq[Any] => s.map(convert(valueEntity, _)).asJava
      case _ => throw CaseYamlException("Requested seq-compatible type, got " + obj.getClass)
    }

    def convertClassMap[T](entries: Iterable[YEntry[T]], obj: T): java.util.Map[String, Any] =
      entries.map { entry =>
        entry.name -> convert(entry.entity, entry.field(obj).get)
      }.toMap.asJava

    def convertInt(obj: Any): Any = obj match {
      case byte: Byte => byte.toInt
      case short: Short => short.toInt
      case int: Int => int
      case long: Long => long
      case bigint: BigInt => bigint.bigInteger
      case _ => throw CaseYamlException("Requested int-compatible type, got " + obj.getClass)
    }

    def convertFloat(obj: Any): Any = obj match {
      case float: Float => float
      case double: Double => double
      case bigdec: BigDecimal => bigdec
      case _ => throw CaseYamlException("Requested float-compatible type, got " + obj.getClass)
    }
  }

  object FromMap {

  }
}
