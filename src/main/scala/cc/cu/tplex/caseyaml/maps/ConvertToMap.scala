package cc.cu.tplex.caseyaml.maps

import cc.cu.tplex.caseyaml.model._
import scala.Some
import cc.cu.tplex.caseyaml.CaseYamlException
import scala.collection.JavaConverters

/**
 * Date: 15.07.13
 * Time: 11:06
 */
object ConvertToMap {
  import JavaConverters._

  def convert[Obj, Yml](entity: YEntity[Obj, Yml], obj: Obj): Yml = entity match {
    case yn: YNullable[Obj, Yml] => convertNullable[Obj, Yml](yn.entity, obj)
    case ys: YString.type => convertString(checkNull(ys, obj))
    case yb: YBoolean.type => convertBoolean(checkNull(yb, obj))
    case ic: YIntCompatible[Obj] => convertInt(ic, checkNull(ic, obj))
    case fc: YFloatCompatible[Obj] => convertFloat(fc, checkNull(fc, obj))
    case m: YMap[o, y] => convertMap(m, checkNull(m, obj))
    case l: YList[o, y] => convertList(l, checkNull(l, obj))
    case YStringConverted(to: Function1[Obj, String], _) => to(obj)
    case cm: YClassMap[Obj] =>
      if (obj == null || cm.clazz.isInstance(obj)) convertClassMap(cm, checkNull(cm, obj))
      else throw CaseYamlException(s"Expected ${cm.clazz.getName}, got ${obj.getClass.getName}")
    case _: YOptional[_, _] =>
      throw CaseYamlException("YOptional is not applicable outside of YClassMap")
  }

  def checkNull[Obj, Yml](entity: YEntity[Obj, Yml], obj: Obj): Obj = obj match {
    case null => throw CaseYamlException(s"Expected ${entity.objReprName}, got null")
    case _ => obj
  }

  def convertNullable[Obj, Yml](entity: YEntity[Obj, Yml], obj: Obj): Yml = obj match {
    case null => null.asInstanceOf[Yml]
    case _ => convert(entity, obj)
  }

  def convertString[T](obj: T): String = obj match {
    case s: String => s
    case _ => throw CaseYamlException("Expected string, got " + obj.getClass.getName)
  }

  def convertBoolean[T](obj: T): Boolean = obj match {
    case b: Boolean => b
    case _ => throw CaseYamlException("Expected boolean, got " + obj.getClass.getName)
  }

  def convertInt[Obj](ic: YIntCompatible[Obj], obj: Obj): Number = obj match {
    case _: Byte | _: Short | _: Int | _: Long | _: BigInt => ic.toYml(obj)
    case _ => throw CaseYamlException(s"Expected ${ic.objReprName}, got ${obj.getClass.getName}")
  }

  def convertFloat[Obj](fc: YFloatCompatible[Obj], obj: Obj): Number = obj match {
    case _: Float | _: Double | _: BigDecimal => fc.toYml(obj)
    case _ => throw CaseYamlException(s"Expected ${fc.objReprName}, got ${obj.getClass.getName}")
  }

  def convertMap[Obj, Yml, T](entity: YMap[Obj, Yml], obj: T): java.util.Map[String, Yml] = obj match {
    case m: Map[String, Obj] => m.mapValues(v => convert(entity.valueEntity, v)).asJava
    case _ => throw CaseYamlException(s"Expected ${entity.objReprName}, got ${obj.getClass.getName}")
  }

  def convertList[Obj, Yml, T](entity: YList[Obj, Yml], obj: T): java.util.List[Yml] = obj match {
    case s: Seq[Obj] => s.map(v => convert(entity.valueEntity, v)).asJava
    case _ => throw CaseYamlException(s"Expected ${entity.objReprName}, got ${obj.getClass.getName}")
  }

  def convertClassMap[T](cm: YClassMap[T], obj: T): java.util.Map[String, Any] =
    cm.entries.map {
      case entry: YEntry[T, o, y] =>
        val (entity, possibleValue) = entry.entity match {
          case yo: YOptional[`o`, `y`] => yo.entity -> (entry.field(obj).get match {
            case Some(value: `o`) => Some(value)
            case None => None
            case null => throw CaseYamlException(s"Expected ${yo.objReprName}, got null")
            case other => throw CaseYamlException(s"Expected ${yo.objReprName}, got " + other.getClass.getName)
          })
          case _ => entry.entity -> Some(entry.field(obj).get.asInstanceOf[o])
        }
        possibleValue map { value => entry.name -> convert[o, y](entity, value) }
    }.flatten.toMap.asJava
}

