/*******************************************************************************
 * Copyright 2013 Vladimir Matveev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package cc.cu.tplex.caseyaml.model

import scala.reflect.runtime.{universe => ru}
import ru._
import scala.reflect.ClassTag
import cc.cu.tplex.caseyaml.model.generators.ReflectiveEntityTreeGenerator

sealed trait YEntity[Obj, Yml] {
  def objReprName: String
  def ymlReprName: String
}

sealed trait YEntry[Cls, Obj, Yml] {
  def name: String
  def field: Cls => FieldMirror
  def entity: YEntity[Obj, Yml]
}
case class YFieldEntry[Cls, Obj, Yml](name: String, field: Cls => FieldMirror, entity: YEntity[Obj, Yml])
  extends YEntry[Cls, Obj, Yml]

case class YClassMap[Cls: TypeTag](entries: YEntry[Cls, _, _]*) extends YEntity[Cls, java.util.Map[String, Any]] {
  val clazz = typeTag[Cls].mirror runtimeClass typeOf[Cls]
  val ctormirror = {
    val classm = typeTag[Cls].mirror reflectClass typeOf[Cls].typeSymbol.asClass
    classm reflectConstructor typeOf[Cls].declaration(nme.CONSTRUCTOR).asMethod
  }
  val objReprName = clazz.getName
  val ymlReprName = "java.util.Map from string to any for " + clazz.getName
}

case class YSealedTrait[Cls: TypeTag](subclasses: YClassMap[_ <: Cls]*)
  extends YEntity[Cls, java.util.Map[String, Any]] {
  val clazz = typeTag[Cls].mirror runtimeClass typeOf[Cls]
  def objReprName = s"sealed trait ${clazz.getName}"
  def ymlReprName = s"java.util.Map from string to any for sealed trait " + clazz.getName
}

object YSealedTrait {
  import scala.language.experimental.macros
  import scala.reflect.macros.Context

  def construct[T]: YSealedTrait[T] = macro construct_impl[T]

  def construct_impl[T: c.WeakTypeTag](c: Context): c.Expr[YSealedTrait[T]] = {
    import c.universe._

    val tSym = weakTypeOf[T].typeSymbol
    c.echo(c.enclosingPosition, s"Provided symbol: $tSym")

    if (!tSym.isClass) {
      c.abort(c.enclosingPosition, "Can only construct YSealedTrait from _sealed trait_")
    } else if (!tSym.asClass.isTrait) {
      c.abort(c.enclosingPosition, "Can only construct YSealedTrait from sealed _trait_")
    } else if (!tSym.asClass.isSealed) {
      c.abort(c.enclosingPosition, "Can only construct YSealedTrait from _sealed_ trait")
    } else {
      val children = tSym.asClass.knownDirectSubclasses.toSeq
      if (children.isEmpty) {
        // Temporary disabled
        // c.abort(c.enclosingPosition, "Could not find any sealed trait children")
      } else if (!children.forall(c => c.isClass && c.asClass.isCaseClass)) {
        c.abort(c.enclosingPosition, "All children of sealed trait must be case classes")
      }
      c.echo(c.enclosingPosition, s"Found children: [${children.mkString(", ")}]")

      val retgIdent = Ident(weakTypeOf[ReflectiveEntityTreeGenerator[_]].typeSymbol.name)
      val entities: Seq[c.Expr[YClassMap[_ <: T]]] = children.map { chsym =>
        val chIdent = Ident(chsym.name)
        val `new RETGenerator[C]` = New(AppliedTypeTree(retgIdent, chIdent :: Nil))
        val `new RETGenerator[C].<init>` = Select(`new RETGenerator[C]`, nme.CONSTRUCTOR)
        val `new RETGenerator[C].<init>()` = Apply(`new RETGenerator[C].<init>`, Nil)
        val `new RETGenerator[C].<init>().generateClass` =
          Select(`new RETGenerator[C].<init>()`, newTermName("generateClass"))
        c.Expr[YClassMap[_ <: T]](Apply(`new RETGenerator[C].<init>().generateClass`, Nil))
      }

      val ystIdent = reify(YSealedTrait).tree
      val `YSealedTrait.apply` = Select(ystIdent, newTermName("apply"))
      val `YSealedTrait.apply[T]` = TypeApply(`YSealedTrait.apply`, TypeTree(weakTypeOf[T]) :: Nil)
      val `YSealedTrait.apply[T](entities)` = Apply(`YSealedTrait.apply[T]`, entities.map(_.tree).toList)

      c.Expr[YSealedTrait[T]](`YSealedTrait.apply[T](entities)`)
    }
  }
}

case class YMap[Obj, Yml](valueEntity: YEntity[Obj, Yml])
  extends YEntity[Map[String, Obj], java.util.Map[String, Yml]] {
  val objReprName = s"map from string to ${valueEntity.objReprName}"
  val ymlReprName = s"java.util.Map from string to ${valueEntity.ymlReprName}"
}

case class YList[Obj, Yml](valueEntity: YEntity[Obj, Yml]) extends YEntity[Seq[Obj], java.util.List[Yml]] {
  val objReprName = s"list of ${valueEntity.objReprName}"
  val ymlReprName = s"java.util.List of ${valueEntity.ymlReprName}"
}

case class YSet[Obj, Yml](valueEntity: YEntity[Obj, Yml]) extends YEntity[Set[Obj], java.util.Set[Yml]] {
  val objReprName = s"set of ${valueEntity.objReprName}"
  val ymlReprName = s"java.util.Set of ${valueEntity.ymlReprName}"
}

case class YNullable[Obj <: AnyRef, Yml <: AnyRef](entity: YEntity[Obj, Yml]) extends YEntity[Obj, Yml] {
  val objReprName = s"nullable ${entity.objReprName}"
  val ymlReprName = s"nullable ${entity.ymlReprName}"
}

case class YOptional[Obj, Yml](entity: YEntity[Obj, Yml]) extends YEntity[Option[Obj], Yml] {
  val objReprName = s"optional ${entity.objReprName}"
  val ymlReprName = s"optional ${entity.ymlReprName}"
}

case class YDefault[Obj, Yml](entity: YEntity[Obj, Yml], default: Obj) extends YEntity[Obj, Yml] {
  val objReprName = s"${entity.objReprName} with default value"
  val ymlReprName = s"${entity.ymlReprName} with default value"
}

case class YStringConverted[Obj: ClassTag](toStr: Obj => String, fromStr: String => Obj) extends YEntity[Obj, String] {
  val objReprName = s"string for ${implicitly[ClassTag[Obj]].runtimeClass.getName}"
  val ymlReprName = objReprName
}

case object YString extends YEntity[String, String] {
  val objReprName = "string"
  val ymlReprName = objReprName
}
case object YBoolean extends YEntity[Boolean, java.lang.Boolean] {
  val objReprName = "boolean"
  val ymlReprName = objReprName
}

sealed trait YIntCompatible[Obj] extends YEntity[Obj, Number] {
  def toYml(obj: Obj): Number
  def toObj(yml: Number): Obj

  val objReprName = "int-compatible type"
  val ymlReprName = "int, long or java.math.BigInteger"
}
object YIntCompatible {
  case object YByte extends YIntCompatible[Byte] {
    def toYml(obj: Byte) = obj
    def toObj(yml: Number) = yml.byteValue()
  }

  case object YShort extends YIntCompatible[Short] {
    def toYml(obj: Short) = obj
    def toObj(yml: Number) = yml.shortValue()
  }

  case object YInt extends YIntCompatible[Int] {
    def toYml(obj: Int) = obj
    def toObj(yml: Number) = yml.intValue()
  }

  case object YLong extends YIntCompatible[Long] {
    def toYml(obj: Long) = obj
    def toObj(yml: Number) = yml.longValue()
  }

  case object YBigInt extends YIntCompatible[BigInt] {
    def toYml(obj: BigInt) = obj.bigInteger
    def toObj(yml: Number) = yml match {
      case bigint: java.math.BigInteger => bigint
      case other => BigInt(other.longValue())  // widest possible
    }
  }
}

sealed trait YFloatCompatible[Obj] extends YEntity[Obj, Number] {
  def toYml(obj: Obj): Number
  def toObj(yml: Number): Obj

  val objReprName = "float-compatible type"
  val ymlReprName = "double"
}
object YFloatCompatible {
  case object YFloat extends YFloatCompatible[Float] {
    def toYml(obj: Float) = obj
    def toObj(yml: Number) = yml.floatValue()
  }

  case object YDouble extends YFloatCompatible[Double] {
    def toYml(obj: Double) = obj
    def toObj(yml: Number) = yml.doubleValue()
  }

  case object YBigDecimal extends YFloatCompatible[BigDecimal] {
    def toYml(obj: BigDecimal) = obj.bigDecimal
    def toObj(yml: Number) = yml match {
      case bigdec: java.math.BigDecimal => bigdec
      case other => BigDecimal(other.doubleValue())  // widest possible
    }
  }
}
