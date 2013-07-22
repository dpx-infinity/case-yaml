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

package cc.cu.tplex.caseyaml.model.generators

import scala.language.experimental.macros
import cc.cu.tplex.caseyaml.model._
import scala.reflect.macros.Context
import cc.cu.tplex.caseyaml.model.YClassMap
import cc.cu.tplex.caseyaml.CaseYamlException
import scala.Some

/**
 * Date: 21.07.13
 * Time: 12:43
 *
 * @author Vladimir Matveev
 */
class MacroEntityTreeGenerator {
  import MacroEntityTreeGenerator._

  def generate[Obj, Yml](convs: ConvPair*): YEntity[Obj, Yml] =
    macro MacroEntityTreeGenerator.generateImpl[Obj, Yml]

  def generateClass[Obj](convs: ConvPair*): YClassMap[Obj] =
    macro MacroEntityTreeGenerator.generateClassImpl[Obj]
}

object MacroEntityTreeGenerator {
  type ConvPair = (T => String, String => T) forSome { type T }

  def generateClassImpl[Obj: c.TypeTag](c: Context)(convs: c.Expr[ConvPair]*): c.Expr[YClassMap[Obj]] = {
    generateImpl(c)(convs: _*).asInstanceOf[c.Expr[YClassMap[Obj]]]
  }

  def generateImpl[Obj: c.TypeTag, Yml](c: Context)(convs: c.Expr[ConvPair]*): c.Expr[YEntity[Obj, Yml]] = {
    import c.universe._

    implicit class TypeLookable[T](val seq: Iterable[(Type, T)]) extends AnyVal {
      def get(k: Type): Option[T] = seq.find(_._1 =:= k).map(_._2)
    }

    val convPairs: Seq[(c.Expr[T => String], c.Expr[String => T]) forSome { type T }] = {
      val any2ArrowAssocName = c.universe.newTermName("any2ArrowAssoc")
      val minusGreaterName = c.universe.newTermName("$minus$greater")
      val applyName = c.universe.newTermName("apply")
      convs map {
        case Apply(Select(Apply(Select(Ident(scala.Predef), `any2ArrowAssocName`), List(arg1)), `minusGreaterName`), List(arg2)) =>
          (arg1, arg2).asInstanceOf[(c.Expr[T => String], c.Expr[String => T]) forSome { type T }]
        case Apply(Select(Ident(scala.Tuple2), `applyName`), List(arg1, arg2)) =>
          (arg1, arg2).asInstanceOf[(c.Expr[T => String], c.Expr[String => T]) forSome { type T }]
        case _ =>
          c.abort(
            c.enclosingPosition,
            "Invalid argument specified, expected either 'toStr -> fromStr' or (toStr, fromStr)"
          )
      }
    }

    def firstTypeParam(tpe: Type) =
      tpe.asInstanceOf[TypeRefApi].args.head

    def secondTypeParam(tpe: Type) =
      tpe.asInstanceOf[TypeRefApi].args.tail.head

    def generateString(tpe: Type): Option[Expr[YEntity[_, _]]] =
      if (tpe =:= typeOf[String])
        Some(reify(YString))
      else
        None

    def generateBoolean(tpe: Type): Option[Expr[YEntity[_, _]]] =
      if (tpe =:= typeOf[Boolean])
        Some(reify(YBoolean))
      else
        None

    val intCompatibles = Seq[(Type, Expr[YEntity[_, _]])](
      typeOf[Byte]   -> reify(YIntCompatible.YByte),
      typeOf[Short]  -> reify(YIntCompatible.YShort),
      typeOf[Int]    -> reify(YIntCompatible.YInt),
      typeOf[Long]   -> reify(YIntCompatible.YLong),
      typeOf[BigInt] -> reify(YIntCompatible.YBigInt)
    )
    def generateIntCompatible(tpe: Type): Option[Expr[YEntity[_, _]]] =
      intCompatibles get tpe

    val floatCompatibles = Seq[(Type, Expr[YEntity[_, _]])](
      typeOf[Float]      -> reify(YFloatCompatible.YFloat),
      typeOf[Double]     -> reify(YFloatCompatible.YDouble),
      typeOf[BigDecimal] -> reify(YFloatCompatible.YBigDecimal)
    )
    def generateFloatCompatible(tpe: Type): Option[Expr[YEntity[_, _]]] =
      floatCompatibles get tpe

    def generateOptional(tpe: Type): Option[Expr[YEntity[_, _]]] =
      if (tpe <:< typeOf[Option[_]])
        Some(reify(YOptional(generate(firstTypeParam(tpe)).splice)))
      else
        None

    def generateList(tpe: Type): Option[Expr[YEntity[_, _]]] =
      if (tpe <:< typeOf[Seq[_]])
        Some(reify(YList(generate(firstTypeParam(tpe.baseType(typeOf[Seq[_]].typeSymbol))).splice)))
      else
        None

    def generateSet(tpe: Type): Option[Expr[YEntity[_, _]]] =
      if (tpe <:< typeOf[Set[_]])
        Some(reify(YSet(generate(firstTypeParam(tpe.baseType(typeOf[Set[_]].typeSymbol))).splice)))
      else
        None

    def generateMap(tpe: Type): Option[Expr[YEntity[_, _]]] =
      if (tpe <:< typeOf[Map[_, _]])
        Some(reify(YMap(generate(secondTypeParam(tpe.baseType(typeOf[Map[_, _]].typeSymbol))).splice)))
      else
        None

    val generator =
      generateIntCompatible _ +>
      generateFloatCompatible +>
      generateString +>
      generateBoolean +>
      generateOptional +>
      generateList +>
      generateSet +>
      generateMap

    def generate(tpe: Type): Expr[YEntity[_, _]] = generator(tpe) match {
      case Some(expr) => expr
      case None => throw CaseYamlException("Reached the end of type generators chain for type " + tpe)
    }

    generate(typeOf[Obj]).asInstanceOf[Expr[YEntity[Obj, Yml]]]
  }
}
