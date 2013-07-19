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

package cc.cu.tplex.caseyaml

import cc.cu.tplex.caseyaml.model.YEntity
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.SafeConstructor
import java.io._
import cc.cu.tplex.caseyaml.yml.{ConvertToObj, ConvertToYml}
import java.nio.charset.Charset
import cc.cu.tplex.caseyaml.Converter.{Deserializer, Serializer}
import org.yaml.snakeyaml.error.YAMLException

/**
 * Date: 13.07.13
 * Time: 21:36
 *
 * @author Vladimir Matveev
 */
class Converter[Obj, Yml] private[caseyaml] (entity: YEntity[Obj, Yml], yaml: Yaml = new Yaml(new SafeConstructor)) {
  def withYaml(yaml: Yaml) = new Converter(entity, yaml)

  def wrap[T](e: => T) = try {
    e
  } catch {
    case e: YAMLException => throw CaseYamlException("YAML [de]serialization error", e)
  }

  def serialize(obj: Obj) = new Serializer {
    def toStr = wrap(yaml.dump(obj))

    def to(writer: Writer) {
      wrap(yaml.dump(ConvertToYml(entity, obj), writer))
    }
  }

  private abstract class BaseDeserializer extends Deserializer[Obj] {
    def from(reader: Reader) = extractMap(wrap(yaml.load(reader)))

    def from(string: String) = extractMap(wrap(yaml.load(string)))

    protected def extractMap(obj: Any): Obj
  }

  def deserialize: Deserializer[Obj] = new BaseDeserializer {
    protected def extractMap(obj: Any) = ConvertToObj(entity, obj.asInstanceOf[Yml])
  }

  def deserialize(subkey: String): Deserializer[Obj] = new BaseDeserializer {
    protected def extractMap(obj: Any) = ConvertToObj(entity, obj.asInstanceOf[java.util.Map[String, Any]], subkey)
  }
}

object Converter {
  trait Deserializer[Obj] {
    def from(reader: Reader): Obj
    def from(string: String): Obj

    def fromStream(stream: InputStream, charset: Charset = Charset.forName("UTF-8")): Obj =
      from(new InputStreamReader(stream, charset))

    def fromFile(file: File, charset: Charset = Charset.forName("UTF-8")): Obj =
      fromStream(new FileInputStream(file), charset)

    def fromFilePath(path: String, charset: Charset = Charset.forName("UTF-8")): Obj =
      fromFile(new File(path), charset)

    def fromBytes(array: Array[Byte], charset: Charset = Charset.forName("UTF-8")): Obj =
      fromStream(new ByteArrayInputStream(array), charset)
  }

  trait Serializer {
    def toStr: String
    def to(writer: Writer)

    def toStream(stream: OutputStream, charset: Charset = Charset.forName("UTF-8")) {
      to(new OutputStreamWriter(stream ,charset))
    }

    def toFile(file: File, charset: Charset = Charset.forName("UTF-8")) {
      toStream(new FileOutputStream(file), charset)
    }

    def toFilePath(path: String, charset: Charset = Charset.forName("UTF-8")) {
      toFile(new File(path), charset)
    }

    def toBytes(charset: Charset = Charset.forName("UTF-8")): Array[Byte] = {
      val stream = new ByteArrayOutputStream()
      toStream(stream, charset)
      stream.toByteArray
    }
  }
}
