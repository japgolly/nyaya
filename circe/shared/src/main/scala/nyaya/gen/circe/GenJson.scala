package nyaya.gen.circe

import io.circe.{Json, JsonObject}
import japgolly.microlibs.recursion._
import nyaya.gen.GenAbstractJson._
import nyaya.gen._

object GenJson extends GenAbstractJson.Dsl[Json, JsonObject] {

  override val algebra: FAlgebra[JsonF, Json] = {
    case JsonF.Null         => Json.Null
    case JsonF.True         => Json.True
    case JsonF.False        => Json.False
    case j: JsonF.Str       => Json.fromString(j.value)
    case j: JsonF.NumLong   => Json.fromLong(j.value)
    case j: JsonF.NumDouble => Json.fromDoubleOrNull(j.value)
    case JsonF.Arr(values)  => Json.arr(values: _*)
    case JsonF.Obj(fields)  => Json.obj(fields: _*)
  }

  override val JsonObject =
    io.circe.JsonObject.fromIterable(_)
}
