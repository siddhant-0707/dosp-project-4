// Simple JSON utilities for the client
import gleam/int
import gleam/list
import gleam/string

pub type Json {
  String(String)
  Int(Int)
  Bool(Bool)
  Null
  Object(List(#(String, Json)))
  Array(List(Json))
}

pub fn to_string(json: Json) -> String {
  case json {
    String(s) -> "\"" <> escape_string(s) <> "\""
    Int(i) -> int.to_string(i)
    Bool(True) -> "true"
    Bool(False) -> "false"
    Null -> "null"
    Object(props) -> {
      let body =
        props
        |> list.map(fn(prop) {
          let #(k, v) = prop
          "\"" <> escape_string(k) <> "\":" <> to_string(v)
        })
        |> string.join(",")
      "{" <> body <> "}"
    }
    Array(items) -> {
      let body = items |> list.map(to_string) |> string.join(",")
      "[" <> body <> "]"
    }
  }
}

fn escape_string(s: String) -> String {
  s
  |> string.replace("\\", "\\\\")
  |> string.replace("\"", "\\\"")
  |> string.replace("\n", "\\n")
  |> string.replace("\r", "\\r")
  |> string.replace("\t", "\\t")
}

pub fn string(s: String) -> Json {
  String(s)
}

pub fn int(i: Int) -> Json {
  Int(i)
}

pub fn bool(b: Bool) -> Json {
  Bool(b)
}

pub fn null() -> Json {
  Null
}

pub fn object(props: List(#(String, Json))) -> Json {
  Object(props)
}

pub fn array(items: List(a), of converter: fn(a) -> Json) -> Json {
  Array(list.map(items, converter))
}
