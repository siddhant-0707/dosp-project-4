// Simple JSON utilities for the client
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
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

// Simple JSON parsing to extract values from response strings

/// Extract an integer field from a JSON object string
/// Returns None if the field doesn't exist or isn't an integer
pub fn get_int_field(json_str: String, field: String) -> Option(Int) {
  // Look for "field": followed by digits
  let pattern = "\"" <> field <> "\":"
  case string.split(json_str, pattern) {
    [_, rest] -> {
      // Skip whitespace and extract the number
      let trimmed = string.trim_start(rest)
      extract_int(trimmed)
    }
    _ -> None
  }
}

/// Extract an integer from the start of a string
fn extract_int(s: String) -> Option(Int) {
  let chars = string.to_graphemes(s)
  extract_int_chars(chars, "")
}

fn extract_int_chars(chars: List(String), acc: String) -> Option(Int) {
  case chars {
    [] -> {
      case int.parse(acc) {
        Ok(n) -> Some(n)
        Error(_) -> None
      }
    }
    [c, ..rest] -> {
      case c {
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> {
          extract_int_chars(rest, acc <> c)
        }
        "-" if acc == "" -> {
          extract_int_chars(rest, "-")
        }
        _ -> {
          case int.parse(acc) {
            Ok(n) -> Some(n)
            Error(_) -> None
          }
        }
      }
    }
  }
}

/// Extract a string field from a JSON object string
pub fn get_string_field(json_str: String, field: String) -> Option(String) {
  // Look for "field":"value"
  let pattern = "\"" <> field <> "\":\""
  case string.split(json_str, pattern) {
    [_, rest] -> {
      // Find the closing quote (handling escaped quotes)
      extract_string_value(rest, "")
    }
    _ -> None
  }
}

fn extract_string_value(s: String, acc: String) -> Option(String) {
  case string.pop_grapheme(s) {
    Ok(#("\\", rest)) -> {
      // Handle escape sequence
      case string.pop_grapheme(rest) {
        Ok(#(escaped, rest2)) -> {
          let char = case escaped {
            "n" -> "\n"
            "r" -> "\r"
            "t" -> "\t"
            "\"" -> "\""
            "\\" -> "\\"
            _ -> escaped
          }
          extract_string_value(rest2, acc <> char)
        }
        Error(_) -> None
      }
    }
    Ok(#("\"", _)) -> Some(acc)
    Ok(#(c, rest)) -> extract_string_value(rest, acc <> c)
    Error(_) -> None
  }
}
