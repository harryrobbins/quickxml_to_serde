#![allow(clippy::items_after_test_module)]
#![allow(clippy::single_match)]
#![allow(clippy::single_char_pattern)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::ptr_arg)]

extern crate minidom;
extern crate serde_json;

// Add the namespace module and import ONCE at the top with other imports
mod namespace_ext;
use namespace_ext::NamespaceElementExt;

#[cfg(feature = "regex_path")]
extern crate regex;

use minidom::{Element, Error};
use serde_json::{Map, Number, Value};
#[cfg(feature = "json_types")]
use std::collections::HashMap;  // And this line for the non-feature case
use std::str::FromStr;

#[cfg(feature = "regex_path")]
use regex::Regex;

#[cfg(test)]
mod tests;


/// Defines how empty elements like `<x />` should be handled.
/// `Ignore` -> exclude from JSON, `Null` -> `"x":null`, EmptyObject -> `"x":{}`.
/// `EmptyObject` is the default option and is how it was handled prior to v.0.4
/// Using `Ignore` on an XML document with an empty root element falls back to `Null` option.
/// E.g. both `<a><x/></a>` and `<a/>` are converted into `{"a":null}`.
#[derive(Debug, Clone)]
pub enum NullValue {
    Ignore,
    Null,
    EmptyObject,
}



/// Defines how the values of this Node should be converted into a JSON array with the underlying types.
/// * `Infer` - the nodes are converted into a JSON array only if there are multiple identical elements.
/// E.g. `<a><b>1</b></a>` becomes a map `{"a": {"b": 1 }}` and `<a><b>1</b><b>2</b><b>3</b></a>` becomes
/// an array `{"a": {"b": [1, 2, 3] }}`
/// * `Always` - the nodes are converted into a JSON array regardless of how many there are.
/// E.g. `<a><b>1</b></a>` becomes an array with a single value `{"a": {"b": [1] }}` and
/// `<a><b>1</b><b>2</b><b>3</b></a>` also becomes an array `{"a": {"b": [1, 2, 3] }}`
#[derive(Debug, Clone)]
pub enum JsonArray {
    /// Convert the nodes into a JSON array even if there is only one element
    Always(JsonType),
    /// Convert the nodes into a JSON array only if there are multiple identical elements
    Infer(JsonType),
}

/// Used as a parameter for `Config.add_json_type_override`. Defines how the XML path should be matched
/// in order to apply the JSON type overriding rules. This enumerator exists to allow the same function
/// to be used for multiple different types of path matching rules.
#[derive(Debug)]
pub enum PathMatcher {
    /// An absolute path starting with a leading slash (`/`). E.g. `/a/b/c/@d`.
    /// It's implicitly converted from `&str` and automatically includes the leading slash.
    Absolute(String),
    /// A regex that will be checked against the XML path. E.g. `(\w/)*c$`.
    /// It's implicitly converted from `regex::Regex`.
    #[cfg(feature = "regex_path")]
    Regex(Regex),
}

// For retro-compatibility and for syntax's sake, a string may be coerced into an absolute path.
impl From<&str> for PathMatcher {
    fn from(value: &str) -> Self {
        let path_with_leading_slash = if value.starts_with("/") {
            value.into()
        } else {
            ["/", value].concat()
        };

        PathMatcher::Absolute(path_with_leading_slash)
    }
}

// ... While a Regex may be coerced into a regex path.
#[cfg(feature = "regex_path")]
impl From<Regex> for PathMatcher {
    fn from(value: Regex) -> Self {
        PathMatcher::Regex(value)
    }
}

/// Defines which data type to apply in JSON format for consistency of output.
/// E.g., the range of XML values for the same node type may be `1234`, `001234`, `AB1234`.
/// It is impossible to guess with 100% consistency which data type to apply without seeing
/// the entire range of values. Use this enum to tell the converter which data type should
/// be applied.
#[derive(Debug, PartialEq, Clone)]
pub enum JsonType {
    /// Do not try to infer the type and convert the value to JSON string.
    /// E.g. convert `<a>1234</a>` into `{"a":"1234"}` or `<a>true</a>` into `{"a":"true"}`
    AlwaysString,
    /// Convert values included in this member into JSON bool `true` and any other value into `false`.
    /// E.g. `Bool(vec!["True", "true", "TRUE"]) will result in any of these values to become JSON bool `true`.
    Bool(Vec<&'static str>),
    /// Attempt to infer the type by looking at the single value of the node being converted.
    /// Not guaranteed to be consistent across multiple nodes.
    /// E.g. convert `<a>1234</a>` and `<a>001234</a>` into `{"a":1234}`, or `<a>true</a>` into `{"a":true}`
    /// Check if your values comply with JSON data types (case, range, format) to produce the expected result.
    Infer,
}

/// Tells the converter how to perform certain conversions.
/// See docs for individual fields for more info.
#[derive(Debug, Clone)]
pub struct Config {
    pub leading_zero_as_string: bool,
    pub xml_attr_prefix: String,
    pub xml_text_node_prop_name: String,
    pub empty_element_handling: NullValue,
    #[cfg(feature = "json_types")]
    pub json_type_overrides: HashMap<String, JsonArray>,
    #[cfg(feature = "regex_path")]
    pub json_regex_type_overrides: Vec<(Regex, JsonArray)>,
    pub ignored_namespace_uris: Vec<String>,  // Changed from ignored_namespaces
}


impl Config {
    pub fn new_with_defaults() -> Self {
        Config {
            leading_zero_as_string: false,
            xml_attr_prefix: "@".to_owned(),
            xml_text_node_prop_name: "#text".to_owned(),
            empty_element_handling: NullValue::EmptyObject,
            #[cfg(feature = "json_types")]
            json_type_overrides: HashMap::new(),
            #[cfg(feature = "regex_path")]
            json_regex_type_overrides: Vec::new(),
            ignored_namespace_uris: Vec::new(),  // Changed from ignored_namespaces
        }
    }

    pub fn new_with_custom_values(
        leading_zero_as_string: bool,
        xml_attr_prefix: &str,
        xml_text_node_prop_name: &str,
        empty_element_handling: NullValue,
        ignored_namespace_uris: Vec<String>,  // Changed parameter name
    ) -> Self {
        Config {
            leading_zero_as_string,
            xml_attr_prefix: xml_attr_prefix.to_owned(),
            xml_text_node_prop_name: xml_text_node_prop_name.to_owned(),
            empty_element_handling,
            #[cfg(feature = "json_types")]
            json_type_overrides: HashMap::new(),
            #[cfg(feature = "regex_path")]
            json_regex_type_overrides: Vec::new(),
            ignored_namespace_uris,  // Changed field name
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Config::new_with_defaults()
    }
}

/// Returns the text as one of `serde::Value` types: int, float, bool or string.
fn parse_text(text: &str, leading_zero_as_string: bool, json_type: &JsonType) -> Value {
    let text = text.trim();

    // enforce JSON String data type regardless of the underlying type
    if json_type == &JsonType::AlwaysString {
        return Value::String(text.into());
    }

    // enforce JSON Bool data type
    #[cfg(feature = "json_types")]
    if let JsonType::Bool(true_values) = json_type {
        if true_values.contains(&text) {
            // any values matching the `true` list are bool/true
            return Value::Bool(true);
        } else {
            // anything else is false
            return Value::Bool(false);
        }
    }

    // ints
    if let Ok(v) = text.parse::<u64>() {
        // don't parse octal numbers and those with leading 0
        // `text` value "0" will always be converted into number 0, "0000" may be converted
        // into 0 or "0000" depending on `leading_zero_as_string`
        if leading_zero_as_string && text.starts_with("0") && (v != 0 || text.len() > 1) {
            return Value::String(text.into());
        }
        return Value::Number(Number::from(v));
    }

    // floats
    if let Ok(v) = text.parse::<f64>() {
        if text.starts_with("0") && !text.starts_with("0.") {
            return Value::String(text.into());
        }
        if let Some(val) = Number::from_f64(v) {
            return Value::Number(val);
        }
    }

    // booleans
    if let Ok(v) = text.parse::<bool>() {
        return Value::Bool(v);
    }

    Value::String(text.into())
}

/// Converts an XML Element into a JSON property

// Update the convert_node function
/// Converts an XML Element into a JSON property
fn convert_node(el: &Element, config: &Config, path: &String) -> Option<Value> {
    let mut should_include_node = true;
    let mut current_node_data = Map::new();
    let mut children_array = Vec::new();

    // Check if this element's namespace should be ignored
    if let Some(ns) = el.ns() {
        should_include_node = !config.ignored_namespace_uris.contains(&ns.to_string());
    }

    // If this node should be included, process its attributes and text
    if should_include_node {
        // Add element name
        current_node_data.insert("_element".to_string(), Value::String(el.name().to_string()));

        // Add namespace URI as attribute if present
        if let Some(ns) = el.ns() {
            current_node_data.insert(
                format!("{}xmlns", config.xml_attr_prefix),
                Value::String(ns.to_string()),
            );
        }

        // Process attributes
        for (k, v) in el.attrs() {
            if !k.starts_with("xmlns") {
                current_node_data.insert(
                    format!("{}{}", config.xml_attr_prefix, k),
                    parse_text(v, config.leading_zero_as_string, &JsonType::Infer),
                );
            }
        }

        // Handle text content
        let element_text = el.text();
        let text = element_text.trim();
        if !text.is_empty() {
            current_node_data.insert(
                config.xml_text_node_prop_name.clone(),
                parse_text(text, config.leading_zero_as_string, &JsonType::Infer),
            );
        }
    }

    // Process children
    for child in el.children() {
        if let Some(child_value) = convert_node(child, config, path) {
            match child_value {
                Value::Array(arr) => {
                    // Flatten any nested arrays from ignored nodes
                    children_array.extend(arr);
                },
                _ => children_array.push(child_value),
            }
        }
    }

    if should_include_node {
        // If we have any children, add them to the _children array
        if !children_array.is_empty() {
            current_node_data.insert("_children".to_string(), Value::Array(children_array));
        }

        // If we have any data (attributes, text, or children), return this node
        if !current_node_data.is_empty() {
            Some(Value::Object(current_node_data))
        } else {
            match config.empty_element_handling {
                NullValue::Null => Some(Value::Null),
                NullValue::EmptyObject => {
                    current_node_data.insert("_element".to_string(), Value::String(el.name().to_string()));
                    Some(Value::Object(current_node_data))
                },
                NullValue::Ignore => None,
            }
        }
    } else if !children_array.is_empty() {
        // If this node is ignored but has children, return array of children directly
        Some(Value::Array(children_array))
    } else {
        None
    }
}

fn xml_to_map(e: &Element, config: &Config) -> Value {
    let mut root_data = Map::new();
    use std::collections::HashMap;
    // Get namespaces that aren't ignored
    let namespaces: HashMap<String, String> = e
        .namespace_declarations()
        .into_iter()
        .filter(|(_, uri)| !config.ignored_namespace_uris.contains(uri))
        .collect();

    let namespace_count = namespaces.len();

    // Add namespace information if we have any
    if !namespaces.is_empty() {
        root_data.insert(
            "namespaces".to_string(),
            Value::Object(namespaces.into_iter().map(|(k, v)| (k, Value::String(v))).collect())
        );
        root_data.insert(
            "namespace_count".to_string(),
            Value::Number(Number::from(namespace_count as u64))
        );
    }

    // Convert the content
    if let Some(content) = convert_node(e, config, &String::new()) {
        match content {
            Value::Array(arr) => {
                // If we got an array back (because root was ignored), add it to document
                root_data.insert("document".to_string(), Value::Array(arr));
            },
            Value::Object(obj) => {
                if obj.contains_key("_element") {
                    // If we got a single element back, wrap it in an array
                    root_data.insert("document".to_string(), Value::Array(vec![Value::Object(obj)]));
                } else {
                    // Handle legacy case (shouldn't happen with new structure)
                    for (k, v) in obj {
                        root_data.insert(k, v);
                    }
                }
            },
            _ => {
                // Handle other cases (like null) by wrapping in document
                root_data.insert("document".to_string(), content);
            }
        }
    }

    // If we have no content but have namespaces, still return valid output
    if root_data.is_empty() {
        Value::Null
    } else {
        Value::Object(root_data)
    }
}

/// Converts the given XML string into `serde::Value` using settings from `Config` struct.
pub fn xml_str_to_json(xml: &str, config: &Config) -> Result<Value, Error> {
    let root = Element::from_str(xml)?;
    Ok(xml_to_map(&root, config))
}

/// Converts the given XML string into `serde::Value` using settings from `Config` struct.
pub fn xml_string_to_json(xml: String, config: &Config) -> Result<Value, Error> {
    xml_str_to_json(xml.as_str(), config)
}

/// Returns a tuple for Array and Value enforcements for the current node or
/// `(false, JsonArray::Infer(JsonType::Infer)` if the current path is not found
/// in the list of paths with custom config.
#[cfg(feature = "json_types")]
#[inline]
fn get_json_type_with_absolute_path<'conf>(config: &'conf Config, path: &String) -> (bool, &'conf JsonType) {
    match config
        .json_type_overrides
        .get(path)
        .unwrap_or(&JsonArray::Infer(JsonType::Infer))
    {
        JsonArray::Infer(v) => (false, v),
        JsonArray::Always(v) => (true, v),
    }
}

/// Simply returns `get_json_type_with_absolute_path` if `regex_path` feature is disabled.
#[cfg(feature = "json_types")]
#[cfg(not(feature = "regex_path"))]
#[inline]
fn get_json_type<'conf>(config: &'conf Config, path: &String) -> (bool, &'conf JsonType) {
    get_json_type_with_absolute_path(config, path)
}

/// Returns a tuple for Array and Value enforcements for the current node. Searches both absolute paths
/// and regex paths, giving precedence to regex paths. Returns `(false, JsonArray::Infer(JsonType::Infer)`
/// if the current path is not found in the list of paths with custom config.
#[cfg(feature = "json_types")]
#[cfg(feature = "regex_path")]
#[inline]
fn get_json_type<'conf>(config: &'conf Config, path: &String) -> (bool, &'conf JsonType) {
    for (regex, json_array) in &config.json_regex_type_overrides {
        if regex.is_match(path) {
            return match json_array {
                JsonArray::Infer(v) => (false, v),
                JsonArray::Always(v) => (true, v),
            };
        }
    }

    get_json_type_with_absolute_path(config, path)
}

/// Always returns `(false, JsonArray::Infer(JsonType::Infer)` if `json_types` feature is not enabled.
#[cfg(not(feature = "json_types"))]
#[inline]
fn get_json_type<'conf>(_config: &'conf Config, _path: &String) -> (bool, &'conf JsonType) {
    (false, &JsonType::Infer)
}