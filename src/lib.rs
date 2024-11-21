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
    /// Numeric values starting with 0 will be treated as strings.
    /// E.g. convert `<agent>007</agent>` into `"agent":"007"` or `"agent":7`
    /// Defaults to `false`.
    pub leading_zero_as_string: bool,
    /// Prefix XML attribute names with this value to distinguish them from XML elements.
    /// E.g. set it to `@` for `<x a="Hello!" />` to become `{"x": {"@a":"Hello!"}}`
    /// or set it to a blank string for `{"x": {"a":"Hello!"}}`
    /// Defaults to `@`.
    pub xml_attr_prefix: String,
    /// A property name for XML text nodes.
    /// E.g. set it to `text` for `<x a="Hello!">Goodbye!</x>` to become `{"x": {"@a":"Hello!", "text":"Goodbye!"}}`
    /// XML nodes with text only and no attributes or no child elements are converted into JSON properties with the
    /// name of the element. E.g. `<x>Goodbye!</x>` becomes `{"x":"Goodbye!"}`
    /// Defaults to `#text`
    pub xml_text_node_prop_name: String,
    /// Defines how empty elements like `<x />` should be handled.
    pub empty_element_handling: NullValue,
    /// A map of XML paths with their JsonArray overrides. They take precedence over the document-wide `json_type`
    /// property. The path syntax is based on xPath: literal element names and attribute names prefixed with `@`.
    /// The path must start with a leading `/`. It is a bit of an inconvenience to remember about it, but it saves
    /// an extra `if`-check in the code to improve the performance.
    /// # Example
    /// - **XML**: `<a><b c="123">007</b></a>`
    /// - path for `c`: `/a/b/@c`
    /// - path for `b` text node (007): `/a/b`
    #[cfg(feature = "json_types")]
    pub json_type_overrides: HashMap<String, JsonArray>,
    /// A list of pairs of regex and JsonArray overrides. They take precedence over both the document-wide `json_type`
    /// property and the `json_type_overrides` property. The path syntax is based on xPath just like `json_type_overrides`.
    #[cfg(feature = "regex_path")]
    pub json_regex_type_overrides: Vec<(Regex, JsonArray)>,

    /// Custom functionality to filter namespaces
    pub ignored_namespaces: Vec<String>,
}

impl Config {
    /// Numbers with leading zero will be treated as numbers.
    /// Prefix XML Attribute names with `@`
    /// Name XML text nodes `#text` for XML Elements with other children
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
            ignored_namespaces: Vec::new(),
        }
    }

    /// Create a Config object with non-default values. See the `Config` struct docs for more info.
   pub fn new_with_custom_values(
    leading_zero_as_string: bool,
    xml_attr_prefix: &str,
    xml_text_node_prop_name: &str,
    empty_element_handling: NullValue,
    ignored_namespaces: Vec<String>,
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
        ignored_namespaces, // Use the parameter here
    }
}


    /// Adds a single JSON Type override rule to the current config.
    /// # Example
    /// - **XML**: `<a><b c="123">007</b></a>`
    /// - path for `c`: `/a/b/@c`
    /// - path for `b` text node (007): `/a/b`
    /// - regex path for any `element` node: `(\w/)*element$` [requires `regex_path` feature]
    #[cfg(feature = "json_types")]
    pub fn add_json_type_override<P>(self, path: P, json_type: JsonArray) -> Self
    where
        P: Into<PathMatcher>
    {
        let mut conf = self;

        match path.into() {
            PathMatcher::Absolute(path) => {
                conf.json_type_overrides.insert(path, json_type);
            }
            #[cfg(feature = "regex_path")]
            PathMatcher::Regex(regex) => {
                conf.json_regex_type_overrides.push((
                    regex,
                    json_type
                ));
            }
        }

        conf
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
    // Check if this element's namespace should be ignored
    if let Some(prefix) = el.prefix() {
        if config.ignored_namespaces.contains(&prefix.to_string()) {
            let mut combined_content = Map::new();

            // Process attributes first (even for ignored namespaces)
            for (k, v) in el.attrs() {
                if !k.starts_with("xmlns") {
                    combined_content.insert(
                        format!("{}{}", config.xml_attr_prefix, k),
                        parse_text(&v, config.leading_zero_as_string, &JsonType::Infer),
                    );
                }
            }

            // Process child elements recursively
            for child in el.children() {
                if let Some(child_value) = convert_node(child, config, path) {
                    let child_name = child.name().to_string();

                    match combined_content.get_mut(&child_name) {
                        Some(existing) => {
                            // Convert to array if not already
                            if !existing.is_array() {
                                let temp = existing.clone();
                                *existing = Value::Array(vec![temp]);
                            }

                            // Add new value to array
                            if let Some(arr) = existing.as_array_mut() {
                                arr.push(child_value);
                            }
                        },
                        None => {
                            combined_content.insert(child_name, child_value);
                        }
                    }
                }
            }

            // Handle text content if present
            let element_text = el.text();
            let text = element_text.trim();
            if !text.is_empty() {
                combined_content.insert(
                    config.xml_text_node_prop_name.clone(),
                    parse_text(text, config.leading_zero_as_string, &JsonType::Infer),
                );
            }

            return if !combined_content.is_empty() {
                Some(Value::Object(combined_content))
            } else {
                match config.empty_element_handling {
                    NullValue::Null => Some(Value::Null),
                    NullValue::EmptyObject => Some(Value::Object(Map::new())),
                    NullValue::Ignore => None,
                }
            };
        }
    }

    // Build path for this node
    #[cfg(feature = "json_types")]
    let current_path = format!("{}/{}", path, el.name());
    #[cfg(not(feature = "json_types"))]
    let current_path = String::new();

    // Get JSON type for this node
    let (is_array, json_type_value) = get_json_type(config, &current_path);

    let mut data = Map::new();

    // Add namespace URI as attribute if present
    if let Some(ns) = el.ns() {
        data.insert(
            format!("{}xmlns", config.xml_attr_prefix),
            Value::String(ns.to_string()),
        );
    }

    // Process attributes
    for (k, v) in el.attrs() {
        if !k.starts_with("xmlns") {
            #[cfg(feature = "json_types")]
            let attr_path = format!("{}/@{}", current_path, k);
            #[cfg(not(feature = "json_types"))]
            let attr_path = String::new();

            let (_, attr_json_type) = get_json_type(config, &attr_path);

            data.insert(
                format!("{}{}", config.xml_attr_prefix, k),
                parse_text(v, config.leading_zero_as_string, attr_json_type),
            );
        }
    }

    // Handle text content
    let element_text = el.text();
    let text = element_text.trim();
    if !text.is_empty() {
        if !data.is_empty() {
            data.insert(
                config.xml_text_node_prop_name.clone(),
                parse_text(text, config.leading_zero_as_string, json_type_value),
            );
            Some(Value::Object(data))
        } else {
            Some(parse_text(text, config.leading_zero_as_string, json_type_value))
        }
    } else {
        // Process child elements
        for child in el.children() {
            if let Some(child_value) = convert_node(child, config, &current_path) {
                let child_name = child.name().to_string();

                #[cfg(feature = "json_types")]
                let child_path = format!("{}/{}", current_path, child_name);
                #[cfg(not(feature = "json_types"))]
                let child_path = String::new();

                let (child_is_array, _) = get_json_type(config, &child_path);

                if child_is_array || is_array || data.contains_key(&child_name) {
                    match data.get_mut(&child_name) {
                        Some(existing) if existing.is_array() => {
                            existing.as_array_mut().unwrap().push(child_value);
                        },
                        Some(existing) => {
                            let temp = existing.clone();
                            data.insert(child_name, Value::Array(vec![temp, child_value]));
                        },
                        None => {
                            data.insert(child_name, Value::Array(vec![child_value]));
                        }
                    }
                } else {
                    data.insert(child_name, child_value);
                }
            }
        }

        if !data.is_empty() {
            Some(Value::Object(data))
        } else {
            match config.empty_element_handling {
                NullValue::Null => Some(Value::Null),
                NullValue::EmptyObject => Some(Value::Object(data)),
                NullValue::Ignore => None,
            }
        }
    }
}

// Update the xml_to_map function
fn xml_to_map(e: &Element, config: &Config) -> Value {
    let mut data = Map::new();
    let root_name = e.name().to_string();
    use std::collections::HashMap;


    // Check root namespace exclusion
    if let Some(ns) = e.ns() {
        if config.ignored_namespaces.contains(&ns.to_string()) {
            return Value::Null;
        }
    }

    // Get namespaces using our extension trait
    let namespaces: HashMap<String, String> = e
        .namespace_declarations()
        .into_iter()
        .filter(|(_, uri)| !config.ignored_namespaces.contains(uri))
        .collect();

    let namespace_count = namespaces.len();

    // Create regular root content map for attributes and children
    let mut root_content = Map::new();

    // Add root namespace if present and not excluded
    if let Some(ns) = e.ns() {
        if !config.ignored_namespaces.contains(&ns.to_string()) {
            root_content.insert(
                format!("{}xmlns", config.xml_attr_prefix),
                Value::String(ns.to_string()),
            );
        }
    }

    // Process regular attributes
    for (k, v) in e.attrs() {
        if !k.starts_with("xmlns") {
            root_content.insert(
                format!("{}{}", config.xml_attr_prefix, k),
                parse_text(&v, config.leading_zero_as_string, &JsonType::Infer),
            );
        }
    }

    // Process child content
    if let Some(child_content) = convert_node(e, config, &String::new()) {
        match child_content {
            Value::Object(map) => {
                for (k, v) in map {
                    root_content.insert(k, v);
                }
            }
            _ => {
                root_content.insert(config.xml_text_node_prop_name.clone(), child_content);
            }
        }
    }

    // Add namespace information first (if we have any)
    if !namespaces.is_empty() {
        data.insert("namespaces".to_string(), Value::Object(namespaces.into_iter().map(|(k, v)| (k, Value::String(v))).collect()));
        data.insert(
            "namespace_count".to_string(),
            Value::Number(Number::from(namespace_count as u64))
        );
    }

    // Add the root element content
    data.insert(root_name, Value::Object(root_content));

    Value::Object(data)
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