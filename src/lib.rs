#![allow(clippy::items_after_test_module)]
#![allow(clippy::single_match)]
#![allow(clippy::single_char_pattern)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::ptr_arg)]

//! # quickxml_to_serde
//! ... (your existing documentation) ...

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
use std::collections::HashMap;
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
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
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
    pub excluded_namespaces: Vec<String>
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
            excluded_namespaces: Vec::new(),
        }
    }

    /// Create a Config object with non-default values. See the `Config` struct docs for more info.
    pub fn new_with_custom_values(
        leading_zero_as_string: bool,
        xml_attr_prefix: &str,
        xml_text_node_prop_name: &str,
        empty_element_handling: NullValue,
        excluded_namespaces: Vec<String>,
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
            excluded_namespaces,
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
fn convert_node(el: &Element, config: &Config, path: &String) -> Option<Value> {
    // Check if element's namespace should be excluded
    if let Some(ns) = el.ns() {
        if config.excluded_namespaces.contains(&ns.to_string()) {
            return None;
        }
    }

    // add the current node to the path
    #[cfg(feature = "json_types")]
    let path = [path, "/", el.name()].concat();

    // get the json_type for this node
    let (_, json_type_value) = get_json_type(config, &path);

    let mut data = Map::new();

    // Add namespace URI as attribute if present
    if let Some(ns) = el.ns() {
        data.insert(
            format!("{}xmlns", config.xml_attr_prefix),
            Value::String(ns.to_string()),
        );
    }

    // Process attributes first, including namespaces
    for (k, v) in el.attrs() {
        // Skip xmlns attributes as we handle them separately now
        if k.starts_with("xmlns") {
            continue;
        }

        // add the current node to the path
        #[cfg(feature = "json_types")]
        let attr_path = [path.clone(), "/@".to_owned(), k.to_owned()].concat();

        // get the json_type for this node
        #[cfg(feature = "json_types")]
        let (_, json_type_value) = get_json_type(config, &attr_path);

        data.insert(
            [config.xml_attr_prefix.clone(), k.to_owned()].concat(),
            parse_text(&v, config.leading_zero_as_string, &json_type_value),
        );
    }


    // is it an element with text?
    if el.text().trim() != "" {
        if !data.is_empty() {
            // We already have attributes, so add text as a property
            data.insert(
                config.xml_text_node_prop_name.clone(),
                parse_text(
                    &el.text()[..],
                    config.leading_zero_as_string,
                    &json_type_value,
                ),
            );
            Some(Value::Object(data))
        } else {
            // No attributes, just return the text value
            Some(parse_text(
                &el.text()[..],
                config.leading_zero_as_string,
                &json_type_value,
            ))
        }
    } else {
        // Process child elements recursively
        for child in el.children() {
            match convert_node(child, config, &path) {
                Some(val) => {
                    let name = &child.name().to_string();

                    #[cfg(feature = "json_types")]
                    let path = [path.clone(), "/".to_owned(), name.clone()].concat();
                    let (json_type_array, _) = get_json_type(config, &path);

                    // does it have to be an array?
                    if json_type_array || data.contains_key(name) {
                        // was this property converted to an array earlier?
                        if data.get(name).unwrap_or(&Value::Null).is_array() {
                            // add the new value to an existing array
                            data.get_mut(name)
                                .unwrap()
                                .as_array_mut()
                                .unwrap()
                                .push(val);
                        } else {
                            // convert the property to an array with the existing and the new values
                            let new_val = match data.remove(name) {
                                None => vec![val],
                                Some(temp) => vec![temp, val],
                            };
                            data.insert(name.clone(), Value::Array(new_val));
                        }
                    } else {
                        // this is the first time this property is encountered
                        data.insert(name.clone(), val);
                    }
                }
                _ => (),
            }
        }

        // return the JSON object if it's not empty
        if !data.is_empty() {
            return Some(Value::Object(data));
        }

        // empty objects are treated according to config rules
        match config.empty_element_handling {
            NullValue::Null => Some(Value::Null),
            NullValue::EmptyObject => Some(Value::Object(data)),
            NullValue::Ignore => None,
        }
    }
}

fn xml_to_map(e: &Element, config: &Config) -> Value {
    let mut data = Map::new();
    let root_name = e.name().to_string();

    // Check root namespace exclusion
    if let Some(ns) = e.ns() {
        if config.excluded_namespaces.contains(&ns.to_string()) {
            return Value::Null;
        }
    }

    // Get namespaces using our extension trait
    let namespaces: HashMap<String, String> = e
        .namespace_declarations()
        .into_iter()
        .filter(|(_, uri)| !config.excluded_namespaces.contains(uri))
        .collect();

    let namespace_count = namespaces.len();

    // Create regular root content map for attributes and children
    let mut root_content = Map::new();

    // Add root namespace if present and not excluded
    if let Some(ns) = e.ns() {
        if !config.excluded_namespaces.contains(&ns.to_string()) {
            root_content.insert(
                format!("{}xmlns", config.xml_attr_prefix),
                Value::String(ns.to_string()),
            );
        }
    }
    // Create regular root content map for attributes and children
    let mut root_content = Map::new();

    // Process regular attributes
    for (k, v) in e.attrs() {
        root_content.insert(
            format!("{}{}", config.xml_attr_prefix, k),
            parse_text(&v, config.leading_zero_as_string, &JsonType::Infer),
        );
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
        let namespace_map: Map<String, Value> = namespaces
            .into_iter()
            .map(|(k, v)| (k, Value::String(v)))
            .collect();

        data.insert("namespaces".to_string(), Value::Object(namespace_map));
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