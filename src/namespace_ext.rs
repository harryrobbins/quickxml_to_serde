use std::collections::HashMap;
use minidom::Element;

/// Extension trait providing helpers to access namespace declarations of an Element
pub trait NamespaceElementExt {
    /// Get all namespace declarations for this element
    fn namespace_declarations(&self) -> HashMap<String, String>;

    /// Get namespace URI for a specific prefix
    fn get_namespace_uri(&self, prefix: &str) -> Option<String>;

    /// Check if element has a specific namespace declaration
    fn has_namespace(&self, prefix: &str) -> bool;
}

impl NamespaceElementExt for Element {
    fn namespace_declarations(&self) -> HashMap<String, String> {
        let mut namespaces = HashMap::new();

        // Add default namespace if present
        if let Some(ns) = self.ns() {
            namespaces.insert("xmlns".to_string(), ns.to_string());
        }

        // Use Debug output temporarily until we can access NamespaceSet directly
        let debug_str = format!("{:?}", self);
        if let Some(ns_content) = debug_str
            .split("NamespaceSet(")
            .nth(1)
            .and_then(|s| s.split(", parent:").next())
        {
            // Parse namespace declarations
            let mut in_quotes = false;
            let mut current = String::new();

            for c in ns_content.chars() {
                match c {
                    '"' => in_quotes = !in_quotes,
                    ',' if !in_quotes => {
                        if !current.is_empty() {
                            if let Some((key, value)) = current.trim().split_once('=') {
                                let clean_value = value.trim_matches('"');
                                namespaces.insert(key.to_string(), clean_value.to_string());
                            }
                            current.clear();
                        }
                    }
                    _ => current.push(c),
                }
            }

            // Handle last namespace
            if !current.is_empty() {
                if let Some((key, value)) = current.trim().split_once('=') {
                    let clean_value = value.trim_matches('"');
                    namespaces.insert(key.to_string(), clean_value.to_string());
                }
            }
        }

        namespaces
    }

    fn get_namespace_uri(&self, prefix: &str) -> Option<String> {
        self.namespace_declarations().get(prefix).cloned()
    }

    fn has_namespace(&self, prefix: &str) -> bool {
        self.namespace_declarations().contains_key(prefix)
    }
}