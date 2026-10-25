// MultiGen Rust Runtime Library
// Provides Python-like operations using only Rust standard library

use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;
use std::any::Any;
use std::borrow::Borrow;

/// Map for Python dicts: iterates in insertion order, which `HashMap` does not.
/// Overwriting a key keeps its position; `pop` then `insert` moves it to the end.
#[derive(Clone)]
pub struct OrderedHashMap<K, V> {
    entries: Vec<(K, V)>,
    index: HashMap<K, usize>,
}

impl<K: Eq + Hash + Clone, V> OrderedHashMap<K, V> {
    pub fn new() -> Self {
        OrderedHashMap { entries: Vec::new(), index: HashMap::new() }
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(&i) = self.index.get(&key) {
            return Some(std::mem::replace(&mut self.entries[i].1, value));
        }
        self.index.insert(key.clone(), self.entries.len());
        self.entries.push((key, value));
        None
    }

    pub fn get<Q: ?Sized + Eq + Hash>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
    {
        self.index.get(key).map(|&i| &self.entries[i].1)
    }

    pub fn contains_key<Q: ?Sized + Eq + Hash>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
    {
        self.index.contains_key(key)
    }

    /// Python `d.pop(k)`: O(n), shifts later entries down and reindexes them.
    pub fn pop(&mut self, key: K) -> V {
        let i = self.index.remove(&key).expect("KeyError");
        let (_, value) = self.entries.remove(i);
        for (k, _) in &self.entries[i..] {
            *self.index.get_mut(k).unwrap() -= 1;
        }
        value
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Python `d[k]`: raises KeyError on a missing key.
    pub fn getitem<Q: ?Sized + Eq + Hash>(&self, key: &Q) -> &V
    where
        K: Borrow<Q>,
    {
        match self.get(key) {
            Some(value) => value,
            None => std::panic::panic_any(KeyError::new("key not found")),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.entries.iter().map(|(k, v)| (k, v))
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.entries.iter().map(|(k, _)| k)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.entries.iter().map(|(_, v)| v)
    }
}

impl<K: Eq + Hash + Clone, V> FromIterator<(K, V)> for OrderedHashMap<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut map = OrderedHashMap::new();
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

impl<'a, K, V> IntoIterator for &'a OrderedHashMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = std::iter::Map<std::slice::Iter<'a, (K, V)>, fn(&'a (K, V)) -> (&'a K, &'a V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.iter().map(|(k, v)| (k, v))
    }
}

// Python dict equality ignores order.
impl<K: Eq + Hash + Clone, V: PartialEq> PartialEq for OrderedHashMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().all(|(k, v)| other.get(k) == Some(v))
    }
}

impl<K: std::fmt::Debug, V: std::fmt::Debug> std::fmt::Debug for OrderedHashMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.entries.iter().map(|(k, v)| (k, v))).finish()
    }
}

// Python-like Exception Types
// These are used with panic! for exception handling

/// Base trait for MultiGen exceptions
pub trait MultiGenException: std::fmt::Debug + std::fmt::Display {}

#[derive(Debug, Clone)]
pub struct ValueError {
    pub message: String,
}

impl ValueError {
    pub fn new(msg: &str) -> Self {
        ValueError { message: msg.to_string() }
    }
}

impl std::fmt::Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ValueError: {}", self.message)
    }
}

impl MultiGenException for ValueError {}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
}

impl TypeError {
    pub fn new(msg: &str) -> Self {
        TypeError { message: msg.to_string() }
    }
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TypeError: {}", self.message)
    }
}

impl MultiGenException for TypeError {}

#[derive(Debug, Clone)]
pub struct KeyError {
    pub message: String,
}

impl KeyError {
    pub fn new(msg: &str) -> Self {
        KeyError { message: msg.to_string() }
    }
}

impl std::fmt::Display for KeyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "KeyError: {}", self.message)
    }
}

impl MultiGenException for KeyError {}

#[derive(Debug, Clone)]
pub struct ZeroDivisionError {
    pub message: String,
}

impl ZeroDivisionError {
    pub fn new(msg: &str) -> Self {
        ZeroDivisionError { message: msg.to_string() }
    }
}

impl std::fmt::Display for ZeroDivisionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ZeroDivisionError: {}", self.message)
    }
}

impl MultiGenException for ZeroDivisionError {}

/// Python `//` and `%`: round toward negative infinity, not zero.
/// `div_euclid`/`rem_euclid` differ from Python when the divisor is negative.
pub trait PyDivMod: Sized {
    fn py_floordiv(self, rhs: Self) -> Self;
    fn py_mod(self, rhs: Self) -> Self;
}

macro_rules! impl_py_divmod_int {
    ($($t:ty),*) => {$(
        impl PyDivMod for $t {
            fn py_floordiv(self, rhs: Self) -> Self {
                if rhs == 0 {
                    std::panic::panic_any(ZeroDivisionError::new("integer division or modulo by zero"));
                }
                let q = self / rhs;
                if self % rhs != 0 && ((self < 0) != (rhs < 0)) { q - 1 } else { q }
            }
            fn py_mod(self, rhs: Self) -> Self {
                if rhs == 0 {
                    std::panic::panic_any(ZeroDivisionError::new("integer division or modulo by zero"));
                }
                if rhs == -1 {
                    return 0; // MIN % -1 overflows
                }
                let r = self % rhs;
                if r != 0 && ((r < 0) != (rhs < 0)) { r + rhs } else { r }
            }
        }
    )*};
}

impl_py_divmod_int!(i64);

impl PyDivMod for f64 {
    fn py_floordiv(self, rhs: Self) -> Self {
        if rhs == 0.0 {
            std::panic::panic_any(ZeroDivisionError::new("float floor division by zero"));
        }
        (self / rhs).floor()
    }
    fn py_mod(self, rhs: Self) -> Self {
        if rhs == 0.0 {
            std::panic::panic_any(ZeroDivisionError::new("float modulo"));
        }
        let r = self % rhs;
        if r != 0.0 && ((r < 0.0) != (rhs < 0.0)) { r + rhs } else { r }
    }
}

/// A number or a reference to one: closures over iterators receive `&i64`.
pub trait PyNum {
    type Val: PyDivMod;
    fn py_val(self) -> Self::Val;
}

macro_rules! impl_py_num {
    ($($t:ty),*) => {$(
        impl PyNum for $t {
            type Val = $t;
            fn py_val(self) -> $t { self }
        }
        impl PyNum for &$t {
            type Val = $t;
            fn py_val(self) -> $t { *self }
        }
    )*};
}

impl_py_num!(i64, f64);

pub fn py_floordiv<A: PyNum, B: PyNum<Val = A::Val>>(a: A, b: B) -> A::Val {
    a.py_val().py_floordiv(b.py_val())
}

pub fn py_mod<A: PyNum, B: PyNum<Val = A::Val>>(a: A, b: B) -> A::Val {
    a.py_val().py_mod(b.py_val())
}

#[derive(Debug, Clone)]
pub struct IndexError {
    pub message: String,
}

impl IndexError {
    pub fn new(msg: &str) -> Self {
        IndexError { message: msg.to_string() }
    }
}

impl std::fmt::Display for IndexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "IndexError: {}", self.message)
    }
}

impl MultiGenException for IndexError {}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
}

impl RuntimeError {
    pub fn new(msg: &str) -> Self {
        RuntimeError { message: msg.to_string() }
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RuntimeError: {}", self.message)
    }
}

impl MultiGenException for RuntimeError {}

/// Python list index: a negative index counts from the end; out of range raises IndexError.
pub fn py_index(index: i64, len: usize) -> usize {
    let i = if index < 0 { index + len as i64 } else { index };
    if i < 0 || i as usize >= len {
        std::panic::panic_any(IndexError::new("list index out of range"));
    }
    i as usize
}

/// Run the program's main: an escaping exception prints "Name: message" and exits 1, as CPython does.
/// The default panic report is silenced; it also printed for panics a try/except caught.
pub fn run_main(py_main: fn()) {
    std::panic::set_hook(Box::new(|_| {}));
    if let Err(err) = std::panic::catch_unwind(py_main) {
        let report = if let Some(e) = err.downcast_ref::<ValueError>() {
            e.to_string()
        } else if let Some(e) = err.downcast_ref::<TypeError>() {
            e.to_string()
        } else if let Some(e) = err.downcast_ref::<KeyError>() {
            e.to_string()
        } else if let Some(e) = err.downcast_ref::<ZeroDivisionError>() {
            e.to_string()
        } else if let Some(e) = err.downcast_ref::<IndexError>() {
            e.to_string()
        } else if let Some(e) = err.downcast_ref::<RuntimeError>() {
            e.to_string()
        } else {
            format!("Exception: {}", get_panic_message(&err))
        };
        use std::io::Write;
        let _ = std::io::stdout().flush();
        eprintln!("{}", report);
        std::process::exit(1);
    }
}

/// Helper function to check if a panic matches a specific exception type
pub fn panic_is_exception<T: 'static>(err: &Box<dyn Any + Send>) -> bool {
    err.downcast_ref::<T>().is_some()
}

/// Helper function to extract exception message from panic
pub fn get_panic_message(err: &Box<dyn Any + Send>) -> String {
    if let Some(s) = err.downcast_ref::<String>() {
        s.clone()
    } else if let Some(s) = err.downcast_ref::<&str>() {
        s.to_string()
    } else {
        "Unknown error".to_string()
    }
}

// String operations using Rust standard library
pub struct StrOps;

impl StrOps {
    pub fn upper(s: &str) -> String {
        s.to_uppercase()
    }

    pub fn lower(s: &str) -> String {
        s.to_lowercase()
    }

    pub fn strip(s: &str) -> String {
        s.trim().to_string()
    }

    pub fn strip_chars(s: &str, chars: &str) -> String {
        let chars_to_trim: Vec<char> = chars.chars().collect();
        s.trim_matches(&chars_to_trim[..]).to_string()
    }

    pub fn find(s: &str, substr: &str) -> i64 {
        match s.find(substr) {
            Some(pos) => pos as i64,
            None => -1,
        }
    }

    pub fn replace(s: &str, old: &str, new: &str) -> String {
        s.replace(old, new)
    }

    pub fn split(s: &str) -> Vec<String> {
        s.split_whitespace().map(|s| s.to_string()).collect()
    }

    pub fn split_sep(s: &str, sep: &str) -> Vec<String> {
        s.split(sep).map(|s| s.to_string()).collect()
    }
}

// Built-in operations using Rust standard library
pub struct Builtins;

impl Builtins {
    pub fn len<T>(container: &[T]) -> usize {
        container.len()
    }

    pub fn len_string(s: &str) -> usize {
        s.chars().count()
    }

    pub fn len_vec<T>(v: &Vec<T>) -> usize {
        v.len()
    }

    pub fn len_hashmap<K: Eq + Hash + Clone, V>(m: &OrderedHashMap<K, V>) -> usize {
        m.len()
    }

    pub fn len_hashset<T>(s: &HashSet<T>) -> usize {
        s.len()
    }

    pub fn abs_i64(x: i64) -> i64 {
        x.abs()
    }

    pub fn abs_f64(x: f64) -> f64 {
        x.abs()
    }

    pub fn min_i64(a: i64, b: i64) -> i64 {
        a.min(b)
    }

    pub fn max_i64(a: i64, b: i64) -> i64 {
        a.max(b)
    }

    pub fn min_f64(a: f64, b: f64) -> f64 {
        a.min(b)
    }

    pub fn max_f64(a: f64, b: f64) -> f64 {
        a.max(b)
    }

    pub fn sum_i64(vec: &Vec<i64>) -> i64 {
        vec.iter().sum()
    }

    pub fn sum_f64(vec: &Vec<f64>) -> f64 {
        vec.iter().sum()
    }

    pub fn any(vec: &Vec<bool>) -> bool {
        vec.iter().any(|&x| x)
    }

    pub fn all(vec: &Vec<bool>) -> bool {
        vec.iter().all(|&x| x)
    }
}

// Type conversion utilities
pub fn to_bool<T: PartialEq<T> + Default>(value: T) -> bool {
    value != T::default()
}

pub fn to_i64_from_f64(value: f64) -> i64 {
    value as i64
}

pub fn to_f64_from_i64(value: i64) -> f64 {
    value as f64
}

pub fn to_string<T: Display>(value: T) -> String {
    format!("{}", value)
}

/// Python's repr of a float: shortest round-trip digits, `.0` on whole numbers,
/// and exponent form when the decimal exponent is below -4 or at least 16.
pub fn py_float_str(value: f64) -> String {
    if value.is_nan() {
        return "nan".to_string();
    }
    if value.is_infinite() {
        return if value > 0.0 { "inf" } else { "-inf" }.to_string();
    }
    let sci = format!("{:e}", value);
    let (mantissa, exp) = sci.split_once('e').unwrap();
    let exp: i32 = exp.parse().unwrap();
    if (-4..16).contains(&exp) {
        let plain = format!("{}", value);
        return if plain.contains('.') { plain } else { plain + ".0" };
    }
    format!("{}e{}{:02}", mantissa, if exp < 0 { '-' } else { '+' }, exp.abs())
}

pub fn py_bool_str(value: bool) -> String {
    (if value { "True" } else { "False" }).to_string()
}

// Range structure for Python-like iteration
#[derive(Debug, Clone)]
pub struct Range {
    start: i64,
    stop: i64,
    step: i64,
    current: i64,
}

impl Range {
    pub fn new(stop: i64) -> Self {
        Range {
            start: 0,
            stop,
            step: 1,
            current: 0,
        }
    }

    pub fn new_with_start(start: i64, stop: i64) -> Self {
        Range {
            start,
            stop,
            step: 1,
            current: start,
        }
    }

    pub fn new_with_step(start: i64, stop: i64, step: i64) -> Self {
        Range {
            start,
            stop,
            step,
            current: start,
        }
    }

    pub fn collect(&self) -> Vec<i64> {
        let mut result = Vec::new();
        let mut current = self.start;

        if self.step > 0 {
            while current < self.stop {
                result.push(current);
                current += self.step;
            }
        } else {
            while current > self.stop {
                result.push(current);
                current += self.step;
            }
        }

        result
    }
}

impl Iterator for Range {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        if (self.step > 0 && self.current < self.stop) || (self.step < 0 && self.current > self.stop) {
            let result = self.current;
            self.current += self.step;
            Some(result)
        } else {
            None
        }
    }
}

// Comprehension operations using functional programming patterns
pub struct Comprehensions;

// Every closure receives an owned element, as a Python comprehension variable
// is a value; the filtered variants clone it for the predicate.
impl Comprehensions {
    // List comprehensions
    pub fn list_comprehension<T, U, F>(
        iterable: Vec<T>,
        transform: F,
    ) -> Vec<U>
    where
        F: Fn(T) -> U,
    {
        iterable.into_iter().map(transform).collect()
    }

    pub fn list_comprehension_with_filter<T, U, F, P>(
        iterable: Vec<T>,
        transform: F,
        predicate: P,
    ) -> Vec<U>
    where
        F: Fn(T) -> U,
        P: Fn(T) -> bool,
        T: Clone,
    {
        iterable.into_iter()
            .filter(|item| predicate(item.clone()))
            .map(transform)
            .collect()
    }

    // Dict comprehensions
    pub fn dict_comprehension<T, K, V, F>(
        iterable: Vec<T>,
        key_value_transform: F,
    ) -> OrderedHashMap<K, V>
    where
        F: Fn(T) -> (K, V),
        K: Eq + Hash + Clone,
    {
        iterable.into_iter().map(key_value_transform).collect()
    }

    pub fn dict_comprehension_with_filter<T, K, V, F, P>(
        iterable: Vec<T>,
        key_value_transform: F,
        predicate: P,
    ) -> OrderedHashMap<K, V>
    where
        F: Fn(T) -> (K, V),
        P: Fn(T) -> bool,
        K: Eq + Hash + Clone,
        T: Clone,
    {
        iterable.into_iter()
            .filter(|item| predicate(item.clone()))
            .map(key_value_transform)
            .collect()
    }

    // Set comprehensions
    pub fn set_comprehension<T, U, F>(
        iterable: Vec<T>,
        transform: F,
    ) -> HashSet<U>
    where
        F: Fn(T) -> U,
        U: Eq + Hash,
    {
        iterable.into_iter().map(transform).collect()
    }

    pub fn set_comprehension_with_filter<T, U, F, P>(
        iterable: Vec<T>,
        transform: F,
        predicate: P,
    ) -> HashSet<U>
    where
        F: Fn(T) -> U,
        P: Fn(T) -> bool,
        U: Eq + Hash,
        T: Clone,
    {
        iterable.into_iter()
            .filter(|item| predicate(item.clone()))
            .map(transform)
            .collect()
    }
}

// Print function for Python-like output
pub fn print_value<T: Display>(value: T) {
    println!("{}", value);
}

pub fn print_debug<T: std::fmt::Debug>(value: T) {
    println!("{:?}", value);
}

// Convenience functions for common operations
pub fn new_range(stop: i64) -> Range {
    Range::new(stop)
}

pub fn new_range_with_start(start: i64, stop: i64) -> Range {
    Range::new_with_start(start, stop)
}

pub fn new_range_with_step(start: i64, stop: i64, step: i64) -> Range {
    Range::new_with_step(start, stop, step)
}