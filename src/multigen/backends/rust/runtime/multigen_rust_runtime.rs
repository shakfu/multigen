// MultiGen Rust Runtime Library
// Provides Python-like operations using only Rust standard library

use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;

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

    pub fn find(s: &str, substr: &str) -> i32 {
        match s.find(substr) {
            Some(pos) => pos as i32,
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

    pub fn len_hashmap<K, V>(m: &HashMap<K, V>) -> usize {
        m.len()
    }

    pub fn len_hashset<T>(s: &HashSet<T>) -> usize {
        s.len()
    }

    pub fn abs_i32(x: i32) -> i32 {
        x.abs()
    }

    pub fn abs_f64(x: f64) -> f64 {
        x.abs()
    }

    pub fn min_i32(a: i32, b: i32) -> i32 {
        a.min(b)
    }

    pub fn max_i32(a: i32, b: i32) -> i32 {
        a.max(b)
    }

    pub fn min_f64(a: f64, b: f64) -> f64 {
        a.min(b)
    }

    pub fn max_f64(a: f64, b: f64) -> f64 {
        a.max(b)
    }

    pub fn sum_i32(vec: &Vec<i32>) -> i32 {
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

pub fn to_i32_from_f64(value: f64) -> i32 {
    value as i32
}

pub fn to_f64_from_i32(value: i32) -> f64 {
    value as f64
}

pub fn to_string<T: Display>(value: T) -> String {
    format!("{}", value)
}

// Range structure for Python-like iteration
#[derive(Debug, Clone)]
pub struct Range {
    start: i32,
    stop: i32,
    step: i32,
    current: i32,
}

impl Range {
    pub fn new(stop: i32) -> Self {
        Range {
            start: 0,
            stop,
            step: 1,
            current: 0,
        }
    }

    pub fn new_with_start(start: i32, stop: i32) -> Self {
        Range {
            start,
            stop,
            step: 1,
            current: start,
        }
    }

    pub fn new_with_step(start: i32, stop: i32, step: i32) -> Self {
        Range {
            start,
            stop,
            step,
            current: start,
        }
    }

    pub fn collect(&self) -> Vec<i32> {
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
    type Item = i32;

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
        F: Fn(&T) -> U,
        P: Fn(&T) -> bool,
        T: Clone,
    {
        iterable.iter()
            .filter(|&item| predicate(item))
            .map(|item| transform(item))
            .collect()
    }

    // Dict comprehensions
    pub fn dict_comprehension<T, K, V, F>(
        iterable: Vec<T>,
        key_value_transform: F,
    ) -> HashMap<K, V>
    where
        F: Fn(T) -> (K, V),
        K: Eq + Hash,
    {
        iterable.into_iter().map(key_value_transform).collect()
    }

    pub fn dict_comprehension_with_filter<T, K, V, F, P>(
        iterable: Vec<T>,
        key_value_transform: F,
        predicate: P,
    ) -> HashMap<K, V>
    where
        F: Fn(&T) -> (K, V),
        P: Fn(&T) -> bool,
        K: Eq + Hash,
        T: Clone,
    {
        iterable.iter()
            .filter(|&item| predicate(item))
            .map(|item| key_value_transform(item))
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
        F: Fn(&T) -> U,
        P: Fn(&T) -> bool,
        U: Eq + Hash,
        T: Clone,
    {
        iterable.iter()
            .filter(|&item| predicate(item))
            .map(|item| transform(item))
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
pub fn new_range(stop: i32) -> Range {
    Range::new(stop)
}

pub fn new_range_with_start(start: i32, stop: i32) -> Range {
    Range::new_with_start(start, stop)
}

pub fn new_range_with_step(start: i32, stop: i32, step: i32) -> Range {
    Range::new_with_step(start, stop, step)
}