/**
 * MultiGen C++ Runtime Library
 *
 * STL-based runtime providing Python-like operations using modern C++17 features.
 * This replaces the C runtime's STC containers with standard STL containers.
 */

#ifndef MGEN_CPP_RUNTIME_HPP
#define MGEN_CPP_RUNTIME_HPP

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <sstream>
#include <cctype>
#include <cmath>
#include <stdexcept>
#include <memory>

namespace multigen {

// ============================================================================
// Exception Types
// ============================================================================

class MultiGenError : public std::runtime_error {
public:
    explicit MultiGenError(const std::string& msg) : std::runtime_error(msg) {}
};

class TypeMappingError : public MultiGenError {
public:
    explicit TypeMappingError(const std::string& msg) : MultiGenError("Type mapping error: " + msg) {}
};

class UnsupportedFeatureError : public MultiGenError {
public:
    explicit UnsupportedFeatureError(const std::string& msg) : MultiGenError("Unsupported feature: " + msg) {}
};

// ============================================================================
// String Operations (Python str methods)
// ============================================================================

class StringOps {
public:
    static std::string upper(const std::string& str) {
        std::string result = str;
        std::transform(result.begin(), result.end(), result.begin(), ::toupper);
        return result;
    }

    static std::string lower(const std::string& str) {
        std::string result = str;
        std::transform(result.begin(), result.end(), result.begin(), ::tolower);
        return result;
    }

    static std::string strip(const std::string& str) {
        size_t start = str.find_first_not_of(" \t\n\r");
        if (start == std::string::npos) return "";
        size_t end = str.find_last_not_of(" \t\n\r");
        return str.substr(start, end - start + 1);
    }

    static std::string strip(const std::string& str, const std::string& chars) {
        size_t start = str.find_first_not_of(chars);
        if (start == std::string::npos) return "";
        size_t end = str.find_last_not_of(chars);
        return str.substr(start, end - start + 1);
    }

    static int find(const std::string& str, const std::string& substr) {
        size_t pos = str.find(substr);
        return (pos == std::string::npos) ? -1 : static_cast<int>(pos);
    }

    static std::string replace(const std::string& str, const std::string& old_str, const std::string& new_str) {
        std::string result = str;
        size_t pos = 0;
        while ((pos = result.find(old_str, pos)) != std::string::npos) {
            result.replace(pos, old_str.length(), new_str);
            pos += new_str.length();
        }
        return result;
    }

    static std::vector<std::string> split(const std::string& str, const std::string& delimiter = " ") {
        std::vector<std::string> result;
        if (str.empty()) return result;

        if (delimiter.empty()) {
            // Split on whitespace (Python default behavior)
            std::stringstream ss(str);
            std::string token;
            while (ss >> token) {
                result.push_back(token);
            }
        } else {
            size_t start = 0;
            size_t end = str.find(delimiter);
            while (end != std::string::npos) {
                result.push_back(str.substr(start, end - start));
                start = end + delimiter.length();
                end = str.find(delimiter, start);
            }
            result.push_back(str.substr(start));
        }
        return result;
    }
};

// ============================================================================
// Python Built-in Functions
// ============================================================================

template<typename T>
T abs(T value) {
    return std::abs(value);
}

template<typename Container>
size_t len(const Container& container) {
    return container.size();
}

template<typename T>
bool bool_value(const T& value) {
    if constexpr (std::is_arithmetic_v<T>) {
        return value != 0;
    } else if constexpr (std::is_same_v<T, std::string>) {
        return !value.empty();
    } else {
        return !value.empty();  // For containers
    }
}

template<typename Container>
auto min(const Container& container) -> typename Container::value_type {
    return *std::min_element(container.begin(), container.end());
}

template<typename Container>
auto max(const Container& container) -> typename Container::value_type {
    return *std::max_element(container.begin(), container.end());
}

template<typename Container>
auto sum(const Container& container) -> typename Container::value_type {
    using ValueType = typename Container::value_type;
    ValueType result{};
    for (const auto& item : container) {
        result += item;
    }
    return result;
}

template<typename Container>
bool any(const Container& container) {
    for (const auto& item : container) {
        if (bool_value(item)) {
            return true;
        }
    }
    return false;
}

template<typename Container>
bool all(const Container& container) {
    for (const auto& item : container) {
        if (!bool_value(item)) {
            return false;
        }
    }
    return true;
}

// ============================================================================
// STL Container Aliases (Python-like naming)
// ============================================================================

template<typename T>
using List = std::vector<T>;

template<typename K, typename V>
using Dict = std::unordered_map<K, V>;

template<typename T>
using Set = std::unordered_set<T>;

// ============================================================================
// Range Implementation (Python-like range)
// ============================================================================

class Range {
public:
    class Iterator {
        int current;
        int step;
    public:
        Iterator(int start, int step) : current(start), step(step) {}
        Iterator& operator++() { current += step; return *this; }
        int operator*() const { return current; }
        bool operator!=(const Iterator& other) const { return current != other.current; }
    };

private:
    int start_, stop_, step_;

public:
    explicit Range(int stop) : start_(0), stop_(stop), step_(1) {}
    Range(int start, int stop) : start_(start), stop_(stop), step_(1) {}
    Range(int start, int stop, int step) : start_(start), stop_(stop), step_(step) {}

    Iterator begin() const { return Iterator(start_, step_); }
    Iterator end() const { return Iterator(stop_, step_); }
};

// ============================================================================
// List Comprehension Helpers
// ============================================================================

template<typename T, typename Func>
auto list_comprehension(const Range& range, Func transform) -> std::vector<decltype(transform(0))> {
    std::vector<decltype(transform(0))> result;
    for (auto i : range) {
        result.push_back(transform(i));
    }
    return result;
}

template<typename T, typename Func, typename Pred>
auto list_comprehension(const Range& range, Func transform, Pred condition) -> std::vector<decltype(transform(0))> {
    std::vector<decltype(transform(0))> result;
    for (auto i : range) {
        if (condition(i)) {
            result.push_back(transform(i));
        }
    }
    return result;
}

// Overloads for container iteration
template<typename Container, typename Func>
auto list_comprehension(const Container& container, Func transform)
    -> std::vector<decltype(transform(*container.begin()))> {
    std::vector<decltype(transform(*container.begin()))> result;
    for (const auto& item : container) {
        result.push_back(transform(item));
    }
    return result;
}

template<typename Container, typename Func, typename Pred>
auto list_comprehension(const Container& container, Func transform, Pred condition)
    -> std::vector<decltype(transform(*container.begin()))> {
    std::vector<decltype(transform(*container.begin()))> result;
    for (const auto& item : container) {
        if (condition(item)) {
            result.push_back(transform(item));
        }
    }
    return result;
}

// ============================================================================
// Dict Comprehension Helpers
// ============================================================================

template<typename Func>
auto dict_comprehension(const Range& range, Func key_value_func)
    -> std::unordered_map<decltype(key_value_func(0).first), decltype(key_value_func(0).second)> {
    std::unordered_map<decltype(key_value_func(0).first), decltype(key_value_func(0).second)> result;
    for (auto i : range) {
        auto pair = key_value_func(i);
        result[pair.first] = pair.second;
    }
    return result;
}

template<typename Func, typename Pred>
auto dict_comprehension(const Range& range, Func key_value_func, Pred condition)
    -> std::unordered_map<decltype(key_value_func(0).first), decltype(key_value_func(0).second)> {
    std::unordered_map<decltype(key_value_func(0).first), decltype(key_value_func(0).second)> result;
    for (auto i : range) {
        if (condition(i)) {
            auto pair = key_value_func(i);
            result[pair.first] = pair.second;
        }
    }
    return result;
}

// Overloads for container iteration
template<typename Container, typename Func>
auto dict_comprehension(const Container& container, Func key_value_func)
    -> std::unordered_map<decltype(key_value_func(*container.begin()).first),
                          decltype(key_value_func(*container.begin()).second)> {
    std::unordered_map<decltype(key_value_func(*container.begin()).first),
                       decltype(key_value_func(*container.begin()).second)> result;
    for (const auto& item : container) {
        auto pair = key_value_func(item);
        result[pair.first] = pair.second;
    }
    return result;
}

template<typename Container, typename Func, typename Pred>
auto dict_comprehension(const Container& container, Func key_value_func, Pred condition)
    -> std::unordered_map<decltype(key_value_func(*container.begin()).first),
                          decltype(key_value_func(*container.begin()).second)> {
    std::unordered_map<decltype(key_value_func(*container.begin()).first),
                       decltype(key_value_func(*container.begin()).second)> result;
    for (const auto& item : container) {
        if (condition(item)) {
            auto pair = key_value_func(item);
            result[pair.first] = pair.second;
        }
    }
    return result;
}

// ============================================================================
// Set Comprehension Helpers
// ============================================================================

template<typename Func>
auto set_comprehension(const Range& range, Func transform) -> std::unordered_set<decltype(transform(0))> {
    std::unordered_set<decltype(transform(0))> result;
    for (auto i : range) {
        result.insert(transform(i));
    }
    return result;
}

template<typename Func, typename Pred>
auto set_comprehension(const Range& range, Func transform, Pred condition) -> std::unordered_set<decltype(transform(0))> {
    std::unordered_set<decltype(transform(0))> result;
    for (auto i : range) {
        if (condition(i)) {
            result.insert(transform(i));
        }
    }
    return result;
}

// Overloads for container iteration
template<typename Container, typename Func>
auto set_comprehension(const Container& container, Func transform)
    -> std::unordered_set<decltype(transform(*container.begin()))> {
    std::unordered_set<decltype(transform(*container.begin()))> result;
    for (const auto& item : container) {
        result.insert(transform(item));
    }
    return result;
}

template<typename Container, typename Func, typename Pred>
auto set_comprehension(const Container& container, Func transform, Pred condition)
    -> std::unordered_set<decltype(transform(*container.begin()))> {
    std::unordered_set<decltype(transform(*container.begin()))> result;
    for (const auto& item : container) {
        if (condition(item)) {
            result.insert(transform(item));
        }
    }
    return result;
}

// ============================================================================
// Dictionary Helper Functions
// ============================================================================

/**
 * Extract values from a map (Python dict.values())
 * Returns a vector of values for iteration
 */
template<typename Map>
auto values(const Map& map) -> std::vector<typename Map::mapped_type> {
    std::vector<typename Map::mapped_type> result;
    result.reserve(map.size());
    for (const auto& [key, value] : map) {
        result.push_back(value);
    }
    return result;
}

} // namespace multigen

#endif // MGEN_CPP_RUNTIME_HPP