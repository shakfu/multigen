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
#include <list>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <sstream>
#include <cctype>
#include <cmath>
#include <stdexcept>
#include <exception>
#include <cstdlib>
#include <cstdio>
#include <memory>
#include <limits>
#include <type_traits>

namespace multigen {

// ============================================================================
// Exception Types (Internal)
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
// Python-like Exception Types
// ============================================================================

class ValueError : public std::runtime_error {
public:
    explicit ValueError(const std::string& msg = "ValueError") : std::runtime_error(msg) {}
};

class TypeError : public std::runtime_error {
public:
    explicit TypeError(const std::string& msg = "TypeError") : std::runtime_error(msg) {}
};

class KeyError : public std::runtime_error {
public:
    explicit KeyError(const std::string& msg = "") : std::runtime_error("KeyError: " + msg) {}
};

class ZeroDivisionError : public std::runtime_error {
public:
    explicit ZeroDivisionError(const std::string& msg = "division by zero") : std::runtime_error(msg) {}
};

class IndexError : public std::out_of_range {
public:
    explicit IndexError(const std::string& msg = "list index out of range") : std::out_of_range(msg) {}
};

class RuntimeError : public std::runtime_error {
public:
    explicit RuntimeError(const std::string& msg = "RuntimeError") : std::runtime_error(msg) {}
};

// An uncaught exception prints "Name: message" and exits 1, as CPython does.
// The default std::terminate aborts with SIGABRT.
[[noreturn]] inline void report_uncaught_exception() {
    try {
        if (std::current_exception()) std::rethrow_exception(std::current_exception());
        std::cerr << "terminate called without an active exception\n";
    } catch (const KeyError& e) {
        std::cerr << e.what() << "\n";  // what() already starts with "KeyError: "
    } catch (const ValueError& e) {
        std::cerr << "ValueError: " << e.what() << "\n";
    } catch (const TypeError& e) {
        std::cerr << "TypeError: " << e.what() << "\n";
    } catch (const ZeroDivisionError& e) {
        std::cerr << "ZeroDivisionError: " << e.what() << "\n";
    } catch (const IndexError& e) {
        std::cerr << "IndexError: " << e.what() << "\n";
    } catch (const RuntimeError& e) {
        std::cerr << "RuntimeError: " << e.what() << "\n";
    } catch (const std::exception& e) {
        std::cerr << "Exception: " << e.what() << "\n";
    } catch (...) {
        std::cerr << "Exception\n";
    }
    std::cout.flush();
    std::_Exit(1);
}

inline const bool uncaught_exception_handler_installed = (std::set_terminate(report_uncaught_exception), true);

// ============================================================================
// Python // and %: round toward negative infinity, not zero
// ============================================================================

template <typename A, typename B>
auto floordiv(A a, B b) {
    using T = std::common_type_t<A, B>;
    if (b == 0) {
        throw ZeroDivisionError(std::is_integral_v<T> ? "integer division or modulo by zero" : "float floor division by zero");
    }
    if constexpr (std::is_integral_v<T>) {
        T x = a, y = b;
        if constexpr (std::is_signed_v<T>) {
            if (x == std::numeric_limits<T>::min() && y == -1) {
                throw RuntimeError("integer overflow in floor division");
            }
        }
        T q = x / y;
        if ((x % y != 0) && ((x < 0) != (y < 0))) {
            --q;
        }
        return q;
    } else {
        return std::floor(static_cast<T>(a) / static_cast<T>(b));
    }
}

template <typename A, typename B>
auto pymod(A a, B b) {
    using T = std::common_type_t<A, B>;
    if (b == 0) {
        throw ZeroDivisionError(std::is_integral_v<T> ? "integer division or modulo by zero" : "float modulo by zero");
    }
    T r;
    if constexpr (std::is_integral_v<T>) {
        if (b == -1) {
            return T(0);  // INT_MIN % -1 is undefined in C++
        }
        r = static_cast<T>(a) % static_cast<T>(b);
    } else {
        r = std::fmod(static_cast<T>(a), static_cast<T>(b));
    }
    if (r != 0 && ((r < 0) != (b < 0))) {
        r += b;
    }
    return r;
}

// Python /: always true division, so int operands promote to double.
template <typename A, typename B>
double truediv(A a, B b) {
    if (b == 0) {
        throw ZeroDivisionError(std::is_integral_v<std::common_type_t<A, B>> ? "division by zero" : "float division by zero");
    }
    return static_cast<double>(a) / static_cast<double>(b);
}

// ============================================================================
// Python repr of float and bool for print()
// ============================================================================

// Shortest digits that round-trip, laid out as Python's float repr does.
inline std::string float_repr(double x) {
    if (std::isnan(x)) return "nan";
    if (std::isinf(x)) return x < 0 ? "-inf" : "inf";
    char buf[32];
    for (int p = 0; p < 17; ++p) {
        std::snprintf(buf, sizeof buf, "%.*e", p, x);
        if (std::strtod(buf, nullptr) == x) break;
    }
    std::string s(buf);  // [-]d[.ddd]e[+-]XX
    size_t e = s.find('e');
    int exp = std::stoi(s.substr(e + 1));
    bool neg = s[0] == '-';
    std::string digits;
    for (size_t i = neg ? 1 : 0; i < e; ++i) {
        if (s[i] != '.') digits += s[i];
    }
    while (digits.size() > 1 && digits.back() == '0') digits.pop_back();
    int n = static_cast<int>(digits.size());
    std::string out;
    if (exp < -4 || exp >= 16) {
        out = digits.substr(0, 1) + (n > 1 ? "." + digits.substr(1) : "");
        char eb[8];
        std::snprintf(eb, sizeof eb, "e%c%02d", exp < 0 ? '-' : '+', exp < 0 ? -exp : exp);
        out += eb;
    } else if (exp < 0) {
        out = "0." + std::string(-exp - 1, '0') + digits;
    } else if (n <= exp + 1) {
        out = digits + std::string(exp + 1 - n, '0') + ".0";
    } else {
        out = digits.substr(0, exp + 1) + "." + digits.substr(exp + 1);
    }
    return neg ? "-" + out : out;
}

// print() argument: double and bool get Python's repr; other types pass through to <<.
template <typename T>
const T& print_value(const T& value) {
    return value;
}

inline std::string print_value(double value) {
    return float_repr(value);
}

inline const char* print_value(bool value) {
    return value ? "True" : "False";
}

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

// Python dict: iterates in insertion order; overwriting a key keeps its position.
template<typename K, typename V>
class Dict {
    using Entries = std::list<std::pair<const K, V>>;
    Entries entries_;
    std::unordered_map<K, typename Entries::iterator> index_;

public:
    using key_type = K;
    using mapped_type = V;
    using value_type = std::pair<const K, V>;
    using iterator = typename Entries::iterator;
    using const_iterator = typename Entries::const_iterator;

    Dict() = default;
    Dict(std::initializer_list<value_type> init) {
        for (const auto& kv : init) (*this)[kv.first] = kv.second;
    }
    Dict(const Dict& other) {
        for (const auto& kv : other) (*this)[kv.first] = kv.second;
    }
    // std::list keeps iterators valid across move and swap, so index_ stays correct.
    Dict(Dict&&) noexcept = default;
    Dict& operator=(Dict other) noexcept {
        entries_.swap(other.entries_);
        index_.swap(other.index_);
        return *this;
    }

    V& operator[](const K& key) {
        auto it = index_.find(key);
        if (it != index_.end()) return it->second->second;
        auto pos = entries_.emplace(entries_.end(), key, V{});
        index_.emplace(key, pos);
        return pos->second;
    }

    size_t erase(const K& key) {
        auto it = index_.find(key);
        if (it == index_.end()) return 0;
        entries_.erase(it->second);
        index_.erase(it);
        return 1;
    }

    V pop(const K& key) {
        auto it = index_.find(key);
        if (it == index_.end()) throw KeyError("key not found");
        V value = std::move(it->second->second);
        entries_.erase(it->second);
        index_.erase(it);
        return value;
    }

    V pop(const K& key, const V& default_value) {
        return count(key) ? pop(key) : default_value;
    }

    size_t count(const K& key) const { return index_.count(key); }
    size_t size() const { return entries_.size(); }
    bool empty() const { return entries_.empty(); }
    void clear() { entries_.clear(); index_.clear(); }

    iterator begin() { return entries_.begin(); }
    iterator end() { return entries_.end(); }
    const_iterator begin() const { return entries_.begin(); }
    const_iterator end() const { return entries_.end(); }

    // Order-insensitive, like Python dict ==.
    bool operator==(const Dict& other) const {
        if (size() != other.size()) return false;
        for (const auto& [key, value] : entries_) {
            auto it = other.index_.find(key);
            if (it == other.index_.end() || !(it->second->second == value)) return false;
        }
        return true;
    }
    bool operator!=(const Dict& other) const { return !(*this == other); }
};

// Python container[index] read: a negative index counts from the end, and a
// missing index or key raises IndexError or KeyError instead of reading garbage.
inline size_t checked_index(long long index, size_t size) {
    if (index < 0) index += static_cast<long long>(size);
    if (index < 0 || static_cast<size_t>(index) >= size) throw IndexError();
    return static_cast<size_t>(index);
}

template<typename T>
T& getitem(std::vector<T>& v, long long index) { return v[checked_index(index, v.size())]; }

template<typename T>
const T& getitem(const std::vector<T>& v, long long index) { return v[checked_index(index, v.size())]; }

inline char getitem(const std::string& s, long long index) { return s[checked_index(index, s.size())]; }

template<typename K, typename V, typename Q>
V& getitem(Dict<K, V>& d, const Q& key) {
    if (!d.count(key)) throw KeyError("key not found");
    return d[key];
}

template<typename K, typename V, typename Q>
const V& getitem(const Dict<K, V>& d, const Q& key) {
    if (!d.count(key)) throw KeyError("key not found");
    return const_cast<Dict<K, V>&>(d)[key];
}

// Containers above get checked overloads; the fallback must not outrank them on an exact match.
template<typename C> struct is_checked_container : std::false_type {};
template<typename T> struct is_checked_container<std::vector<T>> : std::true_type {};
template<typename K, typename V> struct is_checked_container<Dict<K, V>> : std::true_type {};
template<> struct is_checked_container<std::string> : std::true_type {};

template<typename C, typename I,
         typename = std::enable_if_t<!is_checked_container<std::decay_t<C>>::value>>
decltype(auto) getitem(C&& c, I&& index) { return std::forward<C>(c)[std::forward<I>(index)]; }

// Python container[index] = value: a dict inserts; a list store is bounds-checked like a read.
template<typename K, typename V, typename Q, typename T>
void setitem(Dict<K, V>& d, const Q& key, T&& value) { d[key] = std::forward<T>(value); }

template<typename C> struct is_dict : std::false_type {};
template<typename K, typename V> struct is_dict<Dict<K, V>> : std::true_type {};

template<typename C, typename I, typename T, typename = std::enable_if_t<!is_dict<C>::value>>
void setitem(C& c, I&& index, T&& value) { getitem(c, std::forward<I>(index)) = std::forward<T>(value); }

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
    -> Dict<decltype(key_value_func(0).first), decltype(key_value_func(0).second)> {
    Dict<decltype(key_value_func(0).first), decltype(key_value_func(0).second)> result;
    for (auto i : range) {
        auto pair = key_value_func(i);
        result[pair.first] = pair.second;
    }
    return result;
}

template<typename Func, typename Pred>
auto dict_comprehension(const Range& range, Func key_value_func, Pred condition)
    -> Dict<decltype(key_value_func(0).first), decltype(key_value_func(0).second)> {
    Dict<decltype(key_value_func(0).first), decltype(key_value_func(0).second)> result;
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
    -> Dict<decltype(key_value_func(*container.begin()).first),
                decltype(key_value_func(*container.begin()).second)> {
    Dict<decltype(key_value_func(*container.begin()).first),
         decltype(key_value_func(*container.begin()).second)> result;
    for (const auto& item : container) {
        auto pair = key_value_func(item);
        result[pair.first] = pair.second;
    }
    return result;
}

template<typename Container, typename Func, typename Pred>
auto dict_comprehension(const Container& container, Func key_value_func, Pred condition)
    -> Dict<decltype(key_value_func(*container.begin()).first),
                decltype(key_value_func(*container.begin()).second)> {
    Dict<decltype(key_value_func(*container.begin()).first),
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
 * Extract keys from a map (Python dict.keys(), or iterating a dict directly)
 */
template<typename Map>
auto keys(const Map& map) -> std::vector<typename Map::key_type> {
    std::vector<typename Map::key_type> result;
    result.reserve(map.size());
    for (const auto& [key, value] : map) {
        result.push_back(key);
    }
    return result;
}

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