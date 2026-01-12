// Package multigen provides runtime support for MultiGen-generated Go code
// This package uses only the Go standard library to provide Python-like operations
// Requires Go 1.18+ for generics support
package multigen

import (
	"fmt"
	"math"
	"strings"
)

// Comparable is a constraint for comparable types
type Comparable interface {
	comparable
}

// Ordered is a constraint for ordered types
type Ordered interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 |
		~float32 | ~float64 | ~string
}

// Numeric is a constraint for numeric types
type Numeric interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64 |
		~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 |
		~float32 | ~float64
}

// StringOps provides Python-like string operations
type StringOps struct{}

// Upper converts string to uppercase
func (s StringOps) Upper(str string) string {
	return strings.ToUpper(str)
}

// Lower converts string to lowercase
func (s StringOps) Lower(str string) string {
	return strings.ToLower(str)
}

// Strip removes whitespace from both ends
func (s StringOps) Strip(str string) string {
	return strings.TrimSpace(str)
}

// StripChars removes specified characters from both ends
func (s StringOps) StripChars(str, chars string) string {
	return strings.Trim(str, chars)
}

// Find returns the index of the first occurrence of substr in str, or -1 if not found
func (s StringOps) Find(str, substr string) int {
	index := strings.Index(str, substr)
	return index
}

// Replace replaces all occurrences of old with new in str
func (s StringOps) Replace(str, old, new string) string {
	return strings.ReplaceAll(str, old, new)
}

// Split splits string by delimiter
func (s StringOps) Split(str string) []string {
	return strings.Fields(str)
}

// SplitSep splits string by specific separator
func (s StringOps) SplitSep(str, sep string) []string {
	return strings.Split(str, sep)
}

// Global StringOps instance
var StrOps = StringOps{}

// Generic built-in functions

// Abs returns absolute value for numeric types
func AbsInt(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func AbsFloat(x float64) float64 {
	return math.Abs(x)
}

// Len functions for different container types
func Len[T any](x []T) int {
	return len(x)
}

func LenMap[K comparable, V any](x map[K]V) int {
	return len(x)
}

func LenString(x string) int {
	return len(x)
}

// Min returns minimum value from slice
func Min[T Ordered](slice []T) T {
	if len(slice) == 0 {
		panic("min() requires non-empty slice")
	}
	min := slice[0]
	for _, item := range slice[1:] {
		if item < min {
			min = item
		}
	}
	return min
}

// Max returns maximum value from slice
func Max[T Ordered](slice []T) T {
	if len(slice) == 0 {
		panic("max() requires non-empty slice")
	}
	max := slice[0]
	for _, item := range slice[1:] {
		if item > max {
			max = item
		}
	}
	return max
}

// Sum returns sum of numeric slice
func Sum[T Numeric](slice []T) T {
	var total T
	for _, v := range slice {
		total += v
	}
	return total
}

// Any returns true if any element in the slice is true
func Any(slice []bool) bool {
	for _, v := range slice {
		if v {
			return true
		}
	}
	return false
}

// All returns true if all elements in the slice are true
func All(slice []bool) bool {
	for _, v := range slice {
		if !v {
			return false
		}
	}
	return true
}

// Legacy BuiltinOps struct for backwards compatibility
type BuiltinOps struct{}

// Global BuiltinOps instance
var Builtins = BuiltinOps{}

// Range provides Python-like range functionality
type Range struct {
	Start int
	Stop  int
	Step  int
}

// NewRange creates a new range with start, stop, step
func NewRange(args ...int) Range {
	switch len(args) {
	case 1:
		return Range{Start: 0, Stop: args[0], Step: 1}
	case 2:
		return Range{Start: args[0], Stop: args[1], Step: 1}
	case 3:
		return Range{Start: args[0], Stop: args[1], Step: args[2]}
	default:
		panic("range() requires 1-3 arguments")
	}
}

// ToSlice converts range to integer slice
func (r Range) ToSlice() []int {
	if r.Step == 0 {
		panic("range() step cannot be zero")
	}

	result := []int{}
	if r.Step > 0 {
		for i := r.Start; i < r.Stop; i += r.Step {
			result = append(result, i)
		}
	} else {
		for i := r.Start; i > r.Stop; i += r.Step {
			result = append(result, i)
		}
	}
	return result
}

// ForEach executes function for each value in range
func (r Range) ForEach(fn func(int)) {
	if r.Step == 0 {
		panic("range() step cannot be zero")
	}

	if r.Step > 0 {
		for i := r.Start; i < r.Stop; i += r.Step {
			fn(i)
		}
	} else {
		for i := r.Start; i > r.Stop; i += r.Step {
			fn(i)
		}
	}
}

// Generic comprehension functions

// ListComprehension creates slice by applying transform function to each element
func ListComprehension[T any, R any](source []T, transform func(T) R) []R {
	result := make([]R, 0, len(source))
	for _, item := range source {
		result = append(result, transform(item))
	}
	return result
}

// ListComprehensionFromRange creates slice from a Range
func ListComprehensionFromRange[R any](source Range, transform func(int) R) []R {
	result := []R{}
	source.ForEach(func(i int) {
		result = append(result, transform(i))
	})
	return result
}

// ListComprehensionWithFilter creates slice with filtering
func ListComprehensionWithFilter[T any, R any](source []T, transform func(T) R, filter func(T) bool) []R {
	result := []R{}
	for _, item := range source {
		if filter(item) {
			result = append(result, transform(item))
		}
	}
	return result
}

// ListComprehensionFromRangeWithFilter creates filtered slice from a Range
func ListComprehensionFromRangeWithFilter[R any](source Range, transform func(int) R, filter func(int) bool) []R {
	result := []R{}
	source.ForEach(func(i int) {
		if filter(i) {
			result = append(result, transform(i))
		}
	})
	return result
}

// DictComprehension creates map by applying transform function
func DictComprehension[T any, K comparable, V any](source []T, transform func(T) (K, V)) map[K]V {
	result := make(map[K]V)
	for _, item := range source {
		k, v := transform(item)
		result[k] = v
	}
	return result
}

// DictComprehensionFromRange creates map from a Range
func DictComprehensionFromRange[K comparable, V any](source Range, transform func(int) (K, V)) map[K]V {
	result := make(map[K]V)
	source.ForEach(func(i int) {
		k, v := transform(i)
		result[k] = v
	})
	return result
}

// KV represents a key-value pair
type KV[K comparable, V any] struct {
	Key   K
	Value V
}

// MapItems converts a map to a slice of key-value pairs
func MapItems[K comparable, V any](m map[K]V) []KV[K, V] {
	result := make([]KV[K, V], 0, len(m))
	for k, v := range m {
		result = append(result, KV[K, V]{Key: k, Value: v})
	}
	return result
}

// MapValues returns a slice of all values from a map
func MapValues[K comparable, V any](m map[K]V) []V {
	result := make([]V, 0, len(m))
	for _, v := range m {
		result = append(result, v)
	}
	return result
}

// DictComprehensionWithFilter creates map with filtering
func DictComprehensionWithFilter[T any, K comparable, V any](source []T, transform func(T) (K, V), filter func(T) bool) map[K]V {
	result := make(map[K]V)
	for _, item := range source {
		if filter(item) {
			k, v := transform(item)
			result[k] = v
		}
	}
	return result
}

// SetComprehension creates map[T]bool set by applying transform function
func SetComprehension[T any, K comparable](source []T, transform func(T) K) map[K]bool {
	result := make(map[K]bool)
	for _, item := range source {
		result[transform(item)] = true
	}
	return result
}

// SetComprehensionFromRange creates set from a Range
func SetComprehensionFromRange[K comparable](source Range, transform func(int) K) map[K]bool {
	result := make(map[K]bool)
	source.ForEach(func(i int) {
		result[transform(i)] = true
	})
	return result
}

// SetComprehensionFromSet creates a new set by applying transform to elements of an existing set
func SetComprehensionFromSet[T comparable, K comparable](source map[T]bool, transform func(T) K) map[K]bool {
	result := make(map[K]bool)
	for key := range source {
		result[transform(key)] = true
	}
	return result
}

// SetComprehensionFromSetWithFilter creates a new set by applying transform to filtered elements
func SetComprehensionFromSetWithFilter[T comparable, K comparable](source map[T]bool, filter func(T) bool, transform func(T) K) map[K]bool {
	result := make(map[K]bool)
	for key := range source {
		if filter(key) {
			result[transform(key)] = true
		}
	}
	return result
}

// Legacy ComprehensionOps struct for backwards compatibility
type ComprehensionOps struct{}

// Global ComprehensionOps instance
var Comprehensions = ComprehensionOps{}

// Helper functions

// ToStr converts various types to string (Python str() equivalent)
func ToStr(x interface{}) string {
	if x == nil {
		return "None"
	}

	switch v := x.(type) {
	case string:
		return v
	case bool:
		if v {
			return "True"
		}
		return "False"
	case int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64:
		return fmt.Sprintf("%d", v)
	case float32, float64:
		return fmt.Sprintf("%g", v)
	default:
		return fmt.Sprintf("%v", v)
	}
}

// Print provides Python-like print function
func Print(args ...interface{}) {
	strs := make([]string, len(args))
	for i, arg := range args {
		strs[i] = ToStr(arg)
	}
	fmt.Println(strings.Join(strs, " "))
}