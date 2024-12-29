# PicoJSON Validator Adapter

Copyright (C) 2024 helc-el

Licensed under [MIT Lisence](https://opensource.org/license/mit)

## Introduction

PicoJSON Validator Adapter is a header-file only adapter for [PicoJSON](https://github.com/kazuho/picojson), which enables us to create PicoJSON value validator easily.
This library uses C++20 features.

## How to use

### Include

```c++
#define PICOJSON_USE_INT64  // Optional.
#include <folder_path/picojson.h>
#include <folder_path/picojson_validator_adapter.hpp>
```

To use this library, download `picojson.h` from [PicoJSON](https://github.com/kazuho/picojson) and `picojson_validator_adapter.hpp` in `include` folder to your workspace. Then include these files in the same order.
If `picojson.h` and `picojson_validator_adapter.hpp` are in the same folder, you can include only the later file.

It also supports PicoJSON's `int64_t` feature. To enable this feature, define the macro `PICOJSON_USE_INT64` before including the header.

### Use case

```c++
using namespace picojson::validator::adapter;
using picojson::validator::ValidatorContainer;

// Create Validator
auto validator =
    object() & std::make_pair("name", string()) &
    std::make_pair("age", number([] (double age) { return age >= 0; })) &
    std::make_pair("address", object() & std::make_pair("city", string()) &
                                  std::make_pair("country", string()));

// Non-template validator container
ValidatorContainer container = validator;

// Validate JSON value
std::string result = validator.validate(json_value, "json_value");
```

#### Validator factory objects

There are 6 (or 7) validator factory object: `any`, `null`, `boolean`, `number`, (`int64`), `string`, `array`, and `object`.
These validator factory objects are correspond to `picojson::value`, `picojson::null`, `bool`, `double`, (`int64_t`), `std::string`, `picojson::array`, and `picojson::object`.
Factory objects are declared in the namespace `picojson::validator::adapters`.

#### `picojson::validator::ValidatorContainer`

The `ValidatorContainer` is a non-template container designed to hold a validator as a portable variable. This allows you to prepare the container before assigning a specific validator to it. For example:

```c++
picojson::validator::ValidatorContainer validator;
{ // Prevent namespace pollution by limiting the scope
  using namespace picojson::validator::adapter;
  // Assign a specific validator to the container
}
```

This approach is particularly useful when you want to avoid global namespace pollution caused by `using namespace`.

#### Validate

To validate the value of `picojson::value`, use `validate` method of either created validator or `picojson::validator::ValidatorContainer`.
It returns empty string if validation succeeded, otherwise returns failure reason.

#### Validation Error Handling

When a validator detects an error during validation, it immediately stops and returns the first encountered error. If multiple errors exist, only the first one will be reported.

### Other

Sample code is in `sample.cpp`.
This shows an example of practical use of this library.

## Combining validators

Validators can be combined by logical AND operator (`&&`) and logical OR operator (`||`).
These behave as expected.

Object validators (`array` and `object`) can be continued with AND operator (`&`) and OR operator (`|`).  
The RHS value of AND operator (`&`) is an object of `std::pair` of first element is the key of `picojson::array` or `picojson::object`, and second element is validator object.
AND operator returns Object validator.  
The RHS value of OR operator (`|`) is validator, and OR operator returns non object validator.

---

## Reference for validator factory objects

In validator factory objects, a concept `StringResultValidator` is used. The definition is shown below.

```c++
namespace picojson::validator::concepts {

  template <typename F, typename T>
  concept StringResultValidator =
      std::constructible_from<std::decay_t<F>, F> &&
      std::regular_invocable<F, T> &&
      std::convertible_to<std::invoke_result_t<F, T>, std::string>;

}
```

Each factory objects have some function-call operators. The role of each Function-call operator is following:
  - (1) Check value type;
  - (2) Check value equals to the argument;
  - (3) Check value satisfies boolean predicate function;
  - (4) Check value with predicate function with reason.

Use cases are in `test.cpp`.

### `any`

```c++
namespace picojson::validator::adapter {
  struct AnyValidatorFn {
    constexpr auto operator() () const; // (1)

    auto operator() (const picojson::value true_value) const; // (2)

    template <typename Pred>
      requires std::predicate<Pred, picojson::value> && (!concepts::StringResultValidator<Pred, picojson::value>)
    constexpr auto operator() (Pred&& value_predicator) const;  // (3)

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, picojson::value>
    constexpr auto operator() (Validator&& value_validator) const;  // (4)
  };

  constexpr inline AnyValidatorFn any;
}
```

### `null`

```c++
namespace picojson::validator::adapter {
  struct NullValidatorFn {
    constexpr auto operator() () const; // (1)
  };

  constexpr inline NullValidatorFn null;
}
```

### `boolean`

```c++
namespace picojson::validator::adapter {
  struct BooleanValidatorFn {
    constexpr auto operator() () const; // (1)

    constexpr auto operator() (const bool true_value) const;  // (2)
  };

  constexpr inline BooleanValidatorFn boolean;
}
```

### `number`

```c++
namespace picojson::validator::adapter {
  struct NumberValidatorFn {
    constexpr auto operator() () const; // (1)

    constexpr auto operator() (const double true_value) const;  // (2)

    template <typename Pred>
      requires std::predicate<Pred, double> && (!concepts::StringResultValidator<Pred, double>)
    constexpr auto operator() (Pred&& value_predicator) const;  // (3)

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, double>
    constexpr auto operator() (Validator&& value_validator) const;  // (4)
  };

  constexpr inline NumberValidatorFn number;
}
```

### `int64`

```c++
namespace picojson::validator::adapter {
  struct Int64ValidatorFn {
    constexpr auto operator() () const; // (1)

    constexpr auto operator() (const int64_t true_value) const; // (2)

    template <typename Pred>
      requires std::predicate<Pred, int64_t> && (!concepts::StringResultValidator<Pred, int64_t>)
    constexpr auto operator() (Pred&& value_predicator) const;  // (3)

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, int64_t>
    constexpr auto operator() (Validator&& value_validator) const;  // (4)
  };

  constexpr inline Int64ValidatorFn int64;
}
```

### `string`

```c++
namespace picojson::validator::adapter {
  struct StringValidatorFn {
    constexpr auto operator() () const; // (1)

    constexpr auto operator() (std::string_view true_value) const;  // (2)

    template <typename Pred>
      requires std::predicate<Pred, std::string> && (!concepts::StringResultValidator<Pred, std::string>)
    constexpr auto operator() (Pred&& value_predicator) const;  // (3)

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, std::string>
    constexpr auto operator() (Validator&& value_validator) const;  // (4)
  };

  constexpr inline StringValidatorFn string;
}
```

### `array`

```c++
namespace picojson::validator::adapter {
  struct ArrayValidatorFn {
    constexpr auto operator() () const; // (1)

    constexpr auto operator() (const picojson::array& true_value) const;  // (2)

    template <typename Pred>
      requires std::predicate<Pred, picojson::array> && (!concepts::StringResultValidator<Pred, picojson::array>)
    constexpr auto operator() (Pred&& value_predicator) const;  // (3)

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, picojson::array>
    constexpr auto operator() (Validator&& value_validator) const;  // (4)
  };

  constexpr inline ArrayValidatorFn array;
}
```

### `object`

```c++
namespace picojson::validator::adapter {
  struct ObjectValidatorFn {
    constexpr auto operator() () const; // (1)

    auto operator() (const picojson::object& true_value) const; // (2)

    template <typename Pred>
      requires std::predicate<Pred, picojson::object> && (!concepts::StringResultValidator<Pred, object>)
    constexpr auto operator() (Pred&& value_predicator) const;  // (3)

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, picojson::object>
    constexpr auto operator() (Validator&& value_validator) const;  // (4)
  };

  constexpr inline ObjectValidatorFn object;
}
```
