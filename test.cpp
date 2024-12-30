// Copyright (C) 2024 helc-el

#include <concepts>
#include <cstdlib>
#include <iostream>
#include <string>
#include <utility>

#define PICOJSON_USE_INT64
#include "include/picojson.h"
#include "include/picojson_validator_adapter.hpp"

using namespace picojson::validator::adapter;  // NOLINT
using picojson::validator::ValidatorContainer;

template <typename T>
  requires (!std::convertible_to<T, std::string>)
void validator_test (T data, const ValidatorContainer& validator) {
  // Create JSON data
  picojson::value json_value(data);

  // Validate JSON value
  std::string result = validator.validate(json_value, "json_value");

  if (result.empty()) {
    std::cout << "Validation successful." << std::endl;
  } else {
    std::cout << "Validation failed: " << result << std::endl;
  }
}

template <typename T>
  requires std::convertible_to<T, std::string>
void validator_test (const T& json_string,
                     const ValidatorContainer& validator) {
  // Parse JSON data
  picojson::value json_value;
  std::string err = picojson::parse(json_value, json_string);
  if (!err.empty()) {
    std::cerr << "JSON parse error: " << err << std::endl;
    std::exit(1);
  }

  // Validate JSON value
  std::string result = validator.validate(json_value, "json_value");

  if (result.empty()) {
    std::cout << "Validation successful." << std::endl;
  } else {
    std::cout << "Validation failed: " << result << std::endl;
  }
}

int main () {
  // Target JSON data
  const std::string true_json_string = R"({
    "name": "John Doe",
    "age": 30,
    "address": {
      "city": "Tokyo",
      "country": "Japan"
    }
  })";
  const std::string incomplete_json_string = R"({
    "age": 30,
    "address": {
      "city": "Tokyo",
      "country": "Japan"
    }
  })";

  // Parse JSON data
  picojson::value true_json_value;
  std::string err = picojson::parse(true_json_value, true_json_string);
  if (!err.empty()) {
    std::cerr << "JSON parse error: " << err << std::endl;
    std::exit(1);
  }

  std::cout << "<Any>\n";
  {
    // constexpr auto AnyValidatorFn::operator() () const
    auto validator = any();
    validator_test(true_json_string, validator);
  }
  {
    // auto AnyValidatorFn::operator() (const picojson::value true_value) const
    auto validator = any(true_json_value);
    validator_test(true_json_string, validator);
    validator_test(incomplete_json_string, validator);
  }
  {
    // constexpr auto AnyValidatorFn::operator() (Pred&& value_validator) const
    auto validator =
        any([] (const picojson::value& v) { return v.is<picojson::object>(); });
    validator_test(true_json_string, validator);
    validator_test(1.234, validator);
  }
  {
    // constexpr auto AnyValidatorFn::operator()
    //     (Validator&& value_validator) const
    auto validator = any([] (const picojson::value& v) -> std::string {
      return v.is<picojson::object>() ? "" : "The value should be Object type.";
    });
    validator_test(true_json_string, validator);
    validator_test("[\"json_str\"]", validator);
  }

  std::cout << "\n<Null>\n";
  {
    // constexpr auto NullValidatorFn::operator() () const
    auto validator = null();
    validator_test("null", validator);
    validator_test(true_json_string, validator);
  }

  std::cout << "\n<Boolean>\n";
  {
    // constexpr auto BooleanValidatorFn::operator() () const
    auto validator = boolean();
    validator_test(true, validator);
    validator_test(true_json_string, validator);
  }
  {
    // constexpr auto BooleanValidatorFn::operator() (const bool true_value)
    // const
    auto validator = boolean(false);
    validator_test(false, validator);
    validator_test(true, validator);
  }

  std::cout << "\n<Number>\n";
  {
    // constexpr auto NumberValidatorFn::operator() () const
    auto validator = number();
    validator_test(3.14, validator);
    validator_test(true, validator);
  }
  {
    // constexpr auto NumberValidatorFn::operator()
    //     (const double true_value) const
    auto validator = number(1.4);
    validator_test(1.4, validator);
    validator_test(0.0, validator);
  }
  {
    // constexpr auto NumberValidatorFn::operator()
    //     (Pred&& value_predicator) const
    auto validator =
        number([] (const double x) { return x >= 0.0 && x < 1.0; });
    validator_test(0.89, validator);
    validator_test(1.5, validator);
  }
  {
    // constexpr auto NumberValidatorFn::operator()
    //     (Validator&& value_validator) const
    auto validator = number([] (const double x) -> std::string {
      return x >= 0.0 ? "" : "The value must be not negative.";
    });
    validator_test(0.89, validator);
    validator_test(-54.321, validator);
  }

  std::cout << "\n<Int64>\n";
  {
    // constexpr auto Int64ValidatorFn::operator() () const
    auto validator = int64();
    validator_test(1024ll, validator);
    validator_test(1.5, validator);
  }
  {
    // constexpr auto Int64ValidatorFn::operator()
    //     (const int64_t true_value) const
    auto validator = int64(100);
    validator_test(100ll, validator);
    validator_test(200ll, validator);
  }
  {
    // constexpr auto Int64ValidatorFn::operator()
    //     (Pred&& value_predicator) const
    auto validator = int64([] (const std::int64_t x) { return x > 0; });
    validator_test(12ll, validator);
    validator_test(0ll, validator);
  }
  {
    // constexpr auto Int64ValidatorFn::operator()
    //     (Validator&& value_validator) const
    auto validator = int64([] (const std::int64_t x) -> std::string {
      return x % 2 == 0 ? "" : "The value must be even.";
    });
    validator_test(48ll, validator);
    validator_test(35ll, validator);
  }

  std::cout << "\n<String>\n";
  {
    // constexpr auto StringValidatorFn::operator() () const
    auto validator = string();
    validator_test("\"hello world.\"", validator);
    validator_test(true_json_string, validator);
  }
  {
    // constexpr auto StringValidatorFn::operator()
    //     (std::string_view true_value) const
    auto validator = string("I'm John Doe.");
    validator_test("\"I'm John Doe.\"", validator);
    validator_test("null", validator);
  }
  {
    // constexpr auto StringValidatorFn::operator()
    //     (Pred&& value_predicator) const
    auto validator = string(
        [] (const std::string& x) { return x.size() >= 1 && x[0] == 'A'; });
    validator_test("\"Ampere\"", validator);
    validator_test("\"Delta\"", validator);
  }
  {
    // constexpr auto StringValidatorFn::operator()
    //     (Validator&& value_validator) const
    auto validator = string([] (const std::string& x) -> std::string {
      return x.size() == 8 ? "" : "The value must be 8 length string.";
    });
    validator_test("\"abcdefgh\"", validator);
    validator_test("\"xyz\"", validator);
  }

  std::cout << "\n<Array>\n";
  {
    // constexpr auto StringValidatorFn::operator() () const
    auto validator = array();
    validator_test("[true, null, 1]", validator);
    validator_test("{\"format\": \"alpha\"}", validator);
  }
  {
    // constexpr auto StringValidatorFn::operator()
    //     (const picojson::array& true_value) const
    auto validator = array(picojson::array(
        {picojson::value(1.1), picojson::value(2.1), picojson::value(3.1)}));
    validator_test("[1.1,2.1,3.1]", validator);
    validator_test("[1,2,3]", validator);
  }
  {
    // constexpr auto StringValidatorFn::operator()
    //     (Pred&& value_predicator) const
    auto validator =
        array([] (const picojson::array& arr) { return !arr.empty(); });
    validator_test("[{}]", validator);
    validator_test("[]", validator);
  }
  {
    // constexpr auto StringValidatorFn::operator()
    //     (Validator&& value_validator) const
    auto validator = array([] (const picojson::array& arr) -> std::string {
      return arr.size() == 3 ? "" : "The value length must be 3.";
    });
    validator_test("[{}, null, \"temp\"]", validator);
    validator_test("[]", validator);
  }

  {
    auto validator =
        array() & std::make_pair(0, number(1)) | (number() || string());
    validator_test("[1, \"foo\", -32]", validator);
    validator_test("[1, false]", validator);
  }

  std::cout << "\n<Object>\n";
  {
    // constexpr auto ObjectValidatorFn::operator() () const
    auto validator = object();
    validator_test("{\"x\": 10, \"y\": 3}", validator);
    validator_test("\"The quick brown fox jumps over the lazy dog\"",
                   validator);
  }
  {
    // auto ObjectValidatorFn::operator()
    //     (const picojson::object& true_value) const
    auto validator = object(true_json_value.get<picojson::object>());
    validator_test(true_json_string, validator);
    validator_test("{\"length\": 5, \"0\": true}", validator);
  }
  {
    // constexpr auto ObjectValidatorFn::operator()
    //     (Pred&& value_predicator) const
    auto validator = object(
        [] (const picojson::object& obj) { return obj.contains("version"); });
    validator_test("{\"version\": [1,12,20]}", validator);
    validator_test("{\"language\": \"en\"}", validator);
  }
  {
    // constexpr auto ObjectValidatorFn::operator()
    //     (Validator&& value_validator) const
    auto validator = object([] (const picojson::object& obj) -> std::string {
      return obj.contains("version") ? ""
                                     : "The value must have `version` array.";
    });
    validator_test("{\"version\": [1,12,20]}", validator);
    validator_test("{\"language\": \"en\"}", validator);
  }

  {
    auto validator = object() & std::make_pair("name", string()) &&
                     object() & std::make_pair("age", number()) |
                         (number() || string() || object());
    validator_test(true_json_string, validator);
    validator_test(incomplete_json_string, validator);
  }

  return 0;
}
