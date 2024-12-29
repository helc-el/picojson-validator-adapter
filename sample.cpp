// Copyright (C) 2024 helc-el

#include <iostream>
#include <string>

#include "include/picojson.h"
#include "include/picojson_validator_adapter.hpp"

int main () {
  // Target JSON data
  const std::string json_str = R"JSON({
    "name": "John Doe",
    "age": 30,
    "address": {
      "city": "Tokyo",
      "country": "Japan"
    }
  })JSON";

  // Parse JSON data
  picojson::value json_value;
  std::string err = picojson::parse(json_value, json_str);
  if (!err.empty()) {
    std::cerr << "JSON parse error: " << err << std::endl;
    return 1;
  }

  /* With validator */
  {
    using namespace picojson::validator::adapter;  // NOLINT

    // Create Validator
    auto validator =
        object() & std::make_pair("name", string()) &
        std::make_pair("age", number([] (double age) { return age >= 0; })) &
        std::make_pair("address", object() & std::make_pair("city", string()) &
                                      std::make_pair("country", string()));

    picojson::validator::ValidatorContainer vld = validator;

    auto x = picojson::validator::ValueValidator(vld) &&
             object() & std::make_pair("is_admin", boolean(true));
    auto y = vld && object() & std::make_pair("is_admin", boolean(true));

    // Validate JSON value
    std::string result = validator.validate(json_value, "json_value");

    if (result.empty()) {
      std::cout << "Validation successful." << std::endl;
    } else {
      std::cerr << "Validation failed: " << result << std::endl;
      return 1;
    }
  }

  /* Without validator */
  {
    std::string result;
    if (!json_value.is<picojson::object>()) {
      result = "`json_value` is not Object.";
    } else {
      const auto& obj = json_value.get<picojson::object>();

      if (obj.contains("name")) {
        if (!obj.at("name").is<std::string>()) {
          result = "`json_value[\"name\"] is not String.";
        }
      } else {
        result = "`json_value[\"name\"] does not exist.";
      }

      if (result.empty()) {
        if (obj.contains("age")) {
          if (!obj.at("age").is<double>()) {
            result = "`json_value[\"age\"] is not Number.";
          } else if (obj.at("age").get<double>() < 0) {
            result = "`json_value[\"age\"] is invalid Number value.";
          }
        } else {
          result = "`json_value[\"age\"] does not exist.";
        }
      }

      if (result.empty()) {
        if (obj.contains("address")) {
          if (!obj.at("address").is<picojson::object>()) {
            result = "`json_value[\"address\"] is not Object.";
          } else {
            const auto& address = obj.at("address").get<picojson::object>();

            if (address.contains("city")) {
              if (!address.at("city").is<std::string>()) {
                result = "`json_value[\"address\"][\"city\"] is not String.";
              }
            } else {
              result = "`json_value[\"address\"][\"city\"] does not exist.";
            }

            if (result.empty()) {
              if (address.contains("country")) {
                if (!address.at("country").is<std::string>()) {
                  result =
                      "`json_value[\"address\"][\"country\"] is not String.";
                }
              } else {
                result =
                    "`json_value[\"address\"][\"country\"] does not exist.";
              }
            }
          }
        } else {
          result = "`json_value[\"address\"] does not exist.";
        }
      }
    }

    if (result.empty()) {
      std::cout << "Validation successful." << std::endl;
    } else {
      std::cerr << "Validation failed: " << result << std::endl;
      return 1;
    }
  }

  return 0;
}
