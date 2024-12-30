/*
 * PicoJSON Validator Adapter for C++20
 *
 * The MIT License (MIT)
 *
 * Copyright (C) 2024 helc-el
 *
 * Distributed under the MIT License.
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * provided to do so, subject to the following conditions:
 * - The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * This library depends on PicoJSON.
 * Please use this library under the environment where PicoJSON can be used.
 * PicoJSON is licensed under the 2-Clause BSD License. The full text of the
 * license is included in the original source.
 */

#ifndef PICOJSON_VALIDATOR_ADAPTER_HPP_
#define PICOJSON_VALIDATOR_ADAPTER_HPP_

// temporary
// #define PICOJSON_USE_INT64

#ifndef picojson_h
#  include "picojson.h"
#endif

#include <concepts>     // constructible_from reular_invocable convertible_to
#include <cstddef>      // size_t
#include <format>       // format
#include <functional>   // function
#include <string>       // string
#include <string_view>  // string_view
#include <type_traits>  // decay_t invoke_result_t
#include <utility>      // move forward pair make_pair declval

namespace picojson::validator {

namespace concepts {

  template <typename F, typename T>
  concept StringResultValidator =
      std::constructible_from<std::decay_t<F>, F> &&
      std::regular_invocable<F, T> &&
      std::convertible_to<std::invoke_result_t<F, T>, std::string>;

  template <typename F>
  concept PicojsonValueValidator = StringResultValidator<F, picojson::value>;

  template <typename T>
  concept PicojsonArrayKey = std::convertible_to<T, picojson::array::size_type>;

  template <typename T>
  concept PicojsonObjectKey =
      std::convertible_to<T, picojson::object::key_type>;

}  // namespace concepts

template <typename T = void, typename... Args>
struct EmptyStringFunctor {
  using result_type = std::string;

  constexpr result_type operator() (T, Args...) const
      noexcept(noexcept(std::string{})) {
    return result_type{};
  }
};

template <>
struct EmptyStringFunctor<void> {
  using result_type = std::string;
  using is_transparent = int;

  template <typename... Args>
  constexpr result_type operator() (Args...) const
      noexcept(noexcept(std::string{})) {
    return result_type{};
  }
};

class ValidatorContainer;

template <typename Key, typename TypeFn, typename ValueFn>
  requires (concepts::PicojsonArrayKey<Key> ||
            concepts::PicojsonObjectKey<Key>) &&
           concepts::PicojsonValueValidator<TypeFn> &&
           concepts::PicojsonValueValidator<ValueFn>
class ObjectValidatorWrapper;

template <typename TypeFn, typename ValueFn>
  requires concepts::PicojsonValueValidator<TypeFn> &&
           concepts::PicojsonValueValidator<ValueFn>
struct ValueValidator {
  using type_validator_type = std::decay_t<TypeFn>;
  using value_validator_type = std::decay_t<ValueFn>;
  using validate_result_type = std::string;

  constexpr ValueValidator (type_validator_type type_validator,
                            value_validator_type value_validator)
      : type_validator(std::move(type_validator)),
        value_validator(std::move(value_validator)) {}

  explicit ValueValidator(const ValidatorContainer& rhs);

  template <typename Key>
  constexpr explicit ValueValidator(
      ObjectValidatorWrapper<Key, TypeFn, ValueFn>&);

  template <typename Key>
  constexpr explicit ValueValidator(
      ObjectValidatorWrapper<Key, TypeFn, ValueFn>&&);

  constexpr ValueValidator(const ValueValidator&) = default;

  constexpr ValueValidator(ValueValidator&&) = default;

  constexpr ~ValueValidator() = default;

  [[nodiscard]] validate_result_type validate (
      const picojson::value& value, std::string_view position = "root") const {
    std::string result = type_validator(value);
    if (result.empty()) {
      result = value_validator(value);
    }
    if (!result.empty()) {
      result = std::format("`{}{}", position, result);
    }
    return result;
  }

  type_validator_type type_validator;
  value_validator_type value_validator;
};

class ValidatorContainer {
 public:
  using validate_result_type = std::string;
  using validator_type = std::function<std::string(const picojson::value&)>;

 public:
  ValidatorContainer() = default;
  ValidatorContainer(const ValidatorContainer&) = default;
  ValidatorContainer(ValidatorContainer&&) = default;
  ValidatorContainer& operator= (const ValidatorContainer&) = default;
  ValidatorContainer& operator= (ValidatorContainer&&) = default;

  template <typename TypeFn, typename ValueFn>
  ValidatorContainer(ValueValidator<TypeFn, ValueFn> rhs)
      : type_validator_(std::move(rhs.type_validator)),
        value_validator_(std::move(rhs.value_validator)) {}

  template <typename Key, typename TypeFn, typename ValueFn>
  ValidatorContainer(ObjectValidatorWrapper<Key, TypeFn, ValueFn> rhs)
      : ValidatorContainer(std::move(rhs).get_container()) {}

  template <typename TypeFn, typename ValueFn>
    requires concepts::PicojsonValueValidator<TypeFn> &&
             concepts::PicojsonValueValidator<ValueFn>
  friend ValueValidator<TypeFn, ValueFn>::ValueValidator(
      const ValidatorContainer&);

 public:
  operator bool () const { return type_validator_ && value_validator_; }

  [[nodiscard]] validate_result_type validate (
      const picojson::value& value, std::string_view position = "root") const {
    if (!(*this)) {
      return {};
    }
    std::string result = type_validator_(value);
    if (result.empty()) {
      result = value_validator_(value);
    }
    if (!result.empty()) {
      result = std::format("`{}{}", position, result);
    }
    return result;
  }

 private:
  validator_type type_validator_{nullptr};
  validator_type value_validator_{nullptr};
};

template <typename TypeFn, typename ValueFn>
  requires concepts::PicojsonValueValidator<TypeFn> &&
           concepts::PicojsonValueValidator<ValueFn>
inline ValueValidator<TypeFn, ValueFn>::ValueValidator(
    const ValidatorContainer& rhs) {
  if (rhs) {
    this->type_validator = rhs.type_validator_;
    this->value_validator = rhs.value_validator_;
  } else {
    this->type_validator = EmptyStringFunctor{};
    this->value_validator = EmptyStringFunctor{};
  }
}

explicit ValueValidator(const ValidatorContainer&)
    -> ValueValidator<ValidatorContainer::validator_type,
                      ValidatorContainer::validator_type>;

template <typename Key, typename TypeFn, typename ValueFn>
  requires (concepts::PicojsonArrayKey<Key> ||
            concepts::PicojsonObjectKey<Key>) &&
           concepts::PicojsonValueValidator<TypeFn> &&
           concepts::PicojsonValueValidator<ValueFn>
class ObjectValidatorWrapper {
 public:
  using key_type = Key;
  using type_validator_type = std::decay_t<TypeFn>;
  using value_validator_type = std::decay_t<ValueFn>;
  using container_type = ValueValidator<TypeFn, ValueFn>;
  using validate_result_type = std::string;

 public:
  constexpr ObjectValidatorWrapper (type_validator_type type_validator,
                                    value_validator_type value_validator = {})
      : validator_{std::move(type_validator), std::move(value_validator)} {}

  constexpr ObjectValidatorWrapper(const ObjectValidatorWrapper&) = default;

  constexpr ObjectValidatorWrapper(ObjectValidatorWrapper&&) = default;

  constexpr ~ObjectValidatorWrapper() = default;

 public:
  constexpr container_type& get_container () const& { return validator_; }
  constexpr container_type& get_container () & { return validator_; }
  constexpr container_type get_container () && { return std::move(validator_); }

  template <typename Key1, typename TypeFn1, typename ValueFn1, typename Key2,
            typename TypeFn2, typename ValueFn2>
    requires std::convertible_to<Key2, Key1>
  friend constexpr auto operator& (
      ObjectValidatorWrapper<Key1, TypeFn1, ValueFn1> lhs,
      std::pair<Key2, ValueValidator<TypeFn2, ValueFn2>> rhs);

  template <typename Key1, typename TypeFn1, typename ValueFn1,
            typename TypeFn2, typename ValueFn2>
  friend constexpr auto operator| (
      ObjectValidatorWrapper<Key1, TypeFn1, ValueFn1> lhs,
      ValueValidator<TypeFn2, ValueFn2> rhs);

  [[nodiscard]] validate_result_type validate (
      const picojson::value& value, std::string_view position = "root") const {
    std::string result = validator_.type_validator(value);
    if (result.empty()) {
      result = validator_.value_validator(value);
    }
    if (!result.empty()) {
      result = std::format("`{}{}", position, result);
    }
    return result;
  }

 private:
  container_type validator_;
};

template <typename TypeFn, typename ValueFn>
  requires concepts::PicojsonValueValidator<TypeFn> &&
           concepts::PicojsonValueValidator<ValueFn>
template <typename Key>
constexpr ValueValidator<TypeFn, ValueFn>::ValueValidator(
    ObjectValidatorWrapper<Key, TypeFn, ValueFn>& rhs)
    : ValueValidator(rhs.get_container()) {}

template <typename TypeFn, typename ValueFn>
  requires concepts::PicojsonValueValidator<TypeFn> &&
           concepts::PicojsonValueValidator<ValueFn>
template <typename Key>
constexpr ValueValidator<TypeFn, ValueFn>::ValueValidator(
    ObjectValidatorWrapper<Key, TypeFn, ValueFn>&& rhs)
    : ValueValidator(std::move(rhs).get_container()) {}

template <typename Key1, typename TypeFn1, typename ValueFn1, typename Key2,
          typename TypeFn2, typename ValueFn2>
  requires std::convertible_to<Key2, Key1>
[[nodiscard]] constexpr auto operator& (
    ObjectValidatorWrapper<Key1, TypeFn1, ValueFn1> lhs,
    std::pair<Key2, ValueValidator<TypeFn2, ValueFn2>> rhs) {
  auto value_lambda =
      [lhs_value_validator = std::move(lhs.validator_.value_validator),
       rhs_ = std::move(rhs)] (const picojson::value& value) -> std::string {
    auto&& [key, validator_container] = std::move(rhs_);
    std::string result;
    result = lhs_value_validator(value);
    if (result.empty()) {
      if constexpr (concepts::PicojsonArrayKey<Key1>) {
        const auto container = value.get<picojson::array>();
        try {
          const auto& value = container.at(key);
          result = validator_container.type_validator(value);
          if (result.empty()) {
            result = validator_container.value_validator(value);
          }
        } catch (...) {
          result = std::format("[{}]` is out of range.", key);
        }
      } else if constexpr (concepts::PicojsonObjectKey<Key1>) {
        const auto container = value.get<picojson::object>();
        try {
          const auto& value = container.at(key);
          result = validator_container.type_validator(value);
          if (result.empty()) {
            result = validator_container.value_validator(value);
          }
        } catch (...) {
          result = std::format(R"(["{}"]` does not exist.)", key);
        }
      }
      if (!result.empty()) {
        result = std::format(R"(["{}"]{})", key, result);
      }
    }
    return result;
  };
  return ObjectValidatorWrapper<Key1, decltype(lhs.validator_.type_validator),
                                decltype(value_lambda)>{
      std::move(lhs.validator_.type_validator), std::move(value_lambda)};
}

template <typename Key1, typename TypeFn1, typename ValueFn1, typename Key2,
          typename Validator>
  requires std::convertible_to<Key2, Key1> &&
           requires { ValueValidator{std::declval<Validator>()}; }
[[nodiscard]] constexpr auto operator& (
    ObjectValidatorWrapper<Key1, TypeFn1, ValueFn1> lhs,
    std::pair<Key2, Validator> rhs) {
  return std::move(lhs) & std::make_pair(std::move(rhs.first),
                                         ValueValidator{std::move(rhs.second)});
}

template <typename Key1, typename TypeFn1, typename ValueFn1, typename TypeFn2,
          typename ValueFn2>
[[nodiscard]] constexpr auto operator| (
    ObjectValidatorWrapper<Key1, TypeFn1, ValueFn1> lhs,
    ValueValidator<TypeFn2, ValueFn2> rhs) {
  auto value_lambda =
      [lhs_validator = std::move(lhs.validator_.value_validator),
       rhs_ = std::move(rhs)] (const picojson::value& value) -> std::string {
    std::string result;
    result = lhs_validator(value);
    if (result.empty()) {
      if constexpr (std::unsigned_integral<Key1>) {
        const auto container = value.get<picojson::array>();
        for (std::size_t i{0ull}; i < container.size(); i++) {
          result = rhs_.type_validator(container[i]);
          if (result.empty()) {
            result = rhs_.value_validator(container[i]);
          }
          if (!result.empty()) {
            result = std::format("[{}]{}", i, result);
            break;
          }
        }
      } else if constexpr (std::convertible_to<std::string, Key1>) {
        const auto container = value.get<picojson::object>();
        for (const auto& [key, elm] : container) {
          result = rhs_.type_validator(elm);
          if (result.empty()) {
            result = rhs_.value_validator(elm);
          }
          if (!result.empty()) {
            result = std::format(R"(["{}"]{})", key, result);
            break;
          }
        }
      }
    }
    return result;
  };
  return ValueValidator<decltype(lhs.validator_.type_validator),
                        decltype(value_lambda)>{
      std::move(lhs.validator_.type_validator), std::move(value_lambda)};
}

template <typename Key1, typename TypeFn1, typename ValueFn1,
          typename Validator>
  requires requires { ValueValidator{std::declval<Validator>()}; }
[[nodiscard]] constexpr auto operator| (
    ObjectValidatorWrapper<Key1, TypeFn1, ValueFn1> lhs, Validator rhs) {
  return std::move(lhs) | ValueValidator{std::move(rhs)};
}

template <typename TypeFn1, typename ValueFn1, typename TypeFn2,
          typename ValueFn2>
[[nodiscard]] constexpr auto operator&& (
    ValueValidator<TypeFn1, ValueFn1> lhs,
    ValueValidator<TypeFn2, ValueFn2> rhs) {
  auto value_lambda = [lhs_ = std::move(lhs), rhs_ = std::move(rhs)] (
                          const picojson::value& value) -> std::string {
    std::string result;
    result = lhs_.type_validator(value);
    if (result.empty()) {
      result = lhs_.value_validator(value);
    }
    if (result.empty()) {
      result = rhs_.type_validator(value);
    }
    if (result.empty()) {
      result = rhs_.value_validator(value);
    }
    return result;
  };
  return ValueValidator<EmptyStringFunctor<picojson::value>,
                        decltype(value_lambda)>{{}, std::move(value_lambda)};
}

template <typename TypeFn, typename ValueFn, typename T>
  requires requires { ValueValidator{std::declval<T>()}; }
[[nodiscard]] constexpr auto operator&& (ValueValidator<TypeFn, ValueFn> lhs,
                                         T rhs) {
  return std::move(lhs) && ValueValidator(std::move(rhs));
}

template <typename T, typename TypeFn, typename ValueFn>
  requires requires { ValueValidator{std::declval<T>()}; }
[[nodiscard]] constexpr auto operator&& (T lhs,
                                         ValueValidator<TypeFn, ValueFn> rhs) {
  return ValueValidator(std::move(lhs)) && std::move(rhs);
}

template <typename T, typename U>
  requires requires { ValueValidator{std::declval<T>()}; } &&
           requires { ValueValidator{std::declval<U>()}; }
[[nodiscard]] constexpr auto operator&& (T lhs, U rhs) {
  return ValueValidator(std::move(lhs)) && ValueValidator(std::move(rhs));
}

template <typename TypeFn1, typename ValueFn1, typename TypeFn2,
          typename ValueFn2>
[[nodiscard]] constexpr auto operator|| (
    ValueValidator<TypeFn1, ValueFn1> lhs,
    ValueValidator<TypeFn2, ValueFn2> rhs) {
  auto value_lambda = [lhs_ = std::move(lhs), rhs_ = std::move(rhs)] (
                          const picojson::value& value) -> std::string {
    std::string result;
    result = lhs_.type_validator(value);
    if (result.empty()) {
      result = lhs_.value_validator(value);
    }
    if (!result.empty()) {
      result = rhs_.type_validator(value);
      if (result.empty()) {
        result = rhs_.value_validator(value);
      }
    }
    return result;
  };
  return ValueValidator<EmptyStringFunctor<picojson::value>,
                        decltype(value_lambda)>{{}, std::move(value_lambda)};
}

template <typename TypeFn, typename ValueFn, typename Validator>
  requires requires { ValueValidator{std::declval<Validator>()}; }
[[nodiscard]] constexpr auto operator|| (ValueValidator<TypeFn, ValueFn> lhs,
                                         Validator rhs) {
  return std::move(lhs) || ValueValidator(std::move(rhs));
}

template <typename Validator, typename TypeFn, typename ValueFn>
  requires requires { ValueValidator{std::declval<Validator>()}; }
[[nodiscard]] constexpr auto operator|| (Validator lhs,
                                         ValueValidator<TypeFn, ValueFn> rhs) {
  return ValueValidator(std::move(lhs)) || std::move(rhs);
}

template <typename LValidator, typename RValidator>
  requires requires { ValueValidator{std::declval<LValidator>()}; } &&
           requires { ValueValidator{std::declval<RValidator>()}; }
[[nodiscard]] constexpr auto operator|| (LValidator lhs, RValidator rhs) {
  return ValueValidator(std::move(lhs)) || ValueValidator(std::move(rhs));
}


namespace adapter {

  struct AnyValidatorFn {
    [[nodiscard]] constexpr auto operator() () const {
      return ValueValidator<EmptyStringFunctor<picojson::value>,
                            EmptyStringFunctor<picojson::value>>{{}, {}};
    }

    [[nodiscard]] auto operator() (const picojson::value true_value) const {
      auto value_lambda = [true_value_ = true_value] (
                              const picojson::value& value) -> std::string {
        if (value == true_value_) {
          return std::string{};
        } else {
          return std::format("` is invalid value. The value must be `{}`.",
                             value.serialize());
        }
      };
      return ValueValidator<EmptyStringFunctor<picojson::value>,
                            decltype(value_lambda)>{{},
                                                    std::move(value_lambda)};
    }

    template <typename Pred>
      requires std::predicate<Pred, picojson::value> &&
               (!concepts::StringResultValidator<Pred, picojson::value>)
    [[nodiscard]] constexpr auto operator() (Pred&& value_predicator) const {
      auto value_lambda = [validator = std::forward<Pred>(value_predicator)] (
                              const picojson::value& value) -> std::string {
        if (validator(value)) {
          return std::string{};
        } else {
          return "` is invalid value.";
        }
      };
      return ValueValidator<EmptyStringFunctor<picojson::value>,
                            decltype(value_lambda)>{{},
                                                    std::move(value_lambda)};
    }

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, picojson::value>
    [[nodiscard]] constexpr auto operator() (
        Validator&& value_validator) const {
      auto value_lambda = [validator =
                               std::forward<Validator>(value_validator)] (
                              const picojson::value& value) -> std::string {
        if (std::string result{validator(value)}; result.empty()) {
          return std::string{};
        } else {
          return "` is invalid value. " + std::move(result);
        }
      };
      return ValueValidator<EmptyStringFunctor<picojson::value>,
                            decltype(value_lambda)>{{},
                                                    std::move(value_lambda)};
    }
  };

  constexpr inline AnyValidatorFn any;

  struct NullValidatorFn {
    [[nodiscard]] constexpr auto operator() () const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::null>()) {
          return std::string{};
        } else {
          return "` is not Null.";
        }
      };
      return ValueValidator<decltype(type_lambda),
                            EmptyStringFunctor<picojson::value>>{
          std::move(type_lambda), {}};
    }
  };

  constexpr inline NullValidatorFn null;

  struct BooleanValidatorFn {
    [[nodiscard]] constexpr auto operator() () const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<bool>()) {
          return std::string{};
        } else {
          return "` is not Boolean.";
        }
      };
      return ValueValidator<decltype(type_lambda),
                            EmptyStringFunctor<picojson::value>>{
          std::move(type_lambda), {}};
    }

    [[nodiscard]] constexpr auto operator() (const bool true_value) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<bool>()) {
          return std::string{};
        } else {
          return "` is not Boolean.";
        }
      };
      auto value_lambda =
          [true_value] (const picojson::value& value) -> std::string {
        if (value.get<bool>() == true_value) {
          return std::string{};
        } else {
          return std::format(
              "` is invalid Boolean value. The value must be `{}`.",
              true_value);
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }
  };

  constexpr inline BooleanValidatorFn boolean;

  struct NumberValidatorFn {
    [[nodiscard]] constexpr auto operator() () const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<double>()) {
          return std::string{};
        } else {
          return "` is not Number.";
        }
      };
      return ValueValidator<decltype(type_lambda),
                            EmptyStringFunctor<picojson::value>>{
          std::move(type_lambda), {}};
    }

    [[nodiscard]] constexpr auto operator() (const double true_value) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<double>()) {
          return std::string{};
        } else {
          return "` is not Number.";
        }
      };
      auto value_lambda =
          [true_value] (const picojson::value& value) -> std::string {
        if (value.get<double>() == true_value) {
          return std::string{};
        } else {
          return std::format(
              "` is invalid Number value. The value must be `{}`.", true_value);
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Pred>
      requires std::predicate<Pred, double> &&
               (!concepts::StringResultValidator<Pred, double>)
    [[nodiscard]] constexpr auto operator() (Pred&& value_predicator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<double>()) {
          return std::string{};
        } else {
          return "` is not Number.";
        }
      };
      auto value_lambda = [validator = std::forward<Pred>(value_predicator)] (
                              const picojson::value& value) -> std::string {
        if (validator(value.get<double>())) {
          return std::string{};
        } else {
          return "` is invalid Number value.";
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, double>
    [[nodiscard]] constexpr auto operator() (
        Validator&& value_validator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<double>()) {
          return std::string{};
        } else {
          return "` is not Number.";
        }
      };
      auto value_lambda = [validator =
                               std::forward<Validator>(value_validator)] (
                              const picojson::value& value) -> std::string {
        if (std::string result{validator(value.get<double>())};
            result.empty()) {
          return std::string{};
        } else {
          return "` is invalid Number value. " + std::move(result);
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }
  };

  constexpr inline NumberValidatorFn number;

#ifdef PICOJSON_USE_INT64
  struct Int64ValidatorFn {
    [[nodiscard]] constexpr auto operator() () const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<int64_t>()) {
          return std::string{};
        } else {
          return "` is not Int64.";
        }
      };
      return ValueValidator<decltype(type_lambda),
                            EmptyStringFunctor<picojson::value>>{
          std::move(type_lambda), {}};
    }

    [[nodiscard]] constexpr auto operator() (const int64_t true_value) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<int64_t>()) {
          return std::string{};
        } else {
          return "` is not Int64.";
        }
      };
      auto value_lambda =
          [true_value] (const picojson::value& value) -> std::string {
        if (value.get<int64_t>() == true_value) {
          return std::string{};
        } else {
          return std::format(
              "` is invalid Int64 value. The value must be `{}`.", true_value);
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Pred>
      requires std::predicate<Pred, int64_t> &&
               (!concepts::StringResultValidator<Pred, int64_t>)
    [[nodiscard]] constexpr auto operator() (Pred&& value_predicator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<int64_t>()) {
          return std::string{};
        } else {
          return "` is not Int64.";
        }
      };
      auto value_lambda = [validator = std::forward<Pred>(value_predicator)] (
                              const picojson::value& value) -> std::string {
        if (validator(value.get<int64_t>())) {
          return std::string{};
        } else {
          return "` is invalid Int64 value.";
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, int64_t>
    [[nodiscard]] constexpr auto operator() (
        Validator&& value_validator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<int64_t>()) {
          return std::string{};
        } else {
          return "` is not Int64.";
        }
      };
      auto value_lambda = [validator =
                               std::forward<Validator>(value_validator)] (
                              const picojson::value& value) -> std::string {
        if (std::string result{validator(value.get<int64_t>())};
            result.empty()) {
          return std::string{};
        } else {
          return "` is invalid Int64 value. " + std::move(result);
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }
  };

  constexpr inline Int64ValidatorFn int64;
#endif

  struct StringValidatorFn {
    [[nodiscard]] constexpr auto operator() () const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<std::string>()) {
          return std::string{};
        } else {
          return "` is not String.";
        }
      };
      return ValueValidator<decltype(type_lambda),
                            EmptyStringFunctor<picojson::value>>{
          std::move(type_lambda), {}};
    }

    [[nodiscard]] constexpr auto operator() (
        std::string_view true_value) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<std::string>()) {
          return std::string{};
        } else {
          return "` is not String.";
        }
      };
      auto value_lambda = [true_value{std::string{true_value}}] (
                              const picojson::value& value) -> std::string {
        if (value.get<std::string>() == true_value) {
          return std::string{};
        } else {
          return std::format(
              "` is invalid String value. The value must be `{}`.", true_value);
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Pred>
      requires std::predicate<Pred, std::string> &&
               (!concepts::StringResultValidator<Pred, std::string>)
    [[nodiscard]] constexpr auto operator() (Pred&& value_predicator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<std::string>()) {
          return std::string{};
        } else {
          return "` is not String.";
        }
      };
      auto value_lambda = [validator = std::forward<Pred>(value_predicator)] (
                              const picojson::value& value) -> std::string {
        if (validator(value.get<std::string>())) {
          return std::string{};
        } else {
          return "` is invalid String value.";
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, std::string>
    [[nodiscard]] constexpr auto operator() (
        Validator&& value_validator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<std::string>()) {
          return std::string{};
        } else {
          return "` is not String.";
        }
      };
      auto value_lambda = [validator =
                               std::forward<Validator>(value_validator)] (
                              const picojson::value& value) -> std::string {
        if (std::string result{validator(value.get<std::string>())};
            result.empty()) {
          return std::string{};
        } else {
          return "` is invalid String value. " + std::move(result);
        }
      };
      return ValueValidator<decltype(type_lambda), decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }
  };

  constexpr inline StringValidatorFn string;

  struct ArrayValidatorFn {
    [[nodiscard]] constexpr auto operator() () const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::array>()) {
          return std::string{};
        } else {
          return "` is not Array.";
        }
      };
      return ObjectValidatorWrapper<picojson::array::size_type,
                                    decltype(type_lambda),
                                    EmptyStringFunctor<picojson::value>>{
          std::move(type_lambda)};
    }

    [[nodiscard]] constexpr auto operator() (
        const picojson::array& true_value) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::array>()) {
          return std::string{};
        } else {
          return "` is not Array.";
        }
      };
      auto value_lambda = [true_value_ = true_value] (
                              const picojson::value& value) -> std::string {
        if (value.get<picojson::array>() == true_value_) {
          return std::string{};
        } else {
          return std::format(
              "` is invalid Array value. The value must be `{}`.",
              picojson::value(std::move(true_value_)).serialize());
        }
      };
      return ObjectValidatorWrapper<picojson::array::size_type,
                                    decltype(type_lambda),
                                    decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Pred>
      requires std::predicate<Pred, picojson::array> &&
               (!concepts::StringResultValidator<Pred, picojson::array>)
    [[nodiscard]] constexpr auto operator() (Pred&& value_predicator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::array>()) {
          return std::string{};
        } else {
          return "` is not Array.";
        }
      };
      auto value_lambda = [validator = std::forward<Pred>(value_predicator)] (
                              const picojson::value& value) -> std::string {
        if (validator(value.get<picojson::array>())) {
          return std::string{};
        } else {
          return "` is invalid Array value.";
        }
      };
      return ObjectValidatorWrapper<picojson::array::size_type,
                                    decltype(type_lambda),
                                    decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, picojson::array>
    [[nodiscard]] constexpr auto operator() (
        Validator&& value_validator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::array>()) {
          return std::string{};
        } else {
          return "` is not Array.";
        }
      };
      auto value_lambda = [validator =
                               std::forward<Validator>(value_validator)] (
                              const picojson::value& value) -> std::string {
        if (std::string result{validator(value.get<picojson::array>())};
            result.empty()) {
          return std::string{};
        } else {
          return "` is invalid Array value. " + std::move(result);
        }
      };
      return ObjectValidatorWrapper<picojson::array::size_type,
                                    decltype(type_lambda),
                                    decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }
  };

  constexpr inline ArrayValidatorFn array;

  struct ObjectValidatorFn {
    [[nodiscard]] constexpr auto operator() () const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::object>()) {
          return std::string{};
        } else {
          return "` is not Object.";
        }
      };
      return ObjectValidatorWrapper<picojson::object::key_type,
                                    decltype(type_lambda),
                                    EmptyStringFunctor<picojson::value>>{
          std::move(type_lambda)};
    }

    [[nodiscard]] auto operator() (const picojson::object& true_value) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::object>()) {
          return std::string{};
        } else {
          return "` is not Object.";
        }
      };
      auto value_lambda = [true_value_ = true_value] (
                              const picojson::value& value) -> std::string {
        if (value.get<picojson::object>() == true_value_) {
          return std::string{};
        } else {
          return std::format(
              "` is invalid Object value. The value must be `{}`.",
              picojson::value(true_value_).serialize());
        }
      };
      return ObjectValidatorWrapper<picojson::object::key_type,
                                    decltype(type_lambda),
                                    decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Pred>
      requires std::predicate<Pred, picojson::object> &&
               (!concepts::StringResultValidator<Pred, picojson::object>)
    [[nodiscard]] constexpr auto operator() (Pred&& value_predicator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::object>()) {
          return std::string{};
        } else {
          return "` is not Object.";
        }
      };
      auto value_lambda = [validator = std::forward<Pred>(value_predicator)] (
                              const picojson::value& value) -> std::string {
        if (validator(value.get<picojson::object>())) {
          return std::string{};
        } else {
          return "` is invalid Object value.";
        }
      };
      return ObjectValidatorWrapper<picojson::object::key_type,
                                    decltype(type_lambda),
                                    decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }

    template <typename Validator>
      requires concepts::StringResultValidator<Validator, picojson::object>
    [[nodiscard]] constexpr auto operator() (
        Validator&& value_validator) const {
      auto type_lambda = [] (const picojson::value& value) -> std::string {
        if (value.is<picojson::object>()) {
          return std::string{};
        } else {
          return "` is not Object.";
        }
      };
      auto value_lambda = [validator =
                               std::forward<Validator>(value_validator)] (
                              const picojson::value& value) -> std::string {
        if (std::string result{validator(value.get<picojson::object>())};
            result.empty()) {
          return std::string{};
        } else {
          return "` is invalid Object value. " + std::move(result);
        }
      };
      return ObjectValidatorWrapper<picojson::object::key_type,
                                    decltype(type_lambda),
                                    decltype(value_lambda)>{
          std::move(type_lambda), std::move(value_lambda)};
    }
  };

  constexpr inline ObjectValidatorFn object;

}  // namespace adapter

}  // namespace picojson::validator

#endif  // PICOJSON_VALIDATOR_ADAPTER_HPP_
