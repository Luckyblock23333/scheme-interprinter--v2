#include "RE.hpp"

RuntimeError::RuntimeError(const std::string& s1) : s(s1) {}

const char* RuntimeError::what() const noexcept {
    return s.c_str();
}
