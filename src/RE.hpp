#ifndef RUNTIMEERROR
#define RUNTIMEERROR

#include <exception>
#include <string>

class RuntimeError : public std::exception {
private:
    std::string s;

public:
    RuntimeError(const std::string& msg);  // 构造函数声明
    const char* what() const noexcept override;  // 重载 what() 返回错误信息
};

#endif
