/**
 * @file evaluation.cpp
 * @brief Expression evaluation implementation for the Scheme interpreter
 * @author luke36
 * 
 * This file implements evaluation methods for all expression types in the Scheme
 * interpreter. Functions are organized according to ExprType enumeration order
 * from Def.hpp for consistency and maintainability.
 */

#include "value.hpp"
#include "expr.hpp"
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
#include <climits>

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Value Fixnum::eval(Assoc &e) { // evaluation of a fixnum
    return IntegerV(n);
}

Value RationalNum::eval(Assoc &e) { // evaluation of a rational number
    return RationalV(numerator, denominator);
}

Value StringExpr::eval(Assoc &e) { // evaluation of a string
    return StringV(s);
}

Value True::eval(Assoc &e) { // evaluation of #t
    return BooleanV(true);
}

Value False::eval(Assoc &e) { // evaluation of #f
    return BooleanV(false);
}

Value MakeVoid::eval(Assoc &e) { // (void)
    return VoidV();
}

Value Exit::eval(Assoc &e) { // (exit)
    return TerminateV();
}

Value Unary::eval(Assoc &e) { // evaluation of single-operator primitive
    return evalRator(rand->eval(e));
}

Value Binary::eval(Assoc &e) { // evaluation of two-operators primitive
    return evalRator(rand1->eval(e), rand2->eval(e));
}

Value Variadic::eval(Assoc &e) { // evaluation of multi-operator primitive
    std::vector<Value> args;
    for (int i = 0; i < rands.size(); i++) {
        args.push_back(rands[i]->eval(e));
    }
    return evalRator(args);
    //TODO: To complete the substraction logic
}

bool is_integer(const std::string& s) {
    if (s.empty()) return false;
    int i = 0;
    if (s[0] == '-' || s[0] == '+') i = 1;
    if (i == s.size()) return false;
    for (; i < s.size(); i++)
        if (!isdigit(s[i])) return false;
    return true;
}

Value Var::eval(Assoc &e) { // evaluation of variable
	if(x.empty()){
		throw RuntimeError("an block?what a fuckerman you are!! GRRRRRRRRRRRR");
	}
    bool is_number = is_integer(x);
    if (is_number) {
        size_t pos;
        long long val = stoll(x, &pos);
        if (pos == x.size()) { // 完全匹配整数
            return IntegerV(static_cast<int>(val));
        }
    }
    // 若不是整数，尝试解析有理数（如 "1/2"、"-3/4"）
    if (!is_number) {
        size_t slash_pos = x.find('/');
        if (slash_pos != std::string::npos && slash_pos > 0 && slash_pos < x.size()-1) {
            try {
                long long numerator = stoll(x.substr(0, slash_pos));
                long long denominator = stoll(x.substr(slash_pos+1));
                if (denominator != 0) { // 分母不为0
                    return RationalV(static_cast<int>(numerator), static_cast<int>(denominator));
                }
            } catch (...) {}
        }
    }
	char first = x[0];
	// 首字符不能是数字、.、@
	if (isdigit(static_cast<unsigned char>(first)) || first == '.' || first == '@') {
        throw RuntimeError("if you keep inputing these invalid symbols ,i will fuck your ass" );
    }
	// 检查所有字符：不能包含 #、'、"、` 或空白
    for (int i = 0; i < x.size(); i++) {
		char other = x[i];
        if (other == '#' || other == '\'' || other == '"' || other == '`' || isspace(static_cast<unsigned char>(other))) {
            throw RuntimeError("if you keep inputing these invalid symbols ,i will fuck your ass");
        }
    }
     // 2. 数字优先识别
    // TODO: TO identify the invalid variable
    // We request all valid variable just need to be a symbol,you should promise:
    //The first character of a variable name cannot be a digit or any character from the set: {.@}
    //If a string can be recognized as a number, it will be prioritized as a number. For example: 1, -1, +123, .123, +124., 1e-3
    //Variable names can overlap with primitives and reserve_words
    //Variable names can contain any non-whitespace characters except #, ', ", `, but the first character cannot be a digit
    //When a variable is not defined in the current scope, your interpreter should output RuntimeError
    Value matched_value = find(x, e);
	if (matched_value.get()!=nullptr) {
		return matched_value;
	}
    else if (matched_value.get() == nullptr) {
        static std::map<ExprType, std::pair<Expr, std::vector<std::string>>> primitive_map = {
            {E_VOID,     {new MakeVoid(), {}}},
            {E_EXIT,     {new Exit(), {}}},
            {E_BOOLQ,    {new IsBoolean(new Var("parm")), {"parm"}}},
            {E_INTQ,     {new IsFixnum(new Var("parm")), {"parm"}}},
            {E_NULLQ,    {new IsNull(new Var("parm")), {"parm"}}},
            {E_PAIRQ,    {new IsPair(new Var("parm")), {"parm"}}},
            {E_PROCQ,    {new IsProcedure(new Var("parm")), {"parm"}}},
            {E_SYMBOLQ,  {new IsSymbol(new Var("parm")), {"parm"}}},
            {E_STRINGQ,  {new IsString(new Var("parm")), {"parm"}}},
            {E_DISPLAY,  {new Display(new Var("parm")), {"parm"}}},
            {E_PLUS,     {new PlusVar({}),  {}}},
            {E_MINUS,    {new MinusVar({}), {}}},
            {E_MUL,      {new MultVar({}),  {}}},
            {E_DIV,      {new DivVar({}),   {}}},
            {E_MODULO,   {new Modulo(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
            {E_EXPT,     {new Expt(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
            {E_LT,       {new LessVar({}), {}}},
            {E_LE,       {new LessEqVar({}), {}}},
            {E_GT,       {new GreaterVar({}), {}}},
            {E_GE,       {new GreaterEqVar({}), {}}},
            {E_EQ,       {new EqualVar({}), {}}},
            {E_EQQ,      {new IsEq(new Var("a"), new Var("b")), {"a", "b"}}},
            {E_NOT,      {new Not(new Var("p")), {"p"}}},
            {E_CONS,     {new Cons(new Var("a"), new Var("b")), {"a", "b"}}},
            {E_CAR,      {new Car(new Var("p")), {"p"}}},
            {E_CDR,      {new Cdr(new Var("p")), {"p"}}},
            {E_LIST,     {new ListFunc({}), {}}},
            {E_SETCAR,   {new SetCar(new Var("p"), new Var("v")), {"p", "v"}}},
            {E_SETCDR,   {new SetCdr(new Var("p"), new Var("v")), {"p", "v"}}}
        };
        if (primitives.count(x)) {
            auto it = primitive_map.find(primitives[x]);
            //TOD0:to PASS THE parameters correctly;
            //COMPLETE THE CODE WITH THE HINT IN IF SENTENCE WITH CORRECT RETURN VALUE
            if (it != primitive_map.end()) {
                return ProcedureV(it->second.second, it->second.first, empty());
            };
        }
    }
    throw RuntimeError("Undefined variable: " + x);
}


Value Plus::evalRator(const Value &rand1, const Value &rand2) { // +
    //TODO: To complete the addition logic
    if (rand1->v_type != V_INT && rand1->v_type != V_RATIONAL) {
        throw(RuntimeError("Wrong typename"));
    }
    // 检查第二个参数是否为数字
    if (rand2->v_type != V_INT && rand2->v_type != V_RATIONAL) {
        throw(RuntimeError("Wrong typename"));
    }
    // 类型正确，执行加法（省略具体计算逻辑）
    // 提取第一个参数的分子和分母（整数视为 "n/1"）
    int num1, den1;
    if (rand1->v_type == V_INT) {
        num1 = dynamic_cast<Integer*>(rand1.get())->n;  // 整数的分子是其值
        den1 = 1;                                      // 整数的分母是 1
    } else {
        num1 = dynamic_cast<Rational*>(rand1.get())->numerator;
        den1 = dynamic_cast<Rational*>(rand1.get())->denominator;
    }

    // 提取第二个参数的分子和分母
    int num2, den2;
    if (rand2->v_type == V_INT) {
        num2 = dynamic_cast<Integer*>(rand2.get())->n;
        den2 = 1;
    } else {
        num2 = dynamic_cast<Rational*>(rand2.get())->numerator;
        den2 = dynamic_cast<Rational*>(rand2.get())->denominator;
    }

    // 计算加法：num1/den1 + num2/den2 = (num1*den2 + num2*den1) / (den1*den2)
    int new_num = num1 * den2 + num2 * den1;
    int new_den = den1 * den2;
    //约分

    //WARNING：不需要约分，Rational 的构造函数里写了

    // int common_divisor = gcd(abs(new_num), abs(new_den));
    // if (common_divisor != 0) {
    //     new_num /= common_divisor;
    //     new_den /= common_divisor;
    // }

    // 确保分母为正数（约定：分数的分母始终为正）
    if (new_den < 0) {
        new_num *= -1;
        new_den *= -1;
    }

    // 结果为整数时返回 IntegerV，否则返回 RationalV
    if (new_den == 1) {
        return IntegerV(new_num);
    } else {
        return RationalV(new_num, new_den);
    }
}

Value Minus::evalRator(const Value &rand1, const Value &rand2) {
    if (rand1->v_type != V_INT && rand1->v_type != V_RATIONAL) {
        throw(RuntimeError("Wrong typename"));
    }
    if (rand2->v_type != V_INT && rand2->v_type != V_RATIONAL) {
        throw(RuntimeError("Wrong typename"));
    }
    int num1, den1;
    if (rand1->v_type == V_INT) {
        num1 = dynamic_cast<Integer*>(rand1.get())->n;  // 整数的分子是其值
        den1 = 1;                                      // 整数的分母是 1
    } else {
        num1 = dynamic_cast<Rational*>(rand1.get())->numerator;
        den1 = dynamic_cast<Rational*>(rand1.get())->denominator;
    }
    int num2, den2;
    if (rand2->v_type == V_INT) {
        num2 = dynamic_cast<Integer*>(rand2.get())->n;
        den2 = 1;
    } else {
        num2 = dynamic_cast<Rational*>(rand2.get())->numerator;
        den2 = dynamic_cast<Rational*>(rand2.get())->denominator;
    }

    // 计算减法：num1/den1 + num2/den2 = (num1*den2 - num2*den1) / (den1*den2)
    int new_num = num1 * den2 - num2 * den1;
    int new_den = den1 * den2;
    //约分
    // int common_divisor = gcd(abs(new_num), abs(new_den));
    // if (common_divisor != 0) {
    //     new_num /= common_divisor;
    //     new_den /= common_divisor;
    // }

    // 确保分母为正数（约定：分数的分母始终为正）
    if (new_den < 0) {
        new_num *= -1;
        new_den *= -1;
    }

    // 结果为整数时返回 IntegerV，否则返回 RationalV
    if (new_den == 1) {
        return IntegerV(new_num);
    } else {
        return RationalV(new_num, new_den);
    }
    //TODO: To complete the substraction logic
}

Value Mult::evalRator(const Value &rand1, const Value &rand2) { // *
    if (rand1->v_type != V_INT && rand1->v_type != V_RATIONAL) {
        throw(RuntimeError("Wrong typename"));
    }
    if (rand2->v_type != V_INT && rand2->v_type != V_RATIONAL) {
        throw(RuntimeError("Wrong typename"));
    }
    int num1, den1;
    if (rand1->v_type == V_INT) {
        num1 = dynamic_cast<Integer*>(rand1.get())->n;  // 整数的分子是其值
        den1 = 1;                                      // 整数的分母是 1
    } else {
        num1 = dynamic_cast<Rational*>(rand1.get())->numerator;
        den1 = dynamic_cast<Rational*>(rand1.get())->denominator;
    }
    int num2, den2;
    if (rand2->v_type == V_INT) {
        num2 = dynamic_cast<Integer*>(rand2.get())->n;
        den2 = 1;
    } else {
        num2 = dynamic_cast<Rational*>(rand2.get())->numerator;
        den2 = dynamic_cast<Rational*>(rand2.get())->denominator;
    }

    // 计算乘法：num1/den1 * num2/den2
    int new_num = num1 * num2;
    int new_den = den1 * den2;
    //约分
    // int common_divisor = gcd(abs(new_num), abs(new_den));
    // if (common_divisor != 0) {
    //     new_num /= common_divisor;
    //     new_den /= common_divisor;
    // }

    // 确保分母为正数（约定：分数的分母始终为正）
    if (new_den < 0) {
        new_num *= -1;
        new_den *= -1;
    }

    // 结果为整数时返回 IntegerV，否则返回 RationalV
    if (new_den == 1) {
        return IntegerV(new_num);
    } else {
        return RationalV(new_num, new_den);
    }
    //TODO: To complete the Multiplication logic
}

Value Div::evalRator(const Value &rand1, const Value &rand2) {
    if (rand1->v_type != V_INT && rand1->v_type != V_RATIONAL) {
        throw(RuntimeError("Wrong typename"));
    }
    if (rand2->v_type != V_INT && rand2->v_type != V_RATIONAL) {
        throw(RuntimeError("Wrong typename"));
    }
    int num1, den1;
    if (rand1->v_type == V_INT) {
        num1 = dynamic_cast<Integer*>(rand1.get())->n;  // 整数的分子是其值
        den1 = 1;                                      // 整数的分母是 1
    } else {
        num1 = dynamic_cast<Rational*>(rand1.get())->numerator;
        den1 = dynamic_cast<Rational*>(rand1.get())->denominator;
    }
    int num2, den2;
    if (rand2->v_type == V_INT) {
        num2 = dynamic_cast<Integer*>(rand2.get())->n;
        den2 = 1;
    } else {
        num2 = dynamic_cast<Rational*>(rand2.get())->numerator;
        den2 = dynamic_cast<Rational*>(rand2.get())->denominator;
    }

    if (num2 == 0) {
        throw(RuntimeError("You are a fucker, can't div 0"));
    }

    // 计算除法：num1/den1 / num2/den2
    int new_num = num1 * den2;
    int new_den = den1 * num2;
    //约分
    // int common_divisor = gcd(abs(new_num), abs(new_den));
    // if (common_divisor != 0) {
    //     new_num /= common_divisor;
    //     new_den /= common_divisor;
    // }

    // 确保分母为正数（约定：分数的分母始终为正）
    if (new_den < 0) {
        new_num *= -1;
        new_den *= -1;
    }

    // 结果为整数时返回 IntegerV，否则返回 RationalV
    if (new_den == 1) {
        return IntegerV(new_num);
    } else {
        return RationalV(new_num, new_den);
    }
    //TODO: To complete the dicision logic
}

Value Modulo::evalRator(const Value &rand1, const Value &rand2) { // modulo
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int dividend = dynamic_cast<Integer*>(rand1.get())->n;
        int divisor = dynamic_cast<Integer*>(rand2.get())->n;
        if (divisor == 0) {
            throw(RuntimeError("Division by zero"));
        }
        return IntegerV(dividend % divisor);
    }
    throw(RuntimeError("modulo is only defined for integers"));
}

Value PlusVar::evalRator(const std::vector<Value> &args) { // + with multiple args
    if (args.empty()) {
        return Value(new Integer(0)); // 空参数时返回 0（Scheme 约定）
    }
    Value sum = args[0];
    for(int i = 1; i < args.size(); i++) {
        sum = Plus(Expr(new PlusVar({})), Expr(new PlusVar({}))).evalRator(sum, args[i]);
    }
    return sum;
}

Value MinusVar::evalRator(const std::vector<Value> &args) { // - with multiple args
    if (args.empty()) {
        throw(RuntimeError("(-)→ RuntimeError")); // 空参数时返回 RuntimeError（Scheme 约定）
    }
    if (args.size() == 1) {
        return Minus(Expr(new MinusVar({})), Expr(new MinusVar({}))).evalRator(IntegerV(0), args[0]);
    }
    Value sum = args[0];
    for(int i = 1; i < args.size(); i++) {
        sum = Minus(Expr(new MinusVar({})), Expr(new MinusVar({}))).evalRator(sum, args[i]);
    }
    return sum;
    //TODO: To complete the substraction logic
}

Value MultVar::evalRator(const std::vector<Value> &args) { // * with multiple args
    if (args.empty()) {
        return Value(new Integer(1)); // 空参数时返回 1（Scheme 约定）
    }
    Value sum = args[0];
    for(int i = 1; i < args.size(); i++) {
        sum = Mult(Expr(new MultVar({})), Expr(new MultVar({}))).evalRator(sum, args[i]);
    }
    return sum;
    //TODO: To complete the multiplication logic
}

Value DivVar::evalRator(const std::vector<Value> &args) { // / with multiple args
    if (args.empty()) {
        throw(RuntimeError("(/)→ RuntimeError")); // 空参数时返回 RuntimeError（Scheme 约定）
    }
    if (args.size() == 1) {
        return Div(Expr(new DivVar({})), Expr(new DivVar({}))).evalRator(IntegerV(1), args[0]);
    }
    Value sum = args[0];
    for(int i = 1; i < args.size(); i++) {
        sum = Div(Expr(new DivVar({})), Expr(new DivVar({}))).evalRator(sum, args[i]);
    }
    return sum;
    //TODO: To complete the divisor logic
}

Value Expt::evalRator(const Value &rand1, const Value &rand2) { // expt
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int base = dynamic_cast<Integer*>(rand1.get())->n;
        int exponent = dynamic_cast<Integer*>(rand2.get())->n;
        
        if (exponent < 0) {
            throw(RuntimeError("Negative exponent not supported for integers"));
        }
        if (base == 0 && exponent == 0) {
            throw(RuntimeError("0^0 is undefined"));
        }
        
        long long result = 1;
        long long b = base;
        int exp = exponent;
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= b;
                if (result > INT_MAX || result < INT_MIN) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            b *= b;
            if (b > INT_MAX || b < INT_MIN) {
                if (exp > 1) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            exp /= 2;
        }
        
        return IntegerV((int)result);
    }
    throw(RuntimeError("Wrong typename"));
}

//A FUNCTION TO SIMPLIFY THE COMPARISON WITH INTEGER AND RATIONAL NUMBER
int compareNumericValues(const Value &v1, const Value &v2) {
    if (v1->v_type == V_INT && v2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        return (n1 < n2) ? -1 : (n1 > n2) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        int left = r1->numerator;
        int right = n2 * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_INT && v2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = n1 * r2->denominator;
        int right = r2->numerator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = r1->numerator * r2->denominator;
        int right = r2->numerator * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    throw RuntimeError("Wrong typename in numeric comparison");
}

Value Less::evalRator(const Value &rand1, const Value &rand2) { // <
    if ((rand1->v_type == V_INT||rand1->v_type == V_RATIONAL) && (rand2->v_type == V_INT||rand2->v_type == V_RATIONAL)) {
        int a = compareNumericValues(rand1, rand2);
        if (a == -1) {
            return Value(new Boolean(true));
        }else{
            return Value(new Boolean(false));
        }
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
    //TODO: To complete the less logic
}

Value LessEq::evalRator(const Value &rand1, const Value &rand2) { // <=
    if ((rand1->v_type == V_INT||rand1->v_type == V_RATIONAL) && (rand2->v_type == V_INT||rand2->v_type == V_RATIONAL)) {
        int a = compareNumericValues(rand1, rand2);
        if (a == -1||a == 0) {
            return Value(new Boolean(true));
        }else{
            return Value(new Boolean(false));
        }
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
    //TODO: To complete the less logic
}

Value Equal::evalRator(const Value &rand1, const Value &rand2) { // =
    if ((rand1->v_type == V_INT||rand1->v_type == V_RATIONAL) && (rand2->v_type == V_INT||rand2->v_type == V_RATIONAL)) {
        int a = compareNumericValues(rand1, rand2);
        if (a == 0) {
            return Value(new Boolean(true));
        }else{
            return Value(new Boolean(false));
        }
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
    //TODO: To complete the less logic
}

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) { // >=
    if ((rand1->v_type == V_INT||rand1->v_type == V_RATIONAL) && (rand2->v_type == V_INT||rand2->v_type == V_RATIONAL)) {
        int a = compareNumericValues(rand1, rand2);
        if (a == 1||a == 0) {
            return Value(new Boolean(true));
        }else{
            return Value(new Boolean(false));
        }
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
    //TODO: To complete the less logic
}

Value Greater::evalRator(const Value &rand1, const Value &rand2) { // >
    if ((rand1->v_type == V_INT||rand1->v_type == V_RATIONAL) && (rand2->v_type == V_INT||rand2->v_type == V_RATIONAL)) {
        int a = compareNumericValues(rand1, rand2);
        if (a == 1) {
            return Value(new Boolean(true));
        }else{
            return Value(new Boolean(false));
        }
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
    //TODO: To complete the less logic
}

Value LessVar::evalRator(const std::vector<Value> &args) { // < with multiple args
    if (args.empty()) {
        return Value(new Boolean(true));
    }
    for (int i = 0; i < args.size()-1; i++) {
        Value t = Less(Expr(new LessVar({})), Expr(new LessVar({}))).evalRator(args[i], args[i+1]);
        Boolean* boolVal = dynamic_cast<Boolean*>(t.get());
        if (boolVal->b == false){
            return Value(new Boolean(false));
        }
    }
    return Value(new Boolean(true));
}

Value LessEqVar::evalRator(const std::vector<Value> &args) { // <= with multiple args
    if (args.empty()) {
        return Value(new Boolean(true));
    }
    for (int i = 0; i < args.size()-1; i++) {
        Value t = LessEq(Expr(new LessEqVar({})), Expr(new LessEqVar({}))).evalRator(args[i], args[i+1]);
        Boolean* boolVal = dynamic_cast<Boolean*>(t.get());
        if (boolVal->b == false){
            return Value(new Boolean(false));
        }
    }
        return Value(new Boolean(true));
}

Value EqualVar::evalRator(const std::vector<Value> &args) { // = with multiple args
    if (args.empty()) {
        return Value(new Boolean(true));
    }
    for (int i = 0; i < args.size()-1; i++) {
        Value t = Equal(Expr(new EqualVar({})), Expr(new EqualVar({}))).evalRator(args[i], args[i+1]);
        Boolean* boolVal = dynamic_cast<Boolean*>(t.get());
        if (boolVal->b == false){
            return Value(new Boolean(false));
        }
    }
    return Value(new Boolean(true));
}

Value GreaterEqVar::evalRator(const std::vector<Value> &args) { // >= with multiple args
    if (args.empty()) {
        return Value(new Boolean(true));
    }
    for (int i = 0; i < args.size()-1; i++) {
        Value t = GreaterEq(Expr(new GreaterEqVar({})), Expr(new GreaterEqVar({}))).evalRator(args[i], args[i+1]);
        Boolean* boolVal = dynamic_cast<Boolean*>(t.get());
        if (boolVal->b == false){
            return Value(new Boolean(false));
        }
    }
    return Value(new Boolean(true));
}

Value GreaterVar::evalRator(const std::vector<Value> &args) { // > with multiple args
    if (args.empty()) {
        return Value(new Boolean(true));
    }
    for (int i = 0; i < args.size()-1; i++) {
        Value t = Greater(Expr(new GreaterVar({})), Expr(new GreaterVar({}))).evalRator(args[i], args[i+1]);
        Boolean* boolVal = dynamic_cast<Boolean*>(t.get());
        if (boolVal->b == false){
            return Value(new Boolean(false));
        }
    }
    return Value(new Boolean(true));
}

Value Cons::evalRator(const Value &rand1, const Value &rand2) { // cons
    Value car = rand1;
    Value cdr = rand2;
    return Value(new Pair(car, cdr));
}

Value ListFunc::evalRator(const std::vector<Value> &args) { // list function
    if (args.empty()) {
        return Value(new Null());
    }else{
        Value p(new Null());
        for (int i = args.size()-1; i >= 0; i--) {
            p = Value(new Pair(args[i],p));
        }
        return p;
    }
}

Value IsList::evalRator(const Value &rand) { // list?
    if (rand->v_type == V_NULL) {
        return Value(new Boolean(true));
    }

    if (rand->v_type != V_PAIR) {
        return Value(new Boolean(false));
    }
    Pair* pair = dynamic_cast<Pair*>(rand.get());

    if (pair->cdr->v_type == V_NULL) {
        return Value(new Boolean(true));
    }

    Value dfs_result = evalRator(pair->cdr);
    Boolean* bool_1 = dynamic_cast<Boolean*>(dfs_result.get());
    return Value(new Boolean(bool_1->b));
}

Value Car::evalRator(const Value &rand) { // car
    Pair* pair = dynamic_cast<Pair*>(rand.get());
    if (pair == nullptr) {
        throw(RuntimeError("while this is not a pair,you are a fucker"));
    }
    Value car_get = pair->car;
    return car_get;
    //TODO: To complete the car logic
}

Value Cdr::evalRator(const Value &rand) { // cdr
    Pair* pair = dynamic_cast<Pair*>(rand.get());
    if (pair == nullptr) {
        throw(RuntimeError("while this is not a pair,you are a fucker"));
    }else {
        Value cdr_get = pair->cdr;
        return cdr_get;
    }
    //TODO: To complete the cdr logic
}

Value SetCar::evalRator(const Value &rand1, const Value &rand2) { // set-car!
    Pair* pair = dynamic_cast<Pair*>(rand1.get());
    if (pair == nullptr) {
        throw(RuntimeError("while this is not a pair,you are a fucker"));
    }else {
        pair->car = rand2;
        return Value(new Void());
    }
    //TODO: To complete the set-car! logic
}

Value SetCdr::evalRator(const Value &rand1, const Value &rand2) { // set-cdr!
   Pair* pair = dynamic_cast<Pair*>(rand1.get());
    if (pair == nullptr) {
        throw(RuntimeError("while this is not a pair,you are a fucker"));
    }else {
        pair->cdr = rand2;
        return Value(new Void());
    }
    //TODO: To complete the set-cdr! logic
}

Value IsEq::evalRator(const Value &rand1, const Value &rand2) { // eq?
    // 检查类型是否为 Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
    }
    // 检查类型是否为 Boolean
    else if (rand1->v_type == V_BOOL && rand2->v_type == V_BOOL) {
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    }
    // 检查类型是否为 Symbol
    else if (rand1->v_type == V_SYM && rand2->v_type == V_SYM) {
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    }
    // 检查类型是否为 Null 或 Void
    else if ((rand1->v_type == V_NULL && rand2->v_type == V_NULL) ||
             (rand1->v_type == V_VOID && rand2->v_type == V_VOID)) {
        return BooleanV(true);
    } else {
        return BooleanV(rand1.get() == rand2.get());
    }
}

Value IsBoolean::evalRator(const Value &rand) { // boolean?
    return BooleanV(rand->v_type == V_BOOL);
}

Value IsFixnum::evalRator(const Value &rand) { // number?
    return BooleanV(rand->v_type == V_INT);
}

Value IsNull::evalRator(const Value &rand) { // null?
    return BooleanV(rand->v_type == V_NULL);
}

Value IsPair::evalRator(const Value &rand) { // pair?
    return BooleanV(rand->v_type == V_PAIR);
}

Value IsProcedure::evalRator(const Value &rand) { // procedure?
    return BooleanV(rand->v_type == V_PROC);
}

Value IsSymbol::evalRator(const Value &rand) { // symbol?
    return BooleanV(rand->v_type == V_SYM);
}

Value IsString::evalRator(const Value &rand) { // string?
    return BooleanV(rand->v_type == V_STRING);
}

Value Begin::eval(Assoc &e) {
	Value result = VoidV();
	for(int i = 0; i < es.size(); i++) {
		result = es[i]->eval(e);
	}
	return result;
    //TODO: To complete the begin logic
}
Value syntax_to_quoted_value(const Syntax &s_we_own) {
    // 1. 判断 SyntaxBase 的具体类型（通过 dynamic_cast）
    SyntaxBase* base = s_we_own.get();
    // 处理整数
    if (auto num = dynamic_cast<Number*>(base)) {
        return Value(new Integer(num->n));
    }
    // 处理有理数
    else if (auto rat = dynamic_cast<RationalSyntax*>(base)) {
        return RationalV(rat->numerator, rat->denominator);
    }
    // 处理 #t
    else if (dynamic_cast<TrueSyntax*>(base)) {
        return Value(new Boolean(true));
    }
    // 处理 #f
    else if (dynamic_cast<FalseSyntax*>(base)) {
        return Value(new Boolean(false));
    }
    // 处理字符串
    else if (auto str = dynamic_cast<StringSyntax*>(base)) {
        return Value(new String(str->s));
    }
    // 处理符号
    else if (auto sym = dynamic_cast<SymbolSyntax*>(base)) {
        return Value(new Symbol(sym->s));
    }
	// 处理列表与pair
    else if (auto list = dynamic_cast<List*>(base)) {
        std::vector<Syntax> &elements = list->stxs;
        Value current = VoidV();

        // 检测 stxs 中是否有点符号
        bool has_valid_dot = false;
        int dot_index = -1;
        int dot_count = 0;
        for (int i = 0; i < elements.size(); ++i){
            auto dot_sym = dynamic_cast<SymbolSyntax*>(elements[i].get());
            if (dot_sym && dot_sym->s == ".") {
                ++ dot_count;
                dot_index = i;
            }
        }
        if (elements.size() >= 3) {
            if (dot_count == 1 && dot_index == elements.size() - 2) has_valid_dot = true;
            if (dot_count > 1) {
                throw RuntimeError("dot_count > 1, you are a fucker,fuck you!");
            }
            if (dot_count == 1 && dot_index != elements.size() - 2) {
                throw RuntimeError("dot position not equal elements.size() - 2, you are a fucker,fuck you!");
            }
        } else if (dot_count >= 1) {
            throw RuntimeError("invalid dot, you are a fucker,fuck you!");
        }
        if (has_valid_dot) {
            // 带点形式：处理 . 前面的元素为 car 链，. 后面的元素为最终 cdr
            current = syntax_to_quoted_value(elements.back());  // . 后面的元素作为最终 cdr
            for (int i = dot_index - 1; i >= 0; --i) {
                Value elem = syntax_to_quoted_value(elements[i]);
                current = Value(new Pair(elem, current));
            }
        } else {
            // 普通列表：最终 cdr 是 Null
            current = Value(new Null());
            for (int i = elements.size() - 1; i >= 0; --i) {
                Value elem = syntax_to_quoted_value(elements[i]);
                current = Value(new Pair(elem, current));
            }
        }
        return current;
    }
    // 其他未定义类型
    else {
        throw RuntimeError("though i can't find this type,you are a fucker,fuck you!");
    }
}
Value Quote::eval(Assoc& e) {
        return syntax_to_quoted_value(this->s);
    //TODO: To complete the quote logic
}

Value AndVar::eval(Assoc &e) { // and with short-circuit evaluation
	if (rands.empty()) {
        return Value(new Boolean(true));
    }
	Value result = VoidV();
	for(int i = 0; i < rands.size(); i++) {
		result = rands[i]->eval(e);
		auto is_bool = dynamic_cast<Boolean*>(result.get());
		if (is_bool) {
			if (is_bool->b == false) {
				return Value(new Boolean(false));
			}
		}
	}
	return result;
    //TODO: To complete the and logic
}

Value OrVar::eval(Assoc &e) { // or with short-circuit evaluation
	if (rands.empty()) {
		return Value(new Boolean(false));
	}
	Value result = VoidV();
	for(int i = 0; i < rands.size(); i++) {
		result = rands[i]->eval(e);
		auto is_bool = dynamic_cast<Boolean*>(result.get());
		if (is_bool) {
			if (is_bool->b != false) {
				return result;
			}
		}
		else{
			return result;
		}
	}
	return Value(new Boolean(false));
    //TODO: To complete the or logic
}

Value Not::evalRator(const Value &rand) { // not
	auto is_bool = dynamic_cast<Boolean*>(rand.get());
	if (is_bool) {
		if (is_bool->b == false) {
			return Value(new Boolean(true));
		}else{
			return Value(new Boolean(false));
		}
	}else{
		return Value(new Boolean(false));
	}
    //TODO: To complete the not logic
}

Value If::eval(Assoc &e) {
	Value result = VoidV();
    Value cond_value = cond->eval(e);
	auto judger = dynamic_cast<Boolean*>(cond_value.get());
	if (judger&&judger->b == false) {
		if(alter.get() != nullptr){
			result = alter->eval(e);
		}else{
			return VoidV();
		}
	}else{
		if(conseq.get() != nullptr){
			result = conseq->eval(e);
		}else{
			return VoidV();
		}
	}
	return result;
    //TODO: To complete the if logic
}
bool is_else_symbol(Expr* expr) {
    Symbol* sym_expr = dynamic_cast<Symbol*>(expr->get());
    return (sym_expr != nullptr) && (sym_expr->s == "else");
}
Value Cond::eval(Assoc &env) {
	for(int i = 0; i < clauses.size(); i++) {
		if (clauses[i].empty()) {
            throw std::runtime_error("if you cin a block ,you are a piece of pigger,fuckman!");
        }
		if(is_else_symbol(&clauses[i][0])) {
			return clauses[i][clauses[i].size()-1]->eval(env);
		}else{
			auto judger = dynamic_cast<Boolean*>((clauses[i][0]->eval(env)).get());
			if (judger&&judger->b == true) {
				return clauses[i][clauses[i].size()-1]->eval(env);
			}else if(!judger){
				return clauses[i][clauses[i].size()-1]->eval(env);
			}
		}
	}
	return Value( new Void() );
    //TODO: To complete the cond logic
}

Value Lambda::eval(Assoc &env) {
	if (!e.get()) {
        throw RuntimeError("fuck you ,beach!,your body is as empty as a vagina");
    }
	Procedure* proc = new Procedure(x, e, env);
    Value ret = ProcedureV(x, e, env);
	return ret;
    //TODO: To complete the lambda logic
}

Value Apply::eval(Assoc &e) {
	Value proc_val = rator->eval(e);
    if (!proc_val.get()  || proc_val->v_type != V_PROC) {throw RuntimeError("Attempt to apply a non-procedure");}
    Procedure* clos_ptr = dynamic_cast<Procedure*>(proc_val.get());
	 if (!clos_ptr) {
        throw RuntimeError("Attempt to apply a non-procedure");
    }
    Expr body = clos_ptr->e;

    std::vector<Value> args;
    for (const auto& arg_expr : rand) {
        args.push_back(arg_expr->eval(e));
    }

    if (clos_ptr->parameters.empty()) {
        // 1. 无参数内置函数
        if (auto* makeVoid = dynamic_cast<MakeVoid*>(body.get())) {
            return makeVoid->eval(e);  // MakeVoid无参数，直接调用eval
        }else if (auto* exitFunc = dynamic_cast<Exit*>(body.get())) {
            return exitFunc->eval(e);  // Exit无参数，直接调用eval
        }
        // 2. 单参数内置函数（isboolean、isfixnum、null?、pair?、procedure?、symbol?、string?、display、not、null?、pair?、islist、car、cdr）
        else if (auto* isBoolean = dynamic_cast<IsBoolean*>(clos_ptr->e.get())) {
            return isBoolean->evalRator(args[0]);  // boolean? 接收1个参数
        } else if (auto* isFixnum = dynamic_cast<IsFixnum*>(clos_ptr->e.get())) {
            return isFixnum->evalRator(args[0]);  // isfixnum 接收1个参数
        } else if (auto* isNull = dynamic_cast<IsNull*>(clos_ptr->e.get())) {
            return isNull->evalRator(args[0]);  // null? 接收1个参数
        } else if (auto* isPair = dynamic_cast<IsPair*>(clos_ptr->e.get())) {
            return isPair->evalRator(args[0]);  // pair? 接收1个参数
        } else if (auto* isProcedure = dynamic_cast<IsProcedure*>(clos_ptr->e.get())) {
            return isProcedure->evalRator(args[0]);  // procedure? 接收1个参数
        } else if (auto* isSymbol = dynamic_cast<IsSymbol*>(clos_ptr->e.get())) {
            return isSymbol->evalRator(args[0]);  // symbol? 接收1个参数
        } else if (auto* isString = dynamic_cast<IsString*>(clos_ptr->e.get())) {
            return isString->evalRator(args[0]);  // string? 接收1个参数
        } else if (auto* displayFunc = dynamic_cast<Display*>(clos_ptr->e.get())) {
            return displayFunc->evalRator(args[0]);  // display 接收1个参数
        } else if (auto* notFunc = dynamic_cast<Not*>(clos_ptr->e.get())) {
            return notFunc->evalRator(args[0]);  // not 接收1个参数
        } else if (auto* isList = dynamic_cast<IsList*>(clos_ptr->e.get())) {
            return isList->evalRator(args[0]);  // list? 接收1个参数
        } else if (auto* carFunc = dynamic_cast<Car*>(clos_ptr->e.get())) {
            return carFunc->evalRator(args[0]);  // car 接收1个参数
        } else if (auto* cdrFunc = dynamic_cast<Cdr*>(clos_ptr->e.get())) {
            return cdrFunc->evalRator(args[0]);  // cdr 接收1个参数
        }
        // 3. 双参数内置函数（modulo、expt、eq?、cons、set-car!、set-cdr!）
        else if (auto* moduloFunc = dynamic_cast<Modulo*>(clos_ptr->e.get())) {
            return moduloFunc->evalRator(args[0], args[1]);  // modulo 接收2个参数
        } else if (auto* exptFunc = dynamic_cast<Expt*>(clos_ptr->e.get())) {
            return exptFunc->evalRator(args[0], args[1]);  // expt 接收2个参数
        } else if (auto* isEq = dynamic_cast<IsEq*>(clos_ptr->e.get())) {
            return isEq->evalRator(args[0], args[1]);  // eq? 接收2个参数
        } else if (auto* consFunc = dynamic_cast<Cons*>(clos_ptr->e.get())) {
            return consFunc->evalRator(args[0], args[1]);  // cons 接收2个参数
        } else if (auto* setCar = dynamic_cast<SetCar*>(clos_ptr->e.get())) {
            return setCar->evalRator(args[0], args[1]);  // set-car! 接收2个参数
        } else if (auto* setCdr = dynamic_cast<SetCdr*>(clos_ptr->e.get())) {
            return setCdr->evalRator(args[0], args[1]);  // set-cdr! 接收2个参数
        }
        // 4. 可变参数内置函数（+、-、*、/、=、<、<=、>、>=、list）
        else if (auto* plusVar = dynamic_cast<PlusVar*>(clos_ptr->e.get())) {
            return plusVar->evalRator(args);  // + 接收可变参数（vector<Value>）
        } else if (auto* minusVar = dynamic_cast<MinusVar*>(clos_ptr->e.get())) {
            return minusVar->evalRator(args);  // - 接收可变参数
        } else if (auto* multVar = dynamic_cast<MultVar*>(clos_ptr->e.get())) {
            return multVar->evalRator(args);  // * 接收可变参数
        } else if (auto* divVar = dynamic_cast<DivVar*>(clos_ptr->e.get())) {
            return divVar->evalRator(args);  // / 接收可变参数
        } else if (auto* equalVar = dynamic_cast<EqualVar*>(clos_ptr->e.get())) {
            return equalVar->evalRator(args);  // = 接收可变参数
        } else if (auto* lessVar = dynamic_cast<LessVar*>(clos_ptr->e.get())) {
            return lessVar->evalRator(args);  // < 接收可变参数
        } else if (auto* lessEqVar = dynamic_cast<LessEqVar*>(clos_ptr->e.get())) {
            return lessEqVar->evalRator(args);  // <= 接收可变参数
        } else if (auto* greaterVar = dynamic_cast<GreaterVar*>(clos_ptr->e.get())) {
            return greaterVar->evalRator(args);  // > 接收可变参数
        } else if (auto* greaterEqVar = dynamic_cast<GreaterEqVar*>(clos_ptr->e.get())) {
            return greaterEqVar->evalRator(args);  // >= 接收可变参数
        } else if (auto* listFunc = dynamic_cast<ListFunc*>(clos_ptr->e.get())) {
            return listFunc->evalRator(args);  // list 接收可变参数
        }
    }

    // -------------------------- 非内置函数：执行用户lambda函数 --------------------------

    if (args.size() != clos_ptr->parameters.size()) {
        throw RuntimeError("Wrong number of arguments for lambda");
    }

    Assoc param_env = clos_ptr->env;
	for (size_t i = 0; i < clos_ptr->parameters.size(); ++i) {
        param_env = extend(clos_ptr->parameters[i], args[i], param_env);  // 绑定形参和实参
    }
    return body->eval(param_env);
}
// extern Assoc global_env;
bool does_expr_reference(const Expr& expr, const std::string& var_name) {
    // 1. Symbol 表达式：直接匹配变量名
    if (auto* sym = dynamic_cast<Symbol*>(expr.get())) {
        return sym->s == var_name;
    }

    // 2. Pair 表达式（表、函数调用等）：递归检查 car 和 cdr
    if (auto* apply_expr = dynamic_cast<Apply*>(expr.get())) {
        // 检查函数部分（rator）
        if (does_expr_reference(apply_expr->rator, var_name)) {
            return true;
        }
        // 检查所有参数（rand 列表）
        for (const auto& arg_expr : apply_expr->rand) {
            if (does_expr_reference(arg_expr, var_name)) {
                return true;
            }
        }
        return false;
    }

    // 3. Begin 表达式：检查所有子表达式
    if (auto* begin_expr = dynamic_cast<Begin*>(expr.get())) {
        for (const auto& sub_expr : begin_expr->es) {
            if (does_expr_reference(sub_expr, var_name)) {
                return true;
            }
        }
    }

    // 4. If 表达式：检查条件、then 分支、else 分支
    if (auto* if_expr = dynamic_cast<If*>(expr.get())) {
        return does_expr_reference(if_expr->cond, var_name) ||
               does_expr_reference(if_expr->conseq, var_name) ||
               does_expr_reference(if_expr->alter, var_name);
    }

    // 5. Lambda 表达式：检查函数体（忽略参数名阴影）
    if (auto* lambda_expr = dynamic_cast<Lambda*>(expr.get())) {
        return does_expr_reference(lambda_expr->e, var_name);
    }

    // 6. 其他表达式（数字、布尔、字符串、Null 等）：不引用变量
    return false;
}
// Value Define::eval(Assoc &env) {
// 	std::string var_name = this->var;
//     Expr value_expr = this->e;
// 	if (primitives.find(var_name) != primitives.end()) {
//         throw RuntimeError("Cannot redefine primitive function: '" + var_name + "'fuck");
//     }
//
//     // 2. 检查是否与保留字（reserved_words）重名
//     if (reserved_words.find(var_name) != reserved_words.end()) {
//         throw RuntimeError("Cannot use reserved word as variable: '" + var_name + "'fuck");
//     }
// 	// 3. 处理递归
// 	bool is_recursive = false;
//     if (auto* lambda_expr = dynamic_cast<Lambda*>(value_expr.get())) {
//         // 检查 lambda 函数体是否引用了自身（调用之前实现的辅助函数）
//         if (does_expr_reference(lambda_expr->e, var_name)) {
//             is_recursive = true;
//             // 占位：先绑定一个临时值（如 Void），避免函数体求值时未定义
//             env = extend(var_name, Value(new Void()), env);
//         }
//     }
//     Value final_val = value_expr->eval(env);
//     if (is_recursive) {
//         modify(var_name, final_val, env);  // 更新占位符为真实闭包
//     } else {
//         // 非递归：存在则更新，不存在则新增绑定
//         if (find(var_name, env).get() != nullptr) {
//             modify(var_name, final_val, env);  // 重定义
//         } else {
//             env = extend(var_name, final_val, env);  // 新定义
//         }
//     }
//
//     // Scheme 约定：define 不返回有意义的值（返回 Void）
//     return Value(new Void());
// }

Value Define::eval(Assoc &env) {
    std::string var_name = this->var;
    Expr value_expr = this->e;

    if (primitives.find(var_name) != primitives.end() || reserved_words.find(var_name) != reserved_words.end()) {
        throw RuntimeError("Cannot redefine primitive or reserved word: '" + var_name + "'");
    }

    // 核心修复：总是先创建占位绑定
    env = extend(var_name, VoidV(), env);

    Value final_val = value_expr->eval(env);

    // 用最终值更新占位符
    modify(var_name, final_val, env);

    return VoidV();
}

Value Let::eval(Assoc &env) {
    Assoc localEnv = env;
    for (const auto& binding : bind) {
        std::string var = binding.first;
        if(var.empty()){
            throw RuntimeError("an block?what a fuckerman you are!! GRRRRRRRRRRRR");
        }
        char first = var[0];
        // 首字符不能是数字、.、@
        if (isdigit(static_cast<unsigned char>(first)) || first == '.' || first == '@') {
            throw RuntimeError("if you keep inputing these invalid symbols ,i will fuck your ass" );
        }
        // 检查所有字符：不能包含 #、'、"、` 或空白
        for (int i = 0; i < var.size(); i++) {
            char other = var[i];
            if (other == '#' || other == '\'' || other == '"' || other == '`' || isspace(static_cast<unsigned char>(other))) {
                throw RuntimeError("if you keep inputing these invalid symbols ,i will fuck your ass");
            }
        }
        Value boundValue = binding.second->eval(env);
        // 计算绑定
        localEnv = extend(binding.first, boundValue, localEnv);
    }
    //TODO: To complete the let logic
    return body->eval(localEnv);
}

Value Letrec::eval(Assoc &env) {
    Assoc localEnv = env;
    for (const auto& binding : bind) {
        std::string var = binding.first;
        if(var.empty()){
            throw RuntimeError("an block?what a fuckerman you are!! GRRRRRRRRRRRR");
        }
        char first = var[0];
        if (isdigit(static_cast<unsigned char>(first)) || first == '.' || first == '@') {
            throw RuntimeError("if you keep inputing these invalid symbols ,i will fuck your ass" );
        }
        for (int i = 0; i < var.size(); i++) {
            char other = var[i];
            if (other == '#' || other == '\'' || other == '"' || other == '`' || isspace(static_cast<unsigned char>(other))) {
                throw RuntimeError("if you keep inputing these invalid symbols ,i will fuck your ass");
            }
        }
        localEnv = extend(var, VoidV(), localEnv);
    }
    for (const auto& binding : bind) {
        Value boundValue = binding.second->eval(localEnv);
        // 计算绑定
        localEnv = extend(binding.first, boundValue, localEnv);
    }
    //TODO: To complete the letrec logic
    return body->eval(localEnv);
}

Value Set::eval(Assoc &env) {
    //TODO: To complete the set logic
    return nullptr;
}

Value Display::evalRator(const Value &rand) { // display function
    if (rand->v_type == V_STRING) {
        String* str_ptr = dynamic_cast<String*>(rand.get());
        std::cout << str_ptr->s;
    } else {
        rand->show(std::cout);
    }
    
    return VoidV();
}
