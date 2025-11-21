/**
 * @file parser.cpp
 * @brief Parsing implementation for Scheme syntax tree to expression tree conversion
 *
 * This file implements the parsing logic that converts syntax trees into
 * expression trees that can be evaluated.
 * primitive operations, and function applications.
 */

#include "RE.hpp"
#include "Def.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include "expr.hpp"
#include <map>
#include <string>
#include <iostream>

#define mp make_pair
using std::string;
using std::vector;
using std::pair;
using std::dynamic_pointer_cast;

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

/**
 * @brief Helper function: Parse list of syntax nodes to vector of Expr (for parameters/body)
 */
vector<Expr> parse_expr_list(const vector<Syntax>& stxs, Assoc &env) {
    vector<Expr> exprs;
    for (const auto& stx : stxs) {
        exprs.push_back(stx->parse(env));
    }
    return exprs;
}

/**
 * @brief Helper function: Parse lambda parameter list (Syntax List → vector<string>)
 */
vector<string> parse_lambda_params(const std::vector<Syntax>& param_stx, Assoc &env) {
    vector<string> params;

    //WARNING: 根据定义并不会有以下形式出现，是不是 AI 生成的呃呃？

    // List* param_list = dynamic_cast<List*>(param_stx.get()->get());
    // if (!param_list) {
    //     // Single parameter (e.g., (lambda x x) → params = {"x"})
    //     SymbolSyntax* single_param = dynamic_cast<SymbolSyntax*>(param_stx.get()->get());
    //     if (single_param) {
    //         params.push_back(single_param->s);
    //         return params;
    //     }
    //     throw RuntimeError("Invalid lambda parameter list");
    // }

    // Multiple parameters (e.g., (a b c) → params = {"a", "b", "c"})
    for (const auto& stx : param_stx) {
        SymbolSyntax* param_sym = dynamic_cast<SymbolSyntax*>(stx.get());
        if (!param_sym) {
            throw RuntimeError("Lambda parameters must be symbols");
        }
        params.push_back(param_sym->s);
    }
    return params;
}

/**
 * @brief Helper function: Check if list is function shorthand (define (name args...) body...)
 */
bool is_define_shorthand(const vector<Syntax>& stxs) {
    if (stxs.size() < 2) return false;
    // Second element must be a List starting with Symbol (e.g., (sum3 a b c))
    List* func_list = dynamic_cast<List*>(stxs[1].get());
    if (!func_list || func_list->stxs.empty()) return false;
    return dynamic_cast<SymbolSyntax*>(func_list->stxs[0].get()) != nullptr;
}

/**
 * @brief Default parse method (should be overridden by subclasses)
 */
Expr Syntax::parse(Assoc &env) {
    throw RuntimeError("Unimplemented parse method");
}

Expr Number::parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr RationalSyntax::parse(Assoc &env) {
    // Parse rational number (e.g., 1/2 → RationalExpr)
    return Expr(new RationalNum(numerator, denominator));
}

Expr SymbolSyntax::parse(Assoc &env) {
    return Expr(new Var(s));
}

Expr StringSyntax::parse(Assoc &env) {
    return Expr(new StringExpr(s));
}

Expr TrueSyntax::parse(Assoc &env) {
    return Expr(new True());
}

Expr FalseSyntax::parse(Assoc &env) {
    return Expr(new False());
}

Expr List::parse(Assoc &env) {
    if (stxs.empty()) {
        // Empty list → (quote ())
        return Expr(new Quote(Syntax(new List())));
    }

    // Step 1: Check if first element is Symbol (for special forms/primitives/variables)

    SymbolSyntax* id = dynamic_cast<SymbolSyntax*>(stxs[0].get());
    if (id == nullptr) {
        // Non-symbol first element → function application (Apply)
        // e.g., ((lambda (x) x) 5) → Apply(lambda_expr, {5})
        Expr func = stxs[0]->parse(env);
        vector<Expr> params = parse_expr_list(vector<Syntax>(stxs.begin()+1, stxs.end()), env);
        return Expr(new Apply(func, params));
    }

    string op = id->s;

    // Step 2: check if symbol is quote
    if (reserved_words.count(op) != 0 && reserved_words[op] == E_QUOTE) {
            // (quote expr)
            if (stxs.size() != 2) throw RuntimeError("quote requires exactly 1 argument");
            return Expr(new Quote(stxs[1]));
    }

    // Step 3: Parse other reserved words (highest priority)
    if (reserved_words.count(op) != 0) {
        switch (reserved_words[op]) {
            case E_DEFINE: {
                // (define var expr) 或 (define (name args...) body...)
                if (stxs.size() < 2) throw RuntimeError("define requires at least 2 arguments");

                // Handle function shorthand: (define (name args...) body...) → (define name (lambda (args...) body...))
                if (is_define_shorthand(stxs)) {
                    List* func_list = dynamic_cast<List*>(stxs[1].get());
                    string func_name = dynamic_cast<SymbolSyntax*>(func_list->stxs[0].get())->s;

                    // Parse parameters: (args...) → vector<string>
                    vector<Syntax> param_stxs(func_list->stxs.begin()+1, func_list->stxs.end());
                    vector<string> lambda_params = parse_lambda_params(param_stxs, env);

                    // Parse body: stxs[2..end] → wrapped in Begin
                    vector<Expr> lambda_body = parse_expr_list(vector<Syntax>(stxs.begin()+2, stxs.end()), env);
                    Expr body = (lambda_body.size() == 1) ? lambda_body[0] : Expr(new Begin(lambda_body));

                    // Create lambda expression
                    Expr lambda = Expr(new Lambda(lambda_params, body));

                    // Return Define expression: (define func_name lambda)
                    return Expr(new Define(func_name, lambda));
                }

                // Normal variable define: (define var expr)
                SymbolSyntax* var_stx = dynamic_cast<SymbolSyntax*>(stxs[1].get());
                if (!var_stx) throw RuntimeError("define first argument must be symbol");
                string var_name = var_stx->s;
                if (stxs.size() != 3) throw RuntimeError("define requires exactly 2 arguments for variable");
                Expr value_expr = stxs[2]->parse(env);
                return Expr(new Define(var_name, value_expr));
            }

            case E_LAMBDA: {
                // (lambda (args...) body...)
                if (stxs.size() < 3) throw RuntimeError("lambda requires at least 2 arguments");

                // Parse parameters
                List* func_list = dynamic_cast<List*>(stxs[1].get());
                vector<Syntax> param_stxs(func_list->stxs.begin(), func_list->stxs.end());
                vector<string> lambda_params = parse_lambda_params(param_stxs, env);

                // Parse body (wrap multiple expressions in Begin)
                vector<Expr> lambda_body = parse_expr_list(vector<Syntax>(stxs.begin()+2, stxs.end()), env);
                Expr body = (lambda_body.size() == 1) ? lambda_body[0] : Expr(new Begin(lambda_body));

                return Expr(new Lambda(lambda_params, body));
            }

            case E_IF: {
                // (if cond conseq [alter])
                if (stxs.size() < 3 || stxs.size() > 4) throw RuntimeError("if requires 2 or 3 arguments");
                Expr cond = stxs[1]->parse(env);
                Expr conseq = stxs[2]->parse(env);
                Expr alter = (stxs.size() == 4) ? stxs[3]->parse(env) : Expr(new MakeVoid());
                return Expr(new If(cond, conseq, alter));
            }

            case E_BEGIN: {
                // (begin expr1 expr2 ...)
                vector<Expr> begin_body = parse_expr_list(vector<Syntax>(stxs.begin()+1, stxs.end()), env);
                return Expr(new Begin(begin_body));
            }

            default:
                throw RuntimeError("Unknown reserved word: " + op);
        }
    }

    vector<Expr> params = parse_expr_list(vector<Syntax>(stxs.begin()+1, stxs.end()), env);
    // Step 4: Parse primitive functions
    if (primitives.count(op) != 0) {
        ExprType op_type = primitives[op];
        switch (op_type) {
            // 可变参数内置函数（用 Var 版本）
            case E_PLUS:
                return Expr(new PlusVar(params));
            case E_MINUS:
                return Expr(new MinusVar(params));
            case E_MUL:
                return Expr(new MultVar(params));
            case E_DIV:
                return Expr(new DivVar(params));
            case E_LT:
                return Expr(new LessVar(params));
            case E_LE:
                return Expr(new LessEqVar(params));
            case E_EQ:
                return Expr(new EqualVar(params));
            case E_GE:
                return Expr(new GreaterEqVar(params));
            case E_GT:
                return Expr(new GreaterVar(params));
            case E_AND:
                return Expr(new AndVar(params));
            case E_OR:
                return Expr(new OrVar(params));
            case E_LIST:
                return Expr(new ListFunc(params));

            // 固定参数内置函数
            case E_MODULO:
                if (params.size() != 2) throw RuntimeError("modulo requires exactly 2 arguments");
                return Expr(new Modulo(params[0], params[1]));
            case E_EXPT:
                if (params.size() != 2) throw RuntimeError("expt requires exactly 2 arguments");
                return Expr(new Expt(params[0], params[1]));
            case E_CONS:
                if (params.size() != 2) throw RuntimeError("cons requires exactly 2 arguments");
                return Expr(new Cons(params[0], params[1]));
            case E_CAR:
                if (params.size() != 1) throw RuntimeError("car requires exactly 1 argument");
                return Expr(new Car(params[0]));
            case E_CDR:
                if (params.size() != 1) throw RuntimeError("cdr requires exactly 1 argument");
                return Expr(new Cdr(params[0]));

            case E_EQQ:
                if (params.size() != 2) throw RuntimeError("eq? requires exactly 2 argument");
                return Expr(new IsEq(params[0], params[1]));
            case E_BOOLQ:
                if (params.size() != 1) throw RuntimeError("boolean? requires exactly 1 argument");
                return Expr(new IsBoolean(params[0]));
            case E_INTQ:
                if (params.size() != 1) throw RuntimeError("fixnum? requires exactly 1 argument");
                return Expr(new IsFixnum(params[0]));
            case E_NULLQ:
                if (params.size() != 1) throw RuntimeError("null? requires exactly 1 argument");
                return Expr(new IsNull(params[0]));
            case E_PAIRQ:
                if (params.size() != 1) throw RuntimeError("pair? requires exactly 1 argument");
                return Expr(new IsPair(params[0]));
            case E_PROCQ:
                if (params.size() != 1) throw RuntimeError("procedure? requires exactly 1 argument");
                return Expr(new IsProcedure(params[0]));
            case E_SYMBOLQ:
                if (params.size() != 1) throw RuntimeError("symbol? requires exactly 1 argument");
                return Expr(new IsSymbol(params[0]));
            case E_LISTQ:
                if (params.size() != 1) throw RuntimeError("list? requires exactly 1 argument");
                return Expr(new IsList(params[0]));
            case E_STRINGQ:
                if (params.size() != 1) throw RuntimeError("string? requires exactly 1 argument");
                return Expr(new IsString(params[0]));
            case E_DISPLAY:
                if (params.size() != 1) throw RuntimeError("display requires exactly 1 argument");
                return Expr(new Display(params[0]));
            case E_NOT:
                if (params.size() != 1) throw RuntimeError("not requires exactly 1 argument");
                return Expr(new Not(params[0]));

            case E_EXIT:
                if (params.size() != 1) throw RuntimeError("E_EXIT requires exactly 1 argument");
                return Expr(new Exit());
            case E_VOID:
                if (params.size() != 1) throw RuntimeError("E_VOID requires exactly 1 argument");
                return Expr(new MakeVoid());

            default:
                throw RuntimeError("Unimplemented primitive: " + op);
        }
    }

    // Step 5: Parse user-defined variables/functions → function application
    // e.g., (sum3 1 2 3) → Apply(Var("sum3"), {1,2,3})
    Expr func = Expr(new Var(op));
    return Expr(new Apply(func, params));
}
