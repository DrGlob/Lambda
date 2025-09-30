#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <functional>
#include <stdexcept>

// Тип данных для лямбда-выражений
struct Term;

using TermPtr = std::shared_ptr<Term>;
using TermList = std::vector<TermPtr>;
using ImplFunc = std::function<TermPtr(TermList)>;

struct Term {
    enum class Type { VAR, ATOM, APP, LAMBDA };
    
    Type type;
    
    // Для VAR
    std::string var_name;
    
    // Для ATOM
    std::string atom_name;
    int arity;
    ImplFunc impl;
    
    // Для APP
    TermList app_terms;
    
    // Для LAMBDA
    std::string bound_var;
    TermPtr body_term;
    
    // Конструкторы
    static TermPtr Var(const std::string& name) {
        auto term = std::make_shared<Term>();
        term->type = Type::VAR;
        term->var_name = name;
        return term;
    }
    
    static TermPtr Atom(const std::string& name, int arity, ImplFunc impl) {
        auto term = std::make_shared<Term>();
        term->type = Type::ATOM;
        term->atom_name = name;
        term->arity = arity;
        term->impl = impl;
        return term;
    }
    
    static TermPtr App(const TermList& terms) {
        auto term = std::make_shared<Term>();
        term->type = Type::APP;
        term->app_terms = terms;
        return term;
    }
    
    static TermPtr Lambda(const std::string& bound_var, TermPtr body_term) {
        auto term = std::make_shared<Term>();
        term->type = Type::LAMBDA;
        term->bound_var = bound_var;
        term->body_term = body_term;
        return term;
    }
};

// Функция подстановки
TermPtr substitute(TermPtr arg, const std::string& n, TermPtr body) {
    switch (body->type) {
        case Term::Type::VAR:
            if (body->var_name == n) {
                return arg;
            }
            return body;
            
        case Term::Type::APP: {
            TermList new_terms;
            for (const auto& term : body->app_terms) {
                new_terms.push_back(substitute(arg, n, term));
            }
            return Term::App(new_terms);
        }
            
        case Term::Type::LAMBDA:
            if (body->bound_var == n) {
                return body;
            }
            return Term::Lambda(body->bound_var, substitute(arg, n, body->body_term));
            
        case Term::Type::ATOM:
            return body;
    }
    return body;
}

// Преобразование термина в строку
std::string term_to_string(TermPtr t) {
    switch (t->type) {
        case Term::Type::VAR:
            return t->var_name;
            
        case Term::Type::ATOM:
            return t->atom_name;
            
        case Term::Type::LAMBDA:
            return "(L" + t->bound_var + "." + term_to_string(t->body_term) + ")";
            
        case Term::Type::APP: {
            std::string result = "(";
            for (size_t i = 0; i < t->app_terms.size(); ++i) {
                result += term_to_string(t->app_terms[i]);
                if (i < t->app_terms.size() - 1) {
                    result += " ";
                }
            }
            result += ")";
            return result;
        }
    }
    return "";
}

// Функция редукции
TermPtr reduce(TermPtr t) {
    switch (t->type) {
        case Term::Type::VAR:
            return t;
            
        case Term::Type::ATOM:
            return t;
            
        case Term::Type::APP:
            if (!t->app_terms.empty()) {
                auto first = reduce(t->app_terms[0]);
                
                // Обработка ATOM
                if (first->type == Term::Type::ATOM) {
                    if (t->app_terms.size() - 1 >= first->arity) {
                        TermList args;
                        for (size_t i = 1; i < t->app_terms.size(); ++i) {
                            args.push_back(reduce(t->app_terms[i]));
                        }
                        return first->impl(args);
                    } else {
                        TermList new_terms = {first};
                        for (size_t i = 1; i < t->app_terms.size(); ++i) {
                            new_terms.push_back(reduce(t->app_terms[i]));
                        }
                        return Term::App(new_terms);
                    }
                }
                // Обработка LAMBDA
                else if (first->type == Term::Type::LAMBDA) {
                    if (t->app_terms.size() >= 2) {
                        auto arg = reduce(t->app_terms[1]);
                        TermList tail;
                        for (size_t i = 2; i < t->app_terms.size(); ++i) {
                            tail.push_back(reduce(t->app_terms[i]));
                        }
                        auto new_body = substitute(arg, first->bound_var, first->body_term);
                        if (tail.empty()) {
                            return reduce(new_body);
                        } else {
                            tail.insert(tail.begin(), new_body);
                            return reduce(Term::App(tail));
                        }
                    } else {
                        return Term::Lambda(first->bound_var, reduce(first->body_term));
                    }
                }
                // Рекурсивная обработка головы
                else {
                    TermList new_terms = {first};
                    for (size_t i = 1; i < t->app_terms.size(); ++i) {
                        new_terms.push_back(reduce(t->app_terms[i]));
                    }
                    return Term::App(new_terms);
                }
            }
            return t;
            
        case Term::Type::LAMBDA:
            return Term::Lambda(t->bound_var, reduce(t->body_term));
    }
    return t;
}

// Комбинаторы
TermPtr cK = Term::Atom("K", 2, [](TermList args) {
    if (args.size() >= 2) {
        return args[0];
    }
    throw std::runtime_error("K: wrong number of arguments");
});

TermPtr cS = Term::Atom("S", 3, [](TermList args) {
    if (args.size() >= 3) {
        TermList new_args = {
            args[0],
            args[2],
            Term::App({args[1], args[2]})
        };
        // Добавляем оставшиеся аргументы
        for (size_t i = 3; i < args.size(); ++i) {
            new_args.push_back(args[i]);
        }
        return Term::App(new_args);
    }
    throw std::runtime_error("S: wrong number of arguments");
});

int main() {
    // Тестирование
    try {
        // y = reduce(App[cS; cK; cK; Var "x"])
        auto y = reduce(Term::App({cS, cK, cK, Term::Var("x")}));
        std::string r = term_to_string(y);
        std::cout << "y = " << r << std::endl;
        
        // os1 = App[Lambda("x", Lambda("y", App[Var "x"; Var "y"; Var "z"])); (App[cK; Var "z"]) ]
        auto os1 = Term::App({
            Term::Lambda("x", Term::Lambda("y", Term::App({Term::Var("x"), Term::Var("y"), Term::Var("z")}))),
            Term::App({cK, Term::Var("z")})
        });
        
        auto o1 = reduce(os1);
        auto o3 = term_to_string(o1);
        std::cout << "o1 = " << o3 << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
    
    return 0;
}