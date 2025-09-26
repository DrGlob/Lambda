#include <vector>
#include <memory>
#include <iostream>
#include <cassert>
#include <functional>

class Term {
public:
    virtual ~Term() = default;
    virtual std::string toString() const = 0;
    virtual std::shared_ptr<Term> reduce() const = 0;
    virtual std::shared_ptr<Term> substitute(const std::shared_ptr<Term>& arg, const std::string& varName) const = 0;
};

using TermPtr = std::shared_ptr<Term>;

class Var : public Term {
    std::string name;
public:
    Var(const std::string& v) : name(v) {}
    std::string toString() const override { return name; }
    TermPtr reduce() const override {
        return std::make_shared<Var>(*this);
    }
    TermPtr substitute(const TermPtr& arg, const std::string& varName) const override {
        if (name == varName) return arg;
        return std::make_shared<Var>(*this);
    }
};

class Atom : public Term {
    std::string name;
    std::function<TermPtr(std::vector<TermPtr>)> reducer;
public:
    int arity;
    Atom(const std::string& n, int a, std::function<TermPtr(std::vector<TermPtr>)> r) 
        : name(n), arity(a), reducer(r) {}
    
    std::string toString() const override { 
        return name; 
    }
    
    TermPtr reduce() const override { 
        return std::make_shared<Atom>(*this); 
    }
    
    TermPtr apply(const std::vector<TermPtr>& args) const {
        if (args.size() != arity) {
            throw std::runtime_error("Arity mismatch for atom " + name);
        }
        return reducer(args);
    }
    
    TermPtr substitute(const TermPtr& arg, const std::string& varName) const override {
        return std::make_shared<Atom>(*this);
    }
};

class Lambda : public Term {
    std::string param;
    TermPtr body;
public:
    Lambda(const std::string& p, TermPtr b) : param(p), body(b) {}
    std::string toString() const override {
        return "λ" + param + ". " + body->toString();
    }
    TermPtr reduce() const override {
        auto reduced_body = body->reduce();
        return std::make_shared<Lambda>(param, reduced_body);
    }
    TermPtr apply(const TermPtr& arg) const {
        return body->substitute(arg, param);
    }
    TermPtr substitute(const TermPtr& arg, const std::string& varName) const override {
        if (param == varName) return std::make_shared<Lambda>(*this);
        return std::make_shared<Lambda>(param, body->substitute(arg, varName));
    }
};

class App : public Term {
    std::vector<TermPtr> terms;
public:
    App(const std::vector<TermPtr>& t) : terms(t) {}

    std::string toString() const override {
        if (terms.empty()) return "()";
        
        std::string result = terms[0]->toString();
        for (size_t i = 1; i < terms.size(); i++) {
            // Для правой ассоциативности: f x y z = (f x) (y z) НЕТ!
            // Правильно: f x y z = ((f x) y) z - левая ассоциативность
            // Но если у нас правая, то нужно показывать как f (x (y z))
            if (i < terms.size() - 1) {
                result = "(" + result + " " + terms[i]->toString() + ")";
            } else {
                result = result + " " + terms[i]->toString();
            }
        }
        return result;
    }

    TermPtr reduce() const override {
        if (terms.empty()) {
            return std::make_shared<App>(terms);
        }
        
        // Редуцируем голову (функцию)
        auto reduced_head = terms[0]->reduce();
        
        // Если голова - лямбда и есть хотя бы один аргумент
        if (auto lambda = std::dynamic_pointer_cast<Lambda>(reduced_head)) {
            if (terms.size() >= 2) {
                // Бета-редукция: применяем лямбду к первому аргументу
                TermPtr new_head = lambda->apply(terms[1]);
                
                // Создаем новое применение с оставшимися аргументами
                std::vector<TermPtr> new_terms = {new_head};
                for (size_t i = 2; i < terms.size(); i++) {
                    new_terms.push_back(terms[i]);
                }
                
                return std::make_shared<App>(new_terms)->reduce();
            }
        }
        
        // Если голова - атом с достаточной арностью
        if (auto atom = std::dynamic_pointer_cast<Atom>(reduced_head)) {
            if (terms.size() - 1 >= atom->arity) {
                std::vector<TermPtr> args(terms.begin() + 1, terms.end());
                return atom->apply(args);
            }
        }
        
        // Если ничего не применилось, но голова редуцировалась
        if (reduced_head != terms[0]) {
            std::vector<TermPtr> new_terms = {reduced_head};
            for (size_t i = 1; i < terms.size(); i++) {
                new_terms.push_back(terms[i]);
            }
            return std::make_shared<App>(new_terms);
        }
        
        // Пытаемся редуцировать аргументы (нормальный порядок)
        for (size_t i = 1; i < terms.size(); i++) {
            auto reduced_arg = terms[i]->reduce();
            if (reduced_arg != terms[i]) {
                std::vector<TermPtr> new_terms = terms;
                new_terms[i] = reduced_arg;
                return std::make_shared<App>(new_terms)->reduce();
            }
        }
        
        return std::make_shared<App>(terms);
    }

    TermPtr substitute(const TermPtr& arg, const std::string& varName) const override {
        std::vector<TermPtr> new_terms;
        for (const auto& term : terms) {
            new_terms.push_back(term->substitute(arg, varName));
        }
        return std::make_shared<App>(new_terms);
    }
};

TermPtr var(const std::string& name) { return std::make_shared<Var>(name); }
TermPtr lambda(const std::string& p, TermPtr b) { return std::make_shared<Lambda>(p, b); }
TermPtr app(const std::vector<TermPtr>& t) { return std::make_shared<App>(t); }

// Вспомогательная функция для создания аппликаций с правой ассоциативностью
TermPtr app_right(const std::vector<TermPtr>& terms) {
    if (terms.empty()) return app({});
    if (terms.size() == 1) return terms[0];
    
    // Правая ассоциативность: a b c d = (a (b (c d)))
    TermPtr result = terms.back();
    for (int i = terms.size() - 2; i >= 0; i--) {
        result = app({terms[i], result});
    }
    return result;
}

// Тесты для правой ассоциативности
int main() {
    // Тест 1: (λx. λy. x) b a → a (правая ассоциативность)
    auto test1 = app_right({
        lambda("x", lambda("y", var("x"))),
        var("b"),
        var("a")
    });
    std::cout << "Тест 1 (правая ассоциативность): " << test1->toString() << " → " << test1->reduce()->toString() << std::endl;
    
    // Тест 2: (λx. x) (λy. y) a → a
    auto test2 = app_right({
        lambda("x", var("x")),
        lambda("y", var("y")),
        var("a")
    });
    std::cout << "Тест 2: " << test2->toString() << " → " << test2->reduce()->toString() << std::endl;
    
    // Тест 3: Ваш пример - (λx. λy. x) b a
    auto test3 = app({
        app({
            lambda("x", lambda("y", var("x"))),
            var("b")
        }),
        var("a")
    });
    std::cout << "Тест 3 (ваш пример): " << test3->toString() << " → " << test3->reduce()->toString() << std::endl;
    
    return 0;
}