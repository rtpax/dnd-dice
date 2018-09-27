#include <iostream>
#include <vector>
#include <random>
#include <chrono>
#include <algorithm>
#include <set>

enum class token_t {
    number, x, mul, div, mod, add, sub, dice, lparen, rparen, choose
};

struct token {
    token_t type;
    int value;
};

token_t c_to_op(char arg) {
    switch(arg) {
    case '*':
        return token_t::mul;
    case '/':
        return token_t::div;
    case '%':
        return token_t::mod;
    case '+':
        return token_t::add;
    case '-':
        return token_t::sub;
    case 'd':
        return token_t::dice;
    case ':':
        return token_t::choose;
    case 'x':
        return token_t::x;
    case '(':
        return token_t::lparen;
    case ')':
        return token_t::rparen;
    default:
        throw std::runtime_error("not a valid operation");
    }
}

static std::mt19937 rng(std::chrono::high_resolution_clock::now()
    .time_since_epoch().count());

int do_binary_op(token_t op, int arg1, int arg2) {
    switch(op) {
    case token_t::dice: {
        if (arg2 < 1)
            throw std::runtime_error("dice number must be at least 1");
        std::uniform_int_distribution<int> die(1, arg2);
        int sum = 0;
        for (int i = 0; i < arg1; ++i) {
            sum += die(rng);
        }
        return sum;
    }
    case token_t::mul:
        return arg1 * arg2;
    case token_t::div:
        if(arg2 == 0)
            throw std::runtime_error("cannot divide by zero");
        return arg1 / arg2;
    case token_t::mod:
        if(arg2 == 0)
            throw std::runtime_error("cannot mod by zero");
        return arg1 % arg2;
    case token_t::add:
        return arg1 + arg2;
    case token_t::sub:
        return arg1 - arg2;
    default:
        throw std::logic_error("not a binary operation on integers");
    }
}

int roll_choose(int arg1, int arg2, int arg3) {
    if (arg2 < 1)
        throw std::runtime_error("dice number must be at least 1");
    if (arg3 > arg1)
        throw std::runtime_error("cannot choose more dice than rolled");
    std::uniform_int_distribution<int> die(1, arg2);
    std::multiset<int> rolls;
    for (int i = 0; i < arg1; ++i) {
        rolls.insert(die(rng));
    }
    auto it = rolls.rbegin();
    int sum = 0;
    for(int i = 0; i < arg3; ++it, ++i) {
        sum += *it;        
    }
    return sum;
}

class expression {
private:
    expression() {}
    static bool is_digit(char c) {
        return (c >= '0' && c <= '9');
    }

    std::string input;
    std::vector<token> toks;

    // also checks that there is exactly one x, no depth
    bool balanced_parens() const { 
        int depth = 0;
        bool found_x = false;
        for(const token& t : toks) {
            if(t.type == token_t::lparen)
                ++depth;
            if(t.type == token_t::rparen)
                --depth;
            if(depth < 0)
                return false;
            if(t.type == token_t::x) {
                if (!found_x && depth == 0)
                    found_x = true;
                else
                    return false;
            }
        }
        return depth == 0;
    }
    bool valid_ops() const {
        if (!balanced_parens())
            return false;
        bool expecting_num = true;
        for(const token& t : toks) {
            if(expecting_num) {
                if(t.type == token_t::number) {
                    expecting_num = false;
                } else if (t.type == token_t::lparen) {
                    //pass it on
                } else {
                    return false;
                }
            } else {
                if(t.type == token_t::number || t.type == token_t::lparen) {
                    return false;
                } else if (t.type == token_t::rparen) {
                    //pass it on
                } else {
                    expecting_num = true;
                }
            }
        }
        return !expecting_num;
    }

    static std::mt19937 rng;
    static auto apply_operator(std::vector<token>& sub, std::vector<token>::iterator it) {
        auto arg1 = std::prev(it);
        auto arg2 = std::next(it);

        if(arg1->type != token_t::number || arg2->type != token_t::number)
            throw std::logic_error("operator arguments must be numbers");
        
        auto choose = std::next(arg2);
        if(it->type == token_t::dice && choose != sub.end() && choose->type == token_t::choose) {
            auto arg3 = std::next(choose);
            arg1->value = roll_choose(arg1->value, arg2->value, arg3->value);
            return sub.erase(it, std::next(it, 4));
        }
        arg1->value = do_binary_op(it->type, arg1->value, arg2->value);
        return sub.erase(it, std::next(it, 2));
    }

    static int evaluate_subtok(std::vector<token>& sub) {
        for(auto it = sub.begin(); it != sub.end(); ++it) {
            if(it->type == token_t::lparen) {
                auto pair = it;
                int depth = 0;
                for(; pair != sub.end(); ++pair) {
                    if(pair->type == token_t::lparen) {
                        ++depth;
                    }
                    if(pair->type == token_t::rparen) {
                        --depth;
                        if(depth == 0)
                            break;
                    }
                }
                if (depth != 0 || pair == sub.end())
                    throw std::logic_error("unbalanced paretheses");
                std::vector<token> subsub(std::next(it),pair);
                sub.erase(std::next(it), std::next(pair));
                it->type = token_t::number;
                it->value = evaluate_subtok(subsub);
            }
        }

        for(auto it = sub.begin(); it != sub.end();) {
            if(it->type == token_t::dice) {
                it = apply_operator(sub, it);
            } else if (it->type == token_t::choose) {
                throw std::runtime_error("choose without dice");
            } else {
                ++it;
            }
        }

        for(auto it = sub.begin(); it != sub.end();) {
            if(it->type == token_t::mul || it->type == token_t::div || it->type == token_t::mod) {
                it = apply_operator(sub, it);
            } else {
                ++it;
            }
        }

        for(auto it = sub.begin(); it != sub.end();) {
            if(it->type == token_t::add || it->type == token_t::sub) {
                it = apply_operator(sub, it);
            } else {
                ++it;
            }
        }

        if(sub.size() != 1 || sub.front().type != token_t::number)
            throw std::runtime_error("could not fully reduce expression");

        return sub.front().value;
    }

    void tokenize() {
        bool on_digit = true;
        std::string tok;
        for(char c : input) {
            if(is_digit(c)) {
                if(!on_digit) {
                    on_digit = true;
                    tok = "";
                }
                tok += c;
            } else {
                if(on_digit) {
                    if(!tok.empty())
                        toks.push_back(token{token_t::number, std::stoi(tok)});
                    on_digit = false;
                    tok = "";
                }
                if(c == 'd' && (toks.empty() || (toks.back().type != token_t::number && toks.back().type != token_t::rparen)))
                    toks.push_back(token{token_t::number, 1});
                if(c != ' ')
                    toks.push_back(token{c_to_op(c), 0});
            }
        }
        if(on_digit && !tok.empty()) {
            toks.push_back(token{token_t::number, std::stoi(tok)});
            on_digit = false;
            tok = "";
        }
    }
public:
    expression(std::string arg) {
        input = arg;
        tokenize();
        if(!valid_ops())
            throw std::runtime_error("bad expression string");
    }
    std::vector<int> operator()() const {
        int rep = 1;
        auto base_it = std::find_if(toks.cbegin(), toks.cend(), 
            [](const token& t){ return t.type == token_t::x; });
        if (base_it == toks.cend())
            base_it = toks.cbegin();
        else {
            std::vector<token> rep_subtok(toks.cbegin(), base_it);
            rep = evaluate_subtok(rep_subtok);
            ++base_it;
        }
        
        std::vector<int> ret;
        for(int i = 0; i < rep; ++i) {
            std::vector<token> subtok(base_it, toks.cend());
            ret.push_back(evaluate_subtok(subtok));
        }
        return ret;
    }
    const std::string& to_string() const { return input; }
};



int main(int argc, char ** argv) {
    for(int i = 1; i < argc; ++i) {
        std::cout << argv[i] << ":\n";
        try {
            expression e(argv[i]);
            auto result = e();
            for(int r : result)
                std::cout << "    " << r << "\n";
        } catch (const std::runtime_error& e) {
            std::cout << "    error: " << e.what() << "\n";
        }
    }
}

