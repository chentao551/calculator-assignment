
#include<iostream>
#include<string>
#include<stack>
#include <vector>
#include <functional>
#include <unordered_map>




enum class tokentype {
	Leftp,
	Rightp,
	Number,
	Operator,
	Comma,
	Iden
};

struct token {
	tokentype type;
	std::string name="";
	double value=0;
};



class calculator {
	std::vector<token>& tokenvec;	
	size_t vecsize,pos;
	double curvalue;
	std::unordered_map<std::string, std::function<double(double, double)>> opsmap;
	std::unordered_map<std::string, std::function<double(const std::vector<double>&)>> funcs;
	std::unordered_map<std::string, double> vars;
	std::unordered_map<tokentype, std::function<double()>> handlers;
	
	struct parenchk {
		calculator& worker;
		parenchk(calculator& calcu):worker(calcu) {
		++worker.pos;
		};

		~parenchk() {
			if (worker.pos >= worker.vecsize||worker.tokenvec[worker.pos].type!=tokentype::Rightp) {
				throw std::invalid_argument("missing right parenthesis at pos" + std::to_string(worker.pos));
			}
			++worker.pos;
		}
	};
	
	double getunary() {
		if (tokenvec[pos].name == "-") {
			++pos;
			double value = getoperand();
			return -value;
		}
		if (tokenvec[pos].name == "+") {
			++pos;
			return getoperand();
		}
		return getoperand();
		}
	
	double getoperand() {
		tokentype tt = tokenvec[pos].type;		
		auto ite = handlers.find(tt);
		if (ite == handlers.end())throw std::runtime_error("invalid token '" + tokenvec[pos].name + "' at pos " + std::to_string(pos));
		return handlers.at(tt)();
	}
	double higherops() {
		double value = getunary();
		while (pos < vecsize && (tokenvec[pos].name == "*" || tokenvec[pos].name == "/")) {
			const auto& oper=opsmap.at(tokenvec[pos].name);
			++pos;
			double rhs = getoperand();
			if (tokenvec[pos].name == "/" && rhs == 0)throw std::runtime_error("divided by zero at pos " + std::to_string(pos) + " !");
			value = oper(value, rhs);
		}
		return value;	
	}

	double lowerops() {
		double value = higherops();
		while (pos < vecsize && (tokenvec[pos].name == "+" || tokenvec[pos].name == "-")) {
			const auto& oper = opsmap.at(tokenvec[pos].name);
			++pos;
			double rhs = higherops();
			value = oper(value, rhs);
		}
		return value;
	};
	double numberh() {
		double value = tokenvec[pos].value;
		++pos;
		return value;
	}
	double leftph() {
		double value;
		parenchk guard(*this);
		value = lowerops();
		return value;
	}
	double idenh() {
		double value;
		std::string& name = tokenvec[pos].name;
		size_t curpos = pos;
		++pos;
		//parse function
		if (pos < vecsize && tokenvec[pos].type == tokentype::Leftp) {
			auto ite = funcs.find(name);
			if (ite == funcs.end()) throw std::runtime_error("invalid function name at position " + std::to_string(curpos) + " !");
			parenchk guard(*this);
			std::vector<double> args;
			value = lowerops();
			args.push_back(value);
			while (tokenvec[pos].type == tokentype::Comma) {
				++pos;
				value = lowerops();
				args.push_back(value);
			}
			size_t expected = 0;
			if (name == "sin" || name == "cos" || name == "sqrt") expected = 1;
			else if (name == "pow") expected = 2;
			// Add more functions as needed

			if (args.size() != expected)
				throw std::runtime_error("Function '" + name + "' expects " + std::to_string(expected) +
					" argument(s), but got " + std::to_string(args.size()) + ".");
			return ite->second(args);
		}
		//parse vars
		auto ite = vars.find(name);
		if (ite == vars.end()) throw std::runtime_error("invalid vars name at position " + std::to_string(curpos) + " !");
		return ite->second;	
	}


public:
	calculator(std::vector<token>& intoken) : tokenvec(intoken), vecsize(intoken.size()), pos(0),curvalue(0) {
		opsmap = {
			{"+",[](double a, double b)->double { return a + b; } },
			{"-",[](double a, double b)->double { return a - b; } },
			{"*",[](double a, double b)->double { return a * b; } },
			{"/",[](double a, double b)->double { return a / b; } },
			{"^",[](double a, double b)->double { return std::pow(a,b);} }
		};

		funcs = {
			{"sin",[](const std::vector<double>& args)->double {double ss = std::sin(args[0]); if(ss==0 && std::signbit(ss) ) ss= 0; return ss;}},
			{"cos",[](const std::vector<double>& args)->double {return std::cos(args[0]);}},
			{"sqrt",[](const std::vector<double>& args)->double {return std::sqrt(args[0]);}},
			{"pow",[](const std::vector<double>& args)->double {return std::pow(args[0],args[1]);}}
		};
		vars = {
			{"pi",3.14159265},
			{"e",2.71828183}
		};

		handlers = {
			{tokentype::Number,[this]() {return numberh();}},
			{tokentype::Leftp,[this]() {return leftph();}},
			{tokentype::Iden,[this]() {return idenh();}}
		};
	};

	double calcuexpr() {
		curvalue = lowerops();
		if (pos < vecsize)throw std::runtime_error("invalid input " + tokenvec[pos].name + " at token position of " + std::to_string(pos) + " !");
		return curvalue;
	};

	void printresult() {
		if (curvalue == 0&&std::signbit(curvalue) )curvalue = 0;
		std::cout << "the result for expression: " << std::endl;
		for (auto& tk : tokenvec) {
			std::cout << tk.name << " ";
		}
		std::cout << "= " << std::to_string(curvalue) << std::endl;
	};
};


class tokenbuilder {

	const std::string& expr;
	const size_t exprsize;
	size_t pos;

public:
	std::vector<token> tokenvec;
	tokenbuilder(std::string& inputline) :expr(inputline), exprsize(inputline.size()), pos(0) {
		while (pos < exprsize) {
			skipspace();
			if (pos >= exprsize) break;
			char cc = expr[pos];
			if (cc == '(') {
				tokenvec.push_back({ tokentype::Leftp,std::string(1,cc) });
				++pos;
				continue;
			}
			if (cc == ')') {
				tokenvec.push_back({ tokentype::Rightp,std::string(1,cc) });
				++pos;
				continue;
			}

			if (cc == '*' || cc == '/' || cc == '='|| cc == '+' || cc == '-'||cc=='^') {
				tokenvec.push_back({ tokentype::Operator,std::string(1,cc) });
				++pos;
				continue;
			}

			/*if (cc == '+' || cc == '-') {
				if (tokenvec.empty()||tokenvec.back().type == tokentype::Leftp){
					tokenvec.push_back(getnumber());
					continue;
				}
				tokenvec.push_back({ tokentype::Operator,std::string(1,cc) });
				++pos;
				continue;
			}			
			*/
			if (cc == ',') {
				tokenvec.push_back({ tokentype::Comma,std::string(1,cc) });
				++pos;
				continue;
			}

			if (isalpha(cc) || cc == '_') {
				tokenvec.push_back(getiden());
				continue;
			}

			if (isdigit(cc) || cc == '.') {
				tokenvec.push_back(getnumber());
				continue;
			}

			else throw std::runtime_error("unknown input " + std::string(1, expr[pos]) + "at position " + std::to_string(pos) + " !");
		}
		if (!tokenvec.empty() && tokenvec.back().name == "=")
			tokenvec.pop_back();
		if(tokenvec.empty()) throw std::runtime_error("No any valid input!");
	}

	void skipspace() {
		while (pos < exprsize && isspace(expr[pos]))
			pos++;
	};

	token getiden() {
		size_t startpos = pos;
		token identoken;
		while (pos < exprsize && (isalpha(expr[pos]) || expr[pos] == '_')) ++pos;
		identoken.type = tokentype::Iden;
		identoken.name = expr.substr(startpos, pos - startpos);
		return identoken;
	}

	token getnumber() {
		size_t endpos;
		token numbertoken;
		try {
			numbertoken.value = stod(expr.substr(pos), &endpos);
		}
		catch (const std::exception&) {
			throw std::runtime_error("unknown input " + std::string(1, expr[pos+1]) + " at position " + std::to_string(pos+1) + " from stod!");
		}
		numbertoken.type = tokentype::Number;
		numbertoken.name = expr.substr(pos, endpos);
		pos += endpos;
		return numbertoken;
	}

	/*void printvec() {
	*	for (auto tt : tokenvec) {
	*		std::cout << tt.name << " ";
	*	}
	*	std::cout << std::endl;
	*}
	*/

};