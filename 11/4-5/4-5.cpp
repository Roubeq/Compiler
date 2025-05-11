#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <unordered_map>
#include <fstream>

// Типы токенов
enum class TokenType {
    IDENTIFIER, KEYWORD, NUMBER, OPERATOR, ASSIGN, COMMENT, PARENTHESIS, SEPARATOR, STRING, END_OF_FILE, UNKNOWN
};

// Функция для преобразования типа токена в строку
std::string tokenTypeToString(TokenType type) {
    switch (type) {
    case TokenType::IDENTIFIER: return "IDENTIFIER";
    case TokenType::KEYWORD: return "KEYWORD";
    case TokenType::NUMBER: return "NUMBER";
    case TokenType::OPERATOR: return "OPERATOR";
    case TokenType::ASSIGN: return "ASSIGN";
    case TokenType::COMMENT: return "COMMENT";
    case TokenType::PARENTHESIS: return "PARENTHESIS";
    case TokenType::SEPARATOR: return "SEPARATOR";
    case TokenType::STRING: return "STRING";
    case TokenType::END_OF_FILE: return "END_OF_FILE";
    default: return "UNKNOWN";
    }
}

// Структура токена
struct Token {
    TokenType type;
    std::string value;
};

// Список ключевых слов
const std::unordered_map<std::string, TokenType> keywords = {
    {"dim", TokenType::KEYWORD},
    {"integer", TokenType::KEYWORD},
    {"real", TokenType::KEYWORD},
    {"boolean", TokenType::KEYWORD},
    {"if", TokenType::KEYWORD},
    {"then", TokenType::KEYWORD},
    {"else", TokenType::KEYWORD},
    {"for", TokenType::KEYWORD},
    {"to", TokenType::KEYWORD},
    {"do", TokenType::KEYWORD},
    {"while", TokenType::KEYWORD},
    {"read", TokenType::KEYWORD},
    {"write", TokenType::KEYWORD},
    {"true", TokenType::KEYWORD},
    {"false", TokenType::KEYWORD},
    {"not", TokenType::OPERATOR},
    {"ass", TokenType::ASSIGN}
};

// Класс лексического анализатора
class Lexer {
    std::string input;
    size_t pos = 0;

public:
    Lexer(const std::string& text) : input(text) {}

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        while (pos < input.size()) {
            if (std::isspace(input[pos])) {
                pos++; // Пропускаем пробелы
            }
            else if (std::isalpha(input[pos])) {
                tokens.push_back(parseIdentifier());
            }
            else if (std::isdigit(input[pos]) || input[pos] == '0') {
                tokens.push_back(parseNumber());
            }
            else if (input[pos] == '{') {
                skipComment();
            }
            else if (input[pos] == '"') {
                tokens.push_back(parseString());
            }
            else {
                tokens.push_back(parseOperator());
            }
        }
        tokens.push_back({ TokenType::END_OF_FILE, "EOF" });
        return tokens;
    }

private:
    Token parseIdentifier() {
        size_t start = pos;
        while (pos < input.size() && (std::isalnum(input[pos]) || input[pos] == '_')) pos++;
        std::string text = input.substr(start, pos - start);

        // Проверяем, является ли это ключевым словом
        if (keywords.find(text) != keywords.end()) {
            return { keywords.at(text), text };  // Возвращаем ключевое слово
        }

        // Если это не ключевое слово, возвращаем как идентификатор
        return { TokenType::IDENTIFIER, text };
    }

    Token parseNumber() {
        size_t start = pos;

        if (input[pos] == '0') {
            if (pos + 1 < input.size()) {
                if (input[pos + 1] == 'b' || input[pos + 1] == 'B') {
                    pos += 2;
                    while (pos < input.size() && (input[pos] == '0' || input[pos] == '1')) pos++;
                    return { TokenType::NUMBER, input.substr(start, pos - start) };
                }
                else if (input[pos + 1] == 'o' || input[pos + 1] == 'O') {
                    pos += 2;
                    while (pos < input.size() && (input[pos] >= '0' && input[pos] <= '7')) pos++;
                    return { TokenType::NUMBER, input.substr(start, pos - start) };
                }
                else if (input[pos + 1] == 'h' || input[pos + 1] == 'H') {
                    pos += 2;
                    while (pos < input.size() && std::isxdigit(input[pos])) pos++;
                    return { TokenType::NUMBER, input.substr(start, pos - start) };
                }
            }
        }

        while (pos < input.size() && std::isdigit(input[pos])) pos++;
        return { TokenType::NUMBER, input.substr(start, pos - start) };
    }

    Token parseString() {
        size_t start = ++pos;
        while (pos < input.size() && input[pos] != '"') pos++;
        std::string str = input.substr(start, pos - start);
        pos++; // Пропустить закрывающую кавычку
        return { TokenType::STRING, str };
    }

    void skipComment() {
        while (pos < input.size() && input[pos] != '}') pos++;
        if (pos < input.size()) pos++;
    }

    Token parseOperator() {
        static const std::unordered_map<std::string, TokenType> operators = {
            {"+", TokenType::OPERATOR}, {"-", TokenType::OPERATOR}, {"*", TokenType::OPERATOR}, {"/", TokenType::OPERATOR},
            {"=", TokenType::OPERATOR}, {"<", TokenType::OPERATOR}, {">", TokenType::OPERATOR}, {"<=", TokenType::OPERATOR},
            {">=", TokenType::OPERATOR}, {"<>", TokenType::OPERATOR}, {"or", TokenType::OPERATOR}, {"and", TokenType::OPERATOR},
            {"ass", TokenType::ASSIGN}, {";", TokenType::SEPARATOR}, {",", TokenType::SEPARATOR},
            {"(", TokenType::PARENTHESIS}, {")", TokenType::PARENTHESIS}
        };

        // Проверка на многосимвольные операторы
        std::string twoCharOp;
        if (pos + 1 < input.size()) {
            twoCharOp = input.substr(pos, 2);
            if (operators.count(twoCharOp)) {
                pos += 2;
                return { operators.at(twoCharOp), twoCharOp };
            }
        }

        // Проверка на однобуквенные/одиночные операторы
        std::string oneCharOp(1, input[pos]);
        if (operators.count(oneCharOp)) {
            pos += 1;
            return { operators.at(oneCharOp), oneCharOp };
        }

        // Если не найдено — возвращаем UNKNOWN
        return { TokenType::UNKNOWN, std::string(1, input[pos++]) };
    }
};




// Дерево разбора
struct ParseNode {
    std::string name;
    std::vector<ParseNode*> children;

    ParseNode(const std::string& name) : name(name) {}
};

void deleteTree(ParseNode* node) {
    if (!node) return;
    for (auto child : node->children) {
        deleteTree(child);
    }
    delete node;
}

// === Парсер ===
class Parser {
    std::vector<Token> tokens;
    size_t pos = 0;
    std::vector<std::string> derivationSteps;

    std::unordered_map<std::string, std::string> symbolTable; // Табличка всех символов

    void logDerivation(const std::string& step) {
        derivationSteps.push_back(step);
        std::cout << step << std::endl;
    }


public:
    Parser(const std::vector<Token>& tokens) : tokens(tokens) {}

    bool isAtEnd() const {
        return pos >= tokens.size() || tokens[pos].type == TokenType::END_OF_FILE;
    }

    ParseNode* parseProgram() {
        logDerivation("Программа -> Последовательность операторов");
        auto node = new ParseNode("Program");
        std::cout << "Начался разбор программы!" << std::endl;
        while (!isAtEnd()) {
            node->children.push_back(parseStatement());
        }

        return node;
    }

    void printExpression(ParseNode* node, const std::string& prefix = "") {
        if (node == nullptr) {
            std::cout << prefix << "NULL\n";
            return;
        }

        // Для операторов
        if (node->name == "+" || node->name == "-" ||
            node->name == "*" || node->name == "/" ||
            node->name == "and" || node->name == "or" ||
            node->name == "=" || node->name == "<>" ||
            node->name == "<" || node->name == ">" ||
            node->name == "<=" || node->name == ">=") {

            std::cout << prefix << "OPERATOR: " << node->name << "\n";

            // Левый операнд
            std::cout << prefix << "|-- LHS: ";
            if (node->children.size() > 0) {
                printTree(node->children[0], prefix + "|   ");
            }
            else {
                std::cout << "MISSING\n";
            }

            // Правый операнд
            std::cout << prefix << "`-- RHS: ";
            if (node->children.size() > 1) {
                printTree(node->children[1], prefix + "    ");
            }
            else {
                std::cout << "MISSING\n";
            }
        }
        else {
            printTree(node, prefix);
        }
    }

    void printTree(ParseNode* node, const std::string& prefix = "") {
        if (node == nullptr) {
            std::cout << prefix << "NULL\n";
            return;
        }

        std::cout << prefix << node->name;

        // Для листьев
        if (node->children.empty()) {
            std::cout << "\n";
            return;
        }

        std::cout << "\n";
        for (size_t i = 0; i < node->children.size(); ++i) {
            // Последний ребёнок?
            bool isLast = (i == node->children.size() - 1);
            std::cout << prefix << (isLast ? "`-- " : "|-- ");

            // Новая префиксная строка
            std::string newPrefix = prefix + (isLast ? "    " : "|   ");

            printTree(node->children[i], newPrefix);
        }
    }

    void printDerivation() const {
        std::cout << "\n=== Цепочка вывода ===\n";
        for (size_t i = 0; i < derivationSteps.size(); ++i) {
            std::cout << (i + 1) << ". " << derivationSteps[i] << std::endl;
        }
    }

    Token previous() const {
        if (pos == 0) throw std::runtime_error("No previous token");
        return tokens[pos - 1];
    }

    Token expectType(TokenType expectedType) {
        if (pos >= tokens.size()) {
            throw std::runtime_error("Ошибка: достигнут конец токенов, ожидался токен типа " + tokenTypeToString(expectedType));
        }
        if (tokens[pos].type != expectedType) {
            throw std::runtime_error("ОШИБКА: ожидался токен типа " + tokenTypeToString(expectedType) +
                ", но получен токен типа " + tokenTypeToString(tokens[pos].type));
        }
        return tokens[pos++];
    }

private:
    // --- Утилиты --- 
    Token advance() {
        if (pos < tokens.size()) {
            Token t = tokens[pos];
            std::cout << "Текущий токен: " << t.value << " Тип: " << tokenTypeToString(t.type) << std::endl;
            pos++;  // Двигаемся к следующему токену
            return t;
        }
        return { TokenType::END_OF_FILE, "EOF" };
    }

    Token peek() const {
        if (pos < tokens.size()) {
            return tokens[pos];
        }
        return { TokenType::END_OF_FILE, "EOF" };
    }

    bool match(TokenType type) {
        if (pos >= tokens.size()) return false;
        if (tokens[pos].type == type) { pos++; return true; }
        return false;
    }
    // Функция accept для проверки и продвижения токенов
    bool accept(const std::string& value) {
        if (pos < tokens.size() && tokens[pos].value == value) {
            pos++;
            return true;
        }
        return false;
    }

    bool check(const std::string& value) {
        if (pos >= tokens.size()) return false;
        return tokens[pos].value == value;
    }

    void expect(const std::string& value) {
        if (pos >= tokens.size()) {
            throw std::runtime_error("Ошибка: достигнут конец токенов, ожидалось '" + value + "'\n");
        }
        if (tokens[pos].value != value) {
            throw std::runtime_error("ОШИБКА: ожидалось '" + value + "', но получено '" + tokens[pos].value + "'\n");
        }
        pos++;
    }

    // --- Разбор --- 
    ParseNode* parseDeclaration() {
        logDerivation("Объявление -> dim список идентификаторов тип ;");

        auto node = new ParseNode("Declaration");
        try {
            // Обрабатываем 'dim'
            expect("dim");
            node->children.push_back(new ParseNode("dim"));

            // Обрабатываем список идентификаторов
            do {
                Token id = expectType(TokenType::IDENTIFIER);
                node->children.push_back(new ParseNode("ID: " + id.value));

                if (accept(",")) {
                    node->children.push_back(new ParseNode(","));
                }
                else {
                    break;
                }
            } while (true);

            // Обрабатываем тип переменной
            Token typeToken = peek();
            if (typeToken.value == "integer" || typeToken.value == "real" || typeToken.value == "boolean") {
                advance();
                node->children.push_back(new ParseNode("type: " + typeToken.value));

                // Сохраняем все объявленные идентификаторы
                for (const auto& child : node->children) {
                    if (child->name.rfind("ID: ", 0) == 0) {
                        std::string varName = child->name.substr(4); // убираем "ID: "
                        if (symbolTable.count(varName)) {
                            throw std::runtime_error("ОШИБКА: переменная '" + varName + "' уже была объявлена");
                        }
                        symbolTable[varName] = typeToken.value;
                    }
                }

            }
            else {
                throw std::runtime_error("ОШИБКА: ожидался тип переменной (integer, real или boolean)");
            }

            // Обрабатываем точку с запятой
            expect(";");
            node->children.push_back(new ParseNode(";"));

            return node;
        }
        catch (...) {
            deleteTree(node); // Удаляем всё дерево в случае ошибки
            throw;
        }
    }

    ParseNode* parseAssignment() {
        logDerivation("Присваивание -> идентификатор ass выражение ;");
        auto node = new ParseNode("Assignment");

        // Левый операнд (идентификатор)
        Token var = expectType(TokenType::IDENTIFIER);
        auto varNode = new ParseNode(var.value);
        node->children.push_back(varNode);

        // Проверка существования переменной
        if (symbolTable.find(var.value) == symbolTable.end()) {
            throw std::runtime_error("Необъявленная переменная: " + var.value);
        }

        // Оператор присваивания
        expect("ass");
        node->children.push_back(new ParseNode(":="));

        // Правая часть (выражение)
        auto exprNode = parseExpression();

        // Проверка и приведение типов
        std::string varType = symbolTable[var.value];
        std::string exprType = getExpressionType(exprNode);

        if (varType == "real" && exprType == "integer") {
            // Автоматическое приведение integer -> real
            auto castNode = new ParseNode("(real)");
            castNode->children.push_back(exprNode);
            node->children.push_back(castNode);
        }
        else if (varType != exprType) {
            throw std::runtime_error("Несовместимость типов: " + varType + " := " + exprType);
        }
        else {
            node->children.push_back(exprNode);
        }

        expect(";");
        node->children.push_back(new ParseNode(";"));

        return node;
    }

    ParseNode* parseExpression() {
        return parseLogicOr();
    }

    ParseNode* parseLogicOr() {
        auto left = parseLogicAnd();

        while (peek().value == "or") {
            Token op = advance();
            auto node = new ParseNode("or");
            node->children.push_back(left);
            node->children.push_back(parseLogicAnd());

            if (getExpressionType(node->children[0]) != "boolean" ||
                getExpressionType(node->children[1]) != "boolean") {
                throw std::runtime_error("ОШИБКА: 'or' требует boolean операнды");
            }

            left = node;
        }
        return left;
    }

    ParseNode* parseLogicAnd() {
        auto left = parseEquality();

        while (peek().value == "and") {
            Token op = advance();
            auto node = new ParseNode("and");
            node->children.push_back(left);
            node->children.push_back(parseEquality());

            if (getExpressionType(node->children[0]) != "boolean" ||
                getExpressionType(node->children[1]) != "boolean") {
                throw std::runtime_error("ОШИБКА: 'and' требует boolean операнды");
            }

            left = node;
        }
        return left;
    }

    ParseNode* parseEquality() {
        auto left = parseComparison();

        while (peek().value == "=" || peek().value == "<>") {
            Token op = advance();
            auto node = new ParseNode(op.value);
            node->children.push_back(left);
            node->children.push_back(parseComparison());

            std::string lt = getExpressionType(node->children[0]);
            std::string rt = getExpressionType(node->children[1]);

            if (lt != rt) {
                throw std::runtime_error("ОШИБКА: несовместимые типы для " + op.value + ": " + lt + " и " + rt);
            }

            left = node;
        }
        return left;
    }

    ParseNode* parseComparison() {
        auto left = parseTerm();

        while (peek().value == ">" || peek().value == "<" ||
            peek().value == ">=" || peek().value == "<=") {
            Token op = advance();
            auto node = new ParseNode(op.value);
            node->children.push_back(left);
            node->children.push_back(parseTerm());

            std::string lt = getExpressionType(node->children[0]);
            std::string rt = getExpressionType(node->children[1]);

            if ((lt != "integer" && lt != "real") || (rt != "integer" && rt != "real")) {
                throw std::runtime_error("ОШИБКА: сравнение требует numeric типы");
            }

            left = node;
        }
        return left;
    }

    ParseNode* parseTerm() {
        auto left = parseFactor();

        while (peek().value == "+" || peek().value == "-") {
            Token op = advance();
            auto node = new ParseNode(op.value);
            node->children.push_back(left);
            auto right = parseFactor();
            node->children.push_back(right);

            std::string lt = getExpressionType(left);
            std::string rt = getExpressionType(right);

            // Автоматическое приведение integer -> real
            if (lt == "integer" && rt == "real") lt = "real";
            if (rt == "integer" && lt == "real") rt = "real";

            if (lt != rt || (lt != "integer" && lt != "real")) {
                throw std::runtime_error("ОШИБКА: несовместимые типы для " + op.value);
            }

            left = node;
        }
        return left;
    }

    ParseNode* parseFactor() {
        auto left = parseUnary();

        while (peek().value == "*" || peek().value == "/") {
            Token op = advance();
            auto node = new ParseNode(op.value);
            node->children.push_back(left);
            auto right = parseUnary();
            node->children.push_back(right);

            std::string lt = getExpressionType(left);
            std::string rt = getExpressionType(right);

            // Автоматическое приведение integer -> real
            if (lt == "integer" && rt == "real") lt = "real";
            if (rt == "integer" && lt == "real") rt = "real";

            if (lt != rt || (lt != "integer" && lt != "real")) {
                throw std::runtime_error("ОШИБКА: несовместимые типы для " + op.value);
            }

            left = node;
        }
        return left;
    }

    ParseNode* parseUnary() {
        if (peek().value == "not") {
            Token op = advance();
            auto node = new ParseNode("not");
            auto operand = parseUnary();
            node->children.push_back(operand);

            if (getExpressionType(operand) != "boolean") {
                throw std::runtime_error("ОШИБКА: 'not' требует boolean операнд");
            }

            return node;
        }
        return parsePrimary();
    }

    ParseNode* parsePrimary() {
        Token current = peek();

        if (current.value == "(") {
            advance();
            auto expr = parseExpression();
            expect(")");
            return expr;
        }
        else if (current.type == TokenType::NUMBER) {
            advance();
            return new ParseNode(current.value);
        }
        else if (current.type == TokenType::IDENTIFIER) {
            advance();
            if (symbolTable.find(current.value) == symbolTable.end()) {
                throw std::runtime_error("Необъявленная переменная: " + current.value);
            }
            return new ParseNode(current.value);
        }
        else if (current.type == TokenType::STRING) {
            advance();
            return new ParseNode("\"" + current.value + "\"");
        }
        else {
            throw std::runtime_error("Ошибка синтаксиса: неожиданный токен: " + current.value);
        }
    }




    ParseNode* parseIf() {
        logDerivation("Условный оператор -> if выражение then Оператор [else Оператор]");
        auto node = new ParseNode("if");

        expect("if");
        node->children.push_back(new ParseNode("if"));

        auto conditionNode = parseExpression();
        std::string conditionType = getExpressionType(conditionNode);

        // Проверка типа условия
        if (conditionType != "boolean") {
            throw std::runtime_error("ОШИБКА: условие в if должно быть типа boolean, а не " + conditionType);
        }
        node->children.push_back(conditionNode);

        expect("then");
        node->children.push_back(new ParseNode("then"));

        node->children.push_back(parseStatement());

        if (check("else")) {
            expect("else");
            node->children.push_back(new ParseNode("else"));
            node->children.push_back(parseStatement());
        }

        return node;
    }


    ParseNode* parseFor() {
        logDerivation("Цикл for -> for идентификатор ass выражение to выражение do Оператор");
        auto node = new ParseNode("for");

        expect("for");
        node->children.push_back(new ParseNode("for"));

        Token loopVarToken = expectType(TokenType::IDENTIFIER);
        node->children.push_back(new ParseNode("ID: " + loopVarToken.value));

        // Получаем тип переменной цикла
        std::string loopVarType = symbolTable[loopVarToken.value];

        expect("ass");
        node->children.push_back(new ParseNode("ass"));

        auto fromExpr = parseExpression();
        node->children.push_back(fromExpr);

        expect("to");
        node->children.push_back(new ParseNode("to"));

        auto toExpr = parseExpression();
        node->children.push_back(toExpr);

        // Проверяем типы переменной цикла и выражений
        if (loopVarType != "integer") {
            throw std::runtime_error("ОШИБКА: переменная цикла '" + loopVarToken.value + "' должна быть типа integer, а не " + loopVarType);
        }

        if (getExpressionType(fromExpr) != "integer" || getExpressionType(toExpr) != "integer") {
            throw std::runtime_error("ОШИБКА: границы цикла должны быть типа integer");
        }

        expect("do");
        node->children.push_back(new ParseNode("do"));

        node->children.push_back(parseStatement());

        return node;
    }


    ParseNode* parseWhile() {
        logDerivation("Цикл while -> while выражение do Оператор");
        auto node = new ParseNode("while");

        expect("while");
        node->children.push_back(new ParseNode("while"));

        auto conditionNode = parseExpression();
        std::string conditionType = getExpressionType(conditionNode);

        // Проверяем тип условия
        if (conditionType != "boolean") {
            throw std::runtime_error("ОШИБКА: условие в while должно быть типа boolean, а не " + conditionType);
        }
        node->children.push_back(conditionNode);

        expect("do");
        node->children.push_back(new ParseNode("do"));

        node->children.push_back(parseStatement());

        return node;
    }


    ParseNode* parseRead() {
        logDerivation("Оператор Read -> read ( идентификатор [, идентификатор] * )");
        auto node = new ParseNode("read");

        expect("read");
        node->children.push_back(new ParseNode("read"));

        expect("(");
        node->children.push_back(new ParseNode("("));

        std::vector<ParseNode*> varNodes;
        do {
            Token varToken = expectType(TokenType::IDENTIFIER);
            std::string varType = symbolTable[varToken.value];

            // Проверка, что переменная существует в таблице символов
            if (varType.empty()) {
                throw std::runtime_error("ОШИБКА: переменная '" + varToken.value + "' не объявлена");
            }

            varNodes.push_back(new ParseNode("ID: " + varToken.value));
        } while (accept(","));

        node->children.insert(node->children.end(), varNodes.begin(), varNodes.end());

        expect(")");
        node->children.push_back(new ParseNode(")"));
        expect(";");
        node->children.push_back(new ParseNode(";"));

        return node;
    }


    ParseNode* parseWrite() {
        logDerivation("Оператор Write -> write ( выражение [, выражение] * )");
        auto node = new ParseNode("write");

        expect("write");
        node->children.push_back(new ParseNode("write"));

        expect("(");
        node->children.push_back(new ParseNode("("));

        std::vector<ParseNode*> exprNodes;
        do {
            auto exprNode = parseExpression();
            std::string exprType = getExpressionType(exprNode);

            // Проверяем типы, поддерживаемые для вывода
            if (exprType != "integer" && exprType != "real" && exprType != "string") {
                throw std::runtime_error("ОШИБКА: выражение должно быть типа integer, real или string для write");
            }

            exprNodes.push_back(exprNode);
        } while (accept(","));

        node->children.insert(node->children.end(), exprNodes.begin(), exprNodes.end());

        expect(")");
        node->children.push_back(new ParseNode(")"));
        expect(";");
        node->children.push_back(new ParseNode(";"));

        return node;
    }


    ParseNode* parseBlock() {
        logDerivation("Блок -> { Операторы }");

        auto node = new ParseNode("Block");
        expect("{");

        while (!check("}") && !isAtEnd()) {
            auto statement = parseStatement();

            // Добавляем проверку типов внутри оператора
            std::string statementType = getExpressionType(statement);
            if (statementType != "valid") {
                throw std::runtime_error("Ошибка в блоке: оператор имеет некорректный тип.");
            }

            node->children.push_back(statement);
        }

        expect("}");
        return node;
    }


    ParseNode* parseStatement() {
        if (check("dim")) {
            logDerivation("Оператор -> Объявление переменных");
            return parseDeclaration();
        }
        else if (check("if")) {
            logDerivation("Оператор -> Условный оператор if");
            return parseIf();
        }
        else if (check("for")) {
            logDerivation("Оператор -> Цикл for");
            return parseFor();
        }
        else if (check("while")) {
            logDerivation("Оператор -> Цикл while");
            return parseWhile();
        }
        else if (check("read")) {
            logDerivation("Оператор -> Ввод read()");
            return parseRead();
        }
        else if (check("write")) {
            logDerivation("Оператор -> Вывод write()");
            return parseWrite();
        }
        else if (check("{")) {
            logDerivation("Оператор -> Блок { ... }");
            return parseBlock();
        }
        else {
            logDerivation("Оператор -> Присваивание");
            return parseAssignment();
        }
    }

    // Анализ выражений
    std::string getExpressionType(ParseNode* node, int depth = 0) {
        if (!node) throw std::runtime_error("Ошибка: пустое выражение");

        std::string indent(depth * 2, ' ');
        std::cout << indent << "[STACK] Проверка узла: " << node->name << std::endl;

        if (node->name == "true" || node->name == "false") return "boolean";
        if (isdigit(node->name[0])) return (node->name.find('.') != std::string::npos) ? "real" : "integer";
        if (symbolTable.count(node->name)) return symbolTable.at(node->name);
        if (node->name == "not") {
            auto childType = getExpressionType(node->children[0], depth + 1);
            if (childType != "boolean") throw std::runtime_error("'not' требует boolean");
            return "boolean";
        }
        if (node->name == "and" || node->name == "or") {
            auto lt = getExpressionType(node->children[0], depth + 1);
            auto rt = getExpressionType(node->children[1], depth + 1);
            if (lt != "boolean" || rt != "boolean") throw std::runtime_error("'" + node->name + "' требует boolean");
            return "boolean";
        }
        if (node->name == "=" || node->name == "<>" || node->name == "<" || node->name == ">" || node->name == "<=" || node->name == ">=") {
            auto lt = getExpressionType(node->children[0], depth + 1);
            auto rt = getExpressionType(node->children[1], depth + 1);
            if ((lt == "integer" || lt == "real") && (rt == "integer" || rt == "real")) return "boolean";
            if (lt == rt && lt == "string") return "boolean";
            throw std::runtime_error("Сравнение: несовместимые типы: " + lt + " и " + rt);
        }
        if (node->name == "+" || node->name == "-" || node->name == "*" || node->name == "/") {
            auto lt = getExpressionType(node->children[0], depth + 1);
            auto rt = getExpressionType(node->children[1], depth + 1);
            if (lt == "integer" && rt == "real") lt = "real";
            if (rt == "integer" && lt == "real") rt = "real";
            if (lt == "integer" && rt == "integer") return "integer";
            if (lt == "real" && rt == "real") return "real";
            throw std::runtime_error("Арифм. операция: несовместимые типы: " + lt + " и " + rt);
        }
        if (node->name.front() == '"' && node->name.back() == '"') return "string";
        if (node->name == "&") {
            auto lt = getExpressionType(node->children[0], depth + 1);
            auto rt = getExpressionType(node->children[1], depth + 1);
            if (lt != "string" || rt != "string") throw std::runtime_error("'&' требует string");
            return "string";
        }

        throw std::runtime_error("Неизвестный тип выражения: " + node->name);
    }

};

int main() {
    setlocale(LC_ALL, "Russian");
    try {
        std::ifstream inputFile("file.txt");
        if (!inputFile) {
            std::cerr << "Ошибка: не удалось открыть файл.\n";
            return 1;
        }

        std::string input((std::istreambuf_iterator<char>(inputFile)), std::istreambuf_iterator<char>());
        Lexer lexer(input);
        std::vector<Token> tokens = lexer.tokenize();

        for (const auto& token : tokens) {
            std::cout << token.value << " -> " << tokenTypeToString(token.type) << "\n";
        }

        Parser parser(tokens);
        ParseNode* root = parser.parseProgram();
        parser.printTree(root);
        deleteTree(root);
        parser.printDerivation();
    }
    catch (const std::exception& e) {
        std::cerr << "Ошибка: " << e.what() << "\n";
    }


    return 0;
}
