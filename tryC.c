#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>

#define POOLSIZE  (256 * 1024)  // arbitrary size
#define SYMTABSIZE (1024*8)     // size of the symbol table stack
#define MAXNAMESIZE 32          // size of variable/function name
#define RETURNFLAG DBL_MAX             // indicate a return statement has occured in a function

/* this structure represent a symbol store in a symbol table */
// symbol
typedef struct symStruct {
    int type;                  //the type of the symbol:  Num, Char, Str, Array, Func
    char name[MAXNAMESIZE];    // record the symbol name
    double value;              // record the symbol value, or the length of string or array
    union {
        char* funcp;            // pointer to an array or an function
        struct symStruct* list;
    } pointer;
    int levelNum;               // indicate the declare nesting level
} symbol;
symbol g_symtab[SYMTABSIZE];    //符号表
int g_curSymOffset = 0;           // the symbol stack pointer
int g_currentlevel = 0;         // current nesting level

char* g_cur_ptr,
    * g_src_line_ptr;           // the text process currently

enum {
    debug, run
};
int g_compileState = run;          // current interpre state

double g_return_val = 0;           // used for reserve the return value of function

/* tokens and classes (operators last and in precedence order) */
enum {
    Num = 128, Char, Str, Array, Func,
    Else, If, Return, While, Print,Puts, Read,
    Assign, OR, AND, Equal, Sym, FuncSym, ArraySym, Void,
    Nequal, LessEqual, GreatEqual
};
char* KEYWORDS = "array func else if return while print puts read";
int g_token_type;    // current token type
union tokenValue {
    symbol* ptr;                // used when return a string or a symbol address for assignment
    double val;                 // token value, for Char or Num
} g_token_val;

/*--------------- function declaration ---------------*/
double function();
double statement();
int boolOR();
int boolAND();
int boolexp();
double expression();
double term();
double factor();
void matchThenNext(int tk);
void parse_token();

/*
exp -> term { addop term }
term -> factor { mulop factor }
factor -> number | ( exp ) | Sym | array_name '[' exp ']' | function        // 处理数组单元、函数、变量等
addop -> + | -
mulop -> * | /

boolOR = boolAND { '||' boolAND }
boolAND = boolexp { '&&' boolexp }
boolexp -> exp boolop exp | ( boolOR ) | !boolexp
boolop -> > | < | >= | <= | ==

statement -> '{' { statement } '}'                                  |       // 语句块
            if-stmt -> if ( exp ) statement [ else statement ]      |       // 选择语句
            while-stmt -> while ( exp ) statement                   |       // 循环语句
            Sym = exp;                                               |       // 赋值语句
            print ( exp );                                           |       // 输入输出语句
            puts ( Str );                                            |
            read ( Sym );                                            |
            return ( exp );                                          |       // 函数的返回语句
            func func_name statement;                                |       // 函数定义
            array array_name length;                                 |       // 数组定义

 **/

/* -------------------  lexical analysis  ---------------------------------*/
/* get the next token of the input string */
// input: g_src
// output: g_src, g_token, g_token_val
void parse_token() {
    char* start_pos;  //token起始字符指针

    while ((g_token_type = *g_cur_ptr++)) {
        if ((g_token_type >= 'a' && g_token_type <= 'z')
                || (g_token_type >= 'A' && g_token_type <= 'Z')
                || (g_token_type == '_')) {        // 标识符首字符: a-zA-Z_
            start_pos = g_cur_ptr - 1;             // process symbols
                                              //
            char nameBuffer[MAXNAMESIZE];     // 符号名缓存
            nameBuffer[0] = g_token_type;
            while ((*g_cur_ptr >= 'a' && *g_cur_ptr <= 'z') 
                    || (*g_cur_ptr >= 'A' && *g_cur_ptr <= 'Z') 
                    || (*g_cur_ptr >= '0' && *g_cur_ptr <= '9') 
                    || (*g_cur_ptr == '_')) {
                nameBuffer[g_cur_ptr - start_pos] = *g_cur_ptr;
                g_cur_ptr++;
            }
            nameBuffer[g_cur_ptr - start_pos] = 0;   // 末尾置0
            // 遍历符号表
            for (int i = g_curSymOffset-1; i >= 0; --i) {  // 遍历符号表
                if (strcmp(nameBuffer, g_symtab[i].name) == 0) {  // 如果符号表中存在同名的符号
                    switch (g_symtab[i].type) {  //根据符号类型, 设置符号值和token
                        case Char:
                        case Num:
                        case Void:     { g_token_val.ptr = &g_symtab[i]; g_token_type = Sym; break; }
                        case FuncSym:
                        case ArraySym: { g_token_val.ptr = &g_symtab[i]; g_token_type = g_symtab[i].type; break; }
                        default:       { g_token_type = g_symtab[i].type; break; }
                    }
                    return;
                }
            }
            // 否则符号表中没有,将当前符号token加入到符号表中
            strcpy(g_symtab[g_curSymOffset].name, nameBuffer);
            g_symtab[g_curSymOffset].levelNum = g_currentlevel;
            g_symtab[g_curSymOffset].type = Void;

            // 设置当前token 及 token_val
            g_token_val.ptr = &g_symtab[g_curSymOffset++];
            g_token_type = Sym;

            return;
        }
        else if (g_token_type >= '0' && g_token_type <= '9') {        // process numbers
            g_token_val.val = (double)g_token_type - '0';
            while (*g_cur_ptr >= '0' && *g_cur_ptr <= '9') {
                g_token_val.val = g_token_val.val * 10.0 + *g_cur_ptr++ - '0';
            }
            if (*g_cur_ptr == '.') {
                g_cur_ptr++;
                int countDig = 1;
                while (*g_cur_ptr >= '0' && *g_cur_ptr <= '9') {
                    g_token_val.val +=  ((double)(*g_cur_ptr++) - '0')/(10.0 * countDig++);
                }
            }
            g_token_type = Num;
            return;
        }
        else {
            switch (g_token_type) {
                case '\n': {  // 换行符
                               if(g_compileState == debug) {
                                   printf("%.*s",  (int)(g_cur_ptr - g_src_line_ptr), g_src_line_ptr);
                               }
                               g_src_line_ptr = g_cur_ptr; //记录下一行行首指针 
                               break;
                           }
                case '#': {  while (*g_cur_ptr != 0 && *g_cur_ptr != '\n') { g_cur_ptr++; } break; }
                case '"': {  // 字符串
                               start_pos = g_cur_ptr;
                               int numCount = 0;
                               while (*g_cur_ptr != 0 && *g_cur_ptr != '"') { g_cur_ptr++; numCount++; }
                               if (*g_cur_ptr) {
                                   *g_cur_ptr = 0;
                                   g_token_val.ptr = malloc(sizeof(char) * numCount + 8);
                                   strcpy((char *)g_token_val.ptr, start_pos);
                                   *g_cur_ptr = g_token_type;
                                   g_cur_ptr++;
                               }
                               g_token_type = Str;
                               return;
                           }
                case '\'': { g_token_val.val = *g_cur_ptr++; g_token_type = Char; g_cur_ptr++; return; }
                case '=': { if (*g_cur_ptr == '=') { g_cur_ptr++; g_token_type = Equal; } return; }
                case '!': { if (*g_cur_ptr == '=') { g_cur_ptr++; g_token_type = Nequal; } return; }
                case '<': { if (*g_cur_ptr == '=') { g_cur_ptr++; g_token_type = LessEqual; } return; }
                case '>': { if (*g_cur_ptr == '=') { g_cur_ptr++; g_token_type = GreatEqual; } return; }
                case '|': { if (*g_cur_ptr == '|') { g_cur_ptr++; g_token_type = OR; } return; }
                case '&': { if (*g_cur_ptr == '&') { g_cur_ptr++; g_token_type = AND; } return; }
                case '*':
                case '/':
                case ';':
                case ',':
                case '+':
                case '-':
                case '(':
                case ')':
                case '{':
                case '}':
                case '[':
                case ']': { return; }
                case ' ':
                case '\t': { break;  }
                default: { printf("unexpected token: %d\n", g_token_type); break; }
            }
        }
    }
}

// 匹配当前token并获取下一个token
void matchThenNext(int tk) {
    if (g_token_type == tk) {
        if (g_compileState == debug) {
            if(isprint(tk))
                printf("match: %c\n", tk );
            else
                printf("match: %d\n", tk);
        }
        parse_token();
    } else {
        printf("line %.*s:expected token: %d\n", (int)(g_cur_ptr - g_src_line_ptr), g_src_line_ptr,  tk);
        exit(-1);
    }
}

/*--------------------------  grammatical analysis and run ----------------------------------*/
// term
// term -> factor { multiop factor }
double term() {
    double temp = factor();
    while (g_token_type == '*' || g_token_type == '/') {
        switch (g_token_type) {
            case '*': { matchThenNext(g_token_type); temp *= factor(); break; }
            case '/': { matchThenNext(g_token_type); temp /= factor(); break; }
            default: break;
        }
    }
    return temp;
}

// 因子
// factor -> num | (expr) | char | sym |
double factor() {
    double temp = 0;
    symbol* ptr = g_token_val.ptr;
    switch (g_token_type) {
        case '(': { matchThenNext('('); temp = expression(); matchThenNext(')'); break; }
        case Num:
        case Char: { temp = g_token_val.val; matchThenNext(g_token_type); break; }
        case Sym: { temp = g_token_val.ptr->value; matchThenNext(Sym); break; } // <symbol>
        case FuncSym: { return function(); }
        case ArraySym: {
                           ptr = g_token_val.ptr;
                           matchThenNext(ArraySym);
                           matchThenNext('[');
                           int index = (int)expression();
                           if (index >= 0 && index < ptr->value) {
                               temp = ptr->pointer.list[index].value;
                           }
                           matchThenNext(']');
                           break;
                       }
        default: break;
    }
    return temp;
}

// 求解表达式, 返回表达式的值
// expr => term { addop term }
double expression() {
    double temp = term();
    while (g_token_type == '+' || g_token_type == '-') {
        switch (g_token_type) {
            case '+': { matchThenNext('+'); temp += term(); break; }
            case '-': { matchThenNext('-'); temp -= term(); break; }
            default: { break; }
        }
    }
    return temp;
}

// 根据当前token, 生成bool表达式
// boolexp -> exp boolop exp | ( boolOR ) | !boolexp
int boolexp() {
    switch (g_token_type) {
        case '(': { matchThenNext('('); int result = boolOR(); matchThenNext(')'); return result; }
        case '!': { matchThenNext('!'); return boolexp(); }
        default: {
                     double temp = expression();
                     switch (g_token_type) {
                         case '>': matchThenNext('>'); return temp > expression();
                         case '<': matchThenNext('<'); return temp < expression();
                         case GreatEqual: matchThenNext(GreatEqual); return temp >= expression();
                         case LessEqual: matchThenNext(LessEqual); return temp <= expression();
                         case Equal: matchThenNext(Equal); return temp == expression();
                         default: return 0;
                     }
                     break;
                 };
    }

    return 0;
}

/**
 * 跳过 ( ... )
 **/
void skipBoolExpr() {
    int count = 0;
    while (g_token_type && !(g_token_type == ')' && count == 0)) {
        switch (g_token_type) {
            case '(': count++; break;
            case ')': count--; break;
            default: break;
        }
        g_token_type = *g_cur_ptr++;
    }
}

// boolAND -> <boolexp> [ & <boolexp> ]
int boolAND() {
    int val = boolexp();
    while (g_token_type == AND) {
        matchThenNext(AND);
        if (val == 0){
            skipBoolExpr();
            return 0;         // short cut
        }
        val = val & boolexp();
        if (val == 0) return 0;
    }
    return val;
}

// boolOR => boolAND [| boolAND ]
int boolOR() {
    int val = boolAND();
    while (g_token_type == OR) {
        matchThenNext(OR);
        if (val == 1){
            skipBoolExpr();
            return 1;         // short cut
        }
        val = val | boolAND();
        if (val == 1) return 1;
    }
    return val;
}

// 跳过statment { ... }
void skipStatments() {
    if(g_token_type == '{')
        g_token_type = *g_cur_ptr++;
    int count = 0;
    while (g_token_type && !(g_token_type == '}' && count == 0)) {
        switch (g_token_type) {
            case '{': count++; break;
            case '}': count--; break;
            default: break;
        }
        g_token_type = *g_cur_ptr++;
    }
    matchThenNext('}');
}

/*
 * statement 语句
statement -> '{' { statement } '}'                                  |       // 语句块
            if-stmt -> if ( exp ) statement [ else statement ]      |       // 判断语句
            while-stmt -> while ( exp ) statement                   |       // 循环语句
            Sym = exp;                                              |       // 赋值语句
            print ( exp );                                          |       // 输入输出语句
            puts ( Str );                                           |
            read ( Sym );                                           |
            return ( exp );                                         |       // 函数的返回语句
            func func_name statement;                               |       // 函数定义
            array array_name length;                                |       // 数组定义
*/
double statement() {
    symbol* s;
    int func;
    switch (g_token_type) {
        case '{':                       // { [statement] }
            matchThenNext('{');
            while (g_token_type != '}') {
                if (RETURNFLAG == statement())
                    return RETURNFLAG;   // return 语句
            }
            matchThenNext('}');
            break;
        case If:                   //if-stmt ->  if ( <bool-exp> ) [statement] [ else <statement> ] ;
            matchThenNext(If);
            matchThenNext('('); int boolresult = boolOR(); matchThenNext(')');
            if (boolresult) {
                if (RETURNFLAG == statement())
                    return RETURNFLAG;
            }
            else skipStatments();

            if (g_token_type == Else) {
                matchThenNext(Else);
                if (!boolresult) {
                    if (RETURNFLAG == statement())
                        return RETURNFLAG;
                }
                else skipStatments();
            }
            break;
        case While:                             // while ( bool-expr ) {
            matchThenNext(While);               //    [ statemet ]
            char* whileStartPos = g_cur_ptr;        // }
            char* whileStartOldPos = g_src_line_ptr;
            do {
                g_cur_ptr = whileStartPos;
                g_src_line_ptr = whileStartOldPos;
                g_token_type = '(';
                matchThenNext('('); boolresult = boolOR(); matchThenNext(')');
                if (boolresult) {
                    if (RETURNFLAG == statement())
                        return RETURNFLAG;
                }
                else skipStatments();
            } while (boolresult);
            break;
        case Sym:   // 标识符
            s = g_token_val.ptr;
            matchThenNext(g_token_type);
            matchThenNext('=');
            switch (g_token_type) {
                case Str: { s->pointer.funcp = (char*)g_token_val.ptr; s->type = g_token_type; matchThenNext(g_token_type); break; }
                case Char: { s->value = g_token_val.val; s->type = g_token_type; matchThenNext(g_token_type); break; }
                default: { s->value = expression(); s->type = Num; break; }
            }
            matchThenNext(';');
            break;
        case ArraySym:                     // <array_name>[<index>] = (expr)
            s = g_token_val.ptr;
            int index;
            matchThenNext(g_token_type);
            matchThenNext('['); index = expression(); matchThenNext(']');
            matchThenNext('=');
            if (index >= 0 && index < s->value) {
                s->pointer.list[index].value = expression();
            } else {
                printf("array %s index %d out of range\n", s->name, index );
                //return RETURNFLAG;
            }
            matchThenNext(';');
            break;
        case Array:
            matchThenNext(Array);
            s = g_token_val.ptr;
            matchThenNext(Sym);
            matchThenNext('('); int length = (int)expression(); matchThenNext(')');
            s->pointer.list = malloc(sizeof(struct symStruct) * length + 1);
            for (int i = 0; i < length; ++i)
                s->pointer.list[i].type = Num;
            s->value = length;
            s->type = ArraySym;
            matchThenNext(';');
            break;
        case Func:          // func <funcname> { ... };
            matchThenNext(Func);    // func
            s = g_token_val.ptr;    //
            s->type = FuncSym;

            matchThenNext(Sym);         // funcname
            s->pointer.funcp = g_cur_ptr;   // 记录函数源码起始位置
            s->value = g_token_type;         // 符号表中记录函数名

            skipStatments();            // 跳过函数体, 在调用时再处理
            matchThenNext(';');         // 函数
            break;
        case Return:      // return (<expr>) ;
            matchThenNext(Return);
            matchThenNext('('); g_return_val = expression(); matchThenNext(')');
            matchThenNext(';');
            return RETURNFLAG;
        case Print:             // print(<expr>);
        case Read:              // read (<expr>);
        case Puts:              // put ( <expr> );
            func = g_token_type;
            double temp;
            matchThenNext(func);
            matchThenNext('(');
            switch (func) {
                case Print: { temp = expression(); printf("%lf\n", temp); break; }
                case Puts: { printf("%s\n", (char*)g_token_val.ptr); matchThenNext(Str); break; }
                case Read: { scanf("%lf", &g_token_val.ptr->value); g_token_val.ptr->type = Num; matchThenNext(Sym); break; }
                default: break;
            }
            matchThenNext(')');
            matchThenNext(';');
            break;
        default:
            break;
    };
    return 0;
}

/*
 * 函数调用
 * <func_name>( [arg1, arg2, ..] );
 * */
double function() {
    g_currentlevel++;   // 函数调用层级
    g_return_val = 0;   // 函数返回值

    // parse func
    symbol* s = g_token_val.ptr;

    matchThenNext(FuncSym);   // func_name

    // 函数参数
    matchThenNext('(');
    while (g_token_type != ')') {    // parse func_args
        g_symtab[g_curSymOffset] = *g_token_val.ptr;      //设置
        strcpy(g_symtab[g_curSymOffset].name, g_token_val.ptr->name);
        g_symtab[g_curSymOffset].levelNum = g_currentlevel;
        g_curSymOffset++;                 //符号表
                                        //
        matchThenNext(Sym);             //
        if (g_token_type == ',')
            matchThenNext(',');
    }
    matchThenNext(')');

    char* startPos = g_cur_ptr; // 保存当前词法分析的源代码位置和token
    char* startOldPos = g_src_line_ptr;
    int startToken = g_token_type;
    g_src_line_ptr = g_cur_ptr = s->pointer.funcp; // 跳转到函数定义时的源代码位置和token；
    g_token_type = (int)s->value;
    statement();                    // 语法分析和执行定义时的函数体
    g_cur_ptr = startPos;
    g_src_line_ptr = startOldPos;
    g_token_type = startToken;           // 恢复保存的当前源代码位置和token；

    // 释放函数调用栈
    while (g_symtab[g_curSymOffset - 1].levelNum == g_currentlevel) {
        g_curSymOffset--;
    }
    g_currentlevel--;

    return g_return_val;
}

/*----------------------------------------------------------------*/

int main(int argc, char** argv)
{
    int i, fd;

    // 初始化符号表
    g_cur_ptr = KEYWORDS;
    for (i = Array; i <= Read; ++i) {
        parse_token();
        g_symtab[g_curSymOffset -1].type = i;
    }

    // 初始化文件缓冲区
    if (!(g_cur_ptr = g_src_line_ptr = (char*)malloc(POOLSIZE))) {
        printf("could not malloc(%d) for source area\n", POOLSIZE);
        return -1;
    }

    // 处理输入参数
    ++argv; --argc;
    if (argc > 0) {
        if (**argv == '-' && (*argv)[1] == 'd') {
            g_compileState = debug;
            ++argv; --argc;
        }
        else {
            g_compileState = run;
        }
    }
    if (argc < 1) {
        printf("usage: tryc [-d] file ...\n");
        return -1;
    }

    // 读取文件到g_src缓冲区
    if ((fd = open(*argv, 0)) < 0) {                // read the source file
        printf("could not open(%s)\n", *argv);
        return -1;
    }
    if ((i = read(fd, g_cur_ptr, POOLSIZE - 1)) <= 0) {
        printf("read() returned %d\n", i);
        return -1;
    }

    g_cur_ptr[i] = 0; // add EOF character
    close(fd);

    // 词法解析
    parse_token();
    while (g_token_type != 0) {
        statement();
    }
    return 0;
}
