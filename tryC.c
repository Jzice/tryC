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
int g_symPointer = 0;           // the symbol stack pointer
int g_currentlevel = 0;         // current nesting level

char* g_src, 
    * g_old_src;           // the text process currently

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
int g_token;                      // current token type
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
double factor();
double term();
void matchThenNext(int tk);
void parse_next_token();

/* -------------------  lexical analysis  ---------------------------------*/
/* get the next token of the input string */
// input: g_src
// output: g_src, g_token, g_token_val
void parse_next_token() {
    char* last_pos;

    while ((g_token = *g_src)) {
        ++g_src;
        if (g_token == '\n') {      // a new line
            if(g_compileState == debug)       // if on debug mode, print the currnet process line
                printf("%.*s",  (int)(g_src - g_old_src), g_old_src);
            g_old_src = g_src;
        }
        else if (g_token == '#') {   // skip comments
            while (*g_src != 0 && *g_src != '\n') {
                g_src++;
            }
        }
        else if ((g_token >= 'a' && g_token <= 'z') || (g_token >= 'A' && g_token <= 'Z') || (g_token == '_')) {
            last_pos = g_src - 1;             // process symbols
            char nameBuffer[MAXNAMESIZE];
            nameBuffer[0] = g_token;
            while ((*g_src >= 'a' && *g_src <= 'z') || (*g_src >= 'A' && *g_src <= 'Z') || (*g_src >= '0' && *g_src <= '9') || (*g_src == '_')) {
                nameBuffer[g_src - last_pos] = *g_src;
                g_src++;
            }
            nameBuffer[g_src - last_pos] = 0;                 // get symbol name
            for (int i = g_symPointer-1; i >= 0; --i) {           // search symbol in symbol table 
                if (strcmp(nameBuffer, g_symtab[i].name) == 0) {      // if find symbol: return the token according to symbol type
                    if (g_symtab[i].type == Num || g_symtab[i].type == Char) {
                        g_token_val.ptr = &g_symtab[i];
                        g_token = Sym;
                    }
                    else if (g_symtab[i].type == FuncSym) {
                        g_token_val.ptr = &g_symtab[i];
                        g_token = g_symtab[i].type;
                    }
                    else if (g_symtab[i].type == ArraySym) {
                        g_token_val.ptr = &g_symtab[i];
                        g_token = g_symtab[i].type;
                    }
                    else {
                        if (g_symtab[i].type == Void) {
                            g_token = Sym;
                            g_token_val.ptr = &g_symtab[i];
                        }
                        else g_token = g_symtab[i].type;
                    }
                    return;
                }
            }
            strcpy(g_symtab[g_symPointer].name, nameBuffer);        // if symbol not found, create a new one 
            g_symtab[g_symPointer].levelNum = g_currentlevel;
            g_symtab[g_symPointer].type = Void;
            g_token_val.ptr = &g_symtab[g_symPointer++];
            g_token = Sym;
            return;
        }
        else if (g_token >= '0' && g_token <= '9') {        // process numbers
            g_token_val.val = (double)g_token - '0';
            while (*g_src >= '0' && *g_src <= '9') {
                g_token_val.val = g_token_val.val * 10.0 + *g_src++ - '0';
            }
            if (*g_src == '.') {
                g_src++;
                int countDig = 1;
                while (*g_src >= '0' && *g_src <= '9') {
                    g_token_val.val +=  ((double)(*g_src++) - '0')/(10.0 * countDig++);
                }
            }
            g_token = Num;
            return;
        }
        else if (g_token == '\'') {               // parse char
            g_token_val.val = *g_src++;
            g_token = Char;
            g_src++;
            return;
        }
        else if (g_token == '"' ) {               // parse string
            last_pos = g_src;
            int numCount = 0;
            while (*g_src != 0 && *g_src != g_token) {
                g_src++;
                numCount++;          
            }
            if (*g_src) {
                *g_src = 0;
                g_token_val.ptr = malloc(sizeof(char) * numCount + 8);
                strcpy((char *)g_token_val.ptr, last_pos);
                *g_src = g_token;
                g_src++;
            }
            g_token = Str;
            return;
        }
        else if (g_token == '=') {            // parse '==' and '='
            if (*g_src == '=') {
                g_src++;
                g_token = Equal;
            }
            return;
        }
        else if (g_token == '!') {               // parse '!='
            if (*g_src == '=') {
                g_src++;
                g_token = Nequal;
            }
            return;
        }
        else if (g_token == '<') {               // parse '<=',  or '<'
            if (*g_src == '=') {
                g_src++;
                g_token = LessEqual;
            }
            return;
        }
        else if (g_token == '>') {                // parse '>=',  or '>'
            if (*g_src == '=') {
                g_src++;
                g_token = GreatEqual;
            }
            return;
        }
        else if (g_token == '|') {                // parse  '||'
            if (*g_src == '|') {
                g_src++;
                g_token = OR;
            }
            return;
        }
        else if (g_token == '&') {                // parse  '&&'
            if (*g_src == '&') {
                g_src++;
                g_token = AND;
            }
            return;
        }
        else if ( g_token == '*' || g_token == '/'  || g_token == ';' || g_token == ',' || g_token == '+' || g_token == '-' ||
            g_token == '(' || g_token == ')' || g_token == '{' || g_token == '}' ||  g_token == '[' || g_token == ']') {
            return;
        }
        else if (g_token == ' ' || g_token == '\t') {        }
        else {
            printf("unexpected token: %d\n", g_token);
        }
    }
}

// 匹配当前token并获取下一个token
void matchThenNext(int tk) {
    if (g_token == tk) {
        if (g_compileState == debug) {
            if(isprint(tk))
                printf("match: %c\n", tk );
            else
                printf("match: %d\n", tk);
        }
        // parse下一个token
        parse_next_token();
    }
    else {
        printf("line %.*s:expected token: %d\n", (int)(g_src - g_old_src), g_old_src,  tk);
        exit(-1);
    }
}

/*--------------------------  grammatical analysis and run ----------------------------------*/

// term
// term -> factor { multiop factor }
double term() {
    double temp = factor();
    while (g_token == '*' || g_token == '/') {
        if (g_token == '*') {
            matchThenNext('*');
            temp *= factor();
        }
        else {
            matchThenNext('/');
            temp /= factor();
        }
    }
    return temp;
}

// 因子
// factor -> num | (expr)
double factor() {
    double temp = 0;
    symbol* ptr = g_token_val.ptr;
    switch (g_token) {
        case '(':                       // ( <expr> )
            matchThenNext('(');
            temp = expression();
            matchThenNext(')');
            break;
        case Num:                           // <num>
        case Char:                          // <char>
            temp = g_token_val.val;
            matchThenNext(g_token);
            break;
        case Sym:                           // <symbol>
            temp = g_token_val.ptr->value;
            matchThenNext(Sym);
            break;
        case FuncSym:                       //函数符号
            return function();
            break;
        case ArraySym:                      //数组符号
            ptr = g_token_val.ptr;
            matchThenNext(ArraySym);
            matchThenNext('[');
            int index = (int)expression();
            if (index >= 0 && index < ptr->value) {
                temp = ptr->pointer.list[index].value;
            }
            matchThenNext(']');
            break;
        default:
            break;
    }
    return temp;
}

// 求解表达式, 返回表达式的值
// expr => term { addop term }
double expression() {
    double temp = term();
    while (g_token == '+' || g_token == '-') {
        if (g_token == '+') {
            matchThenNext('+');
            temp += term();
        }
        else {
            matchThenNext('-');
            temp -= term();
        }
    }
    return temp;
}

// 根据当前token, 生成bool表达式
int boolexp() {
    if (g_token == '(') {
        matchThenNext('(');
        int result = boolOR();
        matchThenNext(')');
        return result;
    }
    else if (g_token == '!') {
        matchThenNext('!');
        return boolexp();
    }

    double temp = expression();
    switch (g_token) {
        case '>':
            matchThenNext('>');
            return temp > expression();
            break;
        case '<':
            matchThenNext('<');
            return temp < expression();
            break;
        case GreatEqual:
            matchThenNext(GreatEqual);
            return temp >= expression();
            break;
        case LessEqual:
            matchThenNext(LessEqual);
            return temp <= expression();
            break;
        case Equal:
            matchThenNext(Equal);
            return temp == expression();
            break;
        default:
            break;
    }
    return 0;
}

void skipBoolExpr() {
    int count = 0;
    while (g_token && !(g_token == ')' && count == 0)) {
        if (g_token == '(') count++;
        if (g_token == ')') count--;
        g_token = *g_src++;
    }
}

// 
int boolAND() {
    int val = boolexp();
    while (g_token == AND) {
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

// parse 
int boolOR() {
    int val = boolAND();
    while (g_token == OR) {
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

// 跳过statment
void skipStatments() {
    if(g_token == '{')
        g_token = *g_src++;
    int count = 0;
    while (g_token && !(g_token == '}' && count == 0)) {
        if (g_token == '}') count++;
        if (g_token == '{') count--;
        g_token = *g_src++;
    }
    matchThenNext('}');
}

// 语句
// statement => { statement } 
double statement() {
    symbol* s;
    int func;
    switch (g_token) {
        case '{':
            matchThenNext('{');
            while (g_token != '}') {
                if (RETURNFLAG == statement()) 
                    return RETURNFLAG;   // return 语句
            }
            matchThenNext('}');
            break;
        case If:                   //if-stmt ->  if ( <bool-exp> ) [statement] ;
            matchThenNext(If);
            matchThenNext('(');
            int boolresult = boolOR();
            matchThenNext(')');
            if (boolresult) {
                if (RETURNFLAG == statement()) 
                    return RETURNFLAG;
            }
            else skipStatments();

            if (g_token == Else) {
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
            char* whileStartPos = g_src;        // }
            char* whileStartOldPos = g_old_src;
            do {
                g_src = whileStartPos;
                g_old_src = whileStartOldPos;
                g_token = '(';
                matchThenNext('(');
                boolresult = boolOR();
                matchThenNext(')');
                if (boolresult) {
                    if (RETURNFLAG == statement()) 
                        return RETURNFLAG;
                }
                else skipStatments();
            }while (boolresult);
            break;
        case Sym:
        case ArraySym:                     // array <array_name>()
            s = g_token_val.ptr;
            int tktype = g_token;
            int index;
            matchThenNext(tktype);
            if (tktype == ArraySym) {
                matchThenNext('[');
                index = expression();
                matchThenNext(']');
                matchThenNext('=');
                if (index >= 0 && index < s->value) {
                    s->pointer.list[index].value = expression();
                }
            }
            else {
                matchThenNext('=');
                if (g_token == Str) {
                    s->pointer.funcp = (char*)g_token_val.ptr;
                    s->type = Str;
                    matchThenNext(Str);
                }
                else if (g_token == Char) {
                    s->value = g_token_val.val;
                    s->type = Char;
                    matchThenNext(Char);
                }
                else {
                    s->value = expression();
                    s->type = Num;
                }
            }
            matchThenNext(';');
            break;
        case Array:
            matchThenNext(Array);
            s = g_token_val.ptr;
            matchThenNext(Sym);
            matchThenNext('(');
            int length = (int)expression();
            matchThenNext(')');
            s->pointer.list = malloc(sizeof(struct symStruct) * length + 1);
            for (int i = 0; i < length; ++i)
                s->pointer.list[i].type = Num;
            s->value = length;
            s->type = ArraySym;
            matchThenNext(';');
            break;
        case Func:                  // func <funcname> { ... };
            matchThenNext(Func);    // func
            s = g_token_val.ptr;    // 
            s->type = FuncSym;
            matchThenNext(Sym);         // funcname
            s->pointer.funcp = g_src;   // 记录函数源码起始位置
            s->value = g_token;         // 符号表中记录函数名
            skipStatments();            // 跳过函数体, 在调用时再处理
            matchThenNext(';');         // 函数
            break;
        case Return:                     // return (<expr>) ;
            matchThenNext(Return);
            matchThenNext('(');
            g_return_val = expression();
            matchThenNext(')');
            matchThenNext(';');
            return RETURNFLAG;
        case Print:             // print(<expr>);
        case Read:              // read (<expr>);
        case Puts:              // put ( <expr> );
            func = g_token;
            double temp;
            matchThenNext(func);
            matchThenNext('(');
            switch (func) {
            case Print: 
                temp = expression();
                printf("%lf\n", temp);
                break;
            case Puts: 
                printf("%s\n", (char*)g_token_val.ptr);
                matchThenNext(Str);
                break;
            case Read:
                scanf("%lf", &g_token_val.ptr->value);
                g_token_val.ptr->type = Num;
                matchThenNext(Sym);
            }
            matchThenNext(')');
            matchThenNext(';');
            break;
        default:
            break;
    };
    return 0;
}

// 调用函数
/*
 * <func_name>( [arg] ); 
 * */
double function() {
    g_currentlevel++;
    g_return_val = 0;

    // parse func
    symbol* s = g_token_val.ptr;
    matchThenNext(FuncSym);
    matchThenNext('(');
    while (g_token != ')') {
        g_symtab[g_symPointer] = *g_token_val.ptr;
        strcpy(g_symtab[g_symPointer].name, g_token_val.ptr->name);
        g_symtab[g_symPointer].levelNum = g_currentlevel;
        g_symPointer++;
        matchThenNext(Sym);
        if (g_token == ',')
            matchThenNext(',');
    }
    matchThenNext(')');

    //
    char* startPos = g_src;
    char* startOldPos = g_old_src;
    int startToken = g_token;
    g_old_src = g_src = s->pointer.funcp;
    g_token = (int)s->value;
    statement();
    g_src = startPos;
    g_old_src = startOldPos;
    g_token = startToken;

    while (g_symtab[g_symPointer - 1].levelNum == g_currentlevel) {
        g_symPointer--;
    }
    
    g_currentlevel--;
    return g_return_val;
}

/*----------------------------------------------------------------*/

int main(int argc, char** argv)
{
    int i, fd;

    // 初始化符号表
    g_src = KEYWORDS;
    for (i = Array; i <= Read; ++i) {
        parse_next_token();
        g_symtab[g_symPointer -1].type = i;
    }

    // 初始化文件缓冲区
    if (!(g_src = g_old_src = (char*)malloc(POOLSIZE))) {
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
    if ((i = read(fd, g_src, POOLSIZE - 1)) <= 0) {
        printf("read() returned %d\n", i);
        return -1;
    }

    g_src[i] = 0; // add EOF character
    close(fd);

    // 词法解析
    parse_next_token();

    while (g_token != 0) {
        statement();
    }
    return 0;
}


