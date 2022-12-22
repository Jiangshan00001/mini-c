//---------------
// mini-c, by Sam Nipps (c) 2015
// MIT license
//---------------

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdbool.h>


void error (char* format);

//No enums :(
int PTR_SIZE = 8;
int WORD_SIZE = 8;

FILE* output;

int token;

int TOKEN_OTHER = 0;
int TOKEN_IDENT = 1;
int TOKEN_INT = 2;
int TOKEN_CHAR = 3;
int TOKEN_STR = 4;



//==== Lexer ====

/// 输入文件名
char* inputname;
/// 输入文件
FILE* input;

/// 当前行
int curln;
/// 当前字符
char curch;

char* buffer;
int buflength;

char next_char () {
    if (curch == '\n')
        curln++;

    curch = fgetc(input);
    //printf("get ch:%02x\n", curch);
    return curch;
}
bool prev_char (char before) {
    ungetc(curch, input);
    curch = before;
    return false;
}

void eat_char ()
{
    //The compiler is typeless, so as a compromise indexing is done
    //in word size jumps, and pointer arithmetic in byte jumps.
    (buffer + buflength++)[0] = curch;
    next_char();
}


void next ()
{
    //Skip whitespace
    while (curch == ' ' || curch == '\r' || curch == '\n' || curch == '\t')
        next_char();

    //Treat preprocessor lines as line comments
    if (   curch == '#'
           || (curch == '/' && (next_char() == '/' || prev_char('/'))))
    {
        //printf("comment...%d\n", curln);
        while (curch != '\n' && !feof(input))
            next_char();

        //Restart the function (to skip subsequent whitespace, comments and pp)
        next();
        return;
    }

    buflength = 0;
    token = TOKEN_OTHER;

    //Identifier, keyword or integer literal
    if (isalpha(curch) || isdigit(curch))
    {
        token = isalpha(curch) ? TOKEN_IDENT : TOKEN_INT;

        while (token == TOKEN_IDENT ? (isalnum(curch) || curch == '_') && !feof(input)
               : isdigit(curch) && !feof(input))
            eat_char();

        //String or character literal
    } else if (curch == '\'' || curch == '"')
    {
        token = curch == '"' ? TOKEN_STR : TOKEN_CHAR;
        //Can't retrieve this from the buffer - mini-c only has int reads
        char delimiter = curch;
        eat_char();

        while (curch != delimiter && !feof(input))
        {
            if (curch == '\\')
                eat_char();

            eat_char();
        }

        eat_char();

        //Two char operators
    } else if (   curch == '+' || curch == '-' || curch == '|' || curch == '&'
                  || curch == '=' || curch == '!' || curch == '>' || curch == '<')
    {
        eat_char();

        if ((curch == buffer[0] && curch != '!') || curch == '=')
            eat_char();

    } else
        eat_char();

    (buffer + buflength++)[0] = 0;
}

void lex_init (char* filename, int maxlen)
{
    inputname = filename;
    input = fopen(filename, "r");
    printf("input:%08x\n",input);
    //Get the lexer into a usable state for the parser
    curln = 1;
    buffer = malloc(maxlen);
    next_char();
    next();
}

//==== Parser helper functions ====

int errors;

void error (char* format) {
    printf("%s:%d: error: ", inputname, curln);
    //Accepting an untrusted format string? Naughty!
    printf(format, buffer);
    errors++;
}

void require (bool condition, char* format) {
    if (!condition)
        error(format);
}

bool see (char* look) {
    return !strcmp(buffer, look);
}

bool waiting_for (char* look) {
    return !see(look) && !feof(input);
}

void must_match (char* look) {
    if (!see(look)) {
        printf("%s:%d: error: expected '%s', found '%s'\n", inputname, curln, look, buffer);
        errors++;
    }

    next();
}

bool try_match (char* look) {
    bool saw = see(look);

    if (saw)
        next();

    return saw;
}

//==== Symbol table ====
///全局变量字符串指针的指针列表
char** globals;
/// 和globals数量一致，代表是否是函数
bool* is_fn;
///是否外部，如果是外部，则间接调用。内部的则直接调用
bool* is_extern;
bool curr_is_extern=false;
///全局变量初始值，以字符串的形式提供
int* globals_init_val;
/// 全局函数/变量的 个数
int global_no = 0;

///局部变量的字符串指针的列表
char** locals;
/// 偏移量，和locals长度一致。
int* offsets;
/// 局部变量个数
int local_no = 0;
int param_no = 0;

///字符串常量
char **const_strs;
int *const_strs_label;
///字符串常量的个数
int const_strs_no = 0;

void sym_init (int max) {
    globals = malloc(PTR_SIZE*max);
    globals_init_val = calloc(max, PTR_SIZE);
    is_fn = calloc(max, PTR_SIZE);
    is_extern = calloc(max, PTR_SIZE);

    locals = malloc(PTR_SIZE*max);
    offsets = calloc(max, WORD_SIZE);

    const_strs=malloc(PTR_SIZE*max);
    const_strs_label = calloc(max, WORD_SIZE);
}

void new_global (char* ident)
{
    globals[global_no++] = ident;
}

void new_fn (char* ident, int is_ext)
{
    printf("func:%s-%d\n", ident, is_ext);
    is_fn[global_no] = true;
    is_extern[global_no]=is_ext;
    new_global(ident);
}

int new_local (char* ident)
{
    int var_index = local_no - param_no;

    locals[local_no] = ident;
    //The first local variable is directly below the base pointer
    offsets[local_no] = -WORD_SIZE*(var_index+1);
    return local_no++;
}

void new_param (char* ident) {
    int local = new_local(ident);

    //At and above the base pointer, in order, are:
    // 1. the old base pointer, [ebp]
    // 2. the return address, [ebp+W]
    // 3. the first parameter, [ebp+2W]
    //   and so on
    offsets[local] = WORD_SIZE*(2 + param_no++);///2->1 jiangshan
}

//Enter the scope of a new function
void new_scope () {
    local_no = 0;
    param_no = 0;
}

int sym_lookup (char** table, int table_size, char* look) {
    int i = 0;

    while (i < table_size)
        if (!strcmp(table[i++], look))
            return i-1;

    return -1;
}

//==== Codegen labels ====

int label_no = 0;

/// 1个函数有2个label，一个开始，一个结束。所有的return都跳到结束的label处
//The label to jump to on `return`
int return_to;

int new_label () {
    return label_no++;
}

int emit_label (int label) {
    fprintf(output, "_%08d:\n", label);
    return label;
}

//==== One-pass parser and code generator ====

bool lvalue;

void needs_lvalue (char* msg) {
    if (!lvalue)
        error(msg);

    lvalue = false;
}

void expr (int level);

int char_preprocess(char* buf)
{
    //    if(strncmp(buf,"'",1)!=0)
    //    {
    //        printf("char error%s. buf0=%08x c=%08x\n", buf, buf[0], '\'');
    //        error("char error%s\n");
    //        return 0;
    //    }
    if(strncmp(buf,"\\",1)!=0)
    {
        //不是特殊字符，直接返回
        return  12345;
    }
    ///特殊字符处理
    if(strncmp(buf+1,"n",1)==0)
    {
        return '\n';
    }
    else if(strncmp(buf+1,"r",1)==0)
    {
        return '\r';
    }
    else if(strncmp(buf+1,"t",1)==0)
    {
        return '\t';
    }
    else if(strncmp(buf+1,"0",1)==0)
    {
        return '\0';
    }
    else if(strncmp(buf+1,"\\",1)==0)
    {
        return '\\';
    }
    else if(strncmp(buf+1,"'",1)==0)
    {
        return '\'';
    }
    else if(strncmp(buf+1,"x",1)==0)
    {
        return 255;//atoi(buf+2); FIXME .此处只有1个字符，就是\xff
    }

    printf("error unknown char:%s\n", buffer);
    error("err: %s\n");
    return 12346;

}

/// 表达式的代码生成，通过将结果放到eax，然后放入堆栈实现
//The code generator for expressions works by placing the results
//in eax and backing them up to the stack.

/// 左值和赋值
/// 表达式返回左值，可以向前看一个赋值算符。如果找到，就把地址放入结果。否则变为右值。
///
/// 全局左值标记跟踪上一个算符是否是左值。赋值算符检查并复位标记。
//Regarding lvalues and assignment:

//An expression which can return an lvalue looks head for an
//assignment operator. If it finds one, then it pushes the
//address of its result. Otherwise, it dereferences it.

//The global lvalue flag tracks whether the last operand was an
//lvalue; assignment operators check and reset it.

///
/// \brief factor
///
///一.lea指令:
///对于寄存器来说:第二个操作数是寄存器必须要加[],不然报错,这里lea就是取[寄存器]的值,如:
/// mov eax,2
/// lea ebx,[eax];执行后ebx=2
/// mov ebx,eax;等同于上句
/// lea ebx,eax;编译器报错: error A2070: invalid instruction operands
/// 对于变量来说加不加[]都是一样的效果,都是取变量的地址,相当于指针
/// 如:
/// num dword 2
/// lea ebx,num
/// lea eax,[num]; eax为num的地址,如eax=4206598,随程序不同不同,这时ebx==eax
///二.mov指令:
/// 对于变量来说
/// num dword 2
/// mov eax,2
/// mov ebx,num
/// mov ecx,[num];执行完ebx==ecx==2
/// 对寄存器
/// mov ebx,eax;ebx==2
/// mov ecx,[eax];可能会报错,因为这里翻译成汇编是mov ecx,DS:[eax]
/// 总的说来加不加中括号[]的区别就是:
/// lea对变量没有影响是取地址,对寄存器来说加[]时取值,第二操作数不加[]非法
/// mov对变量来说没有影响是取值,对寄存器来说是加[]时取地址,第二操作数不加[]是取值

void factor ()
{
    lvalue = false;

    if (see("true") || see("false"))
    {
        fprintf(output, "mov rax, %d\n", see("true") ? 1 : 0);
        next();
    }
    else if (token == TOKEN_IDENT)
    {
        int global = sym_lookup(globals, global_no, buffer);
        int local = sym_lookup(locals, local_no, buffer);

        require(global >= 0 || local >= 0, "no symbol '%s' declared\n");
        next();

        if (see("=") || see("++") || see("--"))
        {
            ///如果有 a=121; 或 a++; 或a--;则 a要保留左值
            lvalue = true;
        }

        ///FIXME: 此处应该是先局部变量，再全局变量???
        if (global >= 0)
        {
            ///全局变量，通过变量名读取
            /// 全局函数，外部和内部的调用方式不一致，所以此处需要记录???
            fprintf(output, "%s rax, [%s]\n", is_fn[global] || lvalue ? "lea" : "mov", globals[global]);
            curr_is_extern=is_extern[global];
        }
        else if (local >= 0)
        {
            /// 局部变量，通过栈指针获取
            fprintf(output, "%s rax, [rbp%+d]\n", lvalue ? "lea" : "mov", offsets[local]);
        }

    }
    else if (token == TOKEN_INT)
    {
        fprintf(output, "mov rax, %s\n", buffer);
        next();
    }
    else if(token==TOKEN_CHAR)
    {
        int char_out = 0;
        if(strncmp(buffer+1,"\\",1)!=0)
        {
            //不是特殊字符，直接返回
            fprintf(output, "mov rax, %s\n", buffer);
        }
        else
        {
            char_out = char_preprocess(buffer+1);
            fprintf(output, "mov rax, %u\n", char_out);
        }
        next();
    }
    else if (token == TOKEN_STR)
    {
        ///20221215
        /// 字符串不再在此处生成，而是保存下来，放在后期生成
        ///
        int str =0;
        str = new_label();
        fprintf(output, "lea rax,  [_%08d]\n", str);
        const_strs_label[const_strs_no]=str;
        const_strs[const_strs_no]=strdup(buffer);
        //printf("currln=%d, id=%d %s\n", curln,str, const_strs[const_strs_no]);

        const_strs_no++;
        next();
        while (token == TOKEN_STR)
        {
            char *str_n = malloc(1024);
            char *str_old = const_strs[const_strs_no-1];
            int len_old = strlen(str_old);
            (str_old+len_old-1)[0]=0;//去除最后的"
            sprintf(str_n, "%s%s",str_old, buffer+1);//buffer是去除最前面的双引号
            ///如果下一个还是字符串，则将下一个字符串放入上一个字符串
            /// 两个字符串连接在一起
            const_strs[const_strs_no-1]=strdup(str_n);
            free(str_n);
            //printf("str cat to:%s\n", const_strs[const_strs_no-1]);
            next();
        }
    }
    else if (try_match("("))
    {
        expr(0);
        must_match(")");
    }
    else
    {
        error("expected an expression, found '%s'\n");
    }
}

void object () {
    int i = 0;
    int local_curr_extern = 0;
    factor();

    while (true) {
        if (try_match("("))
        {
            ///x64中，每个函数调用，栈中必须至少有4个位置
            /// 栈必须是16字节对齐的
            ///
            /// 此处预留4个位置，避免只有1个参数时，栈中其它数值被调用函数覆盖
            fputs("sub rsp, 8*4\n",output);

            ///函数指针
            fputs("push rax\n", output);
            local_curr_extern = curr_is_extern;///此处记录，避免在解析参数时，被函数调用的参数覆盖

            /// 此处是函数调用:
            /// 4个参数，从左到右，依次放入  - RCX、RDX、R8 和 R9
            /// func1(a,b,c);

            int arg_no = 0;

            if (waiting_for(")"))
            {
                //cdecl requires arguments to be pushed on backwards
                
                int start_label = new_label();
                int end_label = new_label();
                int prev_label = end_label;

                fprintf(output, "jmp _%08d\n", start_label);

                do {
                    int next_label = emit_label(new_label());
                    expr(0);
                    fprintf(output, "push rax\n"
                                    "jmp _%08d\n", prev_label);
                    arg_no++;

                    prev_label = next_label;
                } while (try_match(","));

                fprintf(output, "_%08d:\n", start_label);
                fprintf(output, "jmp _%08d\n", prev_label);
                fprintf(output, "_%08d:\n", end_label);
            }

            must_match(")");

            /// dword ptr
            /// 此处进行函数调用
            ///
            for(i=0;i<arg_no;i++)
            {
                if (i==0)
                {
                    fprintf(output, "mov rcx, qword[rsp+%d]\n", (i)*WORD_SIZE);
                }
                else if(i==1)
                {
                    fprintf(output, "mov rdx, qword[rsp+%d]\n", (i)*WORD_SIZE);
                }
                else if(i==2)
                {
                    fprintf(output, "mov r8, qword[rsp+%d]\n", (i)*WORD_SIZE);
                }
                else if(i==3)
                {
                    fprintf(output, "mov r9, qword[rsp+%d]\n", (i)*WORD_SIZE);
                }
            }

            ///将函数地址取出并调用
            if (local_curr_extern)
            {
                fprintf(output, "mov rax,qword [rsp+%d]\n",(arg_no)*WORD_SIZE);
            }
            else
            {
                fprintf(output, "lea rax,qword [rsp+%d]\n",(arg_no)*WORD_SIZE);
            }
            fprintf(output, "call qword [rax]\n");
            fprintf(output, "add rsp, %d\n", (arg_no+1)*WORD_SIZE);

            /// 回收预留的位置---此处预留4个位置，避免只有1个参数时，栈中其它数值被调用函数覆盖
            fputs("add rsp, 8*4\n",output);

        }
        else if (try_match("["))
        {
            /// 中括号：
            /// 1 push eax; 先将左值eax放入栈
            /// 2 val->eax求中括号内的表达式的值（默认会放入eax中）
            /// 3 pop ebx; lea/mov eax, [eax*d+ebx]
            fputs("push rax\n", output);

            expr(0);
            must_match("]");

            if (see("=") || see("++") || see("--"))
                lvalue = true;

            fprintf(output, "pop rbx\n"
                            "%s rax, [rax*%d+rbx]\n", lvalue ? "lea" : "mov", WORD_SIZE);

        }
        else
        {
            return;
        }
    }
}

void unary () {
    if (try_match("!")) {
        /// last in first out.
        //Recurse to allow chains of unary operations, LIFO order
        unary();

        fputs("cmp rax, 0\n"
              "mov rax, 0\n"
              "sete al\n", output);

    } else if (try_match("-")) {
        unary();
        fputs("neg rax\n", output);

    } else {
        //This function call compiles itself
        object();

        if (see("++") || see("--"))
        {
            fprintf(output, "mov rbx, rax\n"
                            "mov rax, [rbx]\n"
                            "%s qword [rbx], 1\n", see("++") ? "add" : "sub");
            //%s dword ptr [rbx], 1
            needs_lvalue("assignment operator '%s' requires a modifiable object\n");
            next();
        }
    }
}

void branch (bool expr);

void expr (int level)
{
    if (level == 5)
    {
        unary();
        return;
    }

    expr(level+1);

    while (  level == 4 ? see("+") || see("-") || see("*")
             : level == 3 ? see("==") || see("!=") || see("<") || see(">=")
             : false)
    {
        fputs("push rax\n", output);

        char* instr = see("+") ? "add" : see("-") ? "sub" : see("*") ? "imul" :
                                                                       see("==") ? "e" : see("!=") ? "ne" : see("<") ? "l" : "ge";

        next();
        expr(level+1);

        if (level == 4)
            fprintf(output, "mov rbx, rax\n"
                            "pop rax\n"
                            "%s rax, rbx\n", instr);

        else
            fprintf(output, "pop rbx\n"
                            "cmp rbx, rax\n"
                            "mov rax, 0\n"
                            "set%s al\n", instr);
    }

    if (level == 2) while (see("||") || see("&&")) {
        int shortcircuit = new_label();

        fprintf(output, "cmp rax, 0\n"
                        "j%s _%08d\n", see("||") ? "nz" : "z", shortcircuit);
        next();
        expr(level+1);

        fprintf(output, "\t_%08d:\n", shortcircuit);
    }

    if (level == 1 && try_match("?"))
        branch(true);


    if (level == 0 && try_match("="))
    {//
        /// a=123;
        /// a=func1();
        fputs("push rax\n", output);

        needs_lvalue("assignment requires a modifiable object\n");
        expr(level+1);

        fputs("pop rbx\n"
              "mov  [rbx], rax\n", output);//dword ptr
    }
}

void line ();

void branch (bool isexpr)
{
    int false_branch = new_label();
    int join = new_label();

    fprintf(output, "cmp rax, 0\n"
                    "je _%08d\n", false_branch);

    isexpr ? expr(1) : line();

    fprintf(output, "jmp _%08d\n", join);
    fprintf(output, "\t_%08d:\n", false_branch);

    if (isexpr) {
        must_match(":");
        expr(1);

    } else if (try_match("else"))
        line();

    fprintf(output, "\t_%08d:\n", join);
}

void if_branch () {
    must_match("if");
    must_match("(");
    expr(0);
    must_match(")");
    branch(false);
}
void for_loop(){

    int if_jmp_start=new_label();
    int every_loop_add=new_label();
    int loop_body_start=new_label();
    int loop_end=new_label();

    must_match("for");
    must_match("(");
    line();

    emit_label(if_jmp_start);
    line();

    fprintf(output, "cmp rax, 0\n"
                    "jne _%08d\n", loop_body_start);
    fprintf(output, "cmp rax, 0\n"
                    "je _%08d\n", loop_end);

    emit_label(every_loop_add);
    expr(0);
    must_match(")");

    fprintf(output, "jmp _%08d\n", if_jmp_start);


    emit_label(loop_body_start);
    line();
    fprintf(output, "jmp _%08d\n", every_loop_add);

    emit_label(loop_end);
}
void while_loop () {
    int loop_to = emit_label(new_label());
    int break_to = new_label();

    bool do_while = try_match("do");

    if (do_while)
        line();

    must_match("while");
    must_match("(");
    expr(0);
    must_match(")");

    fprintf(output, "cmp rax, 0\n"
                    "je _%08d\n", break_to);

    if (do_while)
        must_match(";");

    else
        line();

    fprintf(output, "jmp _%08d\n", loop_to);
    fprintf(output, "\t_%08d:\n", break_to);
}

void decl (int kind);

//See decl() implementation
int DECL_MODULE = 1;
int DECL_LOCAL = 2;
int DECL_PARAM = 3;

///
/// \brief line stat. 一个语句???
///
void line ()
{
    if (see("if"))
        if_branch();

    else if (see("while") || see("do"))
        while_loop();
    else if(see("for"))
        for_loop();
    else if (see("int") || see("char") || see("bool"))
    {
        ///局部变量
        decl(DECL_LOCAL);
    }
    else if (try_match("{"))
    {
        while (waiting_for("}"))
            line();

        must_match("}");

    }
    else
    {
        bool ret = try_match("return");

        if (waiting_for(";"))
            expr(0);

        if (ret)
            fprintf(output, "jmp _%08d\n", return_to);

        must_match(";");
    }
}

void function (char* ident) {
    //Body
    int i=0;
    int body = emit_label(new_label());
    return_to = new_label();

    ///此处是函数体内部
    /// 应该先将参数放入堆栈，方便当前代码使用
    ///

    for(i=0;i<param_no;i++)
    {
        if(i==0)
        {
            fprintf(output, "mov qword [rbp%+d], rcx\n",offsets[i]);
        }
        else if(i==1)
        {
            fprintf(output, "mov qword [rbp%+d], rdx\n",offsets[i]);
        }
        else if(i==2)
        {
            fprintf(output, "mov qword [rbp%+d], r8\n",offsets[i]);
        }
        else if(i==3)
        {
            fprintf(output, "mov qword [rbp%+d], r9\n",offsets[i]);
        }
    }

    line();

    if(strcmp(ident, "main")==0)
    {
        fputs("mov rcx, 0\n",output);
        fputs("call [ExitProcess]\n",output);
    }
    //Epilogue

    fprintf(output, "\t_%08d:\n", return_to);
    fputs("mov rsp, rbp\n"
          "pop rbp\n"
          "ret\n", output);
    
    //Prologue
    //Only after passing the body do we know how much space to allocate for the
    //local variables, so we write the prologue here at the end.
    //fprintf(output, ".globl %s\n"
    //                "%s:\n", ident, ident);
    fprintf(output, "%s:\n", ident);

    fprintf(output, "push rbp\n"
                    "mov rbp, rsp\n"
                    "sub rsp, %d\n"
                    "jmp _%08d\n", local_no*WORD_SIZE, body);
}

void decl (int kind) {
    //A C declaration comes in three forms:
    // - Local decls, which end in a semicolon and can have an initializer.
    // - Parameter decls, which do not and cannot.
    // - Module decls, which end in a semicolon unless there is a function body.

    bool fn = false;
    bool fn_impl = false;
    int local;

    next();

    while (try_match("*"))
        ;

    //Owned (freed) by the symbol table
    char* ident = strdup(buffer);
    next();

    //Functions
    if (try_match("("))
    {
        ///解析函数参数
        if (kind == DECL_MODULE)
            new_scope();

        //Params
        if (waiting_for(")"))
        {
            do
            {
                decl(DECL_PARAM);
            } while (try_match(","));
        }

        must_match(")");

        ///声明新的函数
        new_fn(ident,0);
        fn = true;

        ///解析函数体
        //Body
        if (see("{"))
        {
            require(kind == DECL_MODULE, "a function implementation is illegal here\n");

            fn_impl = true;
            function(ident);
        }

        //Add it to the symbol table
    }
    else
    {
        if (kind == DECL_LOCAL)
        {
            ///是局部变量
            local = new_local(ident);
        } else
        {
            ///新的参数/全局变量
            (kind == DECL_MODULE ? new_global : new_param)(ident);
        }
    }

    //Initialization

    if (see("="))
        require(!fn && kind != DECL_PARAM,
                fn ? "cannot initialize a function\n" : "cannot initialize a parameter\n");

    if (kind == DECL_MODULE)
    {
        ///全局变量初始化，不再放在代码中间
        /// 而是放在最后
        if (try_match("=")) {
            ///int a=1;
            if (token == TOKEN_INT)
            {
                globals_init_val[global_no-1]=atoi(buffer);
            }
            next();
        }
        else if(!fn)
        {
        }
    }
    else if (try_match("="))
    {
        expr(0);
        //dword ptr
        fprintf(output, "mov [rbp%+d], rax\n", offsets[local]);
    }

    if (!fn_impl && kind != DECL_PARAM)
        must_match(";");
}


void program () {
    int i = 0;
    int j = 0;
    fputs("format PE64 console\n", output);
    fputs("include 'win64wx.inc' ;\n", output);
    fputs("entry start \n", output);
    fputs("section '.text' code readable executable\n", output);

    fputs("start:\n",output);
    fputs("sub rsp, 58\n", output);

    fputs("lea rcx, [main_argc]\n", output);
    fputs("lea rdx, [main_argv]\n", output);
    fputs("lea r8, [main_env_arr]\n", output);
    fputs("mov r9,0\n", output);

    fputs("lea rax, [rsp+8*(1+1+1+1+1)]\n",output);
    fputs("and qword [rax],0\n",output);
    fputs("mov qword [rsp+8*4],rax\n", output);

    fputs("call [__getmainargs]\n",output);

    fputs("and rsp, -16\n", output);
    fputs("mov rcx, [main_argc]\n", output);
    fputs("mov rdx, [main_argv]\n", output);

    fputs("jmp main\n",output);

    errors = 0;

    while (!feof(input))
        decl(DECL_MODULE);

    fputs("call	[getchar]\n",output);

    ///此处添加全局变量的初始化
    fputs("section '.data' data readable writeable\n", output);
    for(i=0;i<global_no;i++)
    {
        if (!is_fn[i]){
            fprintf(output, "%s dq  %u\n", globals[i],globals_init_val[i]);
        }
    }
    fputs("main_argc dq ?\nmain_argv dq ?\n main_env_arr dq ?\n", output);
    fputs("db 0,0,0,0\n"
          , output);

    /// 此处添加全局数据.现在只有字符串
    ///
    ///
    if(const_strs_no>=1)
        fputs("section '.rodata' data readable\n", output);
    for(i=0;i<const_strs_no;i++)
    {
        fprintf(output, "_%08d db ", const_strs_label[i]);
        ///FIXME: "abcd" 此处双引号需要去掉。当前通过j=1..strlen-1去掉了。后期需要在别处去掉??
        for(j=1;j<strlen(const_strs[i])-1;j++)
        {
            if(strncmp(const_strs[i]+j,"\\",1)==0)
            {
                ///此处下一个字符是特殊字符
                if(strncmp(const_strs[i]+j+1,"x",1)==0)
                {

                    /// \xFF
                    /// FIXME: 此处只支持0xff
                    fprintf(output, "%u, ", 255);
                    j=j+3;

                }
                else
                {
                    int f1=char_preprocess(const_strs[i]+j);
                    fprintf(output, "%u, ", f1);
                    j++;
                }
            }
            else if(strncmp(const_strs[i]+j,"'",1)==0)
            {
                fprintf(output, "%u, ", '\'');
            }
            else
            {
                //正常字符，直接转为字符
                fprintf(output, "'%c', ", (const_strs[i]+j)[0]);
            }
        }
        fprintf(output, "0\n");


    }

    ///程序结尾
    /// 添加c语言库函数
    fputs("section '.idata' data readable import\n", output);
    fputs("library kernel32, 'kernel32.dll', msvcrt,   'msvcrt.dll',shell,'SHELL32.DLL' \n", output);//, crtdll, 'crtdll.dll'

    fputs("import kernel32, GetCommandLine,'GetCommandLineA', \\\n", output);
    fputs("ExitProcess,'ExitProcess' \n",output);
    fputs("import shell, CommandLineToArgv,'CommandLineToArgv'\n", output);

    fputs("import msvcrt, printf, 'printf', \\\n", output);
    fputs("getchar, 'getchar', \\\n",output);
    fputs("malloc,'malloc',\\\n", output);
    fputs("free,'free',\\\n", output);
    fputs("calloc,'calloc',\\\n", output);
    fputs("atoi,'atoi',\\\n", output);
    fputs("fopen,'fopen',\\\n", output);
    fputs("fclose,'fclose',\\\n", output);
    fputs("fgetc,'fgetc',\\\n", output);
    fputs("ungetc,'ungetc',\\\n", output);
    fputs("feof,'feof',\\\n", output);
    fputs("fputs,'fputs',\\\n", output);
    fputs("fprintf, 'fprintf', \\\n",output);
    fputs("puts,'puts',\\\n", output);
    fputs("isalpha,'isalpha',\\\n", output);
    fputs("isdigit,'isdigit',\\\n", output);
    fputs("isalnum,'isalnum',\\\n", output);
    fputs("strlen,'strlen',\\\n", output);
    fputs("strcmp,'strcmp',\\\n", output);
    fputs("strncmp,'strncmp',\\\n", output);
    fputs("strchr,'strchr',\\\n", output);
    fputs("strcpy,'strcpy',\\\n", output);
    fputs("strdup,'_strdup',\\\n", output);
    fputs("sprintf,'sprintf',\\\n", output);
    fputs("__getmainargs, '__getmainargs',\\\n",output);
    fputs("__wgetmainargs, '__wgetmainargs'\n",output);
}

/// argc argv获取方式：
/// 3 msvcrt.dll 的 __getmainargs
int main (int argc, char** argv)
{

    if (argc != 2) {
        puts("Usage: cc <file>");
        printf(" %d %d\n", argc, argv);
        return 1;
    }
    printf(" %d %s\n", argc, argv[1]);


    output = fopen("a.asm", "w");
    printf("output file:%08x\n", output);

    lex_init(argv[1], 1024*50);

    sym_init(4096);

    //No arrays? Fine! A 0xFFFFFF terminated string of null terminated strings will do.
    //A negative-terminated null-terminated strings string, if you will
    char* std_fns = "getchar\0malloc\0calloc\0free\0atoi\0fopen\0fclose\0fgetc\0ungetc\0feof\0fputs\0fprintf\0puts\0printf\0"
                    "isalpha\0isdigit\0isalnum\0strlen\0strcmp\0strncmp\0strchr\0strcpy\0strdup\0sprintf\0\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF";

    /// 声明系统内部函数
    //Remember that mini-c is typeless, so this is both a byte read and a 4 byte read.
    //(char) 0xFF == -1, (int) 0xFFFFFF == -1
    while (std_fns[0] != -1) {
        char *tmp=strdup(std_fns);
        new_fn(tmp,1);
        std_fns = std_fns+strlen(std_fns)+1;
    }
    printf("start program\n");

    program();

    fclose(output);

    printf("parse finish!%d\n", errors);
    return errors != 0;
}
