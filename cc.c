//---------------
// mini-c, by Sam Nipps (c) 2015; modified to create arm code instead by Dave Mugridge 2015.
// MIT license
//---------------
// remember that, as code is in the arm branch, when it's first cloned to linux, 
// you need to move to the arm branch: "git checkout arm"
//
// To compile: gcc -std=gnu11 -Werror -Wall ccd.c -o ccd
// To run on itself: ./ccd ccd.c
// To generate assembly output: gcc -std=gnu11 -Werror -Wall -S simple.c
// To generate an executable from assembly: gcc -o simple -g simple.s			// -g to include debug
//
// gdb t
// break main
// run <parameters>
// info registers
// si // single instruction
// n  // execute next instruction (won't enter a function)
// disas 0x109f2, 0x10a10	//disassemble a range -- note the comma!
// x /16x 0xaddress  // of form:/nfu 0xaddress
// print variable 
// p/c variable // print variable as char

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdbool.h>

//No enums :(
int ptr_size = 4;
int word_size = 4;

FILE* output;

//==== Lexer ====

char* inputname;
FILE* input;

int curln;
char curch;

char* buffer;
int buflength;
int token;

int token_other = 0;
int token_ident = 1;
int token_int = 2;
int token_char = 3;
int token_str = 4;


char next_char () {
    if (curch == '\n')
        curln++;
	curch = fgetc(input);
	
    return curch;
}

bool prev_char (char before) {
    ungetc(curch, input);
    curch = before;
    return false;
}

void eat_char () {
    //The compiler is typeless, so as a compromise indexing is done
    //in word size jumps, and pointer arithmetic in byte jumps.
    (buffer + buflength)[0] = curch;
    next_char();
	buflength++;
}

void next () {
    //Skip whitespace
    while (curch == ' ' || curch == '\r' || curch == '\n' || curch == '\t')
        next_char();

    //Treat preprocessor lines as line comments
    if (   curch == '#'
        || (curch == '/' && (next_char() == '/' || prev_char('/')))) {
        while (curch != '\n' && !feof(input))
            next_char();

        //Restart the function (to skip subsequent whitespace, comments and pp)
        next();
        return;
    }

    buflength = 0;
    token = token_other;

    //Identifier or keyword
    if (isalpha(curch)) {
        token = token_ident;
        while ((isalnum(curch) || curch == '_') && !feof(input))
            eat_char();

    //Integer literal
    } else if (isdigit(curch)) {
        token = token_int;

        while (isdigit(curch) && !feof(input))
            eat_char();

    //String or character literal
    } else if (curch == '\'' || curch == '"') {
        token = curch == '"' ? token_str : token_char;
		int openQuote = curch;
        eat_char();

        while (curch != openQuote && !feof(input)) {	// bug was here. I'm doing 32 bit read & should be byte; fixed by creating a var
            if (curch == '\\')
                eat_char();

            eat_char();
        }

        eat_char();

    //Two char operators
    } else if (   curch == '+' || curch == '-' || curch == '|' || curch == '&'
               || curch == '=' || curch == '!' || curch == '>' || curch == '<') {
        eat_char();

        if ((curch == buffer[0] && curch != '!') || curch == '=')
            eat_char();

    } else
        eat_char();

    (buffer + buflength)[0] = 0;
	buflength++;
	//printf("DEBUG: next, token = %i, buflength = %i, buffer = %s\n", token, buflength, buffer);
}

void lex_init (char* filename, int maxlen) {
    inputname = filename;
    input = fopen(filename, "r");

    //Get the lexer into a usable state for the parser
    curln = 1;
    buffer = malloc(maxlen);
    next_char();
    next();
}

void lex_end () {
    free(buffer);
    fclose(input);
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

void match (char* look) {
    if (!see(look)) {
        printf("%s:%d: error: ", inputname, curln);
        printf("expected '%s', found '%s'\n", look, buffer);
        errors++;
    }

    next();
}

bool try_match (char* look) {
    if (see(look)) {
        next();
        return true;

    } else
        return false;
}

//==== Symbol table ====

char** globals;
int global_no;
bool* is_fn;

char** locals;
int local_no;
int param_no;
int* offsets;

void sym_init (int max) {
    globals = malloc(ptr_size*max);
    global_no = 0;
    is_fn = calloc(max, ptr_size);

    locals = malloc(ptr_size*max);
    local_no = 0;
    param_no = 0;
    offsets = calloc(max, word_size);
}

void table_end (char** table, int table_size) {
    int i = 0;

    while (i < table_size)
        free(table[i++]);
}

void sym_end () {
    table_end(globals, global_no);
    free(globals);
    free(is_fn);

    table_end(locals, local_no);
    free(locals);
    free(offsets);
}

void new_global (char* ident) {
    globals[global_no++] = ident;
}

void new_fn (char* ident) {
    is_fn[global_no] = true;
    new_global(ident);
}

int new_local (char* ident) {
    int var_index = local_no - param_no;

    locals[local_no] = ident;
    //The first local variable is directly below the base pointer
    offsets[local_no] = -word_size*(var_index+1);
    return local_no++;
}

void new_param (char* ident) {
    int local = new_local(ident);

    //At and above the base pointer r7, in order, are:
	// 1. the first parameter [r7, #0]
	// 2. the second parameter [r7, #4], etc.
	// 3. the old base pointer (r7)
	// 4. the old sp
	// 5. the return address
	offsets[local] = word_size*(param_no++);
}

//Enter the scope of a new function
void new_scope () {
    table_end(locals, local_no);
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

//The label to jump to on `return`. Can be global as there aren't nested function definitions
int return_to;

int new_label () {
    return label_no++;
}

//==== One-pass parser and code generator ====

bool lvalue;

void needs_lvalue (char* msg) {
    if (!lvalue)
        error(msg);

    lvalue = false;
}

void expr (int level);

//The code generator for expressions works by placing the results
//in r0, r1 and backing them up to the stack.

//Regarding lvalues and assignment:

//An expression which can return an lvalue looks head for an
//assignment operator. If it finds one, then it pushes the
//address of its result. Otherwise, it dereferences it.

//The global lvalue flag tracks whether the last operand was an
//lvalue; assignment operators check and reset it.

void factor () {
	int global;	// compiler limitation that local vars have to be declared 1st, not inside {}
	int local;
	int str;
	
    lvalue = false;

    if (see("true") || see("false")) {
        fprintf(output, "\tmovs	r0, #%d\n", see("true") ? 1 : 0);		
        next();

    } else if (token == token_ident) {
        global = sym_lookup(globals, global_no, buffer);
        local = sym_lookup(locals, local_no, buffer);

        require(global >= 0 || local >= 0, "no symbol '%s' declared\n");
        next();

        if (see("=") || see("++") || see("--"))
            lvalue = true;

        if (global >= 0) {
			fprintf(output, "\tmovw r0, #:lower16:%s\n"
				"\tmovt r0, #:upper16:%s\n", globals[global], globals[global]);
			if (!(is_fn[global] || lvalue)) {
				fprintf(output, "\tldr r0, [r0]\n");		
			} 
		}
        else if (local >= 0) {
			if (lvalue) {
				fprintf(output, "\tadd r0, r7, #%i\n", offsets[local]);
			} else {
				fprintf(output, "\tldr r0, [r7, #%i]\n", offsets[local]);				
			}

		}
    } else if (token == token_int || token == token_char) {
        fprintf(output, "\tmovs r0, #%s\n", buffer);	
        next();

    } else if (token == token_str) {
        str = new_label();
        fprintf(output, "\t.section .rodata\n"
						"\t.align 2\n"
                        "_%08d:\n", str);

        //Consecutive string literals are concatenated
        while (token == token_str) {
            fprintf(output, "\t.ascii %s\n", buffer);
            next();
        }

        fputs("\t.byte 0\n"					//why is this is required?
              "\t.text\n", output);

        fprintf(output, "\tmovw	r0, #:lower16:_%08d\n", str);
        fprintf(output, "\tmovt	r0, #:upper16:_%08d\n", str);	

    } else if (try_match("(")) {
        expr(0);
        match(")");

    } else
        error("expected an expression, found '%s'\n");
}

void object () {
	int arg_no;
	
    factor();

    while (true) {
        if (try_match("(")) {
			fputs("\tpush {r0}\n", output);

            arg_no = 0;
            if (waiting_for(")")) {
                do {
                    expr(0);
                    fprintf(output, "\tpush {r0}\n");
                    arg_no++;
                } while (try_match(","));
            }

            match(")");
			if (arg_no >= 5)
				error("maximum of 4 parameters with this limited call implementation\n");
			
			while (arg_no >= 1) {						// take the parameters off the stack and load into the correct registers
				arg_no--;
				fprintf(output, "\tpop {r%i}\n", arg_no);

			};

			fprintf(output, "\tpop {r6}\n"); 		
			fprintf(output, "\tblx r6\n");			

        } else if (try_match("[")) {
			// array indexing
			
			fputs("\tpush {r0}\n", output);

            expr(0);
            match("]");

            if (see("=") || see("++") || see("--"))
                lvalue = true;

			fprintf(output, "\tlsls r0, r0, #2\n"		// the word size is "baked into" the shift left
							"\tpop {r1}\n"
							"\tadd r0, r0, r1\n");
			if (!lvalue) {
				fprintf(output, "\tldr r0, [r0]\n");
			}
        } else
            return;
    }
}

void unary () {
    if (try_match("!")) {
        //Recurse to allow chains of unary operations, LIFO order
        unary();

        fputs("\tcmp r0, #0\n"
              "\tite ne\n"
			  "\tmovne	r0, #0\n"
			  "\tmoveq	r0, #1\n"
			  "\tuxtb	r0, r0\n", output);

    } else if (try_match("-")) {
        unary();
		fputs("\tnegs r0, r0\n", output);		

    } else {
        //This function call compiles itself
        object();

        if (see("++") || see("--")) {
            fprintf(output, "\tadd r2, r0, #0\n"
							"\tldr r0, [r2]\n"
                            "\t%s r1, r0, #1\n"
                            "\tstr r1, [r2]\n", see("++") ? "adds" : "subs");
            needs_lvalue("assignment operator '%s' requires a modifiable object\n");
            next();
        }
    }
}

void branch (bool expr);

void expr (int level) {
	char* instr;
	char* instrNot;
	int shortcircuit;
	
    if (level == 5) {
        unary();
        return;
    }

    expr(level+1);

    while (  level == 4 ? see("+") || see("-") || see("*")
           : level == 3 ? see("==") || see("!=") || see("<") || see(">=")
           : false) {
        fputs("\tpush {r0}\n", output);
		
        instr = see("+") ? "add" : see("-") ? "subs" : see("*") ? "mul" :
                      see("==") ? "eq" : see("!=") ? "ne" : see("<") ? "lt" : "ge";	// this row is level 3, so execute on else clause below.
		instrNot =
					 see("==") ? "ne" : see("!=") ? "eq" : see("<") ? "ge" : "lt";

        next();
        expr(level+1);

        if (level == 4)
            fprintf(output, "\tmov r1, r0\n"
                            "\tpop {r0}\n"
                            "\t%s r0, r0, r1\n", instr);
        else {
			
			//fprintf(output, "\tpop {r1}\n"
            //                "\tcmp r1, r0\n"
			//				"\tite %s\n"
			//				"\tmov%s	r0, #1\n"
			//				"\tmov%s	r0, #0\n"
			//				"\tuxtb	r0, r0\n", instr, instr, instrNot);
							
            fprintf(output, "\tpop {r1}\n"
                            "\tcmp r1, r0\n"
							"\tite %s\n"
							"\tmov%s	r0, #1\n", instr, instr);
            fprintf(output, "\tmov%s	r0, #0\n"
							"\tuxtb	r0, r0\n", instrNot);							
		}
    }

    if (level == 2) while (see("||") || see("&&")) {
        shortcircuit = new_label();

        fprintf(output, "\tcmp r0, #0\n"
                        "\tb%s _%08d\n", see("||") ? "ne" : "eq", shortcircuit);
        next();
        expr(level+1);

        fprintf(output, "_%08d:\n", shortcircuit);
    }

    if (level == 1 && try_match("?"))
        branch(true);

    if (level == 0 && try_match("=")) {
        fputs("\tpush {r0}\n", output);
		
        needs_lvalue("assignment requires a modifiable object\n");
        expr(level+1);

        fputs("\tpop {r1}\n"	
              "\tstr r0, [r1]\n", output);			  
    }
}

void line ();

void branch (bool isexpr) {
    int false_branch = new_label();
    int join = new_label();

	fprintf(output, "\tcmp r0, #0\n"				
					"\tbeq _%08d\n", false_branch);	
					
    isexpr ? expr(1) : line();
	
	fprintf(output, "\tb _%08d\n", join);
    fprintf(output, "_%08d:\n", false_branch);

    if (isexpr) {
        match(":");
        expr(1);

    } else if (try_match("else"))
        line();

    fprintf(output, "_%08d:\n", join);	
}

void if_branch () {
    match("if");
    match("(");
    expr(0);
    match(")");
    branch(false);
}

void while_loop () {
    int loop_to = new_label();
    int break_to = new_label();

	fprintf(output, "_%08d:\n", loop_to);

    bool do_while = try_match("do");

    if (do_while)
        line();

    match("while");
    match("(");
    expr(0);
    match(")");

	fprintf(output, "\tcmp r0, #0\n"				
					"\tbeq _%08d\n", break_to);					

    if (do_while)
        match(";");

    else
        line();

    fprintf(output, "\t b _%08d\n", loop_to);
    fprintf(output, "_%08d:\n", break_to);	
}

void decl (int kind);

//See decl() implementation
int decl_module = 1;
int decl_local = 2;
int decl_param = 3;

void line () {
	bool ret;
	
    if (see("if"))
        if_branch();

    else if (see("while") || see("do"))
        while_loop();

    else if (see("int") || see("char") || see("bool"))
        decl(decl_local);

    else if (try_match("{")) {
        while (waiting_for("}"))
            line();

        match("}");

    } else {
        ret = try_match("return");

        if (waiting_for(";"))
            expr(0);

        if (ret)
            fprintf(output, "\tb _%08d\n", return_to);
			
        match(";");
    }
}

void function (char* ident) {
    //Prologue

	fprintf(output, "\t.text\n");
	fprintf(output, "\t.align	2\n");
	fprintf(output, "\t.global	%s\n", ident);
	fprintf(output, "\t.thumb\n");
	fprintf(output, "\t.thumb_func\n");
	fprintf(output, "\t.type	%s, %%function\n", ident);
	
    fprintf(output, "%s:\n", ident);

	// push the parameters onto the stack along with the base pointer (r7) and sp
	fprintf(output,
		  "\tpush	{");
	int i = 0;
	while (i < param_no) {
		fprintf(output, "r%i, ", i++);
	};
	fprintf(output,	 "r7, lr}\n"
		  "\tadd	r7, sp, #0\n"); 
	
	int localVarSize = new_label();
	fprintf(output,	 "\tsub	sp, sp, #_%08d\n", localVarSize);


    //Body
    return_to = new_label();

    line();

    //Epilogue

    fprintf(output, "_%08d:\n", return_to);
	fprintf(output, "\tadds	r7, r7, #%i\n", param_no*4);
	fprintf(output, "\tmov	sp, r7\n");
	fprintf(output, "\tpop	{r7, pc}\n");
	fprintf(output, "\t.size	%s, .-%s\n", ident, ident);
	fprintf(output, "\t.equ _%08d, %i\n", localVarSize, (local_no - param_no)*4);	// don't allocate extra space for func parameters
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
    if (try_match("(")) {
        if (kind == decl_module)
            new_scope();

        //Params
        if (waiting_for(")")) do {
            decl(decl_param);
        } while (try_match(","));

        match(")");

        new_fn(ident);
        fn = true;

        //Body
        if (see("{")) {
            require(kind == decl_module, "a function implementation is illegal here\n");

            fn_impl = true;
            function(ident); //, paramCount);
        }

    //Add it to the symbol table
    } else {
        if (kind == decl_local) {
            local = new_local(ident);
        } else
            (kind == decl_module ? new_global : new_param)(ident);
    }

    //Initialization

    if (see("="))
        require(!fn && kind != decl_param,
                fn ? "cannot initialize a function\n" : "cannot initialize a parameter\n");

    if (kind == decl_module) {
		fputs("\t.data\n", output);	

        if (try_match("=")) {
            if (token == token_int) {
				fprintf(output, "\t.global	%s\n", ident);
				fprintf(output, "\t.align	2\n");
				fprintf(output, "\t.type	%s, %%object\n", ident);
				fprintf(output, "\t.size	%s, 4\n", ident);
				fprintf(output, "\t%s:\n", ident);
				fprintf(output, "\t.word	%i\n", atoi(buffer));
	
			}
            else
                error("expected a constant expression, found '%s'\n");

            next();

        //Static data defaults to zero if no initializer
        } else if (!fn) {
			fprintf(output, "\t.global	%s\n", ident);	//massive duplication with above - fix?
			fprintf(output, "\t.align	2\n");
			fprintf(output, "\t.type	%s, %%object\n", ident);
			fprintf(output, "\t.size	%s, 4\n", ident);
			fprintf(output, "\t%s:\n", ident);
			fprintf(output, "\t.word	0\n");			
		}

		fputs("\t.text\n", output);	

    } else if (try_match("=")) {
        expr(0);
		fprintf(output, "\tstr r0, [r7,#%+d]\n", offsets[local]);
    }

    if (!fn_impl && kind != decl_param)
        match(";");
}

void program () {
	fputs("\t.syntax unified\n", output);
	fputs("\t.arch armv7-a\n", output);
	fputs("\t.eabi_attribute 27, 3\n", output);
	fputs("\t.eabi_attribute 28, 1\n", output);
	fputs("\t.fpu vfpv3-d16\n", output);
	fputs("\t.eabi_attribute 20, 1\n", output);
	fputs("\t.eabi_attribute 21, 1\n", output);
	fputs("\t.eabi_attribute 23, 3\n", output);
	fputs("\t.eabi_attribute 24, 1\n", output);
	fputs("\t.eabi_attribute 25, 1\n", output);
	fputs("\t.eabi_attribute 26, 2\n", output);
	fputs("\t.eabi_attribute 30, 6\n", output);
	fputs("\t.eabi_attribute 34, 1\n", output);
	fputs("\t.eabi_attribute 18, 4\n", output);
	fputs("\t.thumb\n", output);
	fprintf(output, "\t.file	\"%s\"\n", inputname);
	
    errors = 0;

    while (!feof(input))
        decl(decl_module);
	
	fputs("\t.ident	\"GCC: (Ubuntu/Linaro 4.9.2-10ubuntu13) 4.9.2\"\n", output);
	fputs("\t.section	.note.GNU-stack,\"\",%progbits\n", output);
}

int main (int argc, char** argv) {
	
	//--argc; ++argv;
	//if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
	//if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
	//if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }
	
    if (argc != 3) {
        puts("Usage: cc <Output File> <Input file>");
        return 1;
    }

    output = fopen(argv[1], "w");

    lex_init(argv[2], 256);

    sym_init(256);

    //No arrays? Fine! A 0xFFFFFF terminated string of null terminated strings will do.
    //A negative-terminated null-terminated strings string, if you will
    char* std_fns = "malloc\0calloc\0free\0atoi\0fopen\0fclose\0fgetc\0ungetc\0feof\0fputs\0fprintf\0puts\0printf\0"
                    "isalpha\0isdigit\0isalnum\0strlen\0strcmp\0strchr\0strcpy\0strdup\0\xFF\xFF\xFF\xFF";

    //Remember that mini-c is typeless, so this is both a byte read and a 4 byte read.
    //(char) 0xFF == -1 or 255, (int) 0xFFFFFF == -1
	

    while ((std_fns[0] != -1) && (std_fns[0] != 255)) { //dcm - added test for 255
        new_fn(strdup(std_fns));
        std_fns = std_fns+strlen(std_fns)+1;
    }
    program();

    lex_end();
    sym_end();
    fclose(output);

    return errors != 0;
}
