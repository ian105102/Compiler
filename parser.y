%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

extern int yylex();
extern int yylineno;
void yyerror(const char *s);
extern FILE *yyin;
int hasError = 0;

int tempCount = 0;
char* newTemp() {
    char* temp = (char*)malloc(10);
    sprintf(temp, "t%d", ++tempCount);
    return temp;
}

#define MAX_LOOP_DEPTH 100
int labelCount = 0;
int jumpLabelStack[MAX_LOOP_DEPTH];
int jumpLabelTop = -1;
void pushJumpLabel(int labelCount) {
    if (jumpLabelTop < MAX_LOOP_DEPTH - 1) {
        jumpLabelStack[++jumpLabelTop] = labelCount;
    } else {
        fprintf(stderr, "jumpLabelStack overflow\n");
        exit(1);
    }
}
void popJumpLabel() {
    if (jumpLabelTop >= 0) {
        jumpLabelTop--;
    } else {
        // 堆疊底部錯誤處理
        fprintf(stderr, "jumpLabelStack underflow\n");
        exit(1);
    }
}

int currentJumpLabel() {
    if (jumpLabelTop >= 0) {
        return jumpLabelStack[jumpLabelTop];
    } else {
        // 沒有標籤時返回預設值
        return -1;
    }
}
char* newLabel() {
    char* label = (char*)malloc(20);
    snprintf(label, 20, "L%d", ++labelCount);
    return label;
}

char* concatCode(int count, ...) {
    va_list args;
    va_start(args, count);

    size_t totalLen = 0;
    for (int i = 0; i < count; i++) {
        char* s = va_arg(args, char*);
        if (s && strlen(s) > 0) {
            totalLen += strlen(s) + 1; // +1 給換行或空白
        }
    }
    va_end(args);

    char* result = (char*)calloc(totalLen + 1, sizeof(char));
    result[0] = '\0';

    va_start(args, count);
    for (int i = 0; i < count; i++) {
        char* s = va_arg(args, char*);
        if (s && strlen(s) > 0) {
            if (strlen(result) > 0) {
                strcat(result, "\n"); // 僅在前面有東西時換行
            }
            strcat(result, s);
        }
    }
    va_end(args);

    return result;
}



struct exprType {
    char *addr;  
    char *code;  
    
};


%}

%union {
    char *str;
    int ival;
    float fval;

    struct exprType* expr;
}


%token <str> IDENTIFIER INT_LITERAL FLOAT_LITERAL STRING_LITERAL TRUE FALSE
%token IF ELSE WHILE RETURN INT FLOAT USING CLASS CONSOLE STATIC VOID BOOL BREAK FOR STRING NELLINE
%token EQ NEQ LE GE LT GT 
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token ASSING PLUS MINUS STAR SLASH MOD 
%token SEMICOLON COMMA DOT COLON QUESTION
%token UNKNOWN
%token OR AND NOT
%token INCREMENT DECREMENT


%define parse.error verbose

%type <expr> expression assignment_expression logical_expression relational_expression
             additive_expression multiplicative_expression increment_expression 
             decrement_expression  primary_expression  
             


%type <expr> variable literal  method_call_statement for_condition_opt var_declaration veriable_declaration  
%type <str>  class_body class_body_entry  type argument_list_opt argument_list push_loop push_loop_sec push_loop_thr 
             function_declaration parameter_list_opt parameter_list
            parameter block statements  
             modifier_list modifier  
%type <str> statement if_statement  
%type <str> for_init_opt  for_update_opt  for_statement 
%type  using_list program 



%start program

%%
	
program
    : using_list class_declaration 
    ;
using_list
    : /* 空 */
    | using_list USING variable SEMICOLON

    ;

class_declaration
    : CLASS IDENTIFIER LBRACE class_body RBRACE{
	printf("TAC Start\n");
	printf("\n");
	printf("%s\n", $4);
	printf("\n");
	printf("TAC End\n");
	printf("\n");
    }

    ;

class_body
    : /* 空 */{
    	$$ =strdup("");
    }
    | class_body class_body_entry{
    	 $$ =concatCode(2, $1, $2);
    }
    ;

class_body_entry
    : var_declaration{
	$$ = $1->code;
     }
    | function_declaration{
	$$ = $1;
     }
    ;



var_declaration
    : veriable_declaration {
        $$ = $1; 
    }
    | type IDENTIFIER SEMICOLON {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = strdup($2);
        char buffer[128];
        if (strcmp($1, "int") == 0 || strcmp($1, "bool") == 0) {
            snprintf(buffer, sizeof(buffer), "%s = 0;", $2);
        } else if (strcmp($1, "string") == 0) {
            snprintf(buffer, sizeof(buffer), "%s = \"\";", $2);
        } else {
            buffer[0] = '\0';
        }

        e->code = strdup(buffer);

        free($1);
        free($2);
        $$ = e;
    }
 
    ;
veriable_declaration
    : type assignment_expression SEMICOLON {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = strdup($2->addr);
        int len = strlen($2->code) + 128;
        e->code = (char*)malloc(len);
        snprintf(e->code, len, "%s", $2->code);
        free($2->addr);
        free($2->code);
        free($2);
        free($1);
        $$ = e;
    }
    ;
function_declaration
    : VOID IDENTIFIER LPAREN parameter_list_opt RPAREN block{
       $$ = $6;
    }
    | type IDENTIFIER LPAREN parameter_list_opt RPAREN block{ 
	$$ = $6;
    }
    | modifier_list type IDENTIFIER LPAREN parameter_list_opt RPAREN block{        
	$$ = $7;
    }
    | modifier_list VOID IDENTIFIER LPAREN parameter_list_opt RPAREN block{        
        $$ = $7;
    }
    ;

parameter_list_opt
    : /* 空 */ {
        $$ = strdup("");
    }
    | parameter_list {
        $$ = $1; 
    }
    ;

parameter_list
    : parameter {
        $$ = $1; 
    }
    | parameter COMMA parameter_list {
        int len = strlen($1) + strlen($3) + 2; 
        char* combined = (char*)malloc(len);
        sprintf(combined, "%s, %s", $1, $3);
        $$ = combined;

        free($1);
        free($3);
    }
    ;

parameter
    : type IDENTIFIER {
        int len = strlen($1) + strlen($2) + 2;
        char* param = (char*)malloc(len);
        sprintf(param, "%s %s", $1, $2);
        $$ = param;
    }
    ;

block
    :LBRACE statements RBRACE{
    	$$ = $2;
    }
    ;
    
statements
    : /* 空 */
    {
        $$ = "";
    }
    | statements statement
    {
        $$ =concatCode(2, $1, $2);
    }
    ;
    
statement
    : var_declaration {
   	 $$ = $1->code;
    }
    | method_call_statement {
      $$ = $1->code;
    }
    | for_statement {
     $$ = $1;
    }
    | if_statement {
     $$  = $1;
    }
    | BREAK SEMICOLON {
      int currentLabelNum = currentJumpLabel();  // 取目前堆疊頂端標籤
      if (currentLabelNum == -1) {
         fprintf(stderr, "Error: 'break' used outside of loop.\n");
         exit(1);
      }
      char label[32];
      snprintf(label, sizeof(label), "L%d", currentLabelNum);

      int len = strlen("jmp ") + strlen(label) + strlen(";") + 1;
      char* buf = malloc(len);
      snprintf(buf, len, "jmp %s;", label);
      $$ = buf;
    }

    | assignment_expression SEMICOLON {
       $$ = $1->code;
    }
    ;

if_statement
    : IF LPAREN expression RPAREN block
    {
        char* L_end = newLabel();
        char* cond_code = $3->code;
        char* cond_addr = $3->addr;
        char jump_to_end[128], label_end[64];
        sprintf(jump_to_end, "if False %s goto %s", cond_addr, L_end);
        sprintf(label_end, "%s:", L_end);
 	$$ = concatCode(4,
            cond_code,       
            jump_to_end,    
            $5,              
            label_end         
        );
      
    }

    | IF LPAREN expression RPAREN block ELSE block
    {
        char* L_else = newLabel();
        char* L_end = newLabel();
        char* cond_code = $3->code;
        char* cond_addr = $3->addr;
        char jump_to_else[128], goto_end[64];
        char label_else[64], label_end[64];

        sprintf(jump_to_else, "if False %s goto %s", cond_addr, L_else);
        sprintf(goto_end, "goto %s", L_end);
        sprintf(label_else, "%s:", L_else);
        sprintf(label_end, "%s:", L_end);
	
        $$ = concatCode(7,
            cond_code,       
            jump_to_else,    
            $5,              
            goto_end,       
            label_else,      
            $7,              
            label_end        
        );
 
    }


;

    
for_statement
    : FOR LPAREN for_init_opt SEMICOLON for_condition_opt SEMICOLON for_update_opt RPAREN push_loop push_loop_sec push_loop_thr block 
    {
    
        char* L_cond =$9 ;
       
        char* L_update = $10;
        char* L_end = $11;
        char label_cond[64], label_update[64], label_end[64];
        char jump_to_end[128], goto_cond[64];

        sprintf(label_cond, "%s:", L_cond);
        sprintf(label_update, "%s:", L_update);
        sprintf(label_end, "%s:", L_end);
        sprintf(jump_to_end, "if False %s goto %s", $5->addr, L_end); 
        sprintf(goto_cond, "goto %s", L_cond);
        $$ = concatCode(9,
            $3,            
            label_cond,    
            $5->code,            
            jump_to_end,   
            $12,            
            label_update,  
            $7,           
            goto_cond,     
            label_end      
        );
        popJumpLabel();

    }
;
push_loop
  : /* 空 */ {  
  	$$ = newLabel();

  }
;
 push_loop_sec
  : /* 空 */ {  
  	$$ = newLabel();

  }
;
push_loop_thr
  : /* 空 */ {  
  	$$ = newLabel();
  	pushJumpLabel(labelCount );
  }
;
for_init_opt
    : /* 空 */
      {
          $$ = strdup(""); 
      }
    | type assignment_expression
      {
          $$ = $2->code;
      }
    | assignment_expression
      {
          $$ = $1->code; 
      }

    ;

for_condition_opt
    : /* 空 */
      {
           $$ = NULL;
      }
    | expression {
	$$ = $1;
	 
	}

    ;

for_update_opt
    : /* 空 */
      {
          $$ = strdup(""); 
      }
    | assignment_expression
      {
          $$ = $1->code; 
      }

    ;

method_call_statement
    : variable LPAREN argument_list_opt RPAREN SEMICOLON {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = newTemp();
        char codeBuf[2048] = "";

        int paramCount = 0;

        if ($3 && strlen($3) > 0) {
            for (char* p = $3; *p; p++) {
                if (strncmp(p, "param", 5) == 0) paramCount++;
            }
            strcat(codeBuf, $3);
        }
        char callLine[128];
        snprintf(callLine, sizeof(callLine), "call %s, %d;", $1->addr, paramCount);
        strcat(codeBuf, callLine);
        e->code = strdup(codeBuf);

        $$ = e;
    }
    ;
    
argument_list_opt
    : argument_list      { $$ = $1; }
    ;

argument_list
    : expression {
        int len = strlen("param ") + strlen($1->addr) + strlen(";\n") + 1;
        char* buf = malloc(len);
        snprintf(buf, len, "param %s;\n", $1->addr);

        char* paramLine = concatCode(2, $1->code ? $1->code : "", buf);
        $$ = paramLine;

        free(buf);
        free($1->addr);
        free($1->code);
        free($1);
    }
    | argument_list COMMA expression {
        int len = strlen("param ") + strlen($3->addr) + strlen(";\n") + 1;
        char* buf = malloc(len);
        snprintf(buf, len, "param %s;\n", $3->addr);

        char* paramLine = concatCode(2, $3->code ? $3->code : "", buf);
        char* combined = concatCode(2, $1, paramLine);

        $$ = combined;

        free(buf);
        free(paramLine);
        free($1);
        free($3->addr);
        free($3->code);
        free($3);
    }
    ;
modifier_list
    : modifier { $$ = $1; }
    | modifier_list modifier {
        $$ = concatCode(3, $1, " ", $2);
        free($1);
        free($2);
    }
    ;

modifier
    : STATIC { $$ = strdup("static"); }
    ;

expression
    : logical_expression {
        $$ = $1;

    }
    ;
assignment_expression
    : variable ASSING  expression {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = strdup($1->addr);

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s;", $1->addr, $3->addr);

        e->code = concatCode(2, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);

        $$ = e;
    }
    | increment_expression {
        $$ = $1;
    }
    | decrement_expression {
        $$ = $1;
    }
    ;

logical_expression
    : relational_expression {
        $$ = $1;
    }
    | relational_expression EQ relational_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s == %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);

        $$ = e;
    }
    | relational_expression NEQ relational_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s != %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);

        $$ = e;
    }
    ;
relational_expression
    : additive_expression {
        $$ = $1;
    }
    | additive_expression LT additive_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s < %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);
        $$ = e;
    }
    | additive_expression LE additive_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s <= %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);
        $$ = e;
    }
    | additive_expression GT additive_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s > %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);
        $$ = e;
    }
    | additive_expression GE additive_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s >= %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);
        $$ = e;
    }
    ;



additive_expression
    : multiplicative_expression {
        $$ = $1;
    }
    | additive_expression PLUS multiplicative_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s + %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);

        $$ = e;
    }
    | additive_expression MINUS multiplicative_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s - %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);

        $$ = e;
    }
    ;


multiplicative_expression
    : primary_expression {
        $$ = $1;  
    }
    | multiplicative_expression STAR primary_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s * %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);

        $$ = e;
    }
    | multiplicative_expression SLASH primary_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();

        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s / %s;", e->addr, $1->addr, $3->addr);

        e->code = concatCode(3, $1->code, $3->code, buf);

        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);

        $$ = e;
    }
    | multiplicative_expression MOD primary_expression {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        e->addr = newTemp();
        char buf[256];
	snprintf(buf, sizeof(buf), "%s = %s %c %s;",e->addr, $1->addr, '%',  $3->addr );
        e->code = concatCode(3, $1->code, $3->code, buf);
        free($1->addr); free($1->code); free($1);
        free($3->addr); free($3->code); free($3);

        $$ = e;
    }
    ;


increment_expression
    : variable INCREMENT {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        char* orig = $1->addr;
        char* temp = newTemp();
        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s + 1;\n%s = %s;", temp, orig, orig, temp);

        e->addr = strdup(orig);
        e->code = concatCode(2, $1->code, buf);

        free($1->addr);
        free($1->code);
        free($1);
        $$ = e;
    }
    | INCREMENT variable {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        char* orig = $2->addr;
        char* temp = newTemp();
        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s + 1;\n%s = %s;", temp, orig, orig, temp);
        e->addr = strdup(temp); 
        e->code = concatCode(2, $2->code, buf);
        free($2->addr);
        free($2->code);
        free($2);
        $$ = e;
    }
    ;


decrement_expression
    : variable DECREMENT {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        char* orig = $1->addr;
        char* temp = newTemp();
        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s - 1;\n%s = %s;", temp, orig, orig, temp);
        e->addr = strdup(orig);
        e->code = concatCode(2, $1->code, buf);
        free($1->addr);
        free($1->code);
        free($1);
        $$ = e;
    }
    | DECREMENT variable {
        struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
        char* orig = $2->addr;
        char* temp = newTemp();
        char buf[256];
        snprintf(buf, sizeof(buf), "%s = %s - 1;\n%s = %s;", temp, orig, orig, temp);
        e->addr = strdup(temp);  
        e->code = concatCode(2, $2->code, buf);
        free($2->addr);
        free($2->code);
        free($2);
        $$ = e;
    }
    ;
primary_expression
    : method_call_statement {
        $$ = $1;  
    }
    | variable {
        $$ = $1;  
    }
    | literal {
        $$ = $1; 
    }
    | LPAREN expression RPAREN {
      struct exprType* e = (struct exprType*)malloc(sizeof(struct exprType));
      e->addr = newTemp();
      char buffer[256];
      snprintf(buffer, sizeof(buffer), "%s = (%s);", e->addr, $2->addr);
      e->code = strdup(buffer);
      char* mergedCode = concatCode(2, $2->code, e->code);
      free(e->code);
      e->code = mergedCode;

      free($2->addr);
      free($2->code);
      free($2);

      $$ = e;
     }
     ;


literal
    : INT_LITERAL {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = newTemp();
        
        char buf[128];
        sprintf(buf, "%s = %s;", e->addr, $1);  
        e->code = strdup(buf);

        $$ = e;
    }
    | FLOAT_LITERAL {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = newTemp();
        
        char buf[128];
        sprintf(buf, "%s = %s;", e->addr, $1);
        e->code = strdup(buf);

        $$ = e;
    }
    | STRING_LITERAL {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = newTemp();
        
        char buf[256];
        sprintf(buf, "%s = %s;", e->addr, $1);  
        e->code = strdup(buf);

        $$ = e;
    }
    | TRUE {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = newTemp();
        
        char buf[64];
        sprintf(buf, "%s = true;", e->addr);
        e->code = strdup(buf);

        $$ = e;
    }
    | FALSE {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = newTemp();
        
        char buf[64];
        sprintf(buf, "%s = false;", e->addr);
        e->code = strdup(buf);

        $$ = e;
    }
    ;

variable
    : IDENTIFIER {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = strdup($1);    
        e->code = strdup("");   
        $$ = e;
    }
    | variable DOT IDENTIFIER {
        struct exprType* e = malloc(sizeof(struct exprType));
        e->addr = (char*)malloc(strlen($1->addr) + strlen($3) + 2);
        sprintf(e->addr, "%s.%s", $1->addr, $3);
        e->code = strdup($1->code);  
        $$ = e;
        free($1->addr);
        free($1->code);
        free($1);
    }
    ;

type
    : INT    { $$ = strdup("int"); }
    | STRING { $$ = strdup("string"); }
    | BOOL   { $$ = strdup("bool"); }
    ;

%%



int main() {
    yyin = stdin;


  
    if ( yyparse() == 0 && hasError == 0) {
        printf("程式碼語法正確！\n");
    } else {

    }
   printf("code have %d line" , yylineno);
    return 0;
}
void yyerror(const char* s) {
    printf("第 %d 行：%s\n", yylineno, s);
}
