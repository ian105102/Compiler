%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "parser.tab.h"

extern YYSTYPE yylval;

%}

%option noyywrap

%%


"if"            return IF;
"else"          return ELSE;
"while"         return WHILE;
"return"        return RETURN;
"int"           return INT;
"string"        return STRING;
"float"         return FLOAT;
"using"         return USING;
"class"         return CLASS;

"static"        { return STATIC;}
"void"          { return VOID;}
"bool"          return BOOL;
"break"         return BREAK;
"false"         return FALSE;
"true"          return TRUE;
"for"           return FOR;



\"([^\\\"]|\\.)*\" {
    yylval.str = strdup(yytext);
    return STRING_LITERAL;
}


[0-9]+\.[0-9]+ {
    yylval.str = strdup(yytext);
    return FLOAT_LITERAL;
}

[0-9]+ {
    yylval.str = strdup(yytext); 
    return INT_LITERAL;
}


[a-zA-Z_][a-zA-Z0-9_]* {
    yylval.str = strdup(yytext);
   
    return IDENTIFIER;
}

"=="            return EQ;
"!="            return NEQ;
"<="            return LE;
">="            return GE;
"<"             return LT;
">"             return GT;

"||"            return OR;
"&&"            return AND;
"!"            return NOT;

"("             return LPAREN;
")"             return RPAREN;
"{"             return LBRACE;
"}"             return RBRACE;
"["             return LBRACKET;
"]"             return RBRACKET;


"="             { return ASSING;}
"+"          { return PLUS;}
"-"             return MINUS;
"*"             return STAR;
"/"             return SLASH;
"%"             return MOD;
"++"    	{ return INCREMENT; }
"--"    	{ return DECREMENT; }  


";"             return SEMICOLON;
","             return COMMA;
"."             return DOT;
":"             return COLON;
"?"             return QUESTION;


[ \t\r]+        ;


\n              {  yylineno++; }


. {
    printf("Unknown token: %s\n", yytext);
    return UNKNOWN;
}
%%

