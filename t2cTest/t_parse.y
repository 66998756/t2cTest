%{
	#include <stdio.h>
	#include <stdlib.h>
	#include "t2c.h"
	#include "t_parse.h"
%}

%token lWRITE lREAD lIF lASSIGN
%token lRETURN lBEGIN lEND
%left  lEQU lNEQ lGT lLT lGE lLE
%left  lADD lMINUS
%left  lTIMES lDIVIDE
%token lLP lRP
%token lINT lREAL lSTRING
%token lELSE
%token lMAIN
%token lSEMI lCOMMA
%token lID lINUM lRNUM lQSTR

%expect 1

%%
prog	:	mthdcls
		{ printf("Program -> MethodDecls\n");
		  printf("Parsed OK!\n"); }
	|
		{ printf("****** Parsing failed!\n"); }	
	;

mthdcls	:	mthdcl mthdcls
		{ printf("MethodDecls -> MethodDecl MethodDecls\n"); }	
	|	mthdcl
		{ printf("MethodDecls -> MethodDecl\n"); }	
	;

type	:	lINT
		{ printf("Type -> INT\n"); }	
	|	lREAL
		{ printf("Type -> REAL\n"); }	
	;

mthdcl	:	type lMAIN lID lLP formals lRP block
		{ printf("MethodDecl -> Type MAIN ID LP Formals RP Block\n"); }	
	|	type lID lLP formals lRP block
		{ printf("MethodDecl -> Type ID LP Formals RP Block\n"); }	
	;

formals	:	formal oformal
		{ printf("Formals -> Formal OtherFormals\n"); }	
	|
		{ printf("Formals -> \n"); }	
	;

formal	:	type lID
		{ printf("Formal -> Type ID\n"); }	
	;

oformal	:	lCOMMA formal oformal
		{ printf("OtherFormals -> COMMA Formal OtherFormals\n"); }	
	|
		{ printf("OtherFormals -> \n"); }	
	;

block	:   	lBEGIN Statement oStatement lEND
        	{ printf("In Block"); }
    	;

oStatement	:	Statement oStatement
		{ printf("oStatement -> Statement oStatement\n"); }
	|
		{ printf("oStatement -> \n"); }
	;

Statement : 	block
        	{ printf("Statement -> Block\n"); }
	|	LocalVarDecl
		{ printf("Statement -> LocalVarDecl\n"); }
	|	AssignStmt
		{ printf("Statement -> AssignStmt\n"); }
	|	ReturnStmt
		{ printf("Statement -> ReturnStmt\n"); }
	|	IfStmt
		{ printf("Statement -> IfStmt\n"); }
	|	WriteStmt
		{ printf("Statement -> WriteStmt\n"); }
	|	ReadStmt
		{ printf("Statement -> ReadStmt\n"); }
	;

LocalVarDecl	:	type lID lSEMI
		{ printf("LocalVarDecl -> type ID ';'\n"); }
	|	type AssignStmt
		{ printf("LocalVarDecl -> type AssignStmt\n"); }
	;

AssignStmt	:	lID lASSIGN Expr lSEMI
		{ printf("AssignStmt -> ID := Expression ';'\n"); }
	;

ReturnStmt	:	lRETURN Expr lSEMI
	{ printf("ReturnStmt -> RETURN Expression ';'\n"); }
	;

IfStmt	:	lIF lLP BoolExpr lRP Statement
		{ printf("IfStmt -> IF '(' BoolExpr ')' Statement\n"); }
	|	lIF lLP BoolExpr lRP Statement lELSE Statement
		{ printf("IfStmt -> IF '(' BoolExpr ')' Statement ELSE Statement\n"); }
	;

WriteStmt	:	lWRITE lLP Expr lCOMMA lQSTR lRP lSEMI
		{ printf("WriteStmt -> WRITE '(' Expression ',' QString ')' ';'\n"); }
	;

ReadStmt	:	lREAD lLP lID lCOMMA lQSTR lRP lSEMI
		{ printf("ReadStmt -> READ '(' Id ',' QString ')' ';'\n"); }
	;

Expr	:	MultiplicativeExpr oMultiplicativeExpr
		{ printf("Expr -> '+' Multiplicative oMultiplicativeExpr\n"); }
	;

oMultiplicativeExpr	:	lADD MultiplicativeExpr oMultiplicativeExpr
		{ printf("oMultiplicativeExpr -> + MultiplicativeExpr oMultiplicativeExpr\n"); }
	|	lMINUS MultiplicativeExpr oMultiplicativeExpr
		{ printf("oMultiplicativeExpr -> - MultiplicativeExpr oMultiplicativeExpr\n"); }
	|
		{ printf("oMultiplicativeExpr -> \n"); }
	;

MultiplicativeExpr	:	PrimaryExpr oPrimaryExpr
		{ printf("MultiplicativeExpr -> PrimaryExpr oPrimaryExpr\n"); }
	;

oPrimaryExpr	:	lTIMES PrimaryExpr oPrimaryExpr
		{ printf("oPrivaryExpr -> '*' PrimaryExpr oPrimaryExpr\n"); }
	|	lDIVIDE PrimaryExpr oPrimaryExpr
		{ printf("oPrivaryExpr -> '/' PrimaryExpr oPrimaryExpr\n"); }
	|
		{ printf("oPrivaryExpr -> \n"); }
	;

PrimaryExpr	:	Num
		{ printf("PrimaryExpr -> Num\n"); }
	|	lID
		{ printf("PrimaryExpr -> ID\n"); }
	|	lLP Expr lRP
		{ printf("PrimaryExpr -> '(' Expr ')'\n"); }
	|	lID lLP ActualParams lRP
		{ printf("PrimaryExpr -> ID '(' ActualParams ')'\n"); }
	;

Num	:	lINUM
		{ printf("Num -> INT\n"); }
	|	lRNUM
		{ printf("Num -> REAL\n"); }
	;

ActualParams	:	Expr comExpr
		{ printf("ActualParams -> Expr comExpr\n"); }
	|
		{ printf("ActualParams -> \n"); }
	;

comExpr	:	lCOMMA Expr comExpr
		{ printf("comExpr -> ',' Expr comExpr\n"); }
	|
		{ printf("comExpr -> \n"); }
	;

BoolExpr	:	Expr lEQU Expr
		{ printf("BoolExpr -> EXPR '==' Expr\n"); }
	|	Expr lNEQ Expr
		{ printf("BoolExpr -> EXPR '!=' Expr\n"); }
	|	Expr lGT Expr
		{ printf("BoolExpr -> EXPR '>' Expr\n"); }
	|	Expr lGE Expr
		{ printf("BoolExpr -> EXPR '>=' Expr\n"); }
	|	Expr lLT Expr
		{ printf("BoolExpr -> EXPR '<' Expr\n"); }
	|	Expr lLE Expr
		{ printf("BoolExpr -> EXPR '<=' Expr\n"); }
	;

%%

int yyerror(char *s)
{
	printf("%s\n",s);
	return 1;
}

