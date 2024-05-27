//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Build with "make compilateur"

#include <string>	   // On inclut les chaînes de caractères
#include <iostream>	   // On inclut les flux d'entrée/sortie
#include <cstdlib>	   // On inclut la bibliothèque standard
#include <set>		   // On inclut les ensembles
#include <map>		   // On inclut les maps
#include <FlexLexer.h> // On inclut le lexer
#include "tokeniser.h" // On inclut le tokeniser
#include <cstring>	   // On inclut les chaînes de caractères (variables globales)

using namespace std;

/***********************************************************************
 *  Enumérations et variables globales
 *  On stocke des opérateurs et des types dans des énumérations pour pouvoir les comparer facilement
 ***********************************************************************/
enum OPREL // Enumération des opérateurs relationnels
{
	EQU,  // égalité
	DIFF, // différence
	INF,  // inférieur
	SUP,  // supérieur
	INFE, // inférieur ou égal
	SUPE, // supérieur ou égal
	WTFR  // wtf
};

enum OPADD // Enumération des opérateurs additifs
{
	ADD, // addition
	SUB, // soustraction
	OR,	 // ou
	WTFA // wtf
};

enum OPMUL // Enumération des opérateurs multiplicatifs
{
	MUL, // multiplication
	DIV, // division
	MOD, // modulo
	AND, // et
	WTFM // wtf
};

enum TYPES // Enumération des types de variables
{
	INTEGER, // entier
	BOOLEAN, // booléen
	CHAR,	 // caractère
	DOUBLE	 // réel
};
/* ***********************************************************************/

/***********************************************************************
 *  Variables globales
 ***********************************************************************/
TOKEN current;							   // On stocke le token courant dans cette variable globale pour pouvoir le modifier dans les fonctions
map<string, enum TYPES> DeclaredVariables; // On stocke les variables déclarées et leurs types dans cette map pour pouvoir les retrouver facilement
unsigned long long TagNumber = 0;		   // On stocke le nombre de tags pour pouvoir en générer des uniques à chaque fois
/* ***********************************************************************/

/***********************************************************************
 *  Lexer
 ***********************************************************************/
FlexLexer *lexer = new yyFlexLexer; // on declare un tokeniser qui va lire les tokens dans le flux d'entrée
// les tokens sont lus en utilisant lexer->yylex()
// lexer->yylex() renvoie le type de l'entrée lexicale (voir enum TOKEN dans tokeniser.h)
// et lexer->YYText() renvoie l'entrée lexicale sous forme de string
/* ***********************************************************************/

/***********************************************************************
 *  Fonctions et procédures
 ***********************************************************************/

/*
 * IsDeclared() : Fonction qui vérifie si une variable est déclarée
 * @param id : le nom de la variable
 * @return : true si la variable est déclarée, false sinon
 */
bool IsDeclared(const char *id)
{
	return DeclaredVariables.find(id) != DeclaredVariables.end();
}

/*
 * Error() : Fonction qui affiche une erreur et quitte le programme
 * @param s : le message d'erreur
 */
void Error(string s)
{
	cerr << "Ligne n°" << lexer->lineno() << ", lu : '" << lexer->YYText() << "'(" << current << "), mais ";
	cerr << s << endl;
	exit(-1);
}

// Prototypes des fonctions :
enum TYPES Expression(void);
void Statement(void);
void StatementPart(void);

/*
 * Identifier() : Fonction qui gère les identifiants
 * @return : le type de l'identifiant
 */
enum TYPES Identifier(void)
{
	enum TYPES type;
	if (!IsDeclared(lexer->YYText()))
	{
		cerr << "Erreur : Variable '" << lexer->YYText() << "' non déclarée" << endl;
		exit(-1);
	}
	type = DeclaredVariables[lexer->YYText()];
	cout << "\tpush " << lexer->YYText() << endl;
	current = (TOKEN)lexer->yylex();
	return type;
}

/*
 * Number() : Fonction qui gère les nombres
 * @return : le type du nombre
 */
enum TYPES Number(void)
{
	bool is_a_decimal = false;
	double d;		 // 64-bit float
	unsigned int *i; // Pointeur vers un entier non signé de 32 bits
	string number = lexer->YYText();

	if (number.find(".") != string::npos)
	{
		// Nombre a virgule flottante
		d = atof(lexer->YYText());
		i = (unsigned int *)&d; // i pointe vers la constante double d
		// cout <<"\tpush $"<<*i<<"\t# Conversion of "<<d<<endl;

		//  Equivalent a :
		cout << "\tsubq $8,%rsp\t\t\t# allocate 8 bytes on stack's top" << endl;
		cout << "\tmovl	$" << *i << ", (%rsp)\t# Conversion of " << d << " (32 bit high part)" << endl;
		cout << "\tmovl	$" << *(i + 1) << ", 4(%rsp)\t# Conversion of " << d << " (32 bit low part)" << endl;
		current = (TOKEN)lexer->yylex();

		return DOUBLE;
	}
	else
	{
		// Constante entière
		cout << "\tpush $" << atoi(lexer->YYText()) << endl;
		current = (TOKEN)lexer->yylex();
		return INTEGER;
	}
}

/*
 * CharConst() : Fonction qui gère les caractères
 * @return : le type du caractère
 */
enum TYPES CharConst(void)
{
	cout << "\tmovq $0, %rax" << endl;
	cout << "\tmovb $" << lexer->YYText() << ",%al" << endl;
	cout << "\tpush %rax\t# push a 64-bit version of " << lexer->YYText() << endl;

	current = (TOKEN)lexer->yylex();

	return CHAR;
}

/*
 * Factor() : Fonction qui gère les facteurs
 * @return : le type du facteur
 */
enum TYPES Factor(void)
{
	enum TYPES type;

	switch (current)
	{
	case RPARENT:
		current = (TOKEN)lexer->yylex();
		type = Expression();
		if (current != LPARENT)
		{
			Error("Factor : ')' attendue."); // On vérifie que l'expression est bien fermée
		}
		else
		{
			current = (TOKEN)lexer->yylex();
		}
		break;
	case NUMBER:
		type = Number();
		break;
	case ID:
		type = Identifier();
		break;
	case CHARCONST:
		type = CharConst();
		break;
	default:
		Error("Factor : '(', ou constante ou variable attendue.");
	};

	return type;
}

/*
 * MultiplicativeOperator() : Fonction qui traite un operateur multiplicatif
 * @return : le type de l'operateur multiplicatif
 * Grammaire : MultiplicativeOperator := "*" | "/" | "%" | "&&"
 * Explication : un * ou / ou % ou &&
 */
OPMUL MultiplicativeOperator(void)
{
	OPMUL opmul;

	if (strcmp(lexer->YYText(), "*") == 0)
	{
		opmul = MUL;
	}
	else if (strcmp(lexer->YYText(), "/") == 0)
	{
		opmul = DIV;
	}
	else if (strcmp(lexer->YYText(), "%") == 0)
	{
		opmul = MOD;
	}
	else if (strcmp(lexer->YYText(), "&&") == 0)
	{
		opmul = AND;
	}
	else
	{
		opmul = WTFM;
	}

	current = (TOKEN)lexer->yylex();

	return opmul;
}

/*
 * Term() : Fonction qui gère les termes
 * @return : le type du terme
 * Grammaire : Term := Factor {MultiplicativeOperator Factor}
 * Explication : un facteur suivi de 0 ou plusieurs opérateurs multiplicatifs suivis de facteurs
 */
enum TYPES Term(void)
{
	TYPES type1, type2;
	OPMUL mulop;
	type1 = Factor();
	while (current == MULOP)
	{
		mulop = MultiplicativeOperator(); // Save operator in local variable
		type2 = Factor();
		if (type2 != type1)
		{
			Error(" Term : types incompatibles dans l'expression");
		}
		switch (mulop)
		{
		case AND:
			if (type2 != BOOLEAN)
			{
				Error(" Term (case): type non booléen pour le ET logique");
			}
			cout << "\tpop %rbx" << endl;		  // get first operand
			cout << "\tpop %rax" << endl;		  // get second operand
			cout << "\tmulq	%rbx" << endl;		  // a * b -> %rdx:%rax
			cout << "\tpush %rax\t# AND" << endl; // store result
			break;
		case MUL:
			if (type2 != INTEGER && type2 != DOUBLE)
			{
				Error(" Term (case-n2): type non numérique pour la multiplication");
			}
			if (type2 == INTEGER)
			{
				cout << "\tpop %rbx" << endl;		  // get first operand
				cout << "\tpop %rax" << endl;		  // get second operand
				cout << "\tmulq	%rbx" << endl;		  // a * b -> %rdx:%rax
				cout << "\tpush %rax\t# MUL" << endl; // store result
			}
			else
			{
				cout << "\tfldl	8(%rsp)\t" << endl;
				cout << "\tfldl	(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)" << endl;
				cout << "\tfmulp	%st(0),%st(1)\t# %st(0) <- op1 + op2 ; %st(1)=null" << endl;
				cout << "\tfstpl 8(%rsp)" << endl;
				cout << "\taddq	$8, %rsp\t# result on stack's top" << endl;
			}
			break;
		case DIV:
			if (type2 != INTEGER && type2 != DOUBLE)
			{
				Error(" Term (case-n3): type non numérique pour la division");
			}
			if (type2 == INTEGER)
			{
				cout << "\tpop %rbx" << endl;		  // get first operand
				cout << "\tpop %rax" << endl;		  // get second operand
				cout << "\tmovq $0, %rdx" << endl;	  // Higher part of numerator
				cout << "\tdiv %rbx" << endl;		  // quotient goes to %rax
				cout << "\tpush %rax\t# DIV" << endl; // store result
			}
			else
			{
				cout << "\tfldl	(%rsp)\t" << endl;
				cout << "\tfldl	8(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)" << endl;
				cout << "\tfdivp	%st(0),%st(1)\t# %st(0) <- op1 + op2 ; %st(1)=null" << endl;
				cout << "\tfstpl 8(%rsp)" << endl;
				cout << "\taddq	$8, %rsp\t# result on stack's top" << endl;
			}
			break;
		case MOD:
			if (type2 != INTEGER)
			{
				Error(" Term (case-n4): type non numérique pour le modulo");
			}
			cout << "\tmovq $0, %rdx" << endl;	  // Higher part of numerator
			cout << "\tdiv %rbx" << endl;		  // remainder goes to %rdx
			cout << "\tpush %rdx\t# MOD" << endl; // store result
			break;
		default:
			Error(" Term (case-default): opérateur multiplicatif non reconnu");
		}
	}
	return type1;
}

/*
 * AdditiveOperator() : Fonction qui traite un operateur additif
 * @return : le type de l'operateur additif
 * Grammaire : AdditiveOperator := "+" | "-" | "||"
 * Explication : un + ou - ou ||
 */
OPADD AdditiveOperator(void)
{
	OPADD opadd;
	if (strcmp(lexer->YYText(), "+") == 0)
	{
		opadd = ADD;
	}
	else if (strcmp(lexer->YYText(), "-") == 0)
	{
		opadd = SUB;
	}
	else if (strcmp(lexer->YYText(), "||") == 0)
	{
		opadd = OR;
	}
	else
	{
		opadd = WTFA;
	}

	current = (TOKEN)lexer->yylex();

	return opadd;
}

/*
 * SimpleExpression() : Fonction qui gère les expressions simples
 * @return : le type de l'expression simple
 * Grammaire : SimpleExpression := Term {AdditiveOperator Term}
 * Explication : un terme suivi de 0 ou plusieurs opérateurs additifs suivis de termes
 */
enum TYPES SimpleExpression(void)
{
	enum TYPES type1, type2;
	OPADD adop;
	type1 = Term();

	while (current == ADDOP)
	{
		adop = AdditiveOperator(); // Save operator in local variable
		type2 = Term();
		if (type2 != type1)
		{
			Error("SimpleExpression : types incompatibles dans l'expression");
		}
		switch (adop)
		{
		case OR:
			if (type2 != BOOLEAN)
			{
				Error("SimpleExpression : opérande non booléenne pour l'opérateur OR");
			}

			cout << "\tpop %rbx" << endl;			   // get first operand
			cout << "\tpop %rax" << endl;			   // get second operand
			cout << "\torq	%rbx, %rax\t# OR" << endl; // operand1 OR operand2
			cout << "\tpush %rax" << endl;			   // store result
			break;
		case ADD:
			if (type2 != INTEGER && type2 != DOUBLE)
			{
				Error("SimpleExpression : opérande non numérique pour l'addition");
			}
			if (type2 == INTEGER)
			{
				cout << "\tpop %rbx" << endl;				// get first operand
				cout << "\tpop %rax" << endl;				// get second operand
				cout << "\taddq	%rbx, %rax\t# ADD" << endl; // add both operands
				cout << "\tpush %rax" << endl;				// store result
			}
			else
			{
				cout << "\tfldl	8(%rsp)\t" << endl;
				cout << "\tfldl	(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)" << endl;
				cout << "\tfaddp	%st(0),%st(1)\t# %st(0) <- op1 + op2 ; %st(1)=null" << endl;
				cout << "\tfstpl 8(%rsp)" << endl;
				cout << "\taddq	$8, %rsp\t# result on stack's top" << endl;
			}
			break;
		case SUB:
			if (type2 != INTEGER && type2 != DOUBLE)
			{
				Error("SimpleExpression : opérande non numérique pour la soustraction");
			}
			if (type2 == INTEGER)
			{
				cout << "\tpop %rbx" << endl;				// get first operand
				cout << "\tpop %rax" << endl;				// get second operand
				cout << "\tsubq	%rbx, %rax\t# ADD" << endl; // add both operands
				cout << "\tpush %rax" << endl;				// store result
			}
			else
			{
				cout << "\tfldl	(%rsp)\t" << endl;
				cout << "\tfldl	8(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)" << endl;
				cout << "\tfsubp	%st(0),%st(1)\t# %st(0) <- op1 - op2 ; %st(1)=null" << endl;
				cout << "\tfstpl 8(%rsp)" << endl;
				cout << "\taddq	$8, %rsp\t# result on stack's top" << endl;
			}
			break;
		default:
			Error("SimpleExpression : opérateur additif inconnu");
		}
	}
	return type1;
}

/*
 * Type() : Fonction qui gère les types
 * @return : le type du type
 * Grammaire : Type := "BOOLEAN" | "INTEGER" | "DOUBLE" | "CHAR"
 * Explication : un type parmi les 4 types possibles
 */
enum TYPES Type(void)
{
	if (current != KEYWORD)
	{
		Error("Type (if): type attendu");
	}
	if (strcmp(lexer->YYText(), "BOOLEAN") == 0)
	{
		current = (TOKEN)lexer->yylex();
		return BOOLEAN;
	}
	else if (strcmp(lexer->YYText(), "INTEGER") == 0)
	{
		current = (TOKEN)lexer->yylex();
		return INTEGER;
	}
	else if (strcmp(lexer->YYText(), "DOUBLE") == 0)
	{
		current = (TOKEN)lexer->yylex();
		return DOUBLE;
	}
	else if (strcmp(lexer->YYText(), "CHAR") == 0)
	{
		current = (TOKEN)lexer->yylex();
		return CHAR;
	}
	else
	{
		Error("Type (else): type inconnu");
	}
}

/***********************************************************************
 *  TP 6
 ***********************************************************************/
/*
 * VarDeclaration() : Fonction qui gère les déclarations de variables
 * @return : void
 * Grammaire : VarDeclaration := Ident {"," Ident} ":" Type
 * Explication : une déclaration de variable est composée d'un identificateur, suivi de 0 ou plusieurs identificateurs séparés par des virgules, suivi de deux points, suivi d'un type
 */
void VarDeclaration(void)
{
	set<string> idents;
	enum TYPES type;

	if (current != ID)
	{
		Error("VarDeclaration (if-n1): Un identificater était attendu");
	}

	idents.insert(lexer->YYText());
	current = (TOKEN)lexer->yylex();

	while (current == COMMA)
	{
		current = (TOKEN)lexer->yylex();
		if (current != ID)
			Error("VarDeclaration : Un identificateur était attendu");
		idents.insert(lexer->YYText());
		current = (TOKEN)lexer->yylex();
	}

	if (current != COLON)
	{
		Error("VarDeclaration (if-n2): ':' était attendu");
	}

	current = (TOKEN)lexer->yylex();
	type = Type();

	for (set<string>::iterator it = idents.begin(); it != idents.end(); ++it)
	{
		switch (type)
		{
		case BOOLEAN:
		case INTEGER:
			cout << *it << ":\t.quad 0" << endl;
			break;
		case DOUBLE:
			cout << *it << ":\t.double 0.0" << endl;
			break;
		case CHAR:
			cout << *it << ":\t.byte 0" << endl;
			break;
		default:
			Error("VarDeclaration : type inconnu.");
		};

		DeclaredVariables[*it] = type;
	}
}

/*
 * VarDeclarationPart() : Fonction qui gère les parties de déclarations de variables
 * @return : void
 * Grammaire : VarDeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
 * Explication : une partie de déclaration de variable est composée du mot clé "VAR", suivi d'une déclaration de variable, suivi de 0 ou plusieurs déclarations de variables séparées par des points virgules, suivi d'un point
 */
void VarDeclarationPart(void)
{
	current = (TOKEN)lexer->yylex();
	VarDeclaration();

	while (current == SEMICOLON)
	{
		current = (TOKEN)lexer->yylex();
		VarDeclaration();
	}

	if (current != DOT)
	{
		Error("VarDeclarationPart : '.' attendu");
	}

	current = (TOKEN)lexer->yylex();
}
/* ***********************************************************************/

/*
 * RelationalOperator() : Fonction qui gère les opérateurs relationnels
 * @return : l'opérateur relationnel
 * Grammaire : RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="
 * Explication : un opérateur relationnel parmi les 6 opérateurs possibles
 */
OPREL RelationalOperator(void)
{
	OPREL oprel;
	if (strcmp(lexer->YYText(), "==") == 0)
	{
		oprel = EQU;
	}
	else if (strcmp(lexer->YYText(), "!=") == 0)
	{
		oprel = DIFF;
	}
	else if (strcmp(lexer->YYText(), "<") == 0)
	{
		oprel = INF;
	}
	else if (strcmp(lexer->YYText(), ">") == 0)
	{
		oprel = SUP;
	}
	else if (strcmp(lexer->YYText(), "<=") == 0)
	{
		oprel = INFE;
	}
	else if (strcmp(lexer->YYText(), ">=") == 0)
	{
		oprel = SUPE;
	}
	else
	{
		oprel = WTFR;
	}

	current = (TOKEN)lexer->yylex();

	return oprel;
}

/*
 * Expression() : Fonction qui gère les expressions
 * @return : le type de l'expression
 * Grammaire : Expression := SimpleExpression [RelationalOperator SimpleExpression]
 * Explication : une expression est composée d'une simple expression, suivie d'un opérateur relationnel, suivie d'une simple expression
 */
enum TYPES Expression(void)
{
	enum TYPES type1, type2;
	unsigned long long tag;
	OPREL oprel;
	type1 = SimpleExpression();

	if (current == RELOP)
	{
		tag = ++TagNumber;
		oprel = RelationalOperator();
		type2 = SimpleExpression();

		if (type2 != type1)
		{
			Error("Expression (if): les deux opérandes doivent être du même type");
		}

		if (type1 != DOUBLE)
		{
			cout << "\tpop %rax" << endl;
			cout << "\tpop %rbx" << endl;
			cout << "\tcmpq %rax, %rbx" << endl;
		}
		else
		{
			cout << "\tfldl	(%rsp)\t" << endl;
			cout << "\tfldl	8(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)" << endl;
			cout << "\t addq $16, %rsp\t# 2x pop nothing" << endl;
			cout << "\tfcomip %st(1)\t\t# compare op1 and op2 -> %RFLAGS and pop" << endl;
			cout << "\tfaddp %st(1)\t# pop nothing" << endl;
		}

		switch (oprel)
		{
		case EQU:
			cout << "\tje Vrai" << tag << "\t# If equal" << endl;
			break;
		case DIFF:
			cout << "\tjne Vrai" << tag << "\t# If different" << endl;
			break;
		case SUPE:
			cout << "\tjae Vrai" << tag << "\t# If above or equal" << endl;
			break;
		case INFE:
			cout << "\tjbe Vrai" << tag << "\t# If below or equal" << endl;
			break;
		case INF:
			cout << "\tjb Vrai" << tag << "\t# If below" << endl;
			break;
		case SUP:
			cout << "\tja Vrai" << tag << "\t# If above" << endl;
			break;
		default:
			Error("Expression (case-default) : opérateur relationnel inconnu");
		}

		cout << "\tpush $0\t\t# False" << endl;
		cout << "\tjmp Suite" << tag << endl;
		cout << "Vrai" << tag << ":\tpush $0xFFFFFFFFFFFFFFFF\t\t# True" << endl;
		cout << "Suite" << tag << ":" << endl;

		return BOOLEAN;
	}

	return type1;
}

/*
 * AssignementStatement() : Fonction qui gère les affectations
 * @return : void
 * Grammaire : AssignementStatement := Identifier ":=" Expression
 * Explication : une affectation est composée d'un identificateur, suivi de l'opérateur d'affectation, suivi d'une expression
 */
void AssignementStatement(void)
{
	enum TYPES type1, type2;
	string variable;

	if (current != ID)
	{
		Error("AssignementStatement (if-n1) : identificateur attendu");
	}

	if (!IsDeclared(lexer->YYText()))
	{
		cerr << "AssignementStatement : Erreur : Variable '" << lexer->YYText() << "' non déclarée" << endl;
		exit(-1);
	}

	variable = lexer->YYText();
	type1 = DeclaredVariables[variable];
	current = (TOKEN)lexer->yylex();

	if (current != ASSIGN)
	{
		Error("AssignementStatement (if-n2) : ':=' attendu");
	}

	current = (TOKEN)lexer->yylex();
	type2 = Expression();

	if (type2 != type1)
	{
		cerr << "Type variable " << type1 << endl;
		cerr << "Type Expression " << type2 << endl;
		Error("AssignementStatement (if-n3): types incompatibles dans l'affectation");
	}

	if (type1 == CHAR)
	{
		cout << "\tpop %rax" << endl;
		cout << "\tmovb %al," << variable << endl;
	}

	else
		cout << "\tpop " << variable << endl;
}

/*
 * DisplayStatement() : Fonction qui gère les affichages
 * @return : void
 * Grammaire : DisplayStatement := "DISPLAY" Expression
 * Explication : un affichage est composé du mot clé "DISPLAY", suivi d'une expression
 */
void DisplayStatement(void)
{
	enum TYPES type;
	unsigned long long tag = ++TagNumber;
	current = (TOKEN)lexer->yylex();
	type = Expression();

	switch (type)
	{
	case INTEGER:
		cout << "\tpop %rsi\t# The value to be displayed" << endl;
		cout << "\tmovq $FormatString1, %rdi\t# \"%llu\\n\"" << endl;
		cout << "\tmovl	$0, %eax" << endl;
		cout << "\tcall	printf@PLT" << endl;
		break;
	case BOOLEAN:
		cout << "\tpop %rdx\t# Zero : False, non-zero : true" << endl;
		cout << "\tcmpq $0, %rdx" << endl;
		cout << "\tje False" << tag << endl;
		cout << "\tmovq $TrueString, %rdi\t# \"TRUE\\n\"" << endl;
		cout << "\tjmp Next" << tag << endl;
		cout << "False" << tag << ":" << endl;
		cout << "\tmovq $FalseString, %rdi\t# \"FALSE\\n\"" << endl;
		cout << "Next" << tag << ":" << endl;
		cout << "\tcall	puts@PLT" << endl;
		break;
	case DOUBLE:
		cout << "\tmovsd	(%rsp), %xmm0\t\t# &stack top -> %xmm0" << endl;
		cout << "\tsubq	$16, %rsp\t\t# allocation for 3 additional doubles" << endl;
		cout << "\tmovsd %xmm0, 8(%rsp)" << endl;
		cout << "\tmovq $FormatString2, %rdi\t# \"%lf\\n\"" << endl;
		cout << "\tmovq	$1, %rax" << endl;
		cout << "\tcall	printf" << endl;
		cout << "nop" << endl;
		cout << "\taddq $24, %rsp\t\t\t# pop nothing" << endl;
		break;
	case CHAR:
		cout << "\tpop %rsi\t\t\t# get character in the 8 lowest bits of %si" << endl;
		cout << "\tmovq $FormatString3, %rdi\t# \"%c\\n\"" << endl;
		cout << "\tmovl	$0, %eax" << endl;
		cout << "\tcall	printf@PLT" << endl;
		break;
	default:
		Error("DisplayStatement (case-default) : DISPLAY ne fonctionne pas pour ce type de donnée.");
	}
}

/***********************************************************************
 *  TP 3
 ***********************************************************************/

/*
 * ForStatement() : Fonction qui gère les boucles for
 * @return : void
 * Grammaire : ForStatement := "For" ID ":=" Expression ("TO"|"DOWNTO") Expression "DO" Statement
 * Explication : une boucle for est composée du mot clé "FOR", suivi d'un identificateur, suivi de l'opérateur d'affectation, suivi d'une expression, suivi de l'opérateur "TO" ou "DOWNTO", suivi d'une expression, suivi du mot clé "DO", suivi d'une instruction
 */
void ForStatement(void)
{
	unsigned long long tag = TagNumber++; // On incrémente le numéro de tag donc on crée un nouveau tag
	cout << "For" << tag << ":" << endl;  // On affiche le tag de la boucle for
	current = (TOKEN)lexer->yylex();	  // On récupère le token suivant

	string VAR; // On déclare une variable de type string pour stocker le nom de la variable à affecter

	if (current != ID) // Si le token n'est pas un identificateur
	{
		Error("ForStatement : ID Attendu"); // Si ce n'est pas un identificateur, on affiche une erreur
	}

	cout << "\t# On récupère la valeur de départ de la boucle for et on la met dans la variable pour pouvoir la comparer avec la valeur de fin" << endl;
	VAR = lexer->YYText();	// On récupère le nom de la variable
	AssignementStatement(); // On appelle la fonction d'affectation pour affecter la valeur de départ à la boucle for

	if (current == KEYWORD && strcmp(lexer->YYText(), "TO") == 0) // On vérifie si le mot clé est un KEYWORD et si c'est bien TO
	{
		// Boucle FOR avec 'TO'

		current = (TOKEN)lexer->yylex(); // On récupère le token suivant
		if (Expression() != INTEGER)	 // On vérifie si l'expression est bien de type INTEGER (entier)
		{
			Error("ForStatement (TO) : INTEGER attendu dans l'expression");
		}

		cout << "FORBEGIN" << tag << ":" << endl; // On affiche le tag de début de boucle for
		cout << "pop %rcx"
			 << "\t# On récupère la valeur de fin de la boucle for et on la met dans le registre %rcx pour pouvoir la comparer avec la valeur de départ" << endl;
		cout << "cmp "
			 << "%rcx ," << VAR << "\t# On compare la valeur de départ avec la valeur de fin" << endl;
		cout << "push %rcx"
			 << "\t# On remet la valeur de fin dans la pile" << endl;
		cout << "jae ENDFOR" << tag << "\t# On saute à la fin de la boucle si la valeur de départ est supérieure ou égale à la valeur de fin" << endl;

		if (current != KEYWORD || strcmp(lexer->YYText(), "DO") != 0) // On vérifie si le token suivant est bien un KEYWORD et si c'est bien DO
		{
			Error("ForStatement (TO) : DO attendu"); // Si ce n'est pas le cas, on affiche une erreur
		}

		current = (TOKEN)lexer->yylex(); // On récupère le token suivant
		Statement();					 // On appelle la fonction Statement pour exécuter les instructions

		cout << "add $1, " << VAR << "\t# On incrémente la valeur de départ de la boucle for" << endl;
		cout << "jmp FORBEGIN" << tag << "\t# On retourne au début de la boucle for" << endl;
		cout << "ENDFOR" << tag << ":" << endl; // On affiche le tag de fin de boucle for
	}

	else if (current == KEYWORD && strcmp(lexer->YYText(), "DOWNTO") == 0)
	{
		// Boucle FOR avec 'DOWNTO'

		current = (TOKEN)lexer->yylex(); // On récupère le token suivant
		if (Expression() != INTEGER)	 // On vérifie si l'expression est bien de type INTEGER (entier)
		{
			Error("ForStatement (DOWNTO) : INTEGER attendu dans l'expression"); // Si ce n'est pas le cas, on affiche une erreur
		}

		cout << "FORBEGIN" << tag << ":" << endl; // On affiche le tag de début de boucle for
		cout << "pop %rcx" << endl;				  // On récupère la valeur de fin de la boucle for et on la met dans le registre %rcx pour pouvoir la comparer avec la valeur de départ
		cout << "cmp " << VAR << ", %rcx "
			 << " #\t On compare la valeur de départ avec la valeur de fin" << endl;
		cout << "push %rcx"
			 << "\t# On remet la valeur de fin dans la pile" << endl;
		cout << "jae ENDFOR" << tag << "\t# On saute à la fin de la boucle si la valeur de départ est supérieure ou égale à la valeur de fin" << endl;

		if (current != KEYWORD || strcmp(lexer->YYText(), "DO") != 0) // On vérifie si le token suivant est bien un KEYWORD et si c'est bien DO
		{
			Error("ForStatement (DOWNTO) : DO attendu"); // Si ce n'est pas le cas, on affiche une erreur
		}

		current = (TOKEN)lexer->yylex(); // On récupère le token suivant
		Statement();					 // On appelle la fonction Statement pour exécuter les instructions

		cout << "sub $1, " << VAR << "\t# On décrémente la valeur de départ de la boucle for" << endl;
		cout << "jmp FORBEGIN" << tag << endl;	// On retourne au début de la boucle for
		cout << "ENDFOR" << tag << ":" << endl; // On affiche le tag de fin de boucle for
	}

	else // Si le mot clé n'est pas TO ou DOWNTO
	{
		Error("ForStatement : DOWNTO ou TO attendu"); // On affiche une erreur
	}
}

/*
 * WhileStatement() : Fonction qui gère les boucles while
 * @return : void
 * Grammaire : WhileStatement := "WHILE" Expression "DO" Statement
 * Explication : une boucle while est composée du mot clé "WHILE", suivi d'une expression, suivi du mot clé "DO", suivi d'une instruction
 */
void WhileStatement(void)
{
	unsigned long long tag = TagNumber++; // On crée un tag pour la boucle

	cout << "While" << tag << ":" << endl; // On affiche le tag
	current = (TOKEN)lexer->yylex();	   // On récupère le token suivant

	if (Expression() != BOOLEAN) // On vérifie que l'expression est bien un booléen
	{
		Error("WhileStatement : expression booléene attendue"); // Sinon on affiche une erreur
	}

	cout << "\tpop %rax\t# Get the result of expression" << endl;						   // On récupère le résultat de l'expression
	cout << "\tcmpq $0, %rax" << endl;													   // On compare le résultat à 0
	cout << "\tje EndWhile" << tag << "\t# if FALSE, jump out of the loop" << tag << endl; // Si le résultat est égal à 0, on sort de la boucle

	if (current != KEYWORD || strcmp(lexer->YYText(), "DO") != 0) // On vérifie que le token suivant est bien le mot clé "DO"
	{
		Error("WhileStatement : mot-clé DO attendu"); // Sinon on affiche une erreur
	}

	current = (TOKEN)lexer->yylex(); // On récupère le token suivant
	Statement();					 // On traite l'instruction

	cout << "\tjmp While" << tag << endl;	  // On retourne au début de la boucle
	cout << "EndWhile" << tag << ":" << endl; // On affiche le tag de fin de boucle
}

/*
 * BlockStatement() : Fonction qui gère les blocs d'instructions
 * @return : void
 * Grammaire : BlockStatement := "BEGIN" Statement {";" Statement} "END"
 * Explication : un bloc d'instructions est composé du mot clé "BEGIN", suivi d'une instruction, suivi de zéro ou plusieurs instructions séparées par des points virgules, suivi du mot clé "END"
 */
void BlockStatement(void)
{
	current = (TOKEN)lexer->yylex(); // On récupère le token suivant
	Statement();					 // On traite l'instruction

	while (current == SEMICOLON) // Tant qu'on a des points virgules, on traite les instructions
	{
		current = (TOKEN)lexer->yylex(); // On récupère le token suivant
		Statement();					 // On traite l'instruction
	}

	if (current != KEYWORD || strcmp(lexer->YYText(), "END") != 0) // On vérifie que le token suivant est bien le mot clé "END"
	{
		Error("BlockStatement : mot-clé END attendu"); // Sinon on affiche une erreur
	}

	current = (TOKEN)lexer->yylex(); // On récupère le token suivant
}

/*
 * IfStatement() : Fonction qui gère les instructions conditionnelles
 * @return : void
 * Grammaire : IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]
 * Explication : une instruction conditionnelle est composée du mot clé "IF", suivi d'une expression, suivi du mot clé "THEN", suivi d'une instruction, suivi du mot clé "ELSE" et d'une instruction optionnelle
 */
void IfStatement(void)
{
	unsigned long long tag = TagNumber++; // On crée un tag pour la condition
	current = (TOKEN)lexer->yylex();	  // On récupère le token suivant

	if (Expression() != BOOLEAN) // On vérifie que l'expression est bien un booléen
	{
		Error("IfStatement : le type de l'expression doit être BOOLEAN"); // Sinon on affiche une erreur
	}

	cout << "\tpop %rax\t# Get the result of expression" << endl;			   // On récupère le résultat de l'expression
	cout << "\tcmpq $0, %rax" << endl;										   // On compare le résultat à 0
	cout << "\tje Else" << tag << "\t# if FALSE, jump to Else" << tag << endl; // Si le résultat est égal à 0, on saute à la condition else

	if (current != KEYWORD || strcmp(lexer->YYText(), "THEN") != 0) // On vérifie que le token suivant est bien le mot clé "THEN"
	{
		Error("IfStatement : mot-clé 'THEN' attendu"); // Sinon on affiche une erreur
	}

	current = (TOKEN)lexer->yylex(); // On récupère le token suivant
	Statement();					 // On traite l'instruction

	cout << "\tjmp Next" << tag << "\t# Do not execute the else statement" << endl; // On saute la condition else
	cout << "Else" << tag << ":" << endl;											// On affiche le tag de la condition else

	if (current == KEYWORD && strcmp(lexer->YYText(), "ELSE") == 0) // On verifie si le token suivant est le mot clé 'ELSE'
	{
		current = (TOKEN)lexer->yylex(); // On récupère le token suivant
		Statement();					 // On traite l'instruction
	}
	cout << "Next" << tag << ":" << endl; // On affiche le tag de fin de condition
}

/*
 * Statement() : Fonction qui gère les instructions
 * @return : void
 * Grammaire : Statement := AssignementStatement|DisplayStatement|....
 * Explication : une instruction est soit une assignation, soit un affichage, soit une boucle for, soit une boucle while, soit un bloc d'instructions
 */
void Statement(void)
{
	if (current == KEYWORD) // On vérifie que le token courant est un mot clé
	{
		if (strcmp(lexer->YYText(), "DISPLAY") == 0) // On vérifie que le mot clé est bien DISPLAY
		{
			DisplayStatement(); // On traite l'affichage
		}
		else if (strcmp(lexer->YYText(), "IF") == 0) // On vérifie que le mot clé est bien IF
		{
			IfStatement(); // On traite la condition
		}
		else if (strcmp(lexer->YYText(), "FOR") == 0) // On vérifie que le mot clé est bien FOR
		{
			ForStatement(); // On traite la boucle for
		}
		else if (strcmp(lexer->YYText(), "WHILE") == 0) // On vérifie que le mot clé est bien WHILE
		{
			WhileStatement(); // On traite la boucle while
		}
		else if (strcmp(lexer->YYText(), "BEGIN") == 0) // On vérifie que le mot clé est bien BEGIN
		{
			BlockStatement(); // On traite le bloc d'instructions
		}
		else // Sinon (mot clé inconnu)
		{
			Error("Statement : mot clé inconnu"); // On affiche une erreur
		}
	}
	else if (current == ID) // On vérifie que le token courant est un identifiant
	{
		AssignementStatement(); // On traite l'assignation
	}
	else // Sinon (instruction inconnue)
	{
		Error("Statement : instruction attendue"); // On affiche une erreur
	}
}
/* ***********************************************************************/

/*
 * StatementPart() : Fonction qui gère les parties d'instructions
 * @return : void
 * Grammaire : StatementPart := Statement {";" Statement} "."
 * Explication : une partie d'instructions est composée d'une instruction, suivi de zéro ou plusieurs instructions séparées par des points virgules, suivi d'un point
 */
void StatementPart(void)
{
	cout << "\t.align 8" << endl; // Alignement on addresses that are a multiple of 8 (64 bits = 8 bytes)
	cout << "\t.text\t\t# The following lines contain the program" << endl;
	cout << "\t.globl main\t# The main function must be visible from outside" << endl;
	cout << "main:\t\t\t# The main function body :" << endl;
	cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top" << endl;
	Statement();
	while (current == SEMICOLON)
	{
		current = (TOKEN)lexer->yylex();
		Statement();
	}
	if (current != DOT)
		Error("StatementPart : caractère '.' attendu");
	current = (TOKEN)lexer->yylex();
}

/*
 * Program() : Fonction qui gère les programmes
 * @return : void
 * Grammaire : Program := [VarDeclarationPart] StatementPart
 * Explication : un programme est composé d'une partie de déclaration de variables optionnelle, suivi d'une partie d'instructions
 */
void Program(void)
{
	if (current == KEYWORD && strcmp(lexer->YYText(), "VAR") == 0)
	{
		VarDeclarationPart();
	}
	StatementPart();
}

/***********************************************************************
 *  TP 8
 ***********************************************************************/

/*
 * CaseLabelList() : Fonction qui gère la liste d'étiquettes des instructions case
 * @param tag : tag de la condition
 * @return : void
 * Grammaire : <case label list> ::= <constant> {, <constant> }
 * Explication : une liste d'étiquettes est composée d'une constante, suivi de zéro ou plusieurs constantes séparées par des virgules
 */
void CaseLabelList(unsigned long long tag)
{
	if (current != NUMBER) // On vérifie que le token courant est un nombre
	{
		Error("CaseLabelList : c'est une constante qui est attendue"); // Si ce n'est pas le cas, on affiche une erreur
	}

	cout << "\tpush $" << lexer->YYText() << "\t# On empile la constante" << endl;
	current = (TOKEN)lexer->yylex(); // On récupère le token suivant

	cout << "\tpop %rcx \t# on depile la valeur de case " << endl;
	cout << "\tcmp %rbx, %rcx \t# on compare la valeur de case avec la valeur de condition" << endl;
	cout << "\tje CaseStatement" << tag << "\t# si les valeurs sont égales, on saute à l'étiquette de l'instruction case" << endl;

	if (current == COMMA) // Si le token suivant est une virgule
	{
		current = (TOKEN)lexer->yylex(); // On récupère le token suivant
		CaseLabelList(tag);				 // On traite la liste d'étiquettes suivante
	}

	cout << "\tjmp FinCasestatement" << tag << endl; // On saute à la fin de l'instruction case
}

/*
 * CaseListElement() : Fonction qui gère la liste des instructions case
 * @param tag : tag de la condition
 * @param tag2 : tag de l'instruction case
 * @return : void
 * Grammaire : <case list element> ::= <case label list> : <statement> | <empty>
 * Explication : une liste d'instructions case est composée d'une liste d'étiquettes, suivi d'un deux points, suivi d'une instruction, ou d'un élément vide
 */
void CaseListElement(unsigned long long tag, unsigned long long tag2)
{
	CaseLabelList(tag);	  // On traite la liste d'étiquettes
	if (current != COLON) // On vérifie que le token courant est un deux points
	{
		Error("CaseListElement : caractère ':' attendu"); // Si ce n'est pas le cas, on affiche une erreur
	}

	cout << "CaseStatement" << tag << ":" << endl; // On affiche l'étiquette de l'instruction case
	current = (TOKEN)lexer->yylex();			   // On récupère le token suivant
	Statement();								   // On traite l'instruction

	cout << "\tjmp FinCasestatement" << tag2 << endl; // On saute à la fin de l'instruction case
	cout << "FinCasestatement" << tag << ":" << endl; // On affiche l'étiquette de fin de l'instruction case
}

/*
 * CaseStatement() : Fonction qui gère les instructions case
 * @return : void
 * Grammaire : <case statement> ::= case <expression> of <case list element> {; <case list element> } end
 * Explication : une instruction case est composée du mot clé "case", suivi d'une expression, suivi du mot clé "of", suivi d'une liste d'instructions case, suivi du mot clé "end"
 */
void CaseStatement(void)
{
	unsigned long long tag = TagNumber++; // On récupère le tag de la condition
	unsigned long long tag2 = tag;		  // On récupère le tag de l'instruction case

	if (current != KEYWORD || strcmp(lexer->YYText(), "CASE") != 0) // On vérifie que le token courant est le mot clé "CASE"
	{
		Error("CaseStatement : mot clé 'CASE' attendu"); // Si ce n'est pas le cas, on affiche une erreur
	}
	cout << "Case" << tag << ":" << endl; // On affiche l'étiquette de la condition

	current = (TOKEN)lexer->yylex(); // On récupère le token suivant
	Expression();					 // On traite l'expression

	cout << "\tpop %rbx  # On depile la valeur de condition" << endl;

	if (current != KEYWORD || strcmp(lexer->YYText(), "OF") != 0) // On vérifie que le token courant est le mot clé "OF"
	{
		Error("CaseStatement : mot clé 'OF' attendu"); // Si ce n'est pas le cas, on affiche une erreur
	}

	current = (TOKEN)lexer->yylex(); // On récupère le token suivant
	tag++;							 // On incrémente le tag de l'instruction case
	CaseListElement(tag, tag2);		 // On traite la liste d'instructions case

	while (current == SEMICOLON) // Tant que le token courant est un point virgule
	{
		tag++;							 // On incrémente le tag de l'instruction case
		current = (TOKEN)lexer->yylex(); // On récupère le token suivant
		CaseListElement(tag, tag2);		 // On traite la liste d'instructions case
	}

	if (current != KEYWORD || strcmp(lexer->YYText(), "END") != 0) // On vérifie que le token courant est le mot clé "END"
	{
		Error("CaseStatement : mot clé 'END' attendu"); // Si ce n'est pas le cas, on affiche une erreur
	}

	current = (TOKEN)lexer->yylex();		  // On récupère le token suivant
	cout << "FinCase" << tag2 << ":" << endl; // On affiche l'étiquette de fin de l'instruction case
}

/*
 * RepeatStatement() : Fonction qui gère les instructions repeat
 * @return : void
 * Grammaire : RepeatStatement := "repeat" Statements "until" Expression
 * Explication : une instruction repeat est composée du mot clé "repeat", suivi d'une liste d'instructions, suivi du mot clé "until", suivi d'une expression
 */
void RepeatStatement(void)
{
	unsigned long long tag = TagNumber++; // On crée un tag pour la condition repeat

	cout << "Repeat" << tag << ":" << endl; // On affiche le tag
	current = (TOKEN)lexer->yylex();		// On récupère le token suivant

	Statement(); // On traite la liste d'instructions

	if (current != KEYWORD || strcmp(lexer->YYText(), "UNTIL") != 0) // On vérifie que le token suivant est bien le mot clé "UNTIL"
	{
		Error("RepeatStatement : mot-clé UNTIL attendu"); // Sinon on affiche une erreur
	}

	current = (TOKEN)lexer->yylex(); // On récupère le token suivant

	if (Expression() != BOOLEAN) // On vérifie que l'expression est bien un booléen
	{
		Error("RepeatStatement : le type de l'expression doit être BOOLEAN"); // Sinon on affiche une erreur
	}

	cout << "\tpop %rax\t# Get the result of expression" << endl;							   // On récupère le résultat de l'expression
	cout << "\tcmpq $0, %rax" << endl;														   // On compare le résultat à 0
	cout << "\tje Repeat" << tag << "\t# if FALSE, jump back to the repeat statement" << endl; // Si le résultat est égal à 0, on retourne au début de la boucle

	cout << "EndRepeat" << tag << ":" << endl; // On affiche le tag de fin de boucle
}

int main(void)
{ // First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler" << endl;
	cout << ".data" << endl;
	cout << "FormatString1:\t.string \"%llu\"\t# used by printf to display 64-bit unsigned integers" << endl;
	cout << "FormatString2:\t.string \"%lf\"\t# used by printf to display 64-bit floating point numbers" << endl;
	cout << "FormatString3:\t.string \"%c\"\t# used by printf to display a 8-bit single character" << endl;
	cout << "TrueString:\t.string \"TRUE\"\t# used by printf to display the boolean value TRUE" << endl;
	cout << "FalseString:\t.string \"FALSE\"\t# used by printf to display the boolean value FALSE" << endl;
	// Let's proceed to the analysis and code production
	current = (TOKEN)lexer->yylex();
	Program();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top" << endl;
	cout << "\tret\t\t\t# Return from main function" << endl;
	if (current != FEOF)
	{
		cerr << "Caractères en trop à la fin du programme : [" << current << "]";
		Error("."); // unexpected characters at the end of program
	}
}
