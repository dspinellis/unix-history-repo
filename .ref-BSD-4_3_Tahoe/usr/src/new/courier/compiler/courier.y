%{
#ifndef lint
static char sccsid[] = "@(#)courier.y	4.2 (Berkeley) 7/7/83";
#endif

#include "Courier.h"
%}

%token
	identifier	number

%token
	ARRAY		_BEGIN		BOOLEAN		CARDINAL
	CHOICE		DEPENDS		END		ERROR
	INTEGER		LONG		OF		PROCEDURE
	PROGRAM		RECORD		REPORTS		RETURNS
	SEQUENCE	STRING		TYPE		UNSPECIFIED
	UPON		VERSION

%union {
	int integer;
	struct object *object;
	list list;
}

%type <object>
	identifier		number 
	ConstructedType		DesignatorType		MaximumNumber
	NumericValue		PredefinedType		ProgramHeader
	ReferencedConstant	ReferencedType		Type

%type <list>
	ArgumentList		Candidate		CandidateList
	Correspondence		CorrespondenceList	Designator
	DesignatorList		ErrorList		Field
	FieldList		NameList		ResultList

%start	Program

%%

Program :
		ProgramHeader ProgramBody
		= {
			program($1);
		}
	;

ProgramHeader :
		identifier ':' PROGRAM '='
		= {
			program_header($1);
			$$ = $1;
		}
	|
		identifier ':' PROGRAM number VERSION number '='
		= {
			program_header($1);
			$$ = $1;
		}
	;

ProgramBody :
		_BEGIN DependencyList DeclarationList END '.'
	;

DependencyList :
		/* empty */
	|	DEPENDS UPON ReferencedProgramList ';'
		= {
			yyerror("Dependencies on other Courier programs are not supported");
		}
	;

ReferencedProgramList :
		ReferencedProgram
	|	ReferencedProgramList ',' ReferencedProgram
	;

ReferencedProgram :
		identifier '(' number ')' VERSION number
	;

DeclarationList :
		/* empty */
	|	DeclarationList Declaration
	;

Declaration :
		identifier ':' TYPE '=' Type ';'
		= {
			compile_type($1, $5);
		}
	|	identifier ':' Type '=' NumericValue ';'
		= {
			if (type_check($3, $5)) {
				compile_def($1, $3, $5);
			} else
				yyerror("Type clash in declaration of %s",
					name_of($1));
		}
	;

Type :
		PredefinedType
		= {
			type_functions($1);
			$$ = $1;
		}
	|	ConstructedType
		= {
			type_functions($1);
			$$ = $1;
		}
	|	ReferencedType
		= {
			type_functions($1);
			$$ = $1;
		}
	;

PredefinedType :
		BOOLEAN
		= {
			$$ = Boolean_type;
		}
	|	CARDINAL
		= {
			$$ = Cardinal_type;
		}
	|	LONG CARDINAL
		= {
			$$ = LongCardinal_type;
		}
	|	INTEGER
		= {
			$$ = Integer_type;
		}
	|	LONG INTEGER
		= {
			$$ = LongInteger_type;
		}
	|	STRING
		= {
			$$ = String_type;
		}
	|	UNSPECIFIED
		= {
			$$ = Unspecified_type;
		}
	|	LONG UNSPECIFIED
		= {
			$$ = LongUnspecified_type;
		}
	;

ConstructedType :
		'{' CorrespondenceList '}'
		= {
			$$ = construct_type1(C_ENUMERATION, $2);
		}
	|	ARRAY NumericValue OF Type
		= {
			$$ = construct_type2(C_ARRAY, $2, $4);
		}
	|	SEQUENCE MaximumNumber OF Type
		= {
			$$ = construct_type2(C_SEQUENCE, $2, $4);
		}
	|	RECORD ArgumentList
		= {
			$$ = construct_type1(C_RECORD, $2);
		}
	|	CHOICE DesignatorType OF '{' CandidateList '}'
		= {
			$$ = construct_choice($2, $5);
		}
	|	PROCEDURE ArgumentList ResultList ErrorList
		= {
			$$ = construct_procedure($2, $3, $4);
		}
	|	ERROR ArgumentList
		= {
			$$ = construct_type1(C_ERROR, $2);
		}
	;

ReferencedType :
		identifier
		= {
			if (check_def($1))
				$$ = $1;
			else
				$$ = Unspecified_type;
		}
	|	identifier '.' identifier
		= {
			yyerror("References to types in other Courier programs are not supported");
			$$ = Unspecified_type;
		}
	;

CorrespondenceList :
		Correspondence
		= {
			$$ = cons($1, NIL);
		}
	|	CorrespondenceList ',' Correspondence
		= {
			$$ = nconc($1, cons($3, NIL));
		}
	;

Correspondence :
		identifier '(' NumericValue ')'
		= {
			$$ = cons($1, $3);
		}
	;

MaximumNumber :
		NumericValue
		= {
			$$ = $1;
		}
	|	/* empty */
		= {
			$$ = NIL;
		}
	;

NumericValue :
		number
		= {
			$$ = $1;
		}
	|	ReferencedConstant
		= {
			$$ = $1;
		}
	;

DesignatorType :
		/* empty */
		= {
			$$ = NIL;
		}
	|	ReferencedType
		= {
			$$ = $1;
		}
	;

CandidateList :
		Candidate
		= {
			$$ = cons($1, NIL);
		}
	|	CandidateList ',' Candidate
		= {
			$$ = nconc($1, cons($3, NIL));
		}
	;

Candidate :
		DesignatorList '=''>' Type
		= {
			$$ = cons($1, $4);
		}
	;

DesignatorList :
		Designator
		= {
			$$ = cons($1, NIL);
		}
	|	DesignatorList ',' Designator
		= {
			$$ = nconc($1, cons($3, NIL));
		}
	;

Designator :
		identifier
		= {
			$$ = cons($1, NIL);
		}
	|	Correspondence
		= {
			$$ = $1;
		}
	;

ArgumentList :
		/* empty */
		= {
			$$ = NIL;
		}
	|	'[' FieldList ']'
		= {
			$$ = $2;
		}
	;

ResultList :
		/* empty */
		= {
			$$ = NIL;
		}
	|	RETURNS '[' FieldList ']'
		= {
			$$ = $3;
		}
	;

ErrorList :
		/* empty */
		= {
			$$ = NIL;
		}
	|	REPORTS '[' NameList ']'
		= {
			$$ = $3;
		}
	;

FieldList :
		Field
		= {
			$$ = cons($1, NIL);
		}
	|	FieldList ',' Field
		= {
			$$ = nconc($1, cons($3, NIL));
		}
	;

Field :
		NameList ':' Type
		= {
			$$ = cons($1, $3);
		}
	;


ReferencedConstant :
		identifier
		= {
			if (check_def($1))
				$$ = $1;
			else
				$$ = Undefined_constant;
		}
	|	identifier '.' identifier
		= {
			yyerror("References to constants in other Courier programs are not supported");
			$$ = Undefined_constant;
		}
	;

NameList :
		identifier
		= {
			$$ = cons($1, NIL);
		}
	|	NameList ',' identifier
		= {
			$$ = nconc($1, cons($3, NIL));
		}
	;
