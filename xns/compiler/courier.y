%{
#ifndef lint
static char RCSid[] = "$Header: courier.y,v 2.2 86/06/06 07:28:39 jqj Exp $";
#endif

/* $Log:	courier.y,v $
 * Revision 2.2  86/06/06  07:28:39  jqj
 * many mods for better symbol table management:  added CurrentModule,
 *  made check_dependency, make_symbol, check_def set/use/use a symbol
 *  table instead of a module name string, etc.  Result is that we can
 *  now handle DEPENDS UPON 2 versions of same program.
 * 
 * Revision 2.1  86/05/16  05:46:50  jqj
 * make enumeration tags local to modules rather than global, to allow
 * DEPENDS UPON two versions of the same program.  For same reason, use
 * gensymed symbol names that include version number.
 * 
 * Revision 2.0  85/11/21  07:21:35  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/11/20  12:58:22  jqj
 * Initial revision
 * 
 * Revision 1.6  85/05/23  06:19:42  jqj
 * Public Beta-test version, released 24 May 1985
 * 
 * Revision 1.5  85/05/06  08:13:14  jqj
 * Almost Beta-test version.
 * 
 * Revision 1.4  85/03/26  06:09:49  jqj
 * Revised public alpha-test version, released 26 March 1985
 * 
 * Revision 1.3  85/03/11  16:39:15  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:05:07  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:53:01  jqj
 * Initial revision
 * 
 */

#include "compiler.h"

static char *currentdecl;
static char streamdecl;
%}

%token
	identifier	number		string

%token
	ARRAY		_BEGIN		BOOLEAN		CARDINAL
	CHOICE		DEPENDS		END		ERROR
	INTEGER		LONG		OF		PROCEDURE
	PROGRAM		RECORD		REPORTS		RETURNS
	SEQUENCE	STRING		TYPE		UNSPECIFIED
	UPON		VERSION		TRUE		FALSE
	_CHOOSES

%union {
	struct type *type;
	struct constant *constant;
	list list;
	char *stringvalue;
}

%type <type>
	ConstructedType	
	DesignatorType
	PredefinedType		
	ReferencedType
	Type

%type <constant>
	ReferencedConstant
	Constant
	PredefinedConstant
	ConstructedConstant

%type <list>
	ArgumentList		Candidate		CandidateList
	Correspondence		CorrespondenceList	Designator
	DesignatorList		ErrorList		Field
	FieldList		NameList		ResultList
	Component		ReferencedProgramList	ElementList
	ComponentList		TypedCandidate		TypedCandidateList
	TypedDesignator		TypedDesignatorList	CNameList

%type <stringvalue>
	NumericValue		MaximumNumber		
	ReferencedProgram	ProgramHeader
	identifier		number			string

%start	Program
%%

Program :
		ProgramHeader ProgramBody
		{
			wrapup_program($1);
		}
	;

ProgramHeader :
		identifier ':' PROGRAM number VERSION number '='
		{
			program_header($1,$4,$6);
			$$ = $1;
		}
	;

ProgramBody :
		_BEGIN DependencyList DeclarationList END '.'
	;

DependencyList :
		/* empty */
		{
			program_body();
		}
	|	DEPENDS UPON ReferencedProgramList ';'
		{
			program_body();
		}
	;

ReferencedProgramList :
		ReferencedProgram
		{
		}
	|	ReferencedProgramList ',' ReferencedProgram
		{
		}
	;

ReferencedProgram :
		identifier '(' number ')' VERSION number
		{
			/* as a side effect, the program is entered into the */
			/* list of dependencies */
			ref_program($1,$3,$6);
			$$ = $1;
		}
	;

DeclarationList :
		/* empty */
	|	DeclarationList Declaration
	;

Declaration :
		Target TypeDeclaration
	|	Target ConstantDeclaration
	|	error ';'
		{
			fprintf(stderr,"\t\t\tDeclaration skipped\n");
		}
	;

Target :
		identifier ':'
		{
			struct object *symbol;

			currentdecl = $1;
			streamdecl = 0;	/* not parsing a StreamOf yet */
			if (symbol = check_def(currentdecl, CurrentModule)) {
				error(ERROR,
					"Attempt to redefine ``%s''",
					name_of(symbol));
				YYERROR;
			}
		}
	;

TypeDeclaration :
		TYPE '=' Type ';'
		{
			struct object *symbol;

			symbol = make_symbol(currentdecl, CurrentModule);
			define_type(symbol, $3);
		}
	;

ConstantDeclaration :
		Type '=' Constant ';'
		{
			struct object *symbol;

			symbol = make_symbol(currentdecl, CurrentModule);
			if (type_check($1, $3)) {
				define_constant(symbol, $1, $3);
			} else 
				error(ERROR,
					"Type clash in declaration of ``%s''",
					name_of(symbol));
		}
	;

Type :
		PredefinedType
		{
			$$ = $1;
		}
	|	ConstructedType
		{
			$$ = $1;
		}
	|	ReferencedType
		{
			$$ = $1;
		}
	;

Constant :
		PredefinedConstant
		{
			$$ = $1;
		}
	|
		ConstructedConstant
		{
			$$ = $1;
		}
	|
		ReferencedConstant
		{
			$$ = $1;
		}
	;


PredefinedType :
		BOOLEAN
		{
			$$ = Boolean_type;
		}
	|	CARDINAL
		{
			$$ = Cardinal_type;
		}
	|	LONG CARDINAL
		{
			$$ = LongCardinal_type;
		}
	|	INTEGER
		{
			$$ = Integer_type;
		}
	|	LONG INTEGER
		{
			$$ = LongInteger_type;
		}
	|	STRING
		{
			$$ = String_type;
		}
	|	UNSPECIFIED
		{
			$$ = Unspecified_type;
		}
	|	LONG UNSPECIFIED
		{
			$$ = LongUnspecified_type;
		}
	;

PredefinedConstant :
		TRUE
		{
			$$ = Boolean_constant("1");
		}
	|
		FALSE
		{
			$$ = Boolean_constant("0");
		}
	|
		number
		{
			$$ = Numeric_constant($1);
		}
	|
		string
		{
			$$ = String_constant($1);
		}
	;

ConstructedConstant :
		/* simple ReferencedConstant */
		identifier
		{
			struct object *sym;

			if ((sym = check_def($1,ONIL)) ||
			    (sym = check_def($1,CurrentModule))) {
				if (class_of(sym) == O_ENUMTAG)
					$$ = enumeration_constant(sym->o_enum->en_name);
				else if (class_of(sym) == O_CONSTANT)
					$$ = sym->o_constant;
				else {
					error(ERROR,
						"``%s'' is not of appropriate type",
						name_of(sym));
					YYERROR;
				}
			} else {
				error(ERROR,"``%s'' is not defined",
					$1);
				YYERROR;
			}
		}
	|	
		/* SequenceConstant */
		/* ArrayConstant */
		'[' ElementList ']'
		{
			$$ = array_constant($2);
		}
	|
		/* RecordConstant */
		'[' ComponentList ']'
		{
			$$ = record_constant($2);
		}
	|
		/* RecordConstant */
		/* SequenceConstant */
		/* ArrayConstant */
		'[' ']'
		{
			$$ = record_constant(NIL);
		}
	|
		/* ChoiceConstant */
		identifier Constant
		{
			struct object* symbol;

			if (((symbol = check_def($1,CurrentModule)) ||
			     (symbol = check_def($1,ONIL)))) {
				if (class_of(symbol) == O_CONSTANT &&
				    symbol->o_constant->cn_constr == C_ENUMERATION) {
					$$ = choice_constant(
						cons((list) symbol->o_constant->cn_value,
						     (list) $2) );
				}
				else if (class_of(symbol) == O_ENUMTAG) {
					$$ = choice_constant(
						cons((list) symbol->o_enum->en_name,
						     (list) $2) );
				}
				else {
					error(ERROR, "Expected enumeration constant but got ``%s''\n",
					name_of(symbol));
					YYERROR;
				}
			}
			else {
				error(ERROR, "Designator ``%s'' undefined\n",
					$1);
				YYERROR;
			}
		}
	;


ElementList :
		Constant
		{
			$$ = cons((list) $1, NIL);

		}
	|
		Constant ',' ElementList
		{
			$$ = cons((list)$1, $3);
		}
	;

ComponentList	:
		Component
		{
			$$ = $1;
		}
	|
		Component ',' ComponentList
		{
			/* flatten */
			cdr($1) = $3;
			$$ = $1;
		}
	;

Component	:
		CNameList ':' Constant
		{
			list p;

			/* flatten this for simplicity of representation */
			for (p = $1; p != NIL; p = cdr(p))
				car(p) = cons(car(p),(list)$3);
			$$ = $1;
		}
	;

CNameList :
		identifier
		{
			/* note that CNameList now is a list of strings */
			$$ = cons((list) $1, NIL);
		}
	|	identifier ',' CNameList
		{
			/* note that NameList now is a list of strings */
			$$ = cons(cons((list)$1, NIL), $3);
		}
	;

ConstructedType :
		'{' CorrespondenceList '}'
		{
			$$ = enumeration_type($2);
		}
	|	ARRAY NumericValue OF Type
		{
			$$ = array_type($2, $4);
		}
	|	SEQUENCE MaximumNumber OF Type
		{
			$$ = sequence_type($2, $4);
		}
	|	RECORD ArgumentList
		{
			$$ = record_type($2);
		}
	|	CHOICE DesignatorType OF '{' TypedCandidateList '}'
		{
			$$ = choice_type($2, $5);
		}
	|	CHOICE OF '{' CandidateList '}'
		{
			if (streamdecl > 0) {
				$$ = choice_type(StreamEnum_type, $4);
			}
			/* as side effect build an anonymous enumerated type */
			else
			  $$ = choice_type((struct type *) NIL, $4);
		}
	|	PROCEDURE ArgumentList ResultList ErrorList
		{
			$$ = procedure_type($2, $3, $4);
		}
	|	ERROR ArgumentList
		{
			$$ = error_type( $2);
		}
	;

ReferencedType :
		identifier
		{
			struct object *symbol;

			if (symbol = check_def($1,CurrentModule)) {
				if (class_of(symbol) == O_TYPE)
					$$ = symbol->o_type;
				else {
				    error(ERROR,"``%s'' is not a type",
					name_of(symbol));
				    YYERROR;
				}
			}
			else if (streq($1,currentdecl)) {
				if (strncmp(currentdecl,"StreamOf",8) == 0) {
					streamdecl++;
					error(WARNING,
						"Stream definition of ``%s'';\n\
\t\t\trecursion treated as Nil record",
						$1);
					$$ = record_type(NIL);
				} else {
					/* fake it */
					$$ = enumeration_type(NIL);
					$$->type_name = make_full_name(
						CurrentProgram, CurrentVersion,
						currentdecl);
				}
			}
			else {
				error(ERROR,"``%s'' is unrecognized", $1);
				YYERROR;
			}
		}
	|	identifier '.' identifier
		{
			struct object *symbol, *module;

			if ((module=check_dependency($1)) &&
			    (symbol = check_def($3,module))) {
				if (class_of(symbol) == O_TYPE)
					$$ = symbol->o_type;
				else {
				    error(ERROR,"``%s'' is not a type",
					name_of(symbol));
				    YYERROR;
				}
			}
			else {
				error(ERROR,"``%s.%s'' is unrecognized",$1,$3);
				YYERROR;
			}
		}
	;

CorrespondenceList :
		Correspondence
		{
			$$ = cons($1, NIL);
		}
	|	CorrespondenceList ',' Correspondence
		{
			$$ = nconc($1, cons($3, NIL));
		}
	;

Correspondence :
		identifier '(' NumericValue ')'
		{
			struct object *symbol;
			char *newid;

			if (!(symbol = check_def($1,ONIL)) &&
			    !(symbol = check_def($1,CurrentModule))) {
				symbol = make_symbol($1,CurrentModule);
				define_enumeration_symbol(symbol,$3);
			}
			else if (class_of(symbol) != O_ENUMTAG) {
				error(ERROR,"``%s'' already defined",
					name_of(symbol));
				YYERROR;
				}
			else if ((streq($1,"nextSegment") &&
				  stringtocard($3) == 0) ||
				 (streq($1,"lastSegment") &&
				  stringtocard($3) == 1)) {
				/* do nothing */
					streamdecl++;
			}
			else /*
			      * if (symbol->o_enum->en_value!=stringtocard($3))
			      */ {
				newid = gensym($1);
				error(WARNING,
					"Enumerator ``%s'' already declared;\n\
\t\t\tusing name ``%s'' instead",
					$1,newid);
				symbol = make_symbol(newid,CurrentModule);
				define_enumeration_symbol(symbol,$3);
			}
			$$ = cons((list) symbol, (list) $3);
		}
	;

MaximumNumber :
		NumericValue
		{
			$$ = $1;
		}
	|	/* empty */
		{
			$$ = "65535";		/* maximum Cardinal */
		}
	;

NumericValue :
		number
		{
			$$ = $1;
		}
	|	ReferencedConstant
		{
			if (($1)->cn_constr != C_NUMERIC) {
				error(ERROR,"Expected numeric constant");
				YYERROR;
			}
			$$ = ($1)->cn_value;
		}
	;

DesignatorType :
		ReferencedType
		{
			$$ = $1;
		}
	;

TypedCandidateList :
		TypedCandidate
		{
			$$ = cons($1, NIL);
		}
	|	TypedCandidateList ',' TypedCandidate
		{
			$$ = nconc($1, cons($3, NIL));
		}
	;

TypedCandidate :
		TypedDesignatorList _CHOOSES Type
		{
			$$ = cons($1, (list) $3);
		}
	;

TypedDesignatorList :
		TypedDesignator
		{
			$$ = cons($1, NIL);
		}
	|	TypedDesignatorList ',' TypedDesignator
		{
			$$ = nconc($1, cons($3, NIL));
		}
	;

TypedDesignator :
		identifier
		{
			struct object *symbol;

			if ((symbol = check_def($1,CurrentModule)) &&
				 symbol->o_constant->cn_constr == C_ENUMERATION) {
				$1 = symbol->o_constant->cn_value;
				}
			else if (((symbol = check_def($1,ONIL)) ||
				  (symbol = check_def($1,CurrentModule))) &&
				 class_of(symbol) == O_ENUMTAG)
				$$ = cons((list) symbol, NIL);
			else {
				error(ERROR,"Designator ``%s'' is not of appropriate type",
					$1);
				YYERROR;
			}
		}
	;

CandidateList :
		Candidate
		{
			$$ = cons($1, NIL);
		}
	|	CandidateList ',' Candidate
		{
			$$ = nconc($1, cons($3, NIL));
		}
	;

Candidate :
		DesignatorList _CHOOSES Type
		{
			$$ = cons($1, (list) $3);
		}
	;

DesignatorList :
		Designator
		{
			$$ = cons($1, NIL);
		}
	|	DesignatorList ',' Designator
		{
			$$ = nconc($1, cons($3, NIL));
		}
	;

Designator :
		Correspondence
		{
			$$ = $1;
		}
	;

ResultList :
		/* empty */
		{
			$$ = NIL;
		}
	|	RETURNS '[' FieldList ']'
		{
			$$ = $3;
		}
	;

ArgumentList :
		/* empty */
		{
			$$ = NIL;
		}
	|	'[' ']'
		{
			$$ = NIL;
		}
	|	'[' FieldList ']'
		{
			$$ = $2;
		}
	;

ErrorList :
		/* empty */
		{
			$$ = NIL;
		}
	|	REPORTS '[' NameList ']'
		{
			$$ = $3;
		}
	;

FieldList :
		Field
		{
			$$ = $1;
		}
	|	FieldList ',' Field
		{
			$$ = nconc($1, $3);
		}
	;

Field :
		NameList ':' Type
		{
			/* flatten representation for simplicity */
			/* note that this could be even simpler, but I */
			/* don't have the patience to change code everywhere */
			list p;

			for (p = $1; p != NIL; p = cdr(p))
				car(p) = cons(cons(car(p),NIL),(list)$3);
			$$ = $1;
		}
	;

ReferencedConstant :
		/* see ConstructedConstant for simple referenced constants */
		identifier '.' identifier
		{
			struct object *symbol, *module;

			if ((module=check_dependency($1)) &&
			    (symbol=check_def($3,module))) {
				if (class_of(symbol) != O_CONSTANT) {
				    error(ERROR,"Constant expected, but got ``%s''",
						name_of(symbol));
				    YYERROR;
				}
				$$ = symbol->o_constant;
			} else {
				error(ERROR,"Unrecognized symbol ``%s.%s''",
					$1,$3);
			}
		}
	;

NameList :
		identifier
		{
			/* note that NameList now is a list of strings */
			$$ = cons((list) $1, NIL);
		}
	|	NameList ',' identifier
		{
			/* note that NameList now is a list of strings */
			$$ = nconc($1, cons((list) $3, NIL));
		}
	;


%%

YYSTYPE yyv[];
int yynerrs;
extern int yylineno;

struct parser_state {
	YYSTYPE yyv[YYMAXDEPTH];
	YYSTYPE yylval;
	YYSTYPE yyval;
	int yychar;
	int yynerrs;
	short yyerrflag;
	int yylineno;
	int recursive_flag;
	char *CurrentProgram;
	int CurrentVersion;
	int CurrentNumber;
	struct object *CurrentModule;
	char yysbuf[200];	 /*YYLMAX*/
	char *yysptr;
};
extern char yysbuf[], *yysptr;

int *
save_parser_state()
{
	struct parser_state *p;

	p = New(struct parser_state);
	bcopy(yyv, p->yyv, YYMAXDEPTH*sizeof(YYSTYPE));
	p->yylval = yylval;
	p->yyval = yyval;
	p->yychar = yychar;
	p->yynerrs = yynerrs;
	p->yyerrflag = yyerrflag;
	p->yylineno = yylineno;
	p->recursive_flag = recursive_flag;
	p->CurrentProgram = CurrentProgram;
	p->CurrentVersion = CurrentVersion;
	p->CurrentNumber = CurrentNumber;
	p->CurrentModule = CurrentModule;
	p->yysptr = yysptr;
	bcopy(yysbuf, p->yysbuf, 200);
	yysptr = yysbuf;
	recursive_flag = 1;
	return ((int*) p);
}

restore_parser_state(p)
	struct parser_state *p;
{
	yysptr = p->yysptr;
	bcopy(p->yysbuf, yysbuf, 200);
	CurrentProgram = p->CurrentProgram;
	CurrentVersion = p->CurrentVersion;
	CurrentNumber = p->CurrentNumber;
	CurrentModule = p->CurrentModule;
	recursive_flag = p->recursive_flag;
	yylineno = p->yylineno;
	yyerrflag = p->yyerrflag;
	yynerrs = p->yynerrs;
	yychar = p->yychar;
	yyval = p->yyval;
	yylval = p->yylval;
	bcopy(p->yyv, yyv, YYMAXDEPTH*sizeof(YYSTYPE));
	free((char *) p);
}
