/*
**  GRAMMAR.Y -- Equel 6.2 grammar
**
**	Yacc grammar and semantic rules for Quel
**	embedded in C.
**
**	The grammar is organized in the following manner:
**		a) control structure
**		b) quel statments
**		c) equel statements
**		d) declaration and use of C vars
**		e) special objects
**		f) expressions
**		g) keywords
**		h) noise words and punctuation
**	Within a single classification, major objects are ordered
**	alphabetically, and minor objects under the major ones they
**	are in.
**
**	Side Effects:
**		performs the translation of an Equel program into
**		a C program with calls on the Equel run-time
**		support
**
**	NOTE:
**		There are two shift/reduce conflicts associated
**		with the non-terminals "[o_]gen_while". It is resolved
**		correctly by yacc.
*/



%{
	/* STANDARD SCANNER & PARSER GLOBALS */

# include	"constants.h"
# include	<stdio.h>
# include	"globals.h"
# include	<sccs.h>

SCCSID(@(#)grammar.y	7.1	2/5/81)


%}
%union
{
	struct disp_node	*u_dn;
};


	/* QUEL keywords */
%token	<u_dn>	APPEND		COPY 		CREATE		DEFINE		
%token	<u_dn>	DELETE		DESTROY		HELP		INDEX
%token	<u_dn>	MODIFY 		PRINT		INTEGRITY	RANGE
%token	<u_dn>	REPLACE		RETRIEVE	SAVE		UNIQUE
%token	<u_dn>	PERMIT		VIEW

	/* special embedded QUEL commands */
%token	<u_dn>	INGRES		EXIT		PARAM

	/* C variable usages */
%token	<u_dn>	TYPE		ALLOC		STRUCT		STRUCT_VAR

	/* QUEL noise words */
%token	<u_dn>	ALL		BY		FROM		IN
%token	<u_dn>	INTO		IS		OF		ON
%token	<u_dn>	ONTO		TO		WHERE		UNTIL
%token	<u_dn>	AT

	/* constants */

%token	<u_dn>	NAME		SCONST		I2CONST		I4CONST
%token	<u_dn>	F8CONST		C_CODE

	/* punctuation */
%token	<u_dn>	COMMA		LPAREN		RPAREN		PERIOD
%token	<u_dn>	QUOTE		BGNCMNT		ENDCMNT		LBRACE
%token	<u_dn>	RBRACE		LBRKT		RBRKT		NONREF
%token	<u_dn>	SEMICOL		POINTER		COLON

	/* operator classes */
%token	<u_dn>	UOP
%token	<u_dn>	BOP
%token	<u_dn>	BDOP
%token	<u_dn>	EOP
%token	<u_dn>	LBOP		LUOP
%token	<u_dn>	FOP		FBOP
%token	<u_dn>	AOP

	/* define ascending precedence for operators */

%left	LBOP
%left	LUOP
%left	BOP		LBRKT
%left	UOP
%nonassoc		unaryop

	/* non-teriminal type info */
%type <u_dn>		endblock
%type <u_dn>		c_variable
%type <u_dn>		cvarx
%type <u_dn>		struct_var
%type <u_dn>		ptr
%type <u_dn>		selector_part
%type <u_dn>		str_var_key
%type <u_dn>		id



%%
%{
	struct cvar	*cvarp;
%}

/*
 * Program Control Structure
 */

program:	program statement = 
		{
			/* for each "statement", free the symbol space
			 * used by that query (lookahed space must not
			 * be freed), and catch up on output lines
			 */
			symspfree();
			equate_lines();
		}
	|	;
;
statement:	startquel quel_statement = 
		{
			w_sync();
			/* the purpose of the actions for startquel
			 * and this action is to make each query
			 * a single compound C statement (if (x) "{query}")
			 */
			w_op("}");
		}
	|	startquel equel_statement =
		{
			end_quote();
			w_op("}");
		}
	|	c_code	
	| 	declaration	 
	| 	error
;
startquel:	=
			w_op("{");
;
/*
 * C_CODE is a token returned by the lexical analyzer
 * when it recognizes that the following line is not
 * equel code. On the NEXT call to the lexical analyzer,
 * the code will be copied from the source to the output file.
 */
c_code: 	C_CODE	
	|	beginblock =
			Block_level += 1;
	|	endblock =
		{
			if (Block_level == 0)
				yyserror("extra '}'", $1);
			else if ((Block_level -= 1) == 0)
			{
				freecvar(&C_locals);
				freecvar(&F_locals);
			}
		}
;

	/* the %prec is to resolve conflicts with the { after a
	 * "tupret".
	 */
beginblock:	LBRACE  %prec LBOP =
			w_op($1->d_elm);
;
endblock:	RBRACE =
			w_op($1->d_elm);
;
quel_statement: append	 
	|	copy 
	| 	create	 
	|	delete 
	| 	destroy	 
	|	help 
	| 	index	 
	| 	integrity	 
	|	modify 
	|	permit
	|	print 
	|	range 
	|	replace 
	|	retrieve 
	|	save 
	|	view
;

	/* commands particular to Equel */

equel_statement:append_p
	|	copy_p
	| 	create_p
	| 	exit	 
	| 	ingres	 
	|	replace_p
	|	retrieve_p
	|	tupret
	|	view_p
;
/*
 * QUEL statements 
 */

append:		append_key apclause tlclause qualclause	
;
apclause:	apkword id	
;

copy: 		copy_key id lparen ctl rparen cp_kword filename 
	| 	copy_key id lparen rparen cp_kword filename
;
filename:	sconst
	|	id 
;

create:		create_key id lparen ctl rparen
;

delete:		delete_key delclause qualclause	
;
delclause:	delnoise id		
;

destroy:	destroy_key idlist
	|	destroy_key integ_permit id int_list_all
;
integ_permit:	integ_key
	|	permit_key
;
int_list_all:	int_list
	|	all
;

help:		help_key	
	|	help_key all
	|	help_key hlist
	|	help_key int_perm_view idlist
;
hlist:		hparam
	|	hlist comma hparam
;
hparam:		id 
	|	sconst
;
int_perm_view:	integ_permit
	|	view_key
;

index:		index_key id is id lparen idlist rparen
;

integrity:	define_key integ_key integnoise id isnoise qual
;

modify:		modify_key id to id on modkeylist density
	|	modify_key id to id
;
modkeylist:	modkey
	|	modkeylist comma modkey
;
modkey:		id
	|	id colon id
;
density:	where modfill
	|	;
;
modfill:	id is mod_var
	|	modfill comma id is mod_var
;
	/* mod_var can be an integer constant or var, or a string var
	 * or a quel name
	 */
mod_var:	I2CONST =
			w_con(I2CONST, $1->d_elm);
	|	c_variable =
		{
			if ($1)
			{
				if (!Cvarp)
					w_key($1->d_elm);
				else if (Fieldp && Fieldp->c_type == opINT
					|| Cvarp->c_type == opINT)
						w_var(Cv_display, opINT);
				else if (Fieldp && Fieldp->c_type == opSTRING 
					|| Cvarp->c_type == opSTRING)
						w_var(Cv_display, opIDSTRING);
				else
					yyserror("in MODIFY, qual var must be in or string",
					$1);
			}
			else
				yyserror("bad modify qualification", 0);
			free_display(Cv_display);
			Cvarp = Fieldp = 0;
		}
;

permit:		def_perm permit_list on_of_to id perm_tl
			perm_who perm_term perm_time perm_day qualclause
;
def_perm:	define_key permit_key 
;
permit_list:	permlistelm
	|	permit_list comma permlistelm
;
permlistelm:	RETRIEVE =
			w_key($1->d_elm);
	|	APPEND =
			w_key($1->d_elm);
	|	DELETE =
			w_key($1->d_elm);
	|	REPLACE = 
			w_key($1->d_elm);
	|	all
;
on_of_to:	on
	|	of
	|	to
;
perm_tl:	lparen idlist rparen
	|	;
;
perm_who:	to id
	|	all
	|	;
;
perm_term:	at id
	|	at all
	|	;
;
perm_time:	from integer colon integer to
			integer colon integer
	|	;
;
perm_day:	on id to id
	|	;
;

print:		print_key idlist 
;

range:		range_of id is id	
;

replace:	replace_key repclause tlclause qualclause	
;
repclause:	repkword id	
;

retrieve:	retrieve_key resclause tlclause qualclause	
;
resclause:	retkword id	
;

save:		save_key id until date
;
date:		id integer integer
;

view:		define_key view_key id tlclause qualclause
;

/*
 * Statements Particular to Equel
 */

append_p:	append_p_key apclause param_tl qualclause
;

copy_p:		copy_p_key id param_tl fr_in_id filename
;
fr_in_id:	cp_kword
	|	c_variable =
		{
			if ($1 && Cvarp)
			{
				if (Fieldp && Fieldp->c_type != opSTRING
				   || !Fieldp && Cvarp->c_type != opSTRING)
					yyserror("string var expected for from/into in COPY",
					$1);
				else
					w_var(Cv_display, opIDSTRING);
			}
			else
				yyserror("into/from expected in COPY", $1);
			free_display(Cv_display);
			Fieldp = Cvarp = 0;
		}
;

create_p:	create_p_key id param_tl
;

exit:		EXIT = 
		{ 
			Opflag = mdEXIT;
			w_new("IIexit();"); 
		}
;

ingres:		ingres_key param_list = 
			w_op(");"); 
;
param_list:	param =
			w_op("0");
	|	param param_list
;
param:		id  =
			w_op(",");
	|	SCONST =
		{
			w_string($1->d_elm, 0);
			w_op(",");
		}
;

replace_p:	replace_p_key repclause param_tl qualclause
;

retrieve_p:	retrieve_p_key resclause param_tl qualclause
;

tupret:		tupret_keyw xc_code =
			w_flush();
	|	tupret_p o_xc_code =
			w_flush();
;
tupret_keyw:	retrieve_key unique c_tlclause qualclause	=
		{
			w_new("IIsetup();");
		}
;
unique: 	UNIQUE =
		{
			Opflag = mdTUPRET;
			w_key($1->d_elm);
		}
	|	=
			Opflag = mdTUPRET;
;
c_tlclause:	lparen	c_tlist	rparen	 
;
c_tlist:	c_tlelm 
	| 	c_tlelm comma c_tlist  
;
c_tlelm:	reduc cvar is_key afcn	 
;
reduc:		=
			Opflag = mdCTLELM;
;
xc_code:	LBRACE gen_while c_code RBRACE %prec LBRACE =
			w_op("}");
	|	gen_while %prec LBOP =
			w_op("}");
;
gen_while:	=
		{
			w_new("while(IIn_get(");
			w_file();
			w_op(")){");
			w_ret();
			free_ret();
			w_op("if(IIerrtest())continue;");
			equate_lines();
		}
;
o_xc_code:	LBRACE  o_gen_while c_code RBRACE %prec LBRACE =
			w_op("}");
	|	o_gen_while %prec LBOP =
			w_op("}");
;
o_gen_while:	=
		{
			w_new("while(IIgettup(");
			w_file();
			w_op(")){");
			equate_lines();
		}
;
tupret_p:	tupret_p_key unique param_tl qualclause =
		{
			w_new("IIsetup();");
		}
;

view_p:		PARAM define_key view_key id param_tl qualclause
;

/*
 * Declarations and use of C variables
 */

declaration:	decl_specifer declarator_list SEMICOL	=
		{
			w_op($3->d_elm);
			Type_spec = 0;
		}
	| 	decl_specifer SEMICOL = 
		{ 
			w_op($2->d_elm);
			Type_spec = 0;
		}
;
decl_specifer:	type_specifier	
	| 	sc_specifier	
	| 	type_specifier sc_specifier	
	| 	sc_specifier type_specifier	
	|	struct_dec =
		{
			Struct_flag = 0;
			Type_spec = opSTRUCT;
		}
	|	sc_specifier struct_dec =
		{
			Struct_flag = 0;
			Type_spec = opSTRUCT;
		}
;
sc_specifier:	ALLOC	=
		{
			Opflag = mdDECL;
			w_key($1->d_elm);
			/* in case the default "int" should be assumed,
			 * the Type_spec is set up for it, if a previous
			 * type hasn't been given
			 */
			if (!Type_spec)
				Type_spec = opINT;
		}
;
type_specifier:	TYPE	=
		{
			Opflag = mdDECL;
			w_key($1->d_elm);
			Type_spec = Opcode;
		}
;
struct_dec:	struct_name field_declaration 
	|	struct_name
	|	struct_key field_declaration
;
struct_name:	struct_key NAME =
			w_key($2->d_elm);
;
field_declaration:	lbrace field_seq RBRACE =
		{
			w_op($3->d_elm);
			Type_spec = 0;
		}
;
field_seq:	field_seq field
	|	;
;
field:		type_specifier declarator_list SEMICOL =
		{
			w_op($3->d_elm);
			Type_spec = 0;
		}
	|	C_CODE
;
declarator_list:	cvar_dec
	|	declarator_list comma cvar_dec
;
cvar_dec:	cvarx =
		{
			if (Type_spec == opSTRING)
				Indir_level -= 1;
			if (Struct_flag)
				decl_field($1->d_elm, Type_spec,
					Indir_level, Block_level);
			else
				decl_cvar($1->d_elm, Type_spec,
					Indir_level, Block_level);
			free_display(Cv_display);
			Indir_level = Field_indir = 0;
			Fieldp = Cvarp = 0;
		}
;
c_variable:	cvarx =
		{
			$$ = $1;
			if (Cvarp && Cvarp->c_indir != Indir_level)
			{
				yyserror("bad indirection on a C variable", $1);
				$$ = 0;
			}
			Indir_level = Field_indir = 0;
		}
	|	NONREF NAME =
		{
			enter_display(Cv_display, salloc($1->d_elm));
			Cvarp = Fieldp = 0;
			$$ = $2;
		}
	|	NONREF STRUCT_VAR =
		{
			enter_display(Cv_display, salloc($1->d_elm));
			Cvarp = Fieldp = 0;
			$$ = $2;
		}
	|	struct_var =
		{
			if (!Fieldp)
			{
				yyserror("undeclared field", $1);
				$$ = $<u_dn>0;
			}
			else if (Fieldp->c_indir != Field_indir)
			{
				yyserror("bad indirection on a structure's field",
				$1);
				$$ = 0;
			}
			if (Cvarp->c_indir != Indir_level)
			{
				yysemerr("bad indirection a structure variable",
				Cvarp->c_indir);
				$$ = 0;
			}
			Indir_level = Field_indir = 0;
		}
;
struct_var:	ptr struct_var	%prec unaryop =
		{
			if ($1->d_elm[1] == '*')
				Field_indir += 1;
			Field_indir += 1;
			$$ = $2;
		}
	|	struct_var arraysub	%prec LBRKT =
			Field_indir += 1;
	|	str_var_key selector_part
;
str_var_key:	STRUCT_VAR =
		{
			Cvarp = getcvar($1->d_elm);
			enter_display(Cv_display, $1->d_elm);
		}
;
selector_part:	arraysub selector_part =
		{
			Indir_level += 1;
			$$ = $2;
		}
	|	select_op NAME =
		{
			enter_display(Cv_display, $2->d_elm);
			Fieldp = getfield($2->d_elm);
			$$ = $2;
		}
;
select_op:	PERIOD =
			enter_display(Cv_display, $1->d_elm);
	|	POINTER =
		{
			enter_display(Cv_display, $1->d_elm);
			Indir_level += 1;
		}
;

/* use of a C variable */
cvar:		c_variable =	
		{
			if ($1)
			{
				if (!Fieldp && ! Cvarp)
				{
					if (!Field_indir && !Indir_level
					  && (sequal($1->d_elm, "dba")
					    || sequal($1->d_elm, "usercode")))
						/* constant operator COP */
						w_key($1->d_elm);
					else
						yyserror("C var expected", $1);
				}
				else if (Opflag == mdCTLELM)
				{
					w_con(NAME,
					  Fieldp ? Fieldp->c_id: Cvarp->c_id);
					enter_ret(Cv_display,
					  Fieldp ? Fieldp->c_type: Cvarp->c_type);
				}
				else
					w_var(Cv_display,
					  Fieldp ? Fieldp->c_type: Cvarp->c_type);
			}
			free_display(Cv_display);
			Fieldp = Cvarp = 0;
			Indir_level = Field_indir = 0;
		}
cvarx: 		NAME =
		{
			if (Opflag == mdDECL)
				w_con(NAME, $1->d_elm);
			else
			{
				Cvarp = getcvar($1->d_elm);
				enter_display(Cv_display, salloc($1->d_elm));
			}
		}
	|	ptr cvarx %prec unaryop =
		{
			if ($1->d_elm [1] == '*')
				Indir_level += 1;
			Indir_level += 1;
			$$ = $2;
		}
	|	cvarx arraysub	%prec LBRKT =
		{
			Indir_level += 1;
		}
;
ptr:		BOP =
		{
			if (!sequal($1->d_elm, "*") && !sequal((struct disp_node *)($1)->d_elm, "**"))
				yyserror(Opflag == mdDECL ?
				"invalid operator in declaration":
				"invalid operator in C variable",
				$1);
			if (Opflag == mdDECL)
				w_op($1->d_elm);
			else
				enter_display(Cv_display, salloc($1->d_elm));
		}
;
arraysub:	LBRKT =
		{
			if (Opflag == mdDECL)
				eat_display(0, '[', ']');
			else
				eat_display(Cv_display, '[', ']');
		}
;

/*
 * Special Objects used throughout grammar
 */

id:		c_variable =
		{
			if ($1)
			{
				if (Cvarp)
				{
					if (Fieldp && Fieldp->c_type != opSTRING
					   || !Fieldp && Cvarp->c_type != opSTRING)
						yyserror("string var expected", $1);
					else if (Opflag == mdFILENAME)
						w_var(Cv_display, opSTRING);
					else if (Opflag == mdINGRES)
						w_display(Cv_display);
					else
						w_var(Cv_display, opIDSTRING);
				}
				else if (Opflag == mdINGRES)
					w_string($1->d_elm, 0);
				else if (Opflag == mdFILENAME)
					yyserror("file for a COPY must be a string or string variable",
					$1);
				else
					w_key($1->d_elm);
			}
			free_display(Cv_display);
			Fieldp = Cvarp = 0;
		}
;

idlist:		id
	|	idlist comma id
;

integer:	I2CONST	=
			w_con(I2CONST, $1->d_elm);
	|	c_variable =
		{
			if ($1)
			{
				if (Cvarp)
					if (Fieldp && Fieldp->c_type == opINT
					   || Cvarp->c_type == opINT)
						w_var(Cv_display, opINT);
					else
						yyserror("integer variable required",
						$1);
				else
					yyserror("integer variable required", $1);
			}
			free_display(Cv_display);
		}
;

int_list:	integer
	|	int_list comma integer
;

param_tl:	LPAREN =
		{
			w_op("(");
			end_quote();
			if (Opflag == mdTUPRET)
				w_key("IIw_left");
			else
				w_key("IIw_right");
			eat_display(0, '(', ')');
			w_op(";");
			begin_quote();
			w_op(")");

		}
;

qualclause:	where qual	
	|	where c_variable =
		{
			if (!$2 || !Cvarp)
				yyserror("C var (string) expected", $2);
			else if (Fieldp && Fieldp->c_type == opSTRING
				|| Cvarp->c_type == opSTRING)
			{
				end_quote();
				w_op("IIwrite(");
				w_display(Cv_display);
				w_op(");");
			}
			else
				yyserror("var must be string valued for qualification",
				$2);
			free_display(Cv_display);
			Cvarp = Fieldp = 0;
		}
	|	;
;
qual:		lparen qual rparen	
	| 	luop qual		%prec LUOP	
	| 	qual lbop qual		%prec LBOP	
	| 	clause	
	| 	;
;
clause:	afcn rop afcn	
	| 	afcn rop afcn bdop afcn	
;
ctl:		id is id 
	| 	ctl comma id is id 
;

sconst:		SCONST = 
			w_con(SCONST, $1->d_elm);
;

tlclause:	lparen tlist rparen	
;
tlist:		tlelm	
	|	tlelm comma tlist	
;
tlelm:		id is_key afcn	
	|	attrib	
;

/*
 * Expressions
 */

afcn:		aggrfcn	 
	| 	aggr	 
	| 	attribfcn  
	| 	afcn bop afcn 	%prec BOP  
	| 	lparen afcn rparen  
	| 	uop afcn 	%prec unaryop	
	| 	fop lparen afcn rparen  
	| 	fbop lparen afcn comma afcn rparen	
;
aggr:		aop lparen afcn qualclause rparen	
;
aggrfcn: 	aop lparen afcn by aseq qualclause rparen
;
attribfcn:	I2CONST	= 
			w_con(I2CONST, $1->d_elm);
	| 	I4CONST	= 
			w_con(I4CONST, $1->d_elm);
	|	F8CONST	= 
			w_con(F8CONST, $1->d_elm);
	| 	SCONST	= 
			w_con(SCONST, $1->d_elm);
	|	cvar
	| 	attrib	
;
aseq:		aseq comma afcn  
	|	afcn	
;
attrib:		id period id	
	|	id period all=
		{
			if (Opflag != mdVIEW && Opflag != mdRETRIEVE
			   && Opflag != mdAPPEND)
				yyserror(
				"'all' applied to this range variable illegal in this kind of statement",
				$1);
		}
;
lbop:		LBOP	= 
			w_key($1->d_elm);
;
luop:		LUOP	= 
			w_key($1->d_elm);
;
bdop:		BDOP	= 
			w_op($1->d_elm);
;
rop:		EOP	= 
			w_op($1->d_elm);
	| 	BDOP	= 
			w_op($1->d_elm);
	| 	IS	= 
			w_op("=");
;
uop:		UOP = 
			w_op($1->d_elm);
;
fop:		FOP = 
			w_key($1->d_elm);
;
fbop:		FBOP = 
			w_key($1->d_elm);
;
bop:		BOP = 
			w_op($1->d_elm);
	| 	UOP = 
			w_op($1->d_elm);
;
by:		BY =
			w_key($1->d_elm);
;
aop:		AOP = 
			w_key($1->d_elm);
;
	
/*
 * Keywords
 */

append_p_key:	PARAM APPEND =
		{
			begin_quote();
			w_key($2->d_elm);
			Opflag = mdAPPEND;
		}
;
append_key:	APPEND	=
		{
			Opflag = mdAPPEND;
			begin_quote();
			w_key($1->d_elm);
		}
;
copy_key:	COPY	=
		{
			Opflag = mdCOPY;
			begin_quote();
			w_key($1->d_elm);
		}
;
copy_p_key:	PARAM COPY =
		{
			Opflag = mdCOPY;
			begin_quote();
			w_key($2->d_elm);
		}
;
cp_kword:	INTO	= 
		{
			w_key($1->d_elm);
			Opflag = mdFILENAME;
		}
	| 	FROM	= 
		{
			w_key($1->d_elm);
			Opflag = mdFILENAME;
		}
;
create_key:	CREATE	=
		{
			Opflag = mdCREATE;
			begin_quote();
			w_key($1->d_elm);
		}
;
create_p_key:	PARAM CREATE =
		{
			Opflag = mdCREATE;
			begin_quote();
			w_key($2->d_elm);
		}
;
define_key:	DEFINE =
		{
			Opflag = mdDEFINE;
			begin_quote();
			w_key($1->d_elm);
		}
;
delete_key:	DELETE	=
		{
			Opflag = mdDELETE;
			begin_quote();
			w_key($1->d_elm);
		}
;
destroy_key:	DESTROY	=
		{
			Opflag = mdDESTROY;
			begin_quote();
			w_key($1->d_elm);
		}
;
help_key:	HELP =
		{
			Opflag = mdHELP;
			begin_quote();
			w_key($1->d_elm);
		}
;
index_key:	INDEX ON =
		{
			Opflag = mdINDEX;
			begin_quote();
			w_key($1->d_elm);
			w_key($2->d_elm);
		}
;
ingres_key:	INGRES	=
		{
			Opflag = mdINGRES;
			w_new("IIingres(");
		}
;
integ_key:	INTEGRITY=
		{
			if (Opflag == mdDEFINE)
				Opflag = mdINTEGRITY;
			w_key($1->d_elm);
		}
;
is_key:		IS =
		{ 
			if (Opflag == mdCTLELM)
				Opflag = mdTUPRET;
			w_op("=");
		}
	| 	BY = 
			w_key($1->d_elm);
;
modify_key:	MODIFY	=
		{
			Opflag = mdMODIFY;
			begin_quote();
			w_key($1->d_elm);
		}
;
permit_key:	PERMIT=
		{
			if (Opflag == mdDEFINE)
				Opflag = mdINTEGRITY;
			w_key($1->d_elm);
		}
;
print_key:	PRINT	=  
		{ 
			Opflag = mdPRINT;
			begin_quote();
			w_key($1->d_elm);
		}
;
range_of:	RANGE OF = 
		{
			Opflag = mdRANGE;
			begin_quote();
			w_key($1->d_elm);
			w_key($2->d_elm);
		}
;
replace_key:	REPLACE	=
		{
			Opflag = mdREPLACE;
			begin_quote();
			w_key($1->d_elm);
		}
;
replace_p_key:	PARAM REPLACE =
		{
			begin_quote();
			Opflag = mdREPLACE;
			w_key($2->d_elm);
		}
;
retrieve_key:	RETRIEVE	=
		{
			Opflag = mdRETRIEVE;
			begin_quote();
			w_key($1->d_elm);
		}
;
retrieve_p_key: PARAM RETRIEVE =
		{
			Opflag = mdRETRIEVE;
			begin_quote();
			w_key($2->d_elm);
		}
;
save_key:	SAVE	=
		{
			Opflag = mdSAVE;
			begin_quote();
			w_key($1->d_elm);
		}
;
struct_key: 	STRUCT =
		{
			Opflag = mdDECL;
			Struct_flag = 1;
			w_key($1->d_elm);
		}
;
tupret_p_key:	PARAM RETRIEVE =
		{
			begin_quote();
			w_key($2->d_elm);
			Opflag = mdTUPRET;
		}
;
view_key:	VIEW =
		{
			if (Opflag == mdDEFINE)
				Opflag = mdVIEW;
			w_key($1->d_elm);
		}
;

/*
 * Noise words and punctuation
 */

all:		ALL=
			w_key($1->d_elm);
;
apkword:	INTO	
	|	ONTO	
	|	TO	
	|	ON	
	|	;
;
at:		AT =
			w_key($1->d_elm);
;
colon:		COLON =
			w_op($1->d_elm);
;
comma:		COMMA	= 
			w_op($1->d_elm);
;
delnoise:	IN	
	|	ON	
	|	FROM	
	|	;
;
from:		FROM =
			w_key($1->d_elm);
;
integnoise:	ON
	|	ONTO
	|	IN
	|	OF
;
is:		IS	= 
			w_op("=");
;
isnoise:	IS
	|	;
;
lbrace:		LBRACE =
			w_op($1->d_elm);
;
lparen:		LPAREN	= 
			w_op($1->d_elm);
;
of:		OF=
			w_key($1->d_elm);
;
on:		ON =
			w_key($1->d_elm);
;
period:		PERIOD =
			w_op($1->d_elm);
;
repkword:	INTO	
	|	IN	
	|	ON	
	|	;
;
rparen:		RPAREN	=
			w_op($1->d_elm);
;
to:		TO = 
			w_key($1->d_elm);
;
retkword:	INTO	
	|	TO	
	|	;
;
until:		UNTIL	= 
			w_key($1->d_elm);
;
where:		WHERE	= 
			w_key($1->d_elm);
;
%%

# include	"tokens.y"
