/*	SC	A Spreadsheet Calculator
 *		Command and expression parser
 *
 *		original by James Gosling, September 1982
 *		modified by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 * 		more mods Robert Bond 12/86
 *
 *		More mods by Alan Silverstein, 3/88, see list of changes.
 *
 *		$Revision: 6.8 $
 */



%{
#include <curses.h>
#include "sc.h"

#define ENULL (struct enode *)0

char *strcpy();
%}

%union {
    int ival;
    double fval;
    struct ent_ptr ent;
    struct enode *enode;
    char *sval;
    struct range_s rval;
}

%type <ent> var
%type <fval> num
%type <rval> range
%type <rval> var_or_range
%type <sval> strarg
%type <enode> e term expr_list
%token <sval> STRING
%token <ival> NUMBER
%token <fval> FNUMBER
%token <rval> RANGE
%token <rval> VAR
%token <sval> WORD
%token <ival> COL
%token S_FORMAT
%token S_LABEL
%token S_LEFTSTRING
%token S_RIGHTSTRING
%token S_GET
%token S_PUT
%token S_MERGE
%token S_LET
%token S_WRITE
%token S_TBL
%token S_COPY
%token S_SHOW
%token S_ERASE
%token S_FILL
%token S_GOTO
%token S_DEFINE
%token S_UNDEFINE
%token S_VALUE
%token S_MDIR
%token S_HIDE
%token S_SET

%token K_FIXED
%token K_SUM
%token K_PROD
%token K_AVG
%token K_STDDEV
%token K_COUNT
%token K_ABS
%token K_ACOS
%token K_ASIN
%token K_ATAN
%token K_ATAN2
%token K_CEIL
%token K_COS
%token K_EXP
%token K_FABS
%token K_FLOOR
%token K_HYPOT
%token K_LN
%token K_LOG
%token K_PI
%token K_POW
%token K_SIN
%token K_SQRT
%token K_TAN
%token K_DTR
%token K_RTD
%token K_MAX
%token K_MIN
%token K_RND
%token K_ROUND
%token K_IF

%token K_PV
%token K_FV
%token K_PMT

%token K_HOUR
%token K_MINUTE
%token K_SECOND
%token K_MONTH
%token K_DAY
%token K_YEAR
%token K_NOW
%token K_DATE
%token K_DTS
%token K_TTS
%token K_FMT
%token K_SUBSTR
%token K_STON
%token K_EQS
%token K_EXT
%token K_NVAL
%token K_SVAL
%token K_LOOKUP
%token K_HLOOKUP
%token K_VLOOKUP
%token K_INDEX
%token K_STINDEX
%token K_AUTO
%token K_AUTOCALC
%token K_BYROWS
%token K_BYCOLS
%token K_BYGRAPH
%token K_ITERATIONS
%token K_NUMERIC
%token K_PRESCALE
%token K_EXTFUN
%token K_CELLCUR
%token K_TOPROW
%token K_TBLSTYLE
%token K_TBL
%token K_LATEX
%token K_TEX

%left '?' ':'
%left '|'
%left '&'
%nonassoc '<' '=' '>' '!'
%left '+' '-' '#'
%left '*' '/' '%'
%left '^'

%%
command:	S_LET var_or_range '=' e
				{ let($2.left.vp, $4); }
	|	S_LABEL var_or_range '=' e
				{ slet($2.left.vp, $4, 0); }
	|	S_LEFTSTRING var_or_range '=' e
				{ slet($2.left.vp, $4, -1); }
	|	S_RIGHTSTRING var_or_range '=' e
				{ slet($2.left.vp, $4, 1); }
	|	S_FORMAT COL ':' COL NUMBER NUMBER
				{ doformat($2,$4,$5,$6); }
	|	S_FORMAT COL NUMBER NUMBER
				{ doformat($2,$2,$3,$4); }
	|	S_GET strarg	{  /* This tmp hack is because readfile
				    * recurses back through yyparse. */
				  char *tmp;
				  tmp = $2;
				  readfile (tmp, 1);
				  xfree(tmp);
				}
	|	S_MERGE strarg	{
				  char *tmp;
				  tmp = $2;
				  readfile (tmp, 0);
				  xfree(tmp);
				}
	|	S_MDIR strarg	
				{ if (mdir) xfree(mdir); mdir = $2; }
	|       S_PUT strarg range
				{ (void) writefile($2, ($3.left.vp)->row, 
			 	($3.left.vp)->col, ($3.right.vp)->row,
			 	($3.right.vp)->col);
			 	xfree($2); }
	|	S_PUT strarg	
				{ (void) writefile ($2, 0, 0, maxrow, maxcol);
			 	xfree($2); }
	|       S_WRITE strarg range { (void) printfile($2, ($3.left.vp)->row, 
			 ($3.left.vp)->col, ($3.right.vp)->row,
			 ($3.right.vp)->col);
			 xfree($2); }
	|	S_WRITE strarg	{ (void) printfile ($2, 0, 0, maxrow, maxcol);
			 xfree($2); }
	|       S_TBL strarg range { (void) tblprintfile($2, ($3.left.vp)->row, 
			 ($3.left.vp)->col, ($3.right.vp)->row,
			 ($3.right.vp)->col);
			 xfree($2); }
	|	S_TBL strarg	{ (void)tblprintfile ($2, 0, 0, maxrow, maxcol);
			 xfree($2); }
	|       S_SHOW COL ':' COL
					{ showcol( $2, $4); }
	|       S_SHOW NUMBER ':' NUMBER
					{ showrow( $2, $4); }
 	|	S_HIDE COL
 					{ hide_col( $2 ); }
 	|	S_HIDE NUMBER
 					{ hide_row( $2 ); }
	|	S_COPY range var_or_range 
					{ copy($2.left.vp,$2.right.vp,
					$3.left.vp,$3.right.vp); }
	|	S_ERASE       
					{ eraser(lookat(showsr, showsc),
				        lookat(currow, curcol)); }
	|	S_ERASE var_or_range 
					{ eraser($2.left.vp, $2.right.vp); }
	|	S_VALUE       { valueize_area(showsr, showsc, currow, curcol);
				 modflg++; }
	|	S_VALUE var_or_range { valueize_area(($2.left.vp)->row,
				($2.left.vp)->col,
				($2.right.vp)->row,
				($2.right.vp)->col); modflg++; }
	|	S_FILL num num  { fill(lookat(showsr, showsc),
				      lookat(currow, curcol), $2, $3); }
	|	S_FILL var_or_range num num
				 { fill($2.left.vp, $2.right.vp, $3, $4); }
	|	S_GOTO var_or_range {moveto($2.left.vp->row, $2.left.vp->col);}
	|       S_GOTO num          {num_search($2);}
	|       S_GOTO STRING       {str_search($2);}
	|	S_GOTO              {go_last();}
	|	S_DEFINE strarg       { struct ent_ptr arg1, arg2;
					arg1.vp = lookat(showsr, showsc);
					arg1.vf = 0;
					arg2.vp = lookat(currow, curcol);
					arg2.vf = 0;
					add_range($2, arg1, arg2, 1); }

	|	S_DEFINE strarg range { add_range($2, $3.left, $3.right, 1); }
	|	S_DEFINE strarg var   { add_range($2, $3, $3, 0); }
	|	S_UNDEFINE var_or_range { del_range($2.left.vp, $2.right.vp); }
 	|	S_SET setlist
	|	/* nothing */
	|	error;

term: 		var		{ $$ = new_var('v', $1); }
	|	K_FIXED term	{ $$ = new ('f', ENULL, $2); }
	|       '@' K_SUM '(' var_or_range ')' 
				{ $$ = new_range(REDUCE | '+', $4); }
	|       '@' K_PROD '(' var_or_range ')' 
				{ $$ = new_range (REDUCE | '*', $4); }
	|       '@' K_AVG '(' var_or_range ')' 
				{ $$ = new_range (REDUCE | 'a', $4); }
	|       '@' K_STDDEV '(' var_or_range ')' 
				{ $$ = new_range (REDUCE | 's', $4); }
	|       '@' K_COUNT '(' var_or_range ')' 
				{ $$ = new_range (REDUCE | 'c', $4); }
	|       '@' K_MAX '(' var_or_range ')' 
				{ $$ = new_range (REDUCE | MAX, $4); }
	|	'@' K_MAX '(' e ',' expr_list ')'
				{ $$ = new(LMAX, $6, $4); }
	|       '@' K_MIN '(' var_or_range ')' 
				{ $$ = new_range (REDUCE | MIN, $4); }
	|	'@' K_MIN '(' e ',' expr_list ')'
				{ $$ = new(LMIN, $6, $4); }
	| '@' K_ABS '(' e ')'	 { $$ = new(ABS, ENULL, $4); }
	| '@' K_ACOS '(' e ')'	 { $$ = new(ACOS, ENULL, $4); }
	| '@' K_ASIN '(' e ')'	 { $$ = new(ASIN, ENULL, $4); }
	| '@' K_ATAN '(' e ')'	 { $$ = new(ATAN, ENULL, $4); }
	| '@' K_ATAN2 '(' e ',' e ')' { $$ = new(ATAN2, $4, $6); }
	| '@' K_CEIL '(' e ')'	 { $$ = new(CEIL, ENULL, $4); }
	| '@' K_COS '(' e ')'	 { $$ = new(COS, ENULL, $4); }
	| '@' K_EXP '(' e ')'	 { $$ = new(EXP, ENULL, $4); }
	| '@' K_FABS '(' e ')'	 { $$ = new(FABS, ENULL, $4); }
	| '@' K_FLOOR '(' e ')'	 { $$ = new(FLOOR, ENULL, $4); }
	| '@' K_HYPOT '(' e ',' e ')'	{ $$ = new(HYPOT, $4, $6); }
	| '@' K_LN '(' e ')'	 { $$ = new(LOG, ENULL, $4); }
	| '@' K_LOG '(' e ')'	 { $$ = new(LOG10, ENULL, $4); }
	| '@' K_POW '(' e ',' e ')'	{ $$ = new(POW, $4, $6); }
	| '@' K_SIN '(' e ')'	 { $$ = new(SIN, ENULL, $4); }
	| '@' K_SQRT '(' e ')'	 { $$ = new(SQRT, ENULL, $4); }
	| '@' K_TAN '(' e ')'	 { $$ = new(TAN, ENULL, $4); }
	| '@' K_DTR '(' e ')'	 { $$ = new(DTR, ENULL, $4); }
	| '@' K_RTD '(' e ')'	 { $$ = new(RTD, ENULL, $4); }
	| '@' K_RND '(' e ')'	 { $$ = new(RND, ENULL, $4); }
	| '@' K_ROUND '(' e ',' e ')'	 { $$ = new(ROUND, $4, $6); }
	| '@' K_IF  '(' e ',' e ',' e ')' { $$ = new(IF,  $4,new(',',$6,$8)); }

	| '@' K_PV  '(' e ',' e ',' e ')' { $$ = new(PV,  $4,new(':',$6,$8)); }
 	| '@' K_FV  '(' e ',' e ',' e ')' { $$ = new(FV,  $4,new(':',$6,$8)); }
 	| '@' K_PMT '(' e ',' e ',' e ')' { $$ = new(PMT, $4,new(':',$6,$8)); }
 
	| '@' K_HOUR '(' e ')' 	 { $$ = new(HOUR,ENULL, $4); }
	| '@' K_MINUTE '(' e ')' { $$ = new(MINUTE,ENULL, $4); }
	| '@' K_SECOND '(' e ')' { $$ = new(SECOND,ENULL, $4); }
	| '@' K_MONTH '(' e ')'	 { $$ = new(MONTH,ENULL,$4); }
	| '@' K_DAY '(' e ')'    { $$ = new(DAY, ENULL, $4); }
	| '@' K_YEAR '(' e ')'   { $$ = new(YEAR, ENULL, $4); }
	| '@' K_NOW              { $$ = new(NOW, ENULL, ENULL);}
	| '@' K_DTS '(' e ',' e ',' e ')'
		  		 { $$ = new(DTS, $4, new(',', $6, $8));}
	| '@' K_TTS '(' e ',' e ',' e ')'
		  		 { $$ = new(TTS, $4, new(',', $6, $8));}
	| '@' K_STON '(' e ')'   { $$ = new(STON, ENULL, $4); }
	| '@' K_EQS '(' e ',' e ')' { $$ = new (EQS, $4, $6); }
	| '@' K_DATE '(' e ')'	 { $$ = new(DATE, ENULL, $4); }
	| '@' K_FMT  '(' e ',' e ')' { $$ = new(FMT, $4, $6); }
	| '@' K_INDEX  '(' e ',' var_or_range ')'
		 { $$ = new(INDEX, $4, new_range(REDUCE | INDEX, $6)); }
	| '@' K_LOOKUP  '(' e ',' var_or_range ')'
		 { $$ = new(LOOKUP, $4, new_range(REDUCE | LOOKUP, $6)); }
	| '@' K_HLOOKUP  '(' e ',' var_or_range ',' e ')'
		 { $$ = new(HLOOKUP, new(',', $4, $8),
		    new_range(REDUCE | HLOOKUP, $6)); }
	| '@' K_VLOOKUP  '(' e ',' var_or_range ',' e ')'
		 { $$ = new(VLOOKUP, new(',', $4, $8),
		    new_range(REDUCE | VLOOKUP, $6)); }
	| '@' K_STINDEX  '(' e ',' var_or_range ')'
		 { $$ = new(STINDEX, $4, new_range(REDUCE | STINDEX, $6)); }
	| '@' K_EXT  '(' e ',' e ')' { $$ = new(EXT, $4, $6); }
	| '@' K_NVAL '(' e ',' e ')' { $$ = new(NVAL, $4, $6); }
	| '@' K_SVAL '(' e ',' e ')' { $$ = new(SVAL, $4, $6); }
	| '@' K_SUBSTR '(' e ',' e ',' e ')'
				 { $$ = new(SUBSTR, $4, new(',', $6, $8)); }
	|	'(' e ')'	 { $$ = $2; }
	|	'+' term	 { $$ = $2; }
	|	'-' term	 { $$ = new ('m', ENULL, $2); }
	|	NUMBER		 { $$ = new_const('k', (double) $1); }
	|	FNUMBER		 { $$ = new_const('k', $1); }
	|	K_PI	{ $$ = new_const('k', (double)3.14159265358979323846); }
	|	STRING	         { $$ = new_str($1); }
	|	'~' term	 { $$ = new ('~', ENULL, $2); }
	|	'!' term	 { $$ = new ('~', ENULL, $2); }
	;

e:		e '+' e		{ $$ = new ('+', $1, $3); }
	|	e '-' e		{ $$ = new ('-', $1, $3); }
	|	e '*' e		{ $$ = new ('*', $1, $3); }
	|	e '/' e		{ $$ = new ('/', $1, $3); }
	|	e '%' e		{ $$ = new ('%', $1, $3); }
	|	e '^' e		{ $$ = new ('^', $1, $3); }
	|	term
	|	e '?' e ':' e	{ $$ = new ('?', $1, new(':', $3, $5)); }
	|	e '<' e		{ $$ = new ('<', $1, $3); }
	|	e '=' e		{ $$ = new ('=', $1, $3); }
	|	e '>' e		{ $$ = new ('>', $1, $3); }
	|	e '&' e		{ $$ = new ('&', $1, $3); }
	|	e '|' e		{ $$ = new ('|', $1, $3); }
	|	e '<' '=' e	{ $$ = new ('~', ENULL, new ('>', $1, $4)); }
	|	e '!' '=' e	{ $$ = new ('~', ENULL, new ('=', $1, $4)); }
	|	e '>' '=' e	{ $$ = new ('~', ENULL, new ('<', $1, $4)); }
	|	e '#' e		{ $$ = new ('#', $1, $3); }
	;

expr_list:	e		{ $$ = new(ELIST, ENULL, $1); }
	|	expr_list ',' e	{ $$ = new(ELIST, $1, $3); }
	;

range:		var ':' var	{ $$.left = $1; $$.right = $3; }
	| 	RANGE		{ $$ = $1; }
	;

var:		COL NUMBER	{ $$.vp = lookat($2 , $1); $$.vf = 0;}
	|	'$' COL NUMBER	{ $$.vp = lookat($3 , $2);
					$$.vf = FIX_COL;}
	|	COL '$' NUMBER	{ $$.vp = lookat($3 , $1);
					$$.vf = FIX_ROW;}
	|	'$' COL '$' NUMBER { $$.vp = lookat($4 , $2);
					$$.vf = FIX_ROW | FIX_COL;}
	|	VAR		{ $$ = $1.left; }
	;

var_or_range:	range		{ $$ = $1; }
	|	var		{ $$.left = $1; $$.right = $1; }
	;

num:		NUMBER		{ $$ = (double) $1; }
	|	FNUMBER		{ $$ = $1; }
	|	'-' num		{ $$ = -$2; }
	|	'+' num		{ $$ = $2; }
	;

strarg:		STRING		{ $$ = $1; }
	|	var		{
				    char *s, *s1;
				    s1 = $1.vp->label;
				    if (!s1)
					s1 = "NULL_STRING";
				    s = xmalloc((unsigned)strlen(s1)+1);
				    (void) strcpy(s, s1);
				    $$ = s;
				}
  	;

setlist :	
	|	setlist setitem
	;

setitem	:	K_AUTO		{ setauto(1); }
	|	K_AUTOCALC	{ setauto(1); }
	|	'~' K_AUTO	{ setauto(0); }
	|	'~' K_AUTOCALC	{ setauto(0); }
	|	'!' K_AUTO	{ setauto(0); }
	|	'!' K_AUTOCALC	{ setauto(0); }
	|	K_BYCOLS	{ setorder(BYCOLS); }
	|	K_BYROWS	{ setorder(BYROWS); }
	|	K_BYGRAPH	{ setorder(BYGRAPH); }
	|	K_NUMERIC	{ numeric = 1; }
	|	'!' K_NUMERIC	{ numeric = 0; }
	|	K_PRESCALE	{ prescale = 0.01; }
	|	'!' K_PRESCALE	{ prescale = 1.0; }
	|	K_EXTFUN	{ extfunc = 1; }
	|	'!' K_EXTFUN	{ extfunc = 0; }
	|	K_CELLCUR	{ showcell = 1; }
	|	'!' K_CELLCUR	{ showcell = 0; }
	|	K_TOPROW	{ showtop = 1; }
	|	'!' K_TOPROW	{ showtop = 0; }
	|	K_ITERATIONS '=' NUMBER	{ setiterations($3); }
	|	K_TBLSTYLE '=' NUMBER	{ tbl_style = $3; }
	|	K_TBLSTYLE '=' K_TBL	{ tbl_style = TBL; }
	|	K_TBLSTYLE '=' K_LATEX	{ tbl_style = LATEX; }
	|	K_TBLSTYLE '=' K_TEX	{ tbl_style = TEX; }
  	;
