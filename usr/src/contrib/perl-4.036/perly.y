/* $RCSfile: perly.y,v $$Revision: 4.0.1.6 $$Date: 1993/02/05 19:41:15 $
 *
 *    Copyright (c) 1991, Larry Wall
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 * $Log: perly.y,v $
 * Revision 4.0.1.6  1993/02/05  19:41:15  lwall
 * patch36: delete with parens dumped core
 *
 * Revision 4.0.1.5  92/06/11  21:12:50  lwall
 * patch34: expectterm incorrectly set to indicate start of program or block
 * 
 * Revision 4.0.1.4  92/06/08  17:33:25  lwall
 * patch20: one of the backdoors to expectterm was on the wrong reduction
 * 
 * Revision 4.0.1.3  92/06/08  15:18:16  lwall
 * patch20: an expression may now start with a bareword
 * patch20: relaxed requirement for semicolon at the end of a block
 * patch20: added ... as variant on ..
 * patch20: fixed double debug break in foreach with implicit array assignment
 * patch20: if {block} {block} didn't work any more
 * patch20: deleted some minor memory leaks
 * 
 * Revision 4.0.1.2  91/11/05  18:17:38  lwall
 * patch11: extra comma at end of list is now allowed in more places (Hi, Felix!)
 * patch11: once-thru blocks didn't display right in the debugger
 * patch11: debugger got confused over nested subroutine definitions
 * 
 * Revision 4.0.1.1  91/06/07  11:42:34  lwall
 * patch4: new copyright notice
 * 
 * Revision 4.0  91/03/20  01:38:40  lwall
 * 4.0 baseline.
 * 
 */

%{
#include "INTERN.h"
#include "perl.h"

/*SUPPRESS 530*/
/*SUPPRESS 593*/
/*SUPPRESS 595*/

STAB *scrstab;
ARG *arg4;	/* rarely used arguments to make_op() */
ARG *arg5;

%}

%start prog

%union {
    int	ival;
    char *cval;
    ARG *arg;
    CMD *cmdval;
    struct compcmd compval;
    STAB *stabval;
    FCMD *formval;
}

%token <ival> '{' ')'

%token <cval> WORD LABEL
%token <ival> APPEND OPEN SSELECT LOOPEX DOTDOT
%token <ival> USING FORMAT DO SHIFT PUSH POP LVALFUN
%token <ival> WHILE UNTIL IF UNLESS ELSE ELSIF CONTINUE SPLIT FLIST
%token <ival> FOR FILOP FILOP2 FILOP3 FILOP4 FILOP22 FILOP25
%token <ival> FUNC0 FUNC1 FUNC2 FUNC2x FUNC3 FUNC4 FUNC5 HSHFUN HSHFUN3
%token <ival> FLIST2 SUB FILETEST LOCAL DELETE
%token <ival> RELOP EQOP MULOP ADDOP PACKAGE AMPER
%token <formval> FORMLIST
%token <stabval> REG ARYLEN ARY HSH STAR
%token <arg> SUBST PATTERN
%token <arg> RSTRING TRANS

%type <ival> prog decl format remember crp
%type <cmdval> block lineseq line loop cond sideff nexpr else
%type <arg> expr sexpr cexpr csexpr term handle aryword hshword
%type <arg> texpr listop bareword
%type <cval> label
%type <compval> compblock

%nonassoc <ival> LISTOP
%left ','
%right '='
%right '?' ':'
%nonassoc DOTDOT
%left OROR
%left ANDAND
%left '|' '^'
%left '&'
%nonassoc EQOP
%nonassoc RELOP
%nonassoc <ival> UNIOP
%nonassoc FILETEST
%left LS RS
%left ADDOP
%left MULOP
%left MATCH NMATCH 
%right '!' '~' UMINUS
%right POW
%nonassoc INC DEC
%left '('

%% /* RULES */

prog	:	/* NULL */
		{
#if defined(YYDEBUG) && defined(DEBUGGING)
		    yydebug = (debug & 1);
#endif
		    expectterm = 2;
		}
	/*CONTINUED*/	lineseq
			{ if (in_eval)
				eval_root = block_head($2);
			    else
				main_root = block_head($2); }
	;

compblock:	block CONTINUE block
			{ $$.comp_true = $1; $$.comp_alt = $3; }
	|	block else
			{ $$.comp_true = $1; $$.comp_alt = $2; }
	;

else	:	/* NULL */
			{ $$ = Nullcmd; }
	|	ELSE block
			{ $$ = $2; }
	|	ELSIF '(' expr ')' compblock
			{ cmdline = $1;
			    $$ = make_ccmd(C_ELSIF,1,$3,$5); }
	;

block	:	'{' remember lineseq '}'
			{ $$ = block_head($3);
			  if (cmdline > (line_t)$1)
			      cmdline = $1;
			  if (savestack->ary_fill > $2)
			    restorelist($2);
			  expectterm = 2; }
	;

remember:	/* NULL */	/* in case they push a package name */
			{ $$ = savestack->ary_fill; }
	;

lineseq	:	/* NULL */
			{ $$ = Nullcmd; }
	|	lineseq line
			{ $$ = append_line($1,$2); }
	;

line	:	decl
			{ $$ = Nullcmd; }
	|	label cond
			{ $$ = add_label($1,$2); }
	|	loop	/* loops add their own labels */
	|	label ';'
			{ if ($1 != Nullch) {
			      $$ = add_label($1, make_acmd(C_EXPR, Nullstab,
				  Nullarg, Nullarg) );
			    }
			    else {
			      $$ = Nullcmd;
			      cmdline = NOLINE;
			    }
			    expectterm = 2; }
	|	label sideff ';'
			{ $$ = add_label($1,$2);
			  expectterm = 2; }
	;

sideff	:	error
			{ $$ = Nullcmd; }
	|	expr
			{ $$ = make_acmd(C_EXPR, Nullstab, $1, Nullarg); }
	|	expr IF expr
			{ $$ = addcond(
			       make_acmd(C_EXPR, Nullstab, Nullarg, $1), $3); }
	|	expr UNLESS expr
			{ $$ = addcond(invert(
			       make_acmd(C_EXPR, Nullstab, Nullarg, $1)), $3); }
	|	expr WHILE expr
			{ $$ = addloop(
			       make_acmd(C_EXPR, Nullstab, Nullarg, $1), $3); }
	|	expr UNTIL expr
			{ $$ = addloop(invert(
			       make_acmd(C_EXPR, Nullstab, Nullarg, $1)), $3); }
	;

cond	:	IF '(' expr ')' compblock
			{ cmdline = $1;
			    $$ = make_icmd(C_IF,$3,$5); }
	|	UNLESS '(' expr ')' compblock
			{ cmdline = $1;
			    $$ = invert(make_icmd(C_IF,$3,$5)); }
	|	IF block compblock
			{ cmdline = $1;
			    $$ = make_icmd(C_IF,cmd_to_arg($2),$3); }
	|	UNLESS block compblock
			{ cmdline = $1;
			    $$ = invert(make_icmd(C_IF,cmd_to_arg($2),$3)); }
	;

loop	:	label WHILE '(' texpr ')' compblock
			{ cmdline = $2;
			    $$ = wopt(add_label($1,
			    make_ccmd(C_WHILE,1,$4,$6) )); }
	|	label UNTIL '(' expr ')' compblock
			{ cmdline = $2;
			    $$ = wopt(add_label($1,
			    invert(make_ccmd(C_WHILE,1,$4,$6)) )); }
	|	label WHILE block compblock
			{ cmdline = $2;
			    $$ = wopt(add_label($1,
			    make_ccmd(C_WHILE, 1, cmd_to_arg($3),$4) )); }
	|	label UNTIL block compblock
			{ cmdline = $2;
			    $$ = wopt(add_label($1,
			    invert(make_ccmd(C_WHILE,1,cmd_to_arg($3),$4)) )); }
	|	label FOR REG '(' expr crp compblock
			{ cmdline = $2;
			    /*
			     * The following gobbledygook catches EXPRs that
			     * aren't explicit array refs and translates
			     *		foreach VAR (EXPR) {
			     * into
			     *		@ary = EXPR;
			     *		foreach VAR (@ary) {
			     * where @ary is a hidden array made by genstab().
			     * (Note that @ary may become a local array if
			     * it is determined that it might be called
			     * recursively.  See cmd_tosave().)
			     */
			    if ($5->arg_type != O_ARRAY) {
				scrstab = aadd(genstab());
				$$ = append_line(
				    make_acmd(C_EXPR, Nullstab,
				      l(make_op(O_ASSIGN,2,
					listish(make_op(O_ARRAY, 1,
					  stab2arg(A_STAB,scrstab),
					  Nullarg,Nullarg )),
					listish(make_list($5)),
					Nullarg)),
				      Nullarg),
				    wopt(over($3,add_label($1,
				      make_ccmd(C_WHILE, 0,
					make_op(O_ARRAY, 1,
					  stab2arg(A_STAB,scrstab),
					  Nullarg,Nullarg ),
					$7)))));
				$$->c_line = $2;
				$$->c_head->c_line = $2;
			    }
			    else {
				$$ = wopt(over($3,add_label($1,
				make_ccmd(C_WHILE,1,$5,$7) )));
			    }
			}
	|	label FOR '(' expr crp compblock
			{ cmdline = $2;
			    if ($4->arg_type != O_ARRAY) {
				scrstab = aadd(genstab());
				$$ = append_line(
				    make_acmd(C_EXPR, Nullstab,
				      l(make_op(O_ASSIGN,2,
					listish(make_op(O_ARRAY, 1,
					  stab2arg(A_STAB,scrstab),
					  Nullarg,Nullarg )),
					listish(make_list($4)),
					Nullarg)),
				      Nullarg),
				    wopt(over(defstab,add_label($1,
				      make_ccmd(C_WHILE, 0,
					make_op(O_ARRAY, 1,
					  stab2arg(A_STAB,scrstab),
					  Nullarg,Nullarg ),
					$6)))));
				$$->c_line = $2;
				$$->c_head->c_line = $2;
			    }
			    else {	/* lisp, anyone? */
				$$ = wopt(over(defstab,add_label($1,
				make_ccmd(C_WHILE,1,$4,$6) )));
			    }
			}
	|	label FOR '(' nexpr ';' texpr ';' nexpr ')' block
			/* basically fake up an initialize-while lineseq */
			{   yyval.compval.comp_true = $10;
			    yyval.compval.comp_alt = $8;
			    cmdline = $2;
			    $$ = append_line($4,wopt(add_label($1,
				make_ccmd(C_WHILE,1,$6,yyval.compval) ))); }
	|	label compblock	/* a block is a loop that happens once */
			{ $$ = add_label($1,make_ccmd(C_BLOCK,1,Nullarg,$2)); }
	;

nexpr	:	/* NULL */
			{ $$ = Nullcmd; }
	|	sideff
	;

texpr	:	/* NULL means true */
			{ (void)scanstr("1",SCAN_DEF); $$ = yylval.arg; }
	|	expr
	;

label	:	/* empty */
			{ $$ = Nullch; }
	|	LABEL
	;

decl	:	format
			{ $$ = 0; }
	|	subrout
			{ $$ = 0; }
	|	package
			{ $$ = 0; }
	;

format	:	FORMAT WORD '=' FORMLIST
			{ if (strEQ($2,"stdout"))
			    make_form(stabent("STDOUT",TRUE),$4);
			  else if (strEQ($2,"stderr"))
			    make_form(stabent("STDERR",TRUE),$4);
			  else
			    make_form(stabent($2,TRUE),$4);
			  Safefree($2); $2 = Nullch; }
	|	FORMAT '=' FORMLIST
			{ make_form(stabent("STDOUT",TRUE),$3); }
	;

subrout	:	SUB WORD block
			{ make_sub($2,$3);
			  cmdline = NOLINE;
			  if (savestack->ary_fill > $1)
			    restorelist($1); }
	;

package :	PACKAGE WORD ';'
			{ char tmpbuf[256];
			  STAB *tmpstab;

			  savehptr(&curstash);
			  saveitem(curstname);
			  str_set(curstname,$2);
			  sprintf(tmpbuf,"'_%s",$2);
			  tmpstab = stabent(tmpbuf,TRUE);
			  if (!stab_xhash(tmpstab))
			      stab_xhash(tmpstab) = hnew(0);
			  curstash = stab_xhash(tmpstab);
			  if (!curstash->tbl_name)
			      curstash->tbl_name = savestr($2);
			  curstash->tbl_coeffsize = 0;
			  Safefree($2); $2 = Nullch;
			  cmdline = NOLINE;
			  expectterm = 2;
			}
	;

cexpr	:	',' expr
			{ $$ = $2; }
	;

expr	:	expr ',' sexpr
			{ $$ = make_op(O_COMMA, 2, $1, $3, Nullarg); }
	|	sexpr
	;

csexpr	:	',' sexpr
			{ $$ = $2; }
	;

sexpr	:	sexpr '=' sexpr
			{   $1 = listish($1);
			    if ($1->arg_type == O_ASSIGN && $1->arg_len == 1)
				$1->arg_type = O_ITEM;	/* a local() */
			    if ($1->arg_type == O_LIST)
				$3 = listish($3);
			    $$ = l(make_op(O_ASSIGN, 2, $1, $3, Nullarg)); }
	|	sexpr POW '=' sexpr
			{ $$ = l(make_op(O_POW, 2, $1, $4, Nullarg)); }
	|	sexpr MULOP '=' sexpr
			{ $$ = l(make_op($2, 2, $1, $4, Nullarg)); }
	|	sexpr ADDOP '=' sexpr
			{ $$ = rcatmaybe(l(make_op($2, 2, $1, $4, Nullarg)));}
	|	sexpr LS '=' sexpr
			{ $$ = l(make_op(O_LEFT_SHIFT, 2, $1, $4, Nullarg)); }
	|	sexpr RS '=' sexpr
			{ $$ = l(make_op(O_RIGHT_SHIFT, 2, $1, $4, Nullarg)); }
	|	sexpr '&' '=' sexpr
			{ $$ = l(make_op(O_BIT_AND, 2, $1, $4, Nullarg)); }
	|	sexpr '^' '=' sexpr
			{ $$ = l(make_op(O_XOR, 2, $1, $4, Nullarg)); }
	|	sexpr '|' '=' sexpr
			{ $$ = l(make_op(O_BIT_OR, 2, $1, $4, Nullarg)); }


	|	sexpr POW sexpr
			{ $$ = make_op(O_POW, 2, $1, $3, Nullarg); }
	|	sexpr MULOP sexpr
			{ if ($2 == O_REPEAT)
			      $1 = listish($1);
			    $$ = make_op($2, 2, $1, $3, Nullarg);
			    if ($2 == O_REPEAT) {
				if ($$[1].arg_type != A_EXPR ||
				  $$[1].arg_ptr.arg_arg->arg_type != O_LIST)
				    $$[1].arg_flags &= ~AF_ARYOK;
			    } }
	|	sexpr ADDOP sexpr
			{ $$ = make_op($2, 2, $1, $3, Nullarg); }
	|	sexpr LS sexpr
			{ $$ = make_op(O_LEFT_SHIFT, 2, $1, $3, Nullarg); }
	|	sexpr RS sexpr
			{ $$ = make_op(O_RIGHT_SHIFT, 2, $1, $3, Nullarg); }
	|	sexpr RELOP sexpr
			{ $$ = make_op($2, 2, $1, $3, Nullarg); }
	|	sexpr EQOP sexpr
			{ $$ = make_op($2, 2, $1, $3, Nullarg); }
	|	sexpr '&' sexpr
			{ $$ = make_op(O_BIT_AND, 2, $1, $3, Nullarg); }
	|	sexpr '^' sexpr
			{ $$ = make_op(O_XOR, 2, $1, $3, Nullarg); }
	|	sexpr '|' sexpr
			{ $$ = make_op(O_BIT_OR, 2, $1, $3, Nullarg); }
	|	sexpr DOTDOT sexpr
			{ arg4 = Nullarg;
			  $$ = make_op(O_F_OR_R, 4, $1, $3, Nullarg);
			  $$[0].arg_flags |= $2; }
	|	sexpr ANDAND sexpr
			{ $$ = make_op(O_AND, 2, $1, $3, Nullarg); }
	|	sexpr OROR sexpr
			{ $$ = make_op(O_OR, 2, $1, $3, Nullarg); }
	|	sexpr '?' sexpr ':' sexpr
			{ $$ = make_op(O_COND_EXPR, 3, $1, $3, $5); }
	|	sexpr MATCH sexpr
			{ $$ = mod_match(O_MATCH, $1, $3); }
	|	sexpr NMATCH sexpr
			{ $$ = mod_match(O_NMATCH, $1, $3); }
	|	term
			{ $$ = $1; }
	;

term	:	'-' term %prec UMINUS
			{ $$ = make_op(O_NEGATE, 1, $2, Nullarg, Nullarg); }
	|	'+' term %prec UMINUS
			{ $$ = $2; }
	|	'!' term
			{ $$ = make_op(O_NOT, 1, $2, Nullarg, Nullarg); }
	|	'~' term
			{ $$ = make_op(O_COMPLEMENT, 1, $2, Nullarg, Nullarg);}
	|	term INC
			{ $$ = addflags(1, AF_POST|AF_UP,
			    l(make_op(O_ITEM,1,$1,Nullarg,Nullarg))); }
	|	term DEC
			{ $$ = addflags(1, AF_POST,
			    l(make_op(O_ITEM,1,$1,Nullarg,Nullarg))); }
	|	INC term
			{ $$ = addflags(1, AF_PRE|AF_UP,
			    l(make_op(O_ITEM,1,$2,Nullarg,Nullarg))); }
	|	DEC term
			{ $$ = addflags(1, AF_PRE,
			    l(make_op(O_ITEM,1,$2,Nullarg,Nullarg))); }
	|	FILETEST WORD
			{ opargs[$1] = 0;	/* force it special */
			    $$ = make_op($1, 1,
				stab2arg(A_STAB,stabent($2,TRUE)),
				Nullarg, Nullarg);
			    Safefree($2); $2 = Nullch;
			}
	|	FILETEST sexpr
			{ opargs[$1] = 1;
			    $$ = make_op($1, 1, $2, Nullarg, Nullarg); }
	|	FILETEST
			{ opargs[$1] = ($1 != O_FTTTY);
			    $$ = make_op($1, 1,
				stab2arg(A_STAB,
				  $1 == O_FTTTY?stabent("STDIN",TRUE):defstab),
				Nullarg, Nullarg); }
	|	LOCAL '(' expr crp
			{ $$ = l(localize(make_op(O_ASSIGN, 1,
				localize(listish(make_list($3))),
				Nullarg,Nullarg))); }
	|	'(' expr crp
			{ $$ = make_list($2); }
	|	'(' ')'
			{ $$ = make_list(Nullarg); }
	|	DO sexpr	%prec FILETEST
			{ $$ = make_op(O_DOFILE,2,$2,Nullarg,Nullarg);
			  allstabs = TRUE;}
	|	DO block	%prec '('
			{ $$ = cmd_to_arg($2); }
	|	REG	%prec '('
			{ $$ = stab2arg(A_STAB,$1); }
	|	STAR	%prec '('
			{ $$ = stab2arg(A_STAR,$1); }
	|	REG '[' expr ']'	%prec '('
			{ $$ = make_op(O_AELEM, 2,
				stab2arg(A_STAB,aadd($1)), $3, Nullarg); }
	|	HSH 	%prec '('
			{ $$ = make_op(O_HASH, 1,
				stab2arg(A_STAB,$1),
				Nullarg, Nullarg); }
	|	ARY 	%prec '('
			{ $$ = make_op(O_ARRAY, 1,
				stab2arg(A_STAB,$1),
				Nullarg, Nullarg); }
	|	REG '{' expr ';' '}'	%prec '('
			{ $$ = make_op(O_HELEM, 2,
				stab2arg(A_STAB,hadd($1)),
				jmaybe($3),
				Nullarg);
			    expectterm = FALSE; }
	|	'(' expr crp '[' expr ']'	%prec '('
			{ $$ = make_op(O_LSLICE, 3,
				Nullarg,
				listish(make_list($5)),
				listish(make_list($2))); }
	|	'(' ')' '[' expr ']'	%prec '('
			{ $$ = make_op(O_LSLICE, 3,
				Nullarg,
				listish(make_list($4)),
				Nullarg); }
	|	ARY '[' expr ']'	%prec '('
			{ $$ = make_op(O_ASLICE, 2,
				stab2arg(A_STAB,aadd($1)),
				listish(make_list($3)),
				Nullarg); }
	|	ARY '{' expr ';' '}'	%prec '('
			{ $$ = make_op(O_HSLICE, 2,
				stab2arg(A_STAB,hadd($1)),
				listish(make_list($3)),
				Nullarg);
			    expectterm = FALSE; }
	|	DELETE REG '{' expr ';' '}'	%prec '('
			{ $$ = make_op(O_DELETE, 2,
				stab2arg(A_STAB,hadd($2)),
				jmaybe($4),
				Nullarg);
			    expectterm = FALSE; }
	|	DELETE '(' REG '{' expr ';' '}' ')'	%prec '('
			{ $$ = make_op(O_DELETE, 2,
				stab2arg(A_STAB,hadd($3)),
				jmaybe($5),
				Nullarg);
			    expectterm = FALSE; }
	|	ARYLEN	%prec '('
			{ $$ = stab2arg(A_ARYLEN,$1); }
	|	RSTRING	%prec '('
			{ $$ = $1; }
	|	PATTERN	%prec '('
			{ $$ = $1; }
	|	SUBST	%prec '('
			{ $$ = $1; }
	|	TRANS	%prec '('
			{ $$ = $1; }
	|	DO WORD '(' expr crp
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_WORD,stabent($2,MULTI)),
				make_list($4),
				Nullarg); Safefree($2); $2 = Nullch;
			    $$->arg_flags |= AF_DEPR; }
	|	AMPER WORD '(' expr crp
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_WORD,stabent($2,MULTI)),
				make_list($4),
				Nullarg); Safefree($2); $2 = Nullch; }
	|	DO WORD '(' ')'
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_WORD,stabent($2,MULTI)),
				make_list(Nullarg),
				Nullarg);
			    Safefree($2); $2 = Nullch;
			    $$->arg_flags |= AF_DEPR; }
	|	AMPER WORD '(' ')'
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_WORD,stabent($2,MULTI)),
				make_list(Nullarg),
				Nullarg);
			    Safefree($2); $2 = Nullch;
			}
	|	AMPER WORD
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_WORD,stabent($2,MULTI)),
				Nullarg,
				Nullarg);
			    Safefree($2); $2 = Nullch;
			}
	|	DO REG '(' expr crp
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_STAB,$2),
				make_list($4),
				Nullarg);
			    $$->arg_flags |= AF_DEPR; }
	|	AMPER REG '(' expr crp
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_STAB,$2),
				make_list($4),
				Nullarg); }
	|	DO REG '(' ')'
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_STAB,$2),
				make_list(Nullarg),
				Nullarg);
			    $$->arg_flags |= AF_DEPR; }
	|	AMPER REG '(' ')'
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_STAB,$2),
				make_list(Nullarg),
				Nullarg); }
	|	AMPER REG
			{ $$ = make_op((perldb ? O_DBSUBR : O_SUBR), 2,
				stab2arg(A_STAB,$2),
				Nullarg,
				Nullarg); }
	|	LOOPEX
			{ $$ = make_op($1,0,Nullarg,Nullarg,Nullarg); }
	|	LOOPEX WORD
			{ $$ = make_op($1,1,cval_to_arg($2),
			    Nullarg,Nullarg); }
	|	UNIOP
			{ $$ = make_op($1,0,Nullarg,Nullarg,Nullarg); }
	|	UNIOP block
			{ $$ = make_op($1,1,cmd_to_arg($2),Nullarg,Nullarg); }
	|	UNIOP sexpr
			{ $$ = make_op($1,1,$2,Nullarg,Nullarg); }
	|	SSELECT
			{ $$ = make_op(O_SELECT, 0, Nullarg, Nullarg, Nullarg);}
	|	SSELECT  WORD
			{ $$ = make_op(O_SELECT, 1,
			    stab2arg(A_WORD,stabent($2,TRUE)),
			    Nullarg,
			    Nullarg);
			    Safefree($2); $2 = Nullch; }
	|	SSELECT '(' handle ')'
			{ $$ = make_op(O_SELECT, 1, $3, Nullarg, Nullarg); }
	|	SSELECT '(' sexpr csexpr csexpr csexpr ')'
			{ arg4 = $6;
			  $$ = make_op(O_SSELECT, 4, $3, $4, $5); }
	|	OPEN WORD	%prec '('
			{ $$ = make_op(O_OPEN, 2,
			    stab2arg(A_WORD,stabent($2,TRUE)),
			    stab2arg(A_STAB,stabent($2,TRUE)),
			    Nullarg);
			    Safefree($2); $2 = Nullch;
			}
	|	OPEN '(' WORD ')'
			{ $$ = make_op(O_OPEN, 2,
			    stab2arg(A_WORD,stabent($3,TRUE)),
			    stab2arg(A_STAB,stabent($3,TRUE)),
			    Nullarg);
			    Safefree($3); $3 = Nullch;
			}
	|	OPEN '(' handle cexpr ')'
			{ $$ = make_op(O_OPEN, 2,
			    $3,
			    $4, Nullarg); }
	|	FILOP '(' handle ')'
			{ $$ = make_op($1, 1,
			    $3,
			    Nullarg, Nullarg); }
	|	FILOP WORD
			{ $$ = make_op($1, 1,
			    stab2arg(A_WORD,stabent($2,TRUE)),
			    Nullarg, Nullarg);
			  Safefree($2); $2 = Nullch; }
	|	FILOP REG
			{ $$ = make_op($1, 1,
			    stab2arg(A_STAB,$2),
			    Nullarg, Nullarg); }
	|	FILOP '(' ')'
			{ $$ = make_op($1, 1,
			    stab2arg(A_WORD,Nullstab),
			    Nullarg, Nullarg); }
	|	FILOP	%prec '('
			{ $$ = make_op($1, 0,
			    Nullarg, Nullarg, Nullarg); }
	|	FILOP2 '(' handle cexpr ')'
			{ $$ = make_op($1, 2, $3, $4, Nullarg); }
	|	FILOP3 '(' handle csexpr cexpr ')'
			{ $$ = make_op($1, 3, $3, $4, make_list($5)); }
	|	FILOP22 '(' handle ',' handle ')'
			{ $$ = make_op($1, 2, $3, $5, Nullarg); }
	|	FILOP4 '(' handle csexpr csexpr cexpr ')'
			{ arg4 = $6; $$ = make_op($1, 4, $3, $4, $5); }
	|	FILOP25 '(' handle ',' handle csexpr csexpr cexpr ')'
			{ arg4 = $7; arg5 = $8;
			  $$ = make_op($1, 5, $3, $5, $6); }
	|	PUSH '(' aryword ',' expr crp
			{ $$ = make_op($1, 2,
			    $3,
			    make_list($5),
			    Nullarg); }
	|	POP aryword	%prec '('
			{ $$ = make_op(O_POP, 1, $2, Nullarg, Nullarg); }
	|	POP '(' aryword ')'
			{ $$ = make_op(O_POP, 1, $3, Nullarg, Nullarg); }
	|	SHIFT aryword	%prec '('
			{ $$ = make_op(O_SHIFT, 1, $2, Nullarg, Nullarg); }
	|	SHIFT '(' aryword ')'
			{ $$ = make_op(O_SHIFT, 1, $3, Nullarg, Nullarg); }
	|	SHIFT	%prec '('
			{ $$ = make_op(O_SHIFT, 1,
			    stab2arg(A_STAB,
			      aadd(stabent(subline ? "_" : "ARGV", TRUE))),
			    Nullarg, Nullarg); }
	|	SPLIT	%prec '('
			{   static char p[]="/\\s+/";
			    char *oldend = bufend;
			    ARG *oldarg = yylval.arg;
			    
			    bufend=p+5;
			    (void)scanpat(p);
			    bufend=oldend;
			    $$ = make_split(defstab,yylval.arg,Nullarg);
			    yylval.arg = oldarg; }
	|	SPLIT '(' sexpr csexpr csexpr ')'
			{ $$ = mod_match(O_MATCH, $4,
			  make_split(defstab,$3,$5));}
	|	SPLIT '(' sexpr csexpr ')'
			{ $$ = mod_match(O_MATCH, $4,
			  make_split(defstab,$3,Nullarg) ); }
	|	SPLIT '(' sexpr ')'
			{ $$ = mod_match(O_MATCH,
			    stab2arg(A_STAB,defstab),
			    make_split(defstab,$3,Nullarg) ); }
	|	FLIST2 '(' sexpr cexpr ')'
			{ $$ = make_op($1, 2,
			    $3,
			    listish(make_list($4)),
			    Nullarg); }
	|	FLIST '(' expr crp
			{ $$ = make_op($1, 1,
			    make_list($3),
			    Nullarg,
			    Nullarg); }
	|	LVALFUN sexpr	%prec '('
			{ $$ = l(make_op($1, 1, fixl($1,$2),
			    Nullarg, Nullarg)); }
	|	LVALFUN
			{ $$ = l(make_op($1, 1,
			    stab2arg(A_STAB,defstab),
			    Nullarg, Nullarg)); }
	|	FUNC0
			{ $$ = make_op($1, 0, Nullarg, Nullarg, Nullarg); }
	|	FUNC0 '(' ')'
			{ $$ = make_op($1, 0, Nullarg, Nullarg, Nullarg); }
	|	FUNC1 '(' ')'
			{ $$ = make_op($1, 0, Nullarg, Nullarg, Nullarg); }
	|	FUNC1 '(' expr ')'
			{ $$ = make_op($1, 1, $3, Nullarg, Nullarg); }
	|	FUNC2 '(' sexpr cexpr ')'
			{ $$ = make_op($1, 2, $3, $4, Nullarg);
			    if ($1 == O_INDEX && $$[2].arg_type == A_SINGLE)
				fbmcompile($$[2].arg_ptr.arg_str,0); }
	|	FUNC2x '(' sexpr csexpr ')'
			{ $$ = make_op($1, 2, $3, $4, Nullarg);
			    if ($1 == O_INDEX && $$[2].arg_type == A_SINGLE)
				fbmcompile($$[2].arg_ptr.arg_str,0); }
	|	FUNC2x '(' sexpr csexpr cexpr ')'
			{ $$ = make_op($1, 3, $3, $4, $5);
			    if ($1 == O_INDEX && $$[2].arg_type == A_SINGLE)
				fbmcompile($$[2].arg_ptr.arg_str,0); }
	|	FUNC3 '(' sexpr csexpr cexpr ')'
			{ $$ = make_op($1, 3, $3, $4, $5); }
	|	FUNC4 '(' sexpr csexpr csexpr cexpr ')'
			{ arg4 = $6;
			  $$ = make_op($1, 4, $3, $4, $5); }
	|	FUNC5 '(' sexpr csexpr csexpr csexpr cexpr ')'
			{ arg4 = $6; arg5 = $7;
			  $$ = make_op($1, 5, $3, $4, $5); }
	|	HSHFUN '(' hshword ')'
			{ $$ = make_op($1, 1,
				$3,
				Nullarg,
				Nullarg); }
	|	HSHFUN hshword
			{ $$ = make_op($1, 1,
				$2,
				Nullarg,
				Nullarg); }
	|	HSHFUN3 '(' hshword csexpr cexpr ')'
			{ $$ = make_op($1, 3, $3, $4, $5); }
	|	bareword
	|	listop
	;

listop	:	LISTOP
			{ $$ = make_op($1,2,
				stab2arg(A_WORD,Nullstab),
				stab2arg(A_STAB,defstab),
				Nullarg); }
	|	LISTOP expr
			{ $$ = make_op($1,2,
				stab2arg(A_WORD,Nullstab),
				maybelistish($1,make_list($2)),
				Nullarg); }
	|	LISTOP WORD
			{ $$ = make_op($1,2,
				stab2arg(A_WORD,stabent($2,TRUE)),
				stab2arg(A_STAB,defstab),
				Nullarg);
			    Safefree($2); $2 = Nullch;
			}
	|	LISTOP WORD expr
			{ $$ = make_op($1,2,
				stab2arg(A_WORD,stabent($2,TRUE)),
				maybelistish($1,make_list($3)),
				Nullarg); Safefree($2); $2 = Nullch; }
	|	LISTOP REG expr
			{ $$ = make_op($1,2,
				stab2arg(A_STAB,$2),
				maybelistish($1,make_list($3)),
				Nullarg); }
	|	LISTOP block expr
			{ $$ = make_op($1,2,
				cmd_to_arg($2),
				maybelistish($1,make_list($3)),
				Nullarg); }
	;

handle	:	WORD
			{ $$ = stab2arg(A_WORD,stabent($1,TRUE));
			  Safefree($1); $1 = Nullch;}
	|	sexpr
	;

aryword	:	WORD
			{ $$ = stab2arg(A_WORD,aadd(stabent($1,TRUE)));
			    Safefree($1); $1 = Nullch; }
	|	ARY
			{ $$ = stab2arg(A_STAB,$1); }
	;

hshword	:	WORD
			{ $$ = stab2arg(A_WORD,hadd(stabent($1,TRUE)));
			    Safefree($1); $1 = Nullch; }
	|	HSH
			{ $$ = stab2arg(A_STAB,$1); }
	;

crp	:	',' ')'
			{ $$ = 1; }
	|	')'
			{ $$ = 0; }
	;

/*
 * NOTE:  The following entry must stay at the end of the file so that
 * reduce/reduce conflicts resolve to it only if it's the only option.
 */

bareword:	WORD
			{ char *s;
			    $$ = op_new(1);
			    $$->arg_type = O_ITEM;
			    $$[1].arg_type = A_SINGLE;
			    $$[1].arg_ptr.arg_str = str_make($1,0);
			    for (s = $1; *s && isLOWER(*s); s++) ;
			    if (dowarn && !*s)
				warn(
				  "\"%s\" may clash with future reserved word",
				  $1 );
			    Safefree($1); $1 = Nullch;
			}
		;
%% /* PROGRAM */
