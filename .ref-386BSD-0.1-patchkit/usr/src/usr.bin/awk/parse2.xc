
/********************************************
parse2.xc
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	parse2.xc,v $
Revision 5.1  91/12/05  07:52:38  brennan
1.1 pre-release

*/

/* If using Berkeley yacc, we can put the parser table 
   memory to the zmalloc pool.  This is kind of ugly and
   with paged vm probably a nop, but for DOS and MINIX and ??
   it frees a considerably amount of memory.

   This file is part of parse.c via
      cat  y.tab.c  parse2.xc  > parse.c
*/

static struct yacc_mem   yacc_mem[] = 
{
0 , 0 ,  /* don't remove this */

#ifdef   YYBYACC
(PTR) yycheck, sizeof(yycheck)/ZBLOCKSZ,
(PTR) yytable, sizeof(yytable)/ZBLOCKSZ,
#ifndef  YYXBYACC  /* with xbyacc these are storage auto */
(PTR) yyvs , sizeof(yyvs)/ZBLOCKSZ,
(PTR) yyss, sizeof(yyss)/ZBLOCKSZ,
#endif
(PTR) yydefred, sizeof(yydefred)/ZBLOCKSZ,
(PTR) yydgoto, sizeof(yydgoto)/ZBLOCKSZ,
(PTR) yygindex, sizeof(yygindex)/ZBLOCKSZ,
(PTR) yylen, sizeof(yylen)/ZBLOCKSZ,
(PTR) yylhs, sizeof(yylhs)/ZBLOCKSZ,
(PTR) yyrindex, sizeof(yyrindex)/ZBLOCKSZ,
(PTR) yysindex, sizeof(yysindex)/ZBLOCKSZ,
#endif

0,0 } ;

struct yacc_mem  *yacc_memp = yacc_mem ;

