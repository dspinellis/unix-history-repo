/*
 * Copyright (c) 1980 Regents of the University of California.
 * Copyright (c) 1976 Board of Trustees of the University of Illinois.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley and the University
 * of Illinois, Urbana.  The name of either
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)indent_codes.h	5.5 (Berkeley) 6/29/88
 */

/*
FILE NAME:
	indent_codes.h

PURPOSE:
	This include file contains defines for codes used within indent.  They
	are here so that codes passed between and within routines can be
	referenced symbolically.

GLOBALS:
	No global variables, just a bunch of defines

FUNCTIONS:
	None
*/

#define newline		1
#define lparen		2
#define rparen		3
#define unary_op	4
#define binary_op	5
#define postop		6
#define question	7
#define casestmt	8
#define colon		9
#define semicolon	10
#define lbrace		11
#define rbrace		12
#define ident		13
#define comma		14
#define comment		15
#define swstmt		16
#define preesc		17
#define form_feed	18
#define decl		19
#define sp_paren	20
#define sp_nparen	21
#define ifstmt		22
#define whilestmt	23
#define forstmt		24
#define stmt		25
#define stmtl		26
#define elselit		27
#define dolit		28
#define dohead		29
#define ifhead		30
#define elsehead	31
#define period		32
