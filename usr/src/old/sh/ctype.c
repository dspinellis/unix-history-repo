#ifndef lint
static char sccsid[] = "@(#)ctype.c	4.3 %G%";
#endif

#
/*
 *	UNIX shell
 *
 *	S. R. Bourne
 *	Bell Telephone Laboratories
 *
 */

#include	"defs.h"

char	_ctype1[] = {
/*	000	001	002	003	004	005	006	007	*/
	_EOF,	0,	0,	0,	0,	0,	0,	0,

/*	bs	ht	nl	vt	np	cr	so	si	*/
	0,	_TAB,	_EOR,	0,	0,	0,	0,	0,

	0,	0,	0,	0,	0,	0,	0,	0,

	0,	0,	0,	0,	0,	0,	0,	0,

/*	sp	!	"	#	$	%	&	'	*/
	_SPC,	0,	_DQU,	0,	_DOL1,	0,	_AMP,	0,

/*	(	)	*	+	,	-	.	/	*/
	_BRA,	_KET,	0,	0,	0,	0,	0,	0,

/*	0	1	2	3	4	5	6	7	*/
	0,	0,	0,	0,	0,	0,	0,	0,

/*	8	9	:	;	<	=	>	?	*/
	0,	0,	0,	_SEM,	_LT,	0,	_GT,	0,

/*	@	A	B	C	D	E	F	G	*/
	0,	0,	0,	0,	0,	0,	0,	0,

/*	H	I	J	K	L	M	N	O	*/
	0,	0,	0,	0,	0,	0,	0,	0,

/*	P	Q	R	S	T	U	V	W	*/
	0,	0,	0,	0,	0,	0,	0,	0,

/*	X	Y	Z	[	\	]	^	_	*/
	0,	0,	0,	0,	_BSL,	0,	_HAT,	0,

/*	`	a	b	c	d	e	f	g	*/
	_LQU,	0,	0,	0,	0,	0,	0,	0,

/*	h	i	j	k	l	m	n	o	*/
	0,	0,	0,	0,	0,	0,	0,	0,

/*	p	q	r	s	t	u	v	w	*/
	0,	0,	0,	0,	0,	0,	0,	0,

/*	x	y	z	{	|	}	~	del	*/
	0,	0,	0,	0,	_BAR,	0,	0,	0
};


char	_ctype2[] = {
/*	000	001	002	003	004	005	006	007	*/
	0,	0,	0,	0,	0,	0,	0,	0,

/*	bs	ht	nl	vt	np	cr	so	si	*/
	0,	0,	0,	0,	0,	0,	0,	0,

	0,	0,	0,	0,	0,	0,	0,	0,

	0,	0,	0,	0,	0,	0,	0,	0,

/*	sp	!	"	#	$	%	&	'	*/
	0,	_PCS,	0,	_NUM,	_DOL2,	0,	0,	0,

/*	(	)	*	+	,	-	.	/	*/
	0,	0,	_AST,	_PLS,	0,	_MIN,	0,	0,

/*	0	1	2	3	4	5	6	7	*/
	_DIG,	_DIG,	_DIG,	_DIG,	_DIG,	_DIG,	_DIG,	_DIG,

/*	8	9	:	;	<	=	>	?	*/
	_DIG,	_DIG,	0,	0,	0,	_EQ,	0,	_QU,

/*	@	A	B	C	D	E	F	G	*/
	_AT,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,

/*	H	I	J	K	L	M	N	O	*/
	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,

/*	P	Q	R	S	T	U	V	W	*/
	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,	_UPC,

/*	X	Y	Z	[	\	]	^	_	*/
	_UPC,	_UPC,	_UPC,	_SQB,	0,	0,	0,	_UPC,

/*	`	a	b	c	d	e	f	g	*/
	0,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,

/*	h	i	j	k	l	m	n	o	*/
	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,

/*	p	q	r	s	t	u	v	w	*/
	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,

/*	x	y	z	{	|	}	~	del	*/
	_LPC,	_LPC,	_LPC,	_CBR,	0,	_CKT,	0,	0
};

