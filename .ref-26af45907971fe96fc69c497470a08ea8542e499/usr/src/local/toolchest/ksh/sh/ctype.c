/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)ctype.c	1.1 */
/*
 *	UNIX shell
 *
 *	S. R. Bourne
 *	AT&T Bell Laboratories
 *	Rewritten by David Korn
 *
 */

#include	"shtype.h"

/*
 * #define _XBAR _BAR
 * to make ^ a synonym for |.  (Strongly discouraged)
 */

#define _XBAR	0

const char	_ctype1[]=
{
 /*	000	001	002	003	004	005	006	007	*/
	_EOF,	0,	0,	0,	0,	0,	0,	0,

 /*	bs	ht	nl	vt	np	cr	so	si	*/
	0,	_TAB,	_EOR,	0,	0,	0,	0,	0,

	0,	0,	0,	0,	0,	0,	0,	0,

	0,	0,	0,	0,	0,	0,	0,	0,

 /*	sp	!	"	#	$	%	&	'	*/
	_SPC,	0,	_DQU,	0,	_DOL1,	0,	_AMP,	0,

 /*	(	)	*	+	,	-	.	/	*/
	_BRA,	_KET,	_AST1,	0,	0,	0,	0,	0,

 /*	0	1	2	3	4	5	6	7	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	8	9	:	;	<	=	>	?	*/
	0,	0,	0,	_SEM,	_LT,	0,	_GT,	_QU1,

 /*	@	A	B	C	D	E	F	G	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	H	I	J	K	L	M	N	O	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	P	Q	R	S	T	U	V	W	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	X	Y	Z	[	\	]	^	_	*/
	0,	0,	0,	T_EXP,	_BSL,	0,	_XBAR,	0,

 /*	`	a	b	c	d	e	f	g	*/
	_LQU,	0,	0,	0,	0,	0,	0,	0,

 /*	h	i	j	k	l	m	n	o	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	p	q	r	s	t	u	v	w	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	x	y	z	{	|	}	~	del	*/
	0,	0,	0,	0,	_BAR,	0,	0,	0,

 /*	200	201	202	203	204	205	206	207	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	210	211	212	213	214	215	216	217	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	220	221	222	223	224	225	226	227	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	230	231	232	233	234	235	236	237	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	240	241	242	243	244	245	246	247	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	250	251	252	253	254	255	256	257	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	260	261	262	263	264	265	266	267	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	270	271	272	273	274	275	276	277	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	300	301	302	303	304	305	306	307	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	310	311	312	313	314	315	316	317	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	320	321	322	323	324	325	326	327	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	330	331	332	333	334	335	336	337	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	340	341	342	343	344	345	346	347	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	350	351	352	353	354	355	356	357	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	360	361	362	363	364	365	366	367	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	370	371	372	373	374	375	376	377	*/
	0,	0,	0,	0,	0,	0,	0,	0
};


const char	_ctype2[]=
{
 /*	000	001	002	003	004	005	006	007	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	bs	ht	nl	vt	np	cr	so	si	*/
	0,	0,	0,	0,	0,	0,	0,	0,

	0,	0,	0,	0,	0,	0,	0,	0,

	0,	0,	0,	0,	0,	0,	0,	0,

 /*	sp	!	"	#	$	%	&	'	*/
	0,	_PCS,	0,	_NUM,	_DOL2,	_PLS,	0,	0,

 /*	(	)	*	+	,	-	.	/	*/
	_LPAR,	0,	_AST,	_PLS,	0,	_MIN,	0,	0,

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
	_UPC,	_UPC,	_UPC,	0,	0,	0,	0,	_ALP,

 /*	`	a	b	c	d	e	f	g	*/
	0,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,

 /*	h	i	j	k	l	m	n	o	*/
	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,

 /*	p	q	r	s	t	u	v	w	*/
	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,	_LPC,

 /*	x	y	z	{	|	}	~	del	*/
	_LPC,	_LPC,	_LPC,	_CBR,	0,	_CKT,	0,	0,

 /*	200	201	202	203	204	205	206	207	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	210	211	212	213	214	215	216	217	*/
	0,	0,	0,	0,	0,	0,	_SS2,	_SS3,

 /*	220	221	222	223	224	225	226	227	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	230	231	232	233	234	235	236	237	*/
	0,	0,	0,	0,	0,	0,	0,	0,

 /*	240	241	242	243	244	245	246	247	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	250	251	252	253	254	255	256	257	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	260	261	262	263	264	265	266	267	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	270	271	272	273	274	275	276	277	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	300	301	302	303	304	305	306	307	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	310	311	312	313	314	315	316	317	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	320	321	322	323	324	325	326	327	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	330	331	332	333	334	335	336	337	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	340	341	342	343	344	345	346	347	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	350	351	352	353	354	355	356	357	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	360	361	362	363	364	365	366	367	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,

 /*	370	371	372	373	374	375	376	377	*/
	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP,	_ALP
};

const char hdigits[] = "00112233445566778899aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ";
