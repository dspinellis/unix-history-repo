/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)char.c	5.1 (Berkeley) %G%";
#endif not lint

#include "sh.char.h"

char _cmap[256] = {
/*	nul		soh		stx		etx	*/
	0,		0,		0,		0,

/*	eot		enq		ack		bel	*/
	0,		0,		0,		0,

/*	bs		ht		nl		vt	*/
	0,		_SP|_META,	_NL|_META,	0,

/*	np		cr		so		si	*/
	0,		0,		0,		0,

/*	dle		dc1		dc2		dc3	*/
	0,		0,		0,		0,

/*	dc4		nak		syn		etb	*/
	0,		0,		0,		0,

/*	can		em		sub		esc	*/
	0,		0,		0,		0,

/*	fs		gs		rs		us	*/
	0,		0,		0,		0,

/*	sp		!		"		#	*/
	_SP|_META,	0,		_Q,		_META,

/*	$		%		&		'	*/
	_DOL,		0,		_META,		_Q,

/*	(		)		*		+	*/
	_META,		_META,		_GLOB,		0,

/*	,		-		.		/	*/
	0,		0,		0,		0,

/*	0		1		2		3	*/
	0,		0,		0,		0,

/*	4		5		6		7	*/
	0,		0,		0,		0,

/*	8		9		:		;	*/
	0,		0,		0,		_META,

/*	<		=		>		?	*/
	_META,		0,		_META,		_GLOB,

/*	@		A		B		C	*/
	0,		0,		0,		0,

/*	D		E		F		G	*/
	0,		0,		0,		0,

/*	H		I		J		K	*/
	0,		0,		0,		0,

/*	L		M		N		O	*/
	0,		0,		0,		0,

/*	P		Q		R		S	*/
	0,		0,		0,		0,

/*	T		U		V		W	*/
	0,		0,		0,		0,

/*	X		Y		Z		[	*/
	0,		0,		0,		_GLOB,

/*	\		]		^		_	*/
	_ESC,		0,		0,		0,

/*	`		a		b		c	*/
	_Q1|_GLOB,	0,		0,		0,

/*	d		e		f		g	*/
	0,		0,		0,		0,

/*	h		i		j		k	*/
	0,		0,		0,		0,

/*	l		m		n		o	*/
	0,		0,		0,		0,

/*	p		q		r		s	*/
	0,		0,		0,		0,

/*	t		u		v		w	*/
	0,		0,		0,		0,

/*	x		y		z		{	*/
	0,		0,		0,		_GLOB,

/*	|		}		~		del	*/
	_META,		0,		0,		0,
};
