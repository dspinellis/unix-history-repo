/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Apush.c	7.1 (Berkeley) %G%
 */

#include	"align.h"

push (infop,longword)	process_info *infop;
int	longword;
/*
/*	Push the given datum on the current stack.
/*
/******************************************/
{

	struct oprnd temp;

	temp.mode = Add | W; 
	sp -= 4; 
	temp.address = sp; 
	temp.length = 4;
	write_back(infop,longword, &temp) ;
}
