/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)hp2648.h	5.1 (Berkeley) 5/7/85
 *
 *
 * Displays plot files on an hp2648a graphics terminals.  I have heard
 * that all hp plotting devices use the same control sequences, so this
 * might work for all hp devices capable of plotting.
 */

#include <stdio.h>

#define TERMINAL "/dev/tty"

#define	ENQ	05
#define ACK	06
#define ESC	033
#define GRAPHIC	'*'
#define MODE	'm'
#define PLOT	'p'
#define DISPLAY 'd'
#define PENUP	'a'
#define BINARY	'i'
#define ASCII	'f'
#define CR	'\n'

#define TRUE  1
#define FALSE 0

#define xsc(xi) ((int) (xi - lowx) * scalex + 0.5)
#define ysc(yi) ((int) (yi - lowy) * scaley + 0.5)

extern int shakehands;
extern int currentx;
extern int currenty;
extern int buffcount;
extern int fildes;
extern float lowx;
extern float lowy;
extern float scalex;
extern float scaley;
extern struct sgttyb sarg;
