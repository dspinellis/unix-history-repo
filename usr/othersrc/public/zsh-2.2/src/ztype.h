/*
 *
 * ztype.h - character classification macros
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#define IDIGIT  1
#define IALNUM  2
#define IBLANK  4
#define INBLANK 8
#define ITOK    16
#define ISEP    32
#define IALPHA  64
#define IIDENT  128
#define IUSER   256
#define ICNTRL  512
#define IWORD	 1024
#define ISPECIAL 2048
#define _icom(X,Y) (typtab[(int) (unsigned char) (X)] & Y)
#define idigit(X) _icom(X,IDIGIT)
#define ialnum(X) _icom(X,IALNUM)
#define iblank(X) _icom(X,IBLANK)		/* blank, not including \n */
#define inblank(X) _icom(X,INBLANK)		/* blank or \n */
#define itok(X) _icom(X,ITOK)
#define isep(X) _icom(X,ISEP)
#define ialpha(X) _icom(X,IALPHA)
#define iident(X) _icom(X,IIDENT)
#define iuser(X) _icom(X,IUSER)			/* username char */
#define icntrl(X) _icom(X,ICNTRL)
#define iword(X) _icom(X,IWORD)
#define ispecial(X) _icom(X,ISPECIAL)

EXTERN short int typtab[256];

