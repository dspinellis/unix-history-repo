/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit at York University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdd.h	5.2 (Berkeley) %G%
 */

/*
 * standard defines
 */

#define max(a,b) ((a) > (b)? (a): (b))
#define min(a,b) ((a) < (b)? (a): (b))

#define iswhite(c) ((c) == ' ' || (c) == '\t')

/* 
 * STREQ is an optimised strcmp(a,b)==0 
 * STREQN is an optimised strncmp(a,b,n)==0; assumes n > 0 
 */
#define STREQ(a, b) ((a)[0] == (b)[0] && strcmp(a, b) == 0)
#define STREQN(a, b, n) ((a)[0] == (b)[0] && strncmp(a, b, n) == 0)

#define YES 1
#define NO 0
