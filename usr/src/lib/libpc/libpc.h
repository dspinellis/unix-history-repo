/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)libpc.h	1.9 (Berkeley) 4/9/90
 */

extern FILE *ACTFILE();
extern long *ADDT();
extern double ATAN();
extern long CARD();
extern char CHR();
extern long CLCK();
extern double COS();
extern long *CTTOT();
extern long ERROR();
extern int EXCEPT();
extern double EXP();
extern long EXPO();
extern char *FNIL();
extern struct formalrtn *FSAV();
extern struct iorec *GETNAME();
extern bool IN();
extern bool INCT();
extern double LN();
extern long MAX();
extern long *MULT();
extern char *NAM();
extern char *NIL();
extern long PRED();
extern struct iorec *PFCLOSE();
extern double RANDOM();
extern char READC();
extern long READ4();
extern long READE();
extern double READ8();
extern bool RELNE();
extern bool RELEQ();
extern bool RELSLT();
extern bool RELSLE();
extern bool RELSGT();
extern bool RELSGE();
extern bool RELTLT();
extern bool RELTLE();
extern bool RELTGT();
extern bool RELTGE();
extern long ROUND();
extern long RANG4();
extern long RSNG4();
extern long SCLCK();
extern long SEED();
extern double SIN();
extern double SQRT();
extern long SUBSC();
extern long SUBSCZ();
extern long *SUBT();
extern long SUCC();
extern struct seekptr TELL();
extern bool TEOF();
extern bool TEOLN();
extern long TRUNC();
extern struct iorec *UNIT();
