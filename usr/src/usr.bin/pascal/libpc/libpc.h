/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)libpc.h	1.9 (Berkeley) %G%
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
