/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * The game adventure was originally written in Fortran by Will Crowther
 * and Don Woods.  It was later translated to C and enhanced by Jim
 * Gillogly.  This code is derived from software contributed to Berkeley
 * by Jim Gillogly at The Rand Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)hdr.h	8.1 (Berkeley) %G%
 */

/*   ADVENTURE -- Jim Gillogly, Jul 1977
 * This program is a re-write of ADVENT, written in FORTRAN mostly by
 * Don Woods of SAIL.  In most places it is as nearly identical to the
 * original as possible given the language and word-size differences.
 * A few places, such as the message arrays and travel arrays were changed
 * to reflect the smaller core size and word size.  The labels of the
 * original are reflected in this version, so that the comments of the
 * fortran are still applicable here.
 *
 * The data file distributed with the fortran source is assumed to be called
 * "glorkz" in the directory where the program is first run.
 */

/* hdr.h: included by c advent files */

int datfd;                              /* message file descriptor      */
int delhit;
int yea;
extern char data_file[];                /* Virtual data file            */

#define TAB     011
#define LF      012
#define FLUSHLINE while (getchar()!='\n')
#define FLUSHLF   while (next()!=LF)

int loc,newloc,oldloc,oldlc2,wzdark,gaveup,kq,k,k2;
char *wd1,*wd2;                         /* the complete words           */
int verb,obj,spk;
extern int blklin;
int saved,savet,mxscor,latncy;

#define SHORT 50                        /* How short is a demo game?    */

#define MAXSTR  20                      /* max length of user's words   */

#define HTSIZE  512                     /* max number of vocab words    */
struct hashtab                          /* hash table for vocabulary    */
{       int val;                        /* word type &index (ktab)      */
	char *atab;                     /* pointer to actual string     */
} voc[HTSIZE];

#define SEED 1815622                    /* "Encryption" seed            */

struct text
#ifdef OLDSTUFF
{       int seekadr;                    /* DATFILE must be < 2**16      */
#endif OLDSTUFF
{       char *seekadr;                  /* Msg start in virtual disk    */
	int txtlen;                     /* length of msg starting here  */
};

#define RTXSIZ  205
struct text rtext[RTXSIZ];              /* random text messages         */

#define MAGSIZ  35
struct text mtext[MAGSIZ];              /* magic messages               */

int clsses;
#define CLSMAX  12
struct text ctext[CLSMAX];              /* classes of adventurer        */
int cval[CLSMAX];

struct text ptext[101];                 /* object descriptions          */

#define LOCSIZ  141                     /* number of locations          */
struct text ltext[LOCSIZ];              /* long loc description         */
struct text stext[LOCSIZ];              /* short loc descriptions       */

struct travlist                         /* direcs & conditions of travel*/
{       struct travlist *next;          /* ptr to next list entry       */
	int conditions;                 /* m in writeup (newloc / 1000) */
	int tloc;                       /* n in writeup (newloc % 1000) */
	int tverb;                      /* the verb that takes you there*/
} *travel[LOCSIZ],*tkk;                 /* travel is closer to keys(...)*/

int atloc[LOCSIZ];

int  plac[101];                         /* initial object placement     */
int  fixd[101],fixed[101];              /* location fixed?              */

int actspk[35];                         /* rtext msg for verb <n>       */

int cond[LOCSIZ];                       /* various condition bits       */

extern int setbit[16];                  /* bit defn masks 1,2,4,...     */

int hntmax;
int hints[20][5];                       /* info on hints                */
int hinted[20],hintlc[20];

int place[101], prop[101],link[201];
int abb[LOCSIZ];

int maxtrs,tally,tally2;                /* treasure values              */

#define FALSE   0
#define TRUE    1

int keys,lamp,grate,cage,rod,rod2,steps,/* mnemonics                    */
	bird,door,pillow,snake,fissur,tablet,clam,oyster,magzin,
	dwarf,knife,food,bottle,water,oil,plant,plant2,axe,mirror,dragon,
	chasm,troll,troll2,bear,messag,vend,batter,
	nugget,coins,chest,eggs,tridnt,vase,emrald,pyram,pearl,rug,chain,
	spices,
	back,look,cave,null,entrnc,dprssn,
	enter, stream, pour,
	say,lock,throw,find,invent;

int chloc,chloc2,dseen[7],dloc[7],      /* dwarf stuff                  */
	odloc[7],dflag,daltlc;

int tk[21],stick,dtotal,attack;
int turns,lmwarn,iwest,knfloc,detail,   /* various flags & counters     */
	abbnum,maxdie,numdie,holdng,dkill,foobar,bonus,clock1,clock2,
	saved,closng,panic,closed,scorng;

int demo,newloc,limit;

char *malloc();
char *decr();
unsigned long crc();

/* We need to get a little tricky to avoid strings */
#define DECR(a,b,c,d,e) decr('a'+'+','b'+'-','c'+'#','d'+'&','e'+'%')
