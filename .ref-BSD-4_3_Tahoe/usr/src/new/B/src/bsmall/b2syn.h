/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2syn.h,v 1.1 84/06/28 00:48:52 timo Exp $ */

/* bsyn.h: syntax */

/* General parsing routines */

#define Eotc '\0'
#define Eouc '\036'
		/* The character Eotc (end of text char)		*/
		/* is placed after the last read character in txbuf.	*/
		/* In general it follows a newline.			*/
		/* If Eotc is encountered and more input is required,	*/
		/* getline() is called.					*/
		/* Eouc (end of unit char) is similar except the system	*/
		/* never has to read beyond it.				*/

#define Char(tx) (*(tx))
#define Eol(tx) (Char(tx) == '\n' || Char(tx) == Eouc)
#define Ceol(tx) (Char(tx) == '\\' || Eol(tx))
#define To_eol(tx) while (!Eol(tx)) tx++;
#define Mark_unit_end(tx) *tx= Eouc;

#define Space(c) ((c) == ' ' || (c) == '\t')
#define Skipsp(tx) while(Space(Char(tx))) tx++

#define Letter(c) ('a'<=c&&c<='z')
#define Cap(c) ('A'<=c&&c<='Z')
#define Dig(c) ('0'<=c&&c<='9')
#define Keymark(c) (Cap(c) || Dig(c) || c=='\'' || c=='"')
#define Tagmark(c) (Letter(c) || Dig(c) || c=='\'' || c=='"')
#define Keytagmark(c) (Keymark(c) || Letter(c))
#define Anytormark(c) (c=='+' || c=='-' || c=='*' || c=='/' || c=='#')
#define Montormark(c) (c=='~' || Anytormark(c))
#define Dyatormark(c) (Anytormark(c) || c=='^' || c=='<' || c=='>')

/* Procedure upto(); */
/* Procedure need(); */
/* Procedure nothing(); */
/* Procedure thought(); */
/* Procedure findceol(); */
bool ateol();

value findkw();
value keyword();
/* Procedure reqkw(); */
/* Procedure req(); */
value tag();

bool atkw();
bool find();
intlet count();

txptr fcol();
txptr lcol();
extern intlet alino;
extern txptr txstart, txend; /*TEMPORARY if possible*/

/* Procedure getline(); */
intlet ilev();
/* Procedure veli(); */
/* Procedure inistreams(); */
/* Procedure re_streams(); */
/* Procedure open_stream(); */
/* Procedure close_stream(); */
