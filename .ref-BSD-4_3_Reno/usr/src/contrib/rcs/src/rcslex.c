/*
 *                     RCS file input
 */
#ifndef lint
static char rcsid[]= "$Id: rcslex.c,v 4.6 89/05/01 15:13:07 narten Exp $ Purdue CS";
#endif
/*********************************************************************************
 *                     Lexical Analysis.
 *                     Character mapping table,
 *                     hashtable, Lexinit, nextlex, getlex, getkey,
 *                     getid, getnum, readstring, printstring, savestring,
 *                     checkid, serror, fatserror, error, faterror, warn, diagnose
 *                     fflsbuf, puts, fprintf
 *                     Testprogram: define LEXDB
 *********************************************************************************
 */

/* Copyright (C) 1982, 1988, 1989 Walter Tichy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Walter Tichy.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Report all problems and direct all questions to:
 *   rcs-bugs@cs.purdue.edu
 * 







*/



/* $Log:	rcslex.c,v $
 * Revision 4.6  89/05/01  15:13:07  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.5  88/11/08  12:00:54  narten
 * changes from  eggert@sm.unisys.com (Paul Eggert)
 * 
 * Revision 4.5  88/08/28  15:01:12  eggert
 * Don't loop when writing error messages to a full filesystem.
 * Flush stderr/stdout when mixing output.
 * Yield exit status compatible with diff(1).
 * Shrink stdio code size; allow cc -R; remove lint.
 * 
 * Revision 4.4  87/12/18  11:44:47  narten
 * fixed to use "varargs" in "fprintf"; this is required if it is to
 * work on a SPARC machine such as a Sun-4
 * 
 * Revision 4.3  87/10/18  10:37:18  narten
 * Updating version numbers. Changes relative to 1.1 actually relative
 * to version 4.1
 * 
 * Revision 1.3  87/09/24  14:00:17  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:22:33  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:33  kcs
 * Initial revision
 * 
 * Revision 4.1  83/03/25  18:12:51  wft
 * Only changed $Header to $Id.
 * 
 * Revision 3.3  82/12/10  16:22:37  wft
 * Improved error messages, changed exit status on error to 1.
 *
 * Revision 3.2  82/11/28  21:27:10  wft
 * Renamed ctab to map and included EOFILE; ctab is now a macro in rcsbase.h.
 * Added fflsbuf(), fputs(), and fprintf(), which abort the RCS operations
 * properly in case there is an IO-error (e.g., file system full).
 *
 * Revision 3.1  82/10/11  19:43:56  wft
 * removed unused label out:;
 * made sure all calls to getc() return into an integer, not a char.
 */


/*
#define LEXDB
/* version LEXDB is for testing the lexical analyzer. The testprogram
 * reads a stream of lexemes, enters the revision numbers into the
 * hashtable, and prints the recognized tokens. Keywords are recognized
 * as identifiers.
 */



#include "rcsbase.h"
#include <varargs.h>



/* character mapping table */
enum tokens map[] = {
        EOFILE,         /* this will end up at ctab[-1] */
        UNKN,   INSERT, UNKN,   UNKN,   UNKN,   UNKN,   UNKN,   UNKN,
        UNKN,   SPACE,  NEWLN,  UNKN,   SPACE,  UNKN,   UNKN,   UNKN,
        UNKN,   UNKN,   UNKN,   UNKN,   UNKN,   UNKN,   UNKN,   UNKN,
        UNKN,   UNKN,   UNKN,   UNKN,   UNKN,   UNKN,   UNKN,   UNKN,
        SPACE,  EXCLA,  DQUOTE, HASH,   DOLLAR, PERCNT, AMPER,  SQUOTE,
        LPARN,  RPARN,  TIMES,  PLUS,   COMMA,  MINUS,  PERIOD, DIVIDE,
        DIGIT,  DIGIT,  DIGIT,  DIGIT,  DIGIT,  DIGIT,  DIGIT,  DIGIT,
        DIGIT,  DIGIT,  COLON,  SEMI,   LESS,   EQUAL,  GREAT,  QUEST,
        AT,     LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,
        LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,
        LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,
        LETTER, LETTER, LETTER, LBRACK, BACKSL, RBRACK, UPARR,  UNDER,
        ACCENT, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,
        LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,
        LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER, LETTER,
        LETTER, LETTER, LETTER, LBRACE, BAR,    RBRACE, TILDE,  UNKN
};




struct hshentry * nexthsh;  /*pointer to next hashtable-entry, set by lookup*/

enum tokens     nexttok;    /*next token, set by nextlex                    */

int             hshenter;   /*if true, next suitable lexeme will be entered */
                            /*into the symbol table. Handle with care.      */
int             nextc;      /*next input character, initialized by Lexinit  */

int             eof;        /*end-of-file indicator, set to >0 on end of file*/
int             line;       /*current line-number of input                  */
int             nerror;     /*counter for errors                            */
int             nwarn;      /*counter for warnings                          */
char *          cmdid;      /*command identification for error messages     */
int             quietflag;  /*indicates quiet mode                          */
FILE *          finptr;     /*input file descriptor                         */

FILE *          frewrite;   /*file descriptor for echoing input             */

int             rewriteflag;/*indicates whether to echo to frewrite         */

char            StringTab[strtsize]; /* string table and heap               */

char *          NextString;         /*pointer to next identifier in StringTab*/
char *          Topchar;            /*pointer to next free byte in StringTab*/
                                    /*set by nextlex, lookup                */
struct hshentry hshtab[hshsize];    /*hashtable                             */





lookup() {

/* Function: Looks up the character string pointed to by NextString in the
 * hashtable. If the string is not present, a new entry for it is created.
 * If the string is present, TopChar is moved back to save the space for
 * the string, and NextString is set to point to the original string.
 * In any case, the address of the corresponding hashtable entry is placed
 * into nexthsh.
 * Algorithm: Quadratic hash, covering all entries.
 * Assumptions: NextString points at the first character of the string.
 * Topchar points at the first empty byte after the string.
 */

        register int     ihash;      /* index into hashtable */
        register char    * sp, * np;
        int              c, delta, final, FirstScan; /*loop control*/

        /* calculate hash code */
        sp = NextString;
        ihash = 0;
        while (*sp) ihash += *sp++;

        /* set up first search loop (c=0,step=1,until (hshsiz-1)/2 */
        c=0;delta=1;final=(hshsize-1)/2;
        FirstScan=true;   /*first loop */

        for (;;) {
                ihash = (ihash+c)%hshsize;   /*next index*/

                if (hshtab[ihash].num == nil) {
                        /*empty slot found*/
                        hshtab[ihash].num = NextString;
                        nexthsh= &hshtab[ihash];/*save hashtable address*/
#                       ifdef LEXDB
                        VOID printf("\nEntered: %s at %d ",nexthsh->num, ihash);
#                       endif
                        return;
                }
                /* compare strings */
                sp=NextString;np=hshtab[ihash].num;
                while (*sp == *np++) {
                        if (*sp == 0) {
                                /* match found */
                                nexthsh= &hshtab[ihash];
                                Topchar = NextString;
                                NextString = nexthsh->num;
                                return;
                        } else sp++;
                }

                /* neither empty slot nor string found */
                /* calculate next index and repeat */
                if (c != final)
                        c += delta;
                else {
                        if (FirstScan) {
                                /*set up second sweep*/
                                delta = -1; final = 1; FirstScan= false;
                        } else {
                                fatserror("Hashtable overflow");
                        }
                }
        }
};






Lexinit()
/* Function: Initialization of lexical analyzer:
 * initializes the hastable,
 * initializes nextc, nexttok if finptr != NULL
 */
{       register int            c;

        for (c=hshsize-1; c>=0; c--) {
                hshtab[c].num = nil;
        }

        hshenter=true; eof=0; line=1; nerror=0; nwarn=0;
        NextString=nil; Topchar = &StringTab[0];
        if (finptr) {
                nextc = GETC(finptr,frewrite,rewriteflag); /*initial character*/
                nextlex();            /*initial token*/
        } else {
                nextc = '\0';
                nexttok=EOFILE;
        }
}







nextlex()

/* Function: Reads the next token and sets nexttok to the next token code.
 * Only if the hshenter==true, a revision number is entered into the
 * hashtable and a pointer to it is placed into nexthsh.
 * This is useful for avoiding that dates are placed into the hashtable.
 * For ID's and NUM's, NextString is set to the character string in the
 * string table. Assumption: nextc contains the next character.
 */
{       register c;
	register FILE * fin, * frew;
        register char * sp;
        register enum tokens d;

        if (eof) {
                nexttok=EOFILE;
                return;
        }
	fin=finptr; frew=frewrite;
loop:
        switch(nexttok=ctab[nextc]) {

        case UNKN:
        case IDCHAR:
        case PERIOD:
                serror("unknown Character: %c",nextc);
                nextc=GETC(fin,frew,rewriteflag);
                goto loop;

        case NEWLN:
                line++;
#               ifdef LEXDB
                VOID putchar('\n');
#               endif
                /* Note: falls into next case */

        case SPACE:
                nextc=GETC(fin,frew,rewriteflag);
                goto loop;

        case EOFILE:
                eof++;
                nexttok=EOFILE;
                return;

        case DIGIT:
                NextString = sp = Topchar;
                *sp++ = nextc;
                while ((d=ctab[c=GETC(fin,frew,rewriteflag)])==DIGIT ||
                        d==PERIOD) {
                        *sp++ = c;         /* 1.2. and 1.2 are different */
                }
                *sp++ = '\0';
                if (sp >= StringTab+strtsize) {
                        /*may have written outside stringtable already*/
                        fatserror("Stringtable overflow");
                }
                Topchar = sp;
                nextc = c;
                if (hshenter == true)
                        lookup();      /* lookup updates NextString, Topchar*/
                nexttok = NUM;
                return;


        case LETTER:
                NextString = sp = Topchar;
                *sp++ = nextc;
                while ((d=ctab[c=GETC(fin,frew,rewriteflag)])==LETTER ||
                        d==DIGIT || d==IDCHAR) {
                        *sp++ = c;
                }
                *sp++ = '\0';
                if (sp >= StringTab+strtsize) {
                        /*may have written outside stringtable already*/
                        fatserror("Stringtable overflow");
                }
                Topchar = sp;
                nextc = c;
                nexttok = ID;  /* may be ID or keyword */
                return;

        case SBEGIN: /* long string */
                nexttok = STRING;
                /* note: only the initial SBEGIN has been read*/
                /* read the string, and reset nextc afterwards*/
                return;

        default:
                nextc=GETC(fin,frew,rewriteflag);
                return;
        }
}


int getlex(token)
enum tokens token;
/* Function: Checks if nexttok is the same as token. If so,
 * advances the input by calling nextlex and returns true.
 * otherwise returns false.
 * Doesn't work for strings and keywords; loses the character string for ids.
 */
{
        if (nexttok==token) {
                nextlex();
                return(true);
        } else  return(false);
}

int getkey (key)
char * key;
/* Function: If the current token is a keyword identical to key,
 * getkey advances the input by calling nextlex and returns true;
 * otherwise returns false.
 */
{
        register char *s1,*s2;

        if (nexttok==ID) {
                s1=key; s2=NextString;
                while(*s1 == *s2++)
                     if (*s1++ == '\0') {
                         /* match found */
                         Topchar = NextString; /*reset Topchar */
                         nextlex();
                         return(true);
                     }
        }
        return(false);
}



char * getid()
/* Function: Checks if nexttok is an identifier. If so,
 * advances the input by calling nextlex and returns a pointer
 * to the identifier; otherwise returns nil.
 * Treats keywords as identifiers.
 */
{
        register char * name;
        if (nexttok==ID) {
                name = NextString;
                nextlex();
                return name;
        } else  return nil;
}


struct hshentry * getnum()
/* Function: Checks if nexttok is a number. If so,
 * advances the input by calling nextlex and returns a pointer
 * to the hashtable entry. Otherwise returns nil.
 * Doesn't work if hshenter is false.
 */
{
        register struct hshentry * num;
        if (nexttok==NUM) {
                num=nexthsh;
                nextlex();
                return num;
        } else  return nil;
}


readstring()
/* skip over characters until terminating single SDELIM        */
/* if rewriteflag==true, copy every character read to frewrite.*/
/* Does not advance nextlex at the end.                        */
{       register c;
	register FILE * fin,  * frew;
	fin=finptr; frew=frewrite;
        if (rewriteflag) {
                /* copy string verbatim to frewrite */
                while ((c=getc(fin)) != EOF) {
			VOID putc(c,frew);
                        if (c==SDELIM) {
                                if ((c=getc(fin)) == EOF || putc(c,frew) != SDELIM) {
                                        /* end of string */
                                        nextc=c;
                                        return;
                                }
                        }
                }
        } else {
                /* skip string */
                while ((c=getc(fin)) != EOF) {
                        if (c==SDELIM) {
                                if ((c=getc(fin)) != SDELIM) {
                                        /* end of string */
                                        nextc=c;
                                        return;
                                }
                        }
                }
        }
        nextc = c;
        error("Unterminated string");
}


printstring()
/* Function: copy a string to stdout, until terminated with a single SDELIM.
 * Does not advance nextlex at the end.
 */
{
        register c;
	register FILE * fin;
	fin=finptr;
	while ((c=getc(fin)) != EOF) {
                if (c==SDELIM) {
			if ((c=getc(fin)) != SDELIM) {
                                /* end of string */
                                nextc=c;
                                return;
                        }
                }
                VOID putchar(c);
        }
        nextc = c;
        error("Unterminated string");
}



savestring(target,length)
char * target; int length;
/* copies a string terminated with SDELIM from file finptr to buffer target,
 * but not more than length bytes. If the string is longer than length,
 * the extra characters are skipped. The string may be empty, in which
 * case a '\0' is placed into target.
 * Double SDELIM is replaced with SDELIM.
 * If rewriteflag==true, the string is also copied unchanged to frewrite.
 * Returns the length of the saved string.
 * Does not advance nextlex at the end.
 */
{
        register c;
	register FILE * fin, * frew;
        register char * tp, * max;

	fin=finptr; frew=frewrite;
        tp=target; max= target+length; /*max is one too large*/
        while ((c=GETC(fin,frew,rewriteflag))!=EOF) {
		*tp++ =c;
                if (c== SDELIM) {
                        if ((c=GETC(fin,frew,rewriteflag))!=SDELIM) {
                                /* end of string */
                                *(tp-1)='\0';
                                nextc=c;
                                return;
                        }
                }
                if (tp >= max) {
                        /* overflow */
                        error("string buffer overflow -- truncating string");
                        target[length-1]='\0';
                        /* skip rest of string */
                        while ((c=GETC(fin,frew,rewriteflag))!=EOF) {
                                if ((c==SDELIM) && ((c=GETC(fin,frew,rewriteflag))!=SDELIM)) {
                                        /* end of string */
                                        nextc=c;
                                        return;
                                }
                        }
                        nextc = c;
                        error("Can't find %c to terminate string before end of file",SDELIM);
                        return;
                }
        }
        nextc = c;
        error("Can't find %c to terminate string before end of file",SDELIM);
}


char  *checkid(id, delim)
char    *id, delim;
/*   Function:  check whether the string starting at id is an   */
/*              identifier and return a pointer to the last char*/
/*              of the identifer. White space, delim and '\0'   */
/*              are legal delimeters. Aborts the program if not */
/*              a legal identifier. Useful for checking commands*/
{
        register enum  tokens  d;
        register char    *temp;
        register char    c,tc;

        temp = id;
        if ( ctab[*id] == LETTER ) {
            while( (d=ctab[c=(*++id)]) == LETTER || d==DIGIT || d==IDCHAR) ;
            if ( c!=' ' && c!='\t' && c!='\n' && c!='\0' && c!=delim) {
                /* append \0 to end of id before error message */
                tc = c;
                while( (c=(*++id))!=' ' && c!='\t' && c!='\n' && c!='\0' && c!=delim) ;
                *id = '\0';
                faterror("Invalid character %c in identifier %s",tc,temp);
                return nil ;
            } else
                return id;
        } else {
            /* append \0 to end of id before error message */
            while( (c=(*++id))!=' ' && c!='\t' && c!='\n' && c!='\0' && c!=delim) ;
            *id = '\0';
            faterror("Identifier %s does not start with letter",temp);
            return nil;
        }
}

writeerror()
{
	static looping;
	if (looping)
		exit(2);
	looping = 1;
	faterror("write error");
}

nlflush(iop)
register FILE * iop;
{
	if (putc('\n',iop)==EOF || fflush(iop)==EOF)
                writeerror();
}


/*VARARGS1*/
serror(e,e1,e2,e3,e4,e5)
char * e, * e1, * e2, * e3, * e4, * e5;
/* non-fatal syntax error */
{       nerror++;
        VOID fprintf(stderr,"%s error, line %d: ", cmdid, line);
        VOID fprintf(stderr,e, e1, e2, e3, e4, e5);
        nlflush(stderr);
}

/*VARARGS1*/
error(e,e1,e2,e3,e4,e5)
char * e, * e1, * e2, * e3, * e4, * e5;
/* non-fatal error */
{       nerror++;
        VOID fprintf(stderr,"%s error: ",cmdid);
        VOID fprintf(stderr,e, e1, e2, e3, e4, e5);
        nlflush(stderr);
}

/*VARARGS1*/
fatserror(e,e1,e2,e3,e4,e5)
char * e, * e1, * e2, * e3, * e4, * e5;
/* fatal syntax error */
{       nerror++;
        VOID fprintf(stderr,"%s error, line %d: ", cmdid,line);
        VOID fprintf(stderr,e, e1, e2, e3, e4, e5);
        VOID fprintf(stderr,"\n%s aborted\n",cmdid);
        VOID cleanup();
        exit(2);
}

/*VARARGS1*/
faterror(e,e1,e2,e3,e4,e5)
char * e, * e1, * e2, * e3, * e4, * e5;
/* fatal error, terminates program after cleanup */
{       nerror++;
        VOID fprintf(stderr,"%s error: ",cmdid);
        VOID fprintf(stderr,e, e1, e2, e3, e4, e5);
        VOID fprintf(stderr,"\n%s aborted\n",cmdid);
        VOID cleanup();
        exit(2);
}

/*VARARGS1*/
warn(e,e1,e2,e3,e4,e5)
char * e, * e1, * e2, * e3, * e4, * e5;
/* prints a warning message */
{       nwarn++;
        VOID fprintf(stderr,"%s warning: ",cmdid);
        VOID fprintf(stderr,e, e1, e2, e3, e4, e5);
        nlflush(stderr);
}


/*VARARGS1*/
diagnose(e,e1,e2,e3,e4,e5)
char * e, * e1, * e2, * e3, * e4, * e5;
/* prints a diagnostic message */
{
        if (!quietflag) {
                VOID fprintf(stderr,e, e1, e2, e3, e4, e5);
                nlflush(stderr);
        }
}



fflsbuf(c, iop)
unsigned c; register FILE * iop;
/* Function: Flush iop.
 * Same routine as _flsbuf in stdio, but aborts program on error.
 */
{       register result;
        if ((result=_flsbuf(c,iop))==EOF)
                writeerror();
        return result;
}


fputs(s, iop)
register char *s;
register FILE *iop;
/* Function: Put string s on file iop, abort on error.
 * Same as puts in stdio, but with different putc macro.
 */
{
	register r;
	register c;

	while (c = *s++)
		r = putc(c, iop);
	return(r);
}



fprintf(iop, fmt, va_alist)
FILE *iop;
char *fmt;
va_dcl
/* Function: formatted output. Same as fprintf in stdio,
 * but aborts program on error
 */
{
	register int value;
	va_list ap;

	va_start(ap);
#ifdef VFPRINTF
	VOID vfprintf(iop, fmt, ap);
#else
	_doprnt(fmt, ap, iop);
#endif
        if (ferror(iop)) {
		writeerror();
                value = EOF;
        } else value = 0;
	va_end(ap);
	return value;
}



#ifdef LEXDB
/* test program reading a stream of lexems and printing the tokens.
 */



main(argc,argv)
int argc; char * argv[];
{
        cmdid="lextest";
        if (argc<2) {
                VOID fputs("No input file\n",stderr);
                exit(1);
        }
        if ((finptr=fopen(argv[1], "r")) == NULL) {
                faterror("Can't open input file %s\n",argv[1]);
        }
        Lexinit();
        rewriteflag=false;
        while (nexttok != EOFILE) {
        switch (nexttok) {

        case ID:
                VOID printf("ID: %s",NextString);
                break;

        case NUM:
                if (hshenter==true)
                   VOID printf("NUM: %s, index: %d",nexthsh->num, nexthsh-hshtab);
                else
                   VOID printf("NUM, unentered: %s",NextString);
                hshenter = !hshenter; /*alternate between dates and numbers*/
                break;

        case COLON:
                VOID printf("COLON"); break;

        case SEMI:
                VOID printf("SEMI"); break;

        case STRING:
                readstring();
                VOID printf("STRING"); break;

        case UNKN:
                VOID printf("UNKN"); break;

        default:
                VOID printf("DEFAULT"); break;
        }
        VOID printf(" | ");
        nextlex();
        }
        VOID printf("\nEnd of lexical analyzer test\n");
}

cleanup()
/* dummy */
{}


#endif
