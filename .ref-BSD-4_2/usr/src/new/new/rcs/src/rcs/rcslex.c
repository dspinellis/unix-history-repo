/*
 *                     RCS file input
 */
 static char rcsid[]=
 "$Header: /usr/wft/RCS/SRC/RCS/rcslex.c,v 3.3 82/12/10 16:22:37 wft Exp $ Purdue CS";
/*********************************************************************************
 *                     Lexical Analysis.
 *                     Character mapping table,
 *                     hashtable, Lexinit, nextlex, getlex, getkey,
 *                     getid, getnum, readstring, printstring, savestring,
 *                     checkid, serror, fatserror, error, faterror, warn, diagnose
 *                     fflsbuf, puts, fprintf
 *                     Testprogram: define LEXDB
 *********************************************************************************
 *
 * Copyright (C) 1982 by Walter F. Tichy
 *                       Purdue University
 *                       Computer Science Department
 *                       West Lafayette, IN 47907
 *
 * All rights reserved. No part of this software may be sold or distributed
 * in any form or by any means without the prior written permission of the
 * author.
 * Report problems and direct all inquiries to Tichy@purdue (ARPA net).
 */

/* $Log:	rcslex.c,v $
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

int             hshenter    /*if true, next suitable lexeme will be entered */
                = true;     /*into the symbol table. Handle with care.      */
int             nextc;      /*next input character, initialized by Lexinit  */

int             eof         /*end-of-file indicator, set to >0 on end of file*/
                = 0;
int             line        /*current line-number of input                  */
                = 1;
int             nerror      /*counter for errors                            */
                = 0;
int             nwarn       /*counter for warnings                          */
                = 0;
char *          cmdid       /*command identification for error messages     */
                = nil;
int             quietflag   /*indicates quiet mode                          */
                = false;
FILE *          finptr;     /*input file descriptor                         */

FILE *          frewrite;   /*file descriptor for echoing input             */

int             rewriteflag;/*indicates whether to echo to frewrite         */

char            StringTab[strtsize]; /* string table and heap               */

char *          NextString          /*pointer to next identifier in StringTab*/
                = nil;
char *          Topchar             /*pointer to next free byte in StringTab*/
                = &StringTab[0];    /*set by nextlex, lookup                */
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
                        printf("\nEntered: %s at %d ",nexthsh->num, ihash);
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
{       register int            i;

        for (i=hshsize-1; i>=0; i--) {
                hshtab[i].num = nil;
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
        register char * sp;
        register enum tokens d;

        if (eof) {
                nexttok=EOFILE;
                return;
        }
loop:
        switch(nexttok=ctab[nextc]) {

        case UNKN:
        case IDCHAR:
        case PERIOD:
                serror("unknown Character: %c",nextc);
                nextc=GETC(finptr,frewrite,rewriteflag);
                goto loop;

        case NEWLN:
                line++;
#               ifdef LEXDB
                putchar('\n');
#               endif
                /* Note: falls into next case */

        case SPACE:
                nextc=GETC(finptr,frewrite,rewriteflag);
                goto loop;

        case EOFILE:
                eof++;
                nexttok=EOFILE;
                return;

        case DIGIT:
                NextString = sp = Topchar;
                *sp++ = nextc;
                while ((d=ctab[c=GETC(finptr,frewrite,rewriteflag)])==DIGIT ||
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
                while ((d=ctab[c=GETC(finptr,frewrite,rewriteflag)])==LETTER ||
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
                nextc=GETC(finptr,frewrite,rewriteflag);
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
        if (rewriteflag) {
                /* copy string verbatim to frewrite */
                while ((c=putc(getc(finptr),frewrite)) != EOF) {
                        if (c==SDELIM) {
                                if ((c=putc(getc(finptr),frewrite)) != SDELIM) {
                                        /* end of string */
                                        nextc=c;
                                        return;
                                }
                        }
                }
        } else {
                /* skip string */
                while ((c=getc(finptr)) != EOF) {
                        if (c==SDELIM) {
                                if ((c=getc(finptr)) != SDELIM) {
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
        while ((c=getc(finptr)) != EOF) {
                if (c==SDELIM) {
                        if ((c=getc(finptr)) != SDELIM) {
                                /* end of string */
                                nextc=c;
                                return;
                        }
                }
                putchar(c);
        }
        nextc = c;
        error("Unterminated string");
}



int savestring(target,length)
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
        register char * tp, * max;
        register c;

        tp=target; max= target+length; /*max is one too large*/
        while ((c=GETC(finptr,frewrite,rewriteflag))!=EOF) {
		*tp++ =c;
                if (c== SDELIM) {
                        if ((c=GETC(finptr,frewrite,rewriteflag))!=SDELIM) {
                                /* end of string */
                                *(tp-1)='\0';
                                nextc=c;
                                return tp-target;
                        }
                }
                if (tp >= max) {
                        /* overflow */
                        error("string buffer overflow -- truncating string");
                        target[length-1]='\0';
                        /* skip rest of string */
                        while ((c=GETC(finptr,frewrite,rewriteflag))!=EOF) {
                                if ((c==SDELIM) && ((c=GETC(finptr,frewrite,rewriteflag))!=SDELIM)) {
                                        /* end of string */
                                        nextc=c;
                                        return length;
                                }
                        }
                        nextc = c;
                        error("Can't find %c to terminate string before end of file",SDELIM);
                        return length;
                }
        }
        nextc = c;
        error("Can't find %c to terminate string before end of file",SDELIM);
        return length;
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


serror(e,e1,e2,e3,e4,e5)
char * e, * e1;
/* non-fatal syntax error */
{       nerror++;
        fprintf(stderr,"%s error, line %d: ", cmdid, line);
        fprintf(stderr,e, e1, e2, e3, e4, e5);
        putc('\n',stderr);
}

error(e,e1,e2,e3,e4,e5)
char * e, * e1;
/* non-fatal error */
{       nerror++;
        fprintf(stderr,"%s error: ",cmdid);
        fprintf(stderr,e, e1, e2, e3, e4, e5);
        putc('\n',stderr);
}

fatserror(e,e1,e2,e3,e4,e5)
char * e, * e1;
/* fatal syntax error */
{       nerror++;
        fprintf(stderr,"%s error, line %d: ", cmdid,line);
        fprintf(stderr,e, e1, e2, e3, e4, e5);
        fprintf(stderr,"\n%s aborted\n",cmdid);
        cleanup();
        exit(1);
}

faterror(e,e1,e2,e3,e4,e5)
char * e, * e1;
/* fatal error, terminates program after cleanup */
{       nerror++;
        fprintf(stderr,"%s error: ",cmdid);
        fprintf(stderr,e, e1, e2, e3, e4, e5);
        fprintf(stderr,"\n%s aborted\n",cmdid);
        cleanup();
        exit(1);
}

warn(e,e1,e2,e3,e4,e5)
char * e, * e1;
/* prints a warning message */
{       nwarn++;
        fprintf(stderr,"%s warning: ",cmdid);
        fprintf(stderr,e, e1, e2, e3, e4, e5);
        putc('\n',stderr);
}


diagnose(e,e1,e2,e3,e4,e5)
char * e, * e1;
/* prints a diagnostic message */
{
        if (!quietflag) {
                fprintf(stderr,e, e1, e2, e3, e4, e5);
                putc('\n',stderr);
        }
}



fflsbuf(c, iop)
int c; register FILE * iop;
/* Function: Flush iop.
 * Same routine as _flsbuf in stdio, but aborts program on error.
 */
{       register result;
        if ((result=_flsbuf(c,iop))==EOF)
                faterror("write error");
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



fprintf(iop, fmt, args)
FILE *iop;
char *fmt;
/* Function: formatted output. Same as fprintf in stdio,
 * but aborts program on error
 */
{
	_doprnt(fmt, &args, iop);
        if (ferror(iop)) {
                faterror("write error");
                return EOF;
        } else return 0;
}



#ifdef LEXDB
/* test program reading a stream of lexems and printing the tokens.
 */



main(argc,argv)
int argc; char * argv[];
{
        cmdid="lextest";
        if (argc<2) {
                fputs("No input file\n",stderr);
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
                printf("ID: %s",NextString);
                break;

        case NUM:
                if (hshenter==true)
                   printf("NUM: %s, index: %d",nexthsh->num, nexthsh-hshtab);
                else
                   printf("NUM, unentered: %s",NextString);
                hshenter = !hshenter; /*alternate between dates and numbers*/
                break;

        case COLON:
                printf("COLON"); break;

        case SEMI:
                printf("SEMI"); break;

        case STRING:
                readstring();
                printf("STRING"); break;

        case UNKN:
                printf("UNKN"); break;

        default:
                printf("DEFAULT"); break;
        }
        printf(" | ");
        nextlex();
        }
        printf("\nEnd of lexical analyzer test\n");
}

cleanup()
/* dummy */
{}


#endif
