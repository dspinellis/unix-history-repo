int cmdmsk = 127; /* Command-terminal-to-C-Kermit character mask */

#include "ckcdeb.h"                     /* Formats for debug(), etc. */
_PROTOTYP( int unhex, (char) );

#ifndef NOICP     /* The rest only if interactive command parsing selected */

char *cmdv = "Command package 5A(053), 21 Nov 92";
 
/*  C K U C M D  --  Interactive command package for Unix  */

/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.
  First released January 1985.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/

/*
Modeled after the DECSYSTEM-20 command parser (the COMND JSYS), RIP.
Features:
. parses and verifies keywords, filenames, text strings, numbers, other data
. displays appropriate menu or help message when user types "?"
. does keyword and filename completion when user types ESC or TAB
. does partial filename completion
. accepts any unique abbreviation for a keyword
. allows keywords to have attributes, like "invisible" and "abbreviation"
. can supply defaults for fields omitted by user
. provides command line editing (character, word, and line deletion)
. accepts input from keyboard, command files, or redirected stdin
. allows for full or half duplex operation, character or line input
. settable prompt, protected from deletion

Functions:
 cmsetp - Set prompt (cmprom is prompt string)
 cmsavp - Save current prompt
 prompt - Issue prompt
 cmini - Clear the command buffer (before parsing a new command)
 cmres - Reset command buffer pointers (before reparsing)
 cmkey - Parse a keyword
 cmnum - Parse a number
 cmifi - Parse an input file name
 cmofi - Parse an output file name
 cmdir - Parse a directory name (UNIX only)
 cmfld - Parse an arbitrary field
 cmtxt - Parse a text string
 cmcfm - Parse command confirmation (end of line)

Return codes:
 -3: no input provided when required
 -2: input was invalid (e.g. not a number when a number was required)
 -1: reparse required (user deleted into a preceding field)
  0 or greater: success
See individual functions for greater detail.

Before using these routines, the caller should #include ckucmd.h, and set the
program's prompt by calling cmsetp().  If the file parsing functions cmifi,
cmofi, or cmdir are to be used, this module must be linked with a ck?fio file
system support module for the appropriate system, e.g. ckufio for Unix.  If
the caller puts the terminal in character wakeup ("cbreak") mode with no echo,
then these functions will provide line editing -- character, word, and line
deletion, as well as keyword and filename completion upon ESC and help
strings, keyword, or file menus upon '?'.  If the caller puts the terminal
into character wakeup/noecho mode, care should be taken to restore it before
exit from or interruption of the program.  If the character wakeup mode is not
set, the system's own line editor may be used.

NOTE: Contrary to expectations, many #ifdef's have been added to this module.
Any operation requiring an #ifdef (like clear screen, get character from
keyboard, erase character from screen, etc) should eventually be turned into a
call to a function that is defined in ck?tio.c, but then all the ck?tio.c
modules would have to be changed...
*/

/* Includes */

#include "ckcker.h"			/* (===OS2 addition===) */
#include "ckcasc.h"			/* ASCII character symbols */
#include "ckucmd.h"                     /* Command parsing definitions */
#include <errno.h>			/* Error number symbols */

#ifdef OSK
#define cc ccount			/* OS-9/68K compiler bug */
#endif /* OSK */

#ifdef GEMDOS				/* Atari ST */
#ifdef putchar
#undef putchar
#endif /* putchar */
#define putchar(x) conoc(x)		/* Why doesn't everyone do this? */
#endif /* GEMDOS */

/* Local variables */
 
static
int psetf = 0,                          /* Flag that prompt has been set */
    cc = 0,                             /* Character count */
    dpx = 0,                            /* Duplex (0 = full) */
    inword = 0;				/* In the middle of getting a word */
 
static
int hw = HLPLW,                         /* Help line width */
    hc = HLPCW,                         /* Help line column width */
    hh,                                 /* Current help column number */
    hx;                                 /* Current help line position */
 
#define PROML 160                       /* Maximum length for prompt */
 
char cmprom[PROML+1];                   /* Program's prompt */
char cmprxx[PROML+1];                   /* Program's prompt, unevaluated */
char *dfprom = "Command? ";             /* Default prompt */
 
int cmflgs;                             /* Command flags */
int cmfsav;				/* A saved version of them */
 
#ifdef DCMDBUF
char *cmdbuf;				/* Command buffer */                
char *savbuf;				/* Help string buffer */            
char *hlpbuf;				/* Atom buffer */                   
char *atmbuf;				/* File name buffer */              
char *atxbuf;				/* For expanding the atom buffer */ 
int atxn;				/* Length of expansion buffer */    
char *atybuf;				/* For copying atom buffer */       
char *filbuf;				/* Buffer to save copy of command */
#else
char cmdbuf[CMDBL+4];                   /* Command buffer */                
char hlpbuf[HLPBL+4];                   /* Help string buffer */            
char atmbuf[ATMBL+4];                   /* Atom buffer */                   
char filbuf[ATMBL+4];                   /* File name buffer */              
char atxbuf[CMDBL+4];                   /* For expanding the atom buffer */ 
int atxn;	                        /* Length of expansion buffer */    
char atybuf[ATMBL+4];                   /* For copying atom buffer */       
char savbuf[CMDBL+4];                   /* Buffer to save copy of command */
#endif /* DCMDBUF */
 
/* Command buffer pointers */
 
static char *bp,                        /* Current command buffer position */
    *pp,                                /* Start of current field */
    *np;                                /* Start of next field */
 
static int ungw;			/* For ungetting words */

_PROTOTYP( VOID addhlp, (char *) );
_PROTOTYP( VOID clrhlp, (void) );
_PROTOTYP( VOID dmphlp, (void) );
_PROTOTYP( int gtword, (void) );
_PROTOTYP( int addbuf, (char *) );
_PROTOTYP( int setatm, (char *) );
_PROTOTYP( int cmdgetc, (void) );
_PROTOTYP( VOID cmdnewl, (char) );
_PROTOTYP( VOID cmdchardel, (void) );
_PROTOTYP( VOID cmdecho, (char, int) );
_PROTOTYP( static int test, (int, int) );
#ifdef GEMDOS
_PROTOTYP( extern char *strchr, (char *, int) );
#endif /* GEMDOS */

/*  T E S T  --  Bit test  */
 
static int
test(x,m) int x, m; { /*  Returns 1 if any bits from m are on in x, else 0  */
    return((x & m) ? 1 : 0);
}

/*  C M S E T U P  --  Set up command buffers  */

#ifdef DCMDBUF
int
cmsetup() {
    if (!(cmdbuf = malloc(CMDBL + 4))) return(-1);
    if (!(savbuf = malloc(CMDBL + 4))) return(-1);
    savbuf[0] = '\0';
    if (!(hlpbuf = malloc(HLPBL + 4))) return(-1);
    if (!(atmbuf = malloc(ATMBL + 4))) return(-1);
    if (!(atxbuf = malloc(CMDBL + 4))) return(-1);
    if (!(atybuf = malloc(ATMBL + 4))) return(-1);
    if (!(filbuf = malloc(ATMBL + 4))) return(-1);
    return(0);
}
#endif /* DCMDBUF */ 

/*  C M S E T P  --  Set the program prompt.  */
 
VOID
cmsetp(s) char *s; {
    strncpy(cmprxx,s,PROML - 1);
    cmprxx[PROML] = NUL;
    psetf = 1;                          /* Flag that prompt has been set. */
}
/*  C M S A V P  --  Save a copy of the current prompt.  */
 
VOID
#ifdef CK_ANSIC
cmsavp(char s[], int n)
#else
cmsavp(s,n) char s[]; int n;
#endif /* CK_ANSIC */
/* cmsavp */ {
    strncpy(s,cmprxx,n-1);
    s[n-1] = NUL;
}
 
/*  P R O M P T  --  Issue the program prompt.  */
 
VOID
prompt(f) xx_strp f; {
    char *sx, *sy; int n;

    if (psetf == 0) cmsetp(dfprom);     /* If no prompt set, set default. */

    sx = cmprxx;			/* Unevaluated copy */
    if (f) {				/* If conversion function given */
	sy = cmprom;			/* Evaluate it */
	n = PROML;
	if ((*f)(sx,&sy,&n) < 0)	/* If evaluation failed */
	  sx = cmprxx;			/* revert to unevaluated copy */
	else
	  sx = cmprom;
    }
#ifdef OSK
    fputs(sx, stdout);
#else
#ifdef MAC
    printf("%s", sx);
#else
    printf("\r%s",sx);			/* Print the prompt. */
    fflush(stdout);			/* Now! */
#endif /* MAC */
#endif /* OSK */
}
 
#ifndef NOSPL
VOID
pushcmd() {				/* For use with IF command. */
    strcpy(savbuf,np);			/* Save the dependent clause,  */
    cmres();				/* and clear the command buffer. */
    debug(F110, "pushcmd: savbuf:", savbuf, 0);
}
#endif /* NOSPL */

#ifdef COMMENT
/* no longer used... */
VOID
popcmd() {
    strcpy(cmdbuf,savbuf);		/* Put back the saved material */
    *savbuf = '\0';			/* and clear the save buffer */
    cmres();
}
#endif /* COMMENT */

/*  C M R E S  --  Reset pointers to beginning of command buffer.  */
 
VOID
cmres() {  
    inword = cc = 0;			/* Reset character counter. */
    pp = np = bp = cmdbuf;              /* Point to command buffer. */
    cmflgs = -5;                        /* Parse not yet started. */
    ungw = 0;				/* Don't need to unget a word. */
}
 
/*  C M I N I  --  Clear the command and atom buffers, reset pointers.  */
 
/*
The argument specifies who is to echo the user's typein --
  1 means the cmd package echoes
  0 somebody else (system, front end, terminal) echoes
*/
VOID
cmini(d) int d; {
    for (bp = cmdbuf; bp < cmdbuf+CMDBL; bp++) *bp = NUL;
    *atmbuf = NUL;
    dpx = d;
    cmres();
}
 
#ifndef NOSPL
/* The following bits are to allow the command package to call itself */
/* in the middle of a parse.  To do this, begin by calling cmpush, and */
/* end by calling cmpop. */

#ifdef DCMDBUF
struct cmp {
    int i[5];				/* stack for integers */
    char *c[3];				/* stack for pointers */
    char *b[8];				/* stack for buffer contents */
};
struct cmp *cmp = 0;
#else
int cmp_i[CMDDEP+1][5];			/* Stack for integers */
char *cmp_c[CMDDEP+1][5];		/* for misc pointers */
char *cmp_b[CMDDEP+1][7];		/* for buffer contents pointers */
#endif /* DCMDBUF */

int cmddep = -1;			/* Current stack depth */

int
cmpush() {				/* Save the command environment */
    char *cp;				/* Character pointer */

    if (cmddep >= CMDDEP)		/* Enter a new command depth */
      return(-1);
    cmddep++;
    debug(F101,"&cmpush","",cmddep);

#ifdef DCMDBUF
    /* allocate memory for cmp if not already done */
    if (!cmp && !(cmp = (struct cmp *) malloc(sizeof(struct cmp)*(CMDDEP+1))))
      fatal("cmpush: no memory for cmp");
    cmp[cmddep].i[0] = cmflgs;		/* First do the global ints */
    cmp[cmddep].i[1] = cmfsav;
    cmp[cmddep].i[2] = atxn;
    cmp[cmddep].i[3] = ungw;

    cmp[cmddep].c[0] = bp;		/* Then the global pointers */
    cmp[cmddep].c[1] = pp;
    cmp[cmddep].c[2] = np; 
#else
    cmp_i[cmddep][0] = cmflgs;		/* First do the global ints */
    cmp_i[cmddep][1] = cmfsav;
    cmp_i[cmddep][2] = atxn;
    cmp_i[cmddep][3] = ungw;

    cmp_c[cmddep][0] = bp;		/* Then the global pointers */
    cmp_c[cmddep][1] = pp;
    cmp_c[cmddep][2] = np; 
#endif /* DCMDBUF */

    /* Now the buffers themselves.  A lot of repititious code... */

#ifdef DCMDBUF
    cp = malloc((int)strlen(cmdbuf)+1);	/* 0: Command buffer */
    if (cp) strcpy(cp,cmdbuf);
    cmp[cmddep].b[0] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(savbuf)+1);	/* 1: Save buffer */
    if (cp) strcpy(cp,savbuf);
    cmp[cmddep].b[1] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(hlpbuf)+1);	/* 2: Help string buffer */
    if (cp) strcpy(cp,hlpbuf);
    cmp[cmddep].b[2] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(atmbuf)+1);	/* 3: Atom buffer */
    if (cp) strcpy(cp,atmbuf);
    cmp[cmddep].b[3] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(atxbuf)+1);	/* 4: Expansion buffer */
    if (cp) strcpy(cp,atxbuf);
    cmp[cmddep].b[4] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(atybuf)+1);	/* 5: Atom buffer copy */
    if (cp) strcpy(cp,atybuf);
    cmp[cmddep].b[5] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(filbuf)+1);	/* 6: File name buffer */
    if (cp) strcpy(cp,filbuf);
    cmp[cmddep].b[6] = cp;
    if (cp == NULL) return(-1);
#else
    cp = malloc((int)strlen(cmdbuf)+1);	/* 0: Command buffer */
    if (cp) strcpy(cp,cmdbuf);
    cmp_b[cmddep][0] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(savbuf)+1);	/* 1: Save buffer */
    if (cp) strcpy(cp,savbuf);
    cmp_b[cmddep][1] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(hlpbuf)+1);	/* 2: Help string buffer */
    if (cp) strcpy(cp,hlpbuf);
    cmp_b[cmddep][2] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(atmbuf)+1);	/* 3: Atom buffer */
    if (cp) strcpy(cp,atmbuf);
    cmp_b[cmddep][3] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(atxbuf)+1);	/* 4: Expansion buffer */
    if (cp) strcpy(cp,atxbuf);
    cmp_b[cmddep][4] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(atybuf)+1);	/* 5: Atom buffer copy */
    if (cp) strcpy(cp,atybuf);
    cmp_b[cmddep][5] = cp;
    if (cp == NULL) return(-1);

    cp = malloc((int)strlen(filbuf)+1);	/* 6: File name buffer */
    if (cp) strcpy(cp,filbuf);
    cmp_b[cmddep][6] = cp;
    if (cp == NULL) return(-1);
#endif /* DCMDBUF */

    cmini(dpx);				/* Initize the command parser */
    return(0);
}

int
cmpop() {				/* Restore the command environment */
    debug(F101,"&cmpop","",cmddep);
    if (cmddep < 0) return(-1);		/* Don't pop too much! */

#ifdef DCMDBUF
    cmflgs = cmp[cmddep].i[0];		/* First do the global ints */
    cmfsav = cmp[cmddep].i[1];
    atxn = cmp[cmddep].i[2];
    ungw = cmp[cmddep].i[3];

    bp = cmp[cmddep].c[0];		/* Then the global pointers */
    pp = cmp[cmddep].c[1];
    np = cmp[cmddep].c[2]; 
#else
    cmflgs = cmp_i[cmddep][0];		/* First do the global ints */
    cmfsav = cmp_i[cmddep][1];
    atxn = cmp_i[cmddep][2];
    ungw = cmp_i[cmddep][3];

    bp = cmp_c[cmddep][0];		/* Then the global pointers */
    pp = cmp_c[cmddep][1];
    np = cmp_c[cmddep][2]; 
#endif /* DCMDBUF */

    /* Now the buffers themselves. */

#ifdef DCMDBUF
    if (cmp[cmddep].b[0]) {
	strcpy(cmdbuf,cmp[cmddep].b[0]); /* 0: Command buffer */
	free(cmp[cmddep].b[0]);
	cmp[cmddep].b[0] = NULL;
    }
    if (cmp[cmddep].b[1]) {
	strcpy(savbuf,cmp[cmddep].b[1]); /* 1: Save buffer */
	free(cmp[cmddep].b[1]);
	cmp[cmddep].b[1] = NULL;
    }
    if (cmp[cmddep].b[2]) {
	strcpy(hlpbuf,cmp[cmddep].b[2]); /* 2: Help buffer */
	free(cmp[cmddep].b[2]);
	cmp[cmddep].b[2] = NULL;
    }
    if (cmp[cmddep].b[3]) {
	strcpy(atmbuf,cmp[cmddep].b[3]); /* 3: Atomic buffer! */
	free(cmp[cmddep].b[3]);
	cmp[cmddep].b[3] = NULL;
    }
    if (cmp[cmddep].b[4]) {
	strcpy(atxbuf,cmp[cmddep].b[4]); /* 4: eXpansion buffer */
	free(cmp[cmddep].b[4]);
	cmp[cmddep].b[4] = NULL;
    }
    if (cmp[cmddep].b[5]) {
	strcpy(atybuf,cmp[cmddep].b[5]); /* 5: Atom buffer copY */
	free(cmp[cmddep].b[5]);
	cmp[cmddep].b[5] = NULL;
    }
    if (cmp[cmddep].b[6]) {
	strcpy(filbuf,cmp[cmddep].b[6]); /* 6: Filename buffer */
	free(cmp[cmddep].b[6]);
	cmp[cmddep].b[6] = NULL;
    }
#else
    if (cmp_b[cmddep][0]) {
	strcpy(cmdbuf,cmp_b[cmddep][0]); /* 0: Command buffer */
	free(cmp_b[cmddep][0]);
	cmp_b[cmddep][0] = NULL;
    }
    if (cmp_b[cmddep][1]) {
	strcpy(savbuf,cmp_b[cmddep][1]); /* 1: Save buffer */
	free(cmp_b[cmddep][1]);
	cmp_b[cmddep][1] = NULL;
    }
    if (cmp_b[cmddep][2]) {
	strcpy(hlpbuf,cmp_b[cmddep][2]); /* 2: Help buffer */
	free(cmp_b[cmddep][2]);
	cmp_b[cmddep][2] = NULL;
    }
    if (cmp_b[cmddep][3]) {
	strcpy(atmbuf,cmp_b[cmddep][3]); /* 3: Atomic buffer! */
	free(cmp_b[cmddep][3]);
	cmp_b[cmddep][3] = NULL;
    }
    if (cmp_b[cmddep][4]) {
	strcpy(atxbuf,cmp_b[cmddep][4]); /* 4: eXpansion buffer */
	free(cmp_b[cmddep][4]);
	cmp_b[cmddep][4] = NULL;
    }
    if (cmp_b[cmddep][5]) {
	strcpy(atybuf,cmp_b[cmddep][5]); /* 5: Atom buffer copY */
	free(cmp_b[cmddep][5]);
	cmp_b[cmddep][5] = NULL;
    }
    if (cmp_b[cmddep][6]) {
	strcpy(filbuf,cmp_b[cmddep][6]); /* 6: Filename buffer */
	free(cmp_b[cmddep][6]);
	cmp_b[cmddep][6] = NULL;
    }
#endif /* DCMDBUF */

    cmddep--;				/* Rise, rise */
    debug(F101,"&cmpop","",cmddep);
    return(cmddep);
}
#endif /* NOSPL */

#ifdef COMMENT
VOID
stripq(s) char *s; {                    /* Function to strip '\' quotes */
    char *t;
    while (*s) {
        if (*s == CMDQ) {
            for (t = s; *t != '\0'; t++) *t = *(t+1);
        }
        s++;
    }
}
#endif /* COMMENT */

/* Convert tabs to spaces, one for one */
VOID
untab(s) char *s; {
    while (*s) {
	if (*s == HT) *s = SP;
	s++;
    }
}

/*  C M N U M  --  Parse a number in the indicated radix  */
 
/* 
 The only radix allowed in unquoted numbers is 10.
 Parses unquoted numeric strings in base 10.
 Parses backslash-quoted numbers in the radix indicated by the quote:
   \nnn = \dnnn = decimal, \onnn = octal, \xnn = Hexadecimal.
 If these fail, then if a preprocessing function is supplied, that is applied 
 and then a second attempt is made to parse an unquoted decimal string. 

 Returns:
   -3 if no input present when required,
   -2 if user typed an illegal number,
   -1 if reparse needed,
    0 otherwise, with argument n set to the number that was parsed
*/
int
cmnum(xhlp,xdef,radix,n,f) char *xhlp, *xdef; int radix, *n; xx_strp f; {
    int x; char *s, *zp, *zq;
 
    if (radix != 10) {                  /* Just do base 10 */
        printf("cmnum: illegal radix - %d\n",radix);
        return(-1);
    }
    x = cmfld(xhlp,xdef,&s,(xx_strp)0);
    debug(F101,"cmnum: cmfld","",x);
    if (x < 0) return(x);		/* Parse a field */
    zp = atmbuf;

    if (chknum(zp)) {			/* Check for decimal number */
        *n = atoi(zp);			/* Got one, we're done. */
	debug(F101,"cmnum 1st chknum ok","",*n);
        return(0);
    } else if ((x = xxesc(&zp)) > -1) {	/* Check for backslash escape */

#ifndef OS2
	*n = x;
#else
	*n = wideresult;
#endif /* OS2 */

	debug(F101,"cmnum xxesc ok","",*n);
	return(*zp ? -2 : 0);
    } else if (f) {			/* If conversion function given */
        zp = atmbuf;			/* Try that */
	zq = atxbuf;
	atxn = CMDBL;
	(*f)(zp,&zq,&atxn);		/* Convert */
	zp = atxbuf;
    }
    debug(F110,"cmnum zp",zp,0);
    if (chknum(zp)) {			/* Check again for decimal number */
        *n = atoi(zp);			/* Got one, we're done. */
	debug(F101,"cmnum 2nd chknum ok","",*n);
        return(0);
    } else {				/* Not numeric */
	return(-2);     
    }
}

/*  C M O F I  --  Parse the name of an output file  */
 
/*
 Depends on the external function zchko(); if zchko() not available, use
 cmfld() to parse output file names.
 
 Returns
   -3 if no input present when required,
   -2 if permission would be denied to create the file,
   -1 if reparse needed,
    0 or 1 otherwise, with xp pointing to name.
*/
int
cmofi(xhlp,xdef,xp,f) char *xhlp, *xdef, **xp; xx_strp f; {
    int x; char *s, *zq;
#ifdef DTILDE
    _PROTOTYP( char * tilde_expand, (char *) );
    char *dirp;
#endif 

    if (*xhlp == NUL) xhlp = "Output file";
    *xp = "";
 
    if ((x = cmfld(xhlp,xdef,&s,(xx_strp)0)) < 0) return(x);

    if (f) {				/* If a conversion function is given */
	zq = atxbuf;
	atxn = CMDBL;
	if ((x = (*f)(s,&zq,&atxn)) < 0) return(-2);
	s = atxbuf;
    }

#ifdef DTILDE
    dirp = tilde_expand(s);		/* Expand tilde, if any, */
    if (*dirp != '\0') setatm(dirp);	/* right in the atom buffer. */
    s = atmbuf;
#endif

    if (iswild(s)) {
        printf("?Wildcards not allowed - %s\n",s);
        return(-2);
    }
    if (strcmp(s,CTTNAM) && (zchko(s) < 0)) { /* ok to write to tty */
        printf("?Write permission denied - %s\n",s);
        return(-9);
    } else {
        *xp = s;
        return(x);
    }
}

 
/*  C M I F I  --  Parse the name of an existing file  */
 
/*
 This function depends on the external functions:
   zchki()  - Check if input file exists and is readable.
   zxpand() - Expand a wild file specification into a list.
   znext()  - Return next file name from list.
 If these functions aren't available, then use cmfld() to parse filenames.
*/
/*
 Returns
   -4 EOF
   -3 if no input present when required,
   -2 if file does not exist or is not readable,
   -1 if reparse needed,
    0 or 1 otherwise, with:
        xp pointing to name,
        wild = 1 if name contains '*' or '?', 0 otherwise.
*/
int
cmifi(xhlp,xdef,xp,wild,f) char *xhlp, *xdef, **xp; int *wild; xx_strp f; {
    int i, x, xc; long y; char *sp, *zq, *sv;
#ifdef DTILDE
    char *tilde_expand(), *dirp;
#endif /* DTILDE */

#ifndef NOPARTIAL
    extern char *mtchs[];
#endif /* NOPARTIAL */

    inword = cc = xc = 0;		/* Initialize counts & pointers */
    *xp = "";
    if ((x = cmflgs) != 1) {            /* Already confirmed? */
        x = gtword();                   /* No, get a word */
    } else {
        setatm(xdef);			/* If so, use default, if any. */
    }

    *xp = atmbuf;                       /* Point to result. */
 
    while (1) {
        xc += cc;                       /* Count the characters. */
        debug(F111,"cmifi gtword",atmbuf,xc);
        switch (x) {
            case -4:                    /* EOF */
            case -2:                    /* Out of space. */
            case -1:                    /* Reparse needed */
                return(x);
            case 0:                     /* SP or NL */
            case 1:
                if (xc == 0) *xp = xdef;     /* If no input, return default. */
                if (**xp == NUL) return(-3); /* If field empty, return -3. */

		if (f) {		/* If a conversion function is given */
		    zq = atxbuf;	/* ... */
		    atxn = CMDBL;
		    if ((x = (*f)(*xp,&zq,&atxn)) < 0) return(-2);
		    *xp = atxbuf;
		}
		debug(F110,"cmifi atxbuf",atxbuf,0);
#ifdef COMMENT
/* don't need this stuff, zxpand does it now. */
#ifdef DTILDE

		dirp = tilde_expand(*xp);    /* Expand tilde, if any, */
		if (*dirp != '\0') setatm(dirp); /* right in atom buffer. */
		*xp = atmbuf;
#endif /* DTILDE */

                /* If filespec is wild, see if there are any matches */

                *wild = iswild(*xp);
                debug(F101,"cmifi wild","",*wild);
                if (*wild != 0) {
#endif /* COMMENT */
		    sv = malloc((int)strlen(*xp)+1); /* Make a safe copy */
		    if (!sv) {
			printf("?malloc error 73, cmifi\n");
			return(-9);
		    }
		    strcpy(sv,*xp);
		    debug(F110,"cmifi sv",sv,0);
                    y = zxpand(*xp);
		    *wild = (y > 1);
		    debug(F111,"cmifi sv wild",sv,*wild);
                    if (y == 0) {
                        printf("?No files match - %s\n",*xp);
                        return(-9);
                    } else if (y < 0) {
                        printf("?Too many files match - %s\n",*xp);
                        return(-9);
		    } else if (y > 1) return(x);
#ifdef COMMENT
                }
#endif
                /* If not wild, see if it exists and is readable. */
 
		debug(F111,"cmifi sv not wild",sv,*wild);

		znext(*xp);		/* Get first (only?) matching file */
                y = zchki(*xp);		/* Check its accessibility */
		zxpand(sv);		/* Rewind so next znext() gets 1st */
		free(sv);		/* done with this */
                if (y == -3) {
                    printf("?Read permission denied - %s\n",*xp);
                    return(-9);
                } else if (y == -2) {
                    printf("?File not readable - %s\n",*xp);
                    return(-9);
                } else if (y < 0) {
                    printf("?File not found - %s\n",*xp);
                    return(-9);
                }
                return(x);

#ifndef MAC
            case 2:                     /* ESC */
		debug(F101,"cmifi esc, xc","",xc);
                if (xc == 0) {
                    if (*xdef != '\0') {
                        printf("%s ",xdef); /* If at beginning of field, */
#ifdef GEMDOS
			fflush(stdout);
#endif /* GEMDOS */
			inword = cmflgs = 0;
                        addbuf(xdef);   /* supply default. */
                        setatm(xdef);
                    } else {            /* No default */
                        putchar(BEL);
                    }
                    break;
                } 
		if (f) {		/* If a conversion function is given */
		    zq = atxbuf;	/* ... */
		    atxn = CMDBL;
		    if ((x = (*f)(*xp,&zq,&atxn)) < 0) return(-2);
                    /* reduce cc by number of \\ consumed by conversion */
		    /* function (needed for OS/2, where \ is path separator) */
                    cc -= (strlen(*xp) - strlen(atxbuf));
		    *xp = atxbuf;
		}
/* #ifdef COMMENT */
#ifdef DTILDE
		dirp = tilde_expand(*xp);    /* Expand tilde, if any, */
		if (*dirp != '\0') setatm(dirp); /* in the atom buffer. */
                *xp = atmbuf;
#endif /* DTILDE */
/* #endif */
                sp = *xp + cc;
#ifdef datageneral
                *sp++ = '+';		/* Data General AOS wildcard */
#else
                *sp++ = '*';		/* Others */
#endif /* datageneral */
                *sp-- = '\0';
#ifdef GEMDOS
		if (! strchr(*xp, '.'))	/* abde.e -> abcde.e* */
		  strcat(*xp, ".*");	/* abc -> abc*.* */
#endif /* GEMDOS */
                y = zxpand(*xp);	/* Add wildcard and expand list. */
		if (y > 0) strcpy(filbuf,mtchs[0]);
		else *filbuf = '\0';
                *sp = '\0';             /* Remove wildcard. */
		*wild = (y > 1);
                if (y == 0) {
                    printf("?No files match - %s\n",atmbuf);
                    return(-9);
                } else if (y < 0) {
                    printf("?Too many files match - %s\n",atmbuf);
                    return(-9);
                } else if (y > 1) {     /* Not unique. */
#ifndef NOPARTIAL
/* Partial filename completion */
		    int i, j, k; char c;
		    k = 0;
		    debug(F111,"cmifi partial",filbuf,cc);
		    for (i = cc; (c = filbuf[i]); i++) {
			for (j = 1; j < y; j++)
			  if (mtchs[j][i] != c) break;
			if (j == y) k++;
			else filbuf[i] = filbuf[i+1] = NUL;
		    }
		    debug(F111,"cmifi partial k",filbuf,k);
		    if (k > 0) {	/* Got more characters */
			sp = filbuf + cc; /* Point to new ones */
#ifdef VMS
			for (i = 0; i < cc; i++) {
			    cmdchardel(); /* Back up over old partial spec */
			    bp--;
			}
			sp = filbuf;	/* Point to new word start */
			debug(F100,"cmifi vms erase ok","",0);
#endif /* VMS */
			cc = k;		/* How many new ones we just got */
			printf("%s",sp);        /* Print them */
			while (*bp++ = *sp++) ;	/* Copy to command buffer */
			bp--;	    	        /* Back up over NUL */
			debug(F110,"cmifi partial cmdbuf",cmdbuf,0);
			setatm(filbuf);
			debug(F111,"cmifi partial atmbuf",atmbuf,cc);
			*xp = atmbuf;
		    }
#endif /* NOPARTIAL */
		    putchar(BEL);	/* Beep because not unique. */
                } else {                /* Unique, complete it.  */
                    sp = filbuf + cc;   /* Point past what user typed. */
#ifdef VMS
		    for (i = 0; i < cc; i++) {
			cmdchardel();	/* Back up over old partial spec */
			bp--;
		    }
		    sp = filbuf;	/* Point to new word start */
#endif /* VMS */
                    printf("%s ",sp);   /* Complete the name. */
#ifdef GEMDOS
		    fflush(stdout);
#endif /* GEMDOS */
                    addbuf(sp);         /* Add the characters to cmdbuf. */
                    setatm(filbuf);	/* And to atmbuf. */
		    inword = cmflgs = 0;
                    *xp = atmbuf;       /* Return pointer to atmbuf. */
                    return(0);
                }
                break;
 
            case 3:                     /* Question mark */
                if (*xhlp == NUL)
		  printf(" Input file specification");
                else
		  printf(" %s",xhlp);
#ifdef GEMDOS
		fflush(stdout);
#endif /* GEMDOS */
                if (xc > 0) {
		    if (f) {		/* If a conversion function is given */
			zq = atxbuf;	/* ... */
			atxn = CMDBL;
			if ((x = (*f)(*xp,&zq,&atxn)) < 0) return(-2);
			*xp = atxbuf;
		    }
#ifdef DTILDE
		    dirp = tilde_expand(*xp);    /* Expand tilde, if any */
		    if (*dirp != '\0') setatm(dirp);
		    *xp = atmbuf;
#endif
		    debug(F111,"cmifi ? *xp, cc",*xp,cc);
                    sp = *xp + cc;	/* Insert "*" at end */
#ifdef datageneral
                    *sp++ = '+';        /* Insert +, the DG wild card */
#else
                    *sp++ = '*';
#endif /* datageneral */
                    *sp-- = '\0';
#ifdef GEMDOS
		    if (! strchr(*xp, '.'))	/* abde.e -> abcde.e* */
		      strcat(*xp, ".*");	/* abc -> abc*.* */
#endif /* GEMDOS */
		    debug(F110,"cmifi ? wild",*xp,0);
                    y = zxpand(*xp);
                    *sp = '\0';
                    if (y == 0) {                   
                        printf("?No files match - %s\n",atmbuf);
                        return(-9);
                    } else if (y < 0) {
                        printf("?Too many files match - %s\n",atmbuf);
                        return(-9);
                    } else {
                        printf(", one of the following:\n");
                        clrhlp();
                        for (i = 0; i < y; i++) {
                            znext(filbuf);
#ifdef VMS
			    printf(" %s\n",filbuf); /* VMS names can be long */
#else
                            addhlp(filbuf);
#endif /* VMS */
                        }
                        dmphlp();
                    }
                } else printf("\n");
                printf("%s%s",cmprom,cmdbuf);
		fflush(stdout);
                break;
#endif /* MAC */
        }
    x = gtword();
    *xp = atmbuf;
    }
}

/*  C M D I R  --  Parse a directory specification  */
 
/*
 This function depends on the external functions:
   zchki()  - Check if input file exists and is readable.
 If these functions aren't available, then use cmfld() to parse dir names.
 Note: this function quickly cobbled together, mainly by deleting lots of
 lines from cmifi().  It seems to work, but various services are missing,
 like completion, lists of matching directories on "?", etc.
*/
/*
 Returns
   -4 EOF
   -3 if no input present when required,
   -2 if out of space or other internal error,
   -1 if reparse needed,
    0 or 1, with xp pointing to name, if directory specified,
    2 if a wildcard was included.
*/
int
cmdir(xhlp,xdef,xp,f) char *xhlp, *xdef, **xp; xx_strp f; {
    int x, xc; char *zq;
#ifdef DTILDE
    char *tilde_expand(), *dirp;
#endif /* DTILDE */

    inword = cc = xc = 0;		/* Initialize counts & pointers */
    *xp = "";
    if ((x = cmflgs) != 1) {            /* Already confirmed? */
        x = gtword();                   /* No, get a word */
    } else {
        setatm(xdef);			/* If so, use default, if any. */
    }
    *xp = atmbuf;                       /* Point to result. */
    while (1) {
        xc += cc;                       /* Count the characters. */
        debug(F111,"cmdir gtword",atmbuf,xc);
        switch (x) {
            case -4:                    /* EOF */
            case -2:                    /* Out of space. */
            case -1:                    /* Reparse needed */
                return(x);
            case 0:                     /* SP or NL */
            case 1:
                if (xc == 0) *xp = xdef;     /* If no input, return default. */
		else *xp = atmbuf;
                if (**xp == NUL) return(-3); /* If field empty, return -3. */

		if (f) {		/* If a conversion function is given */
		    zq = atxbuf;	/* ... */
		    atxn = CMDBL;
		    if ((x = (*f)(*xp,&zq,&atxn)) < 0) return(-2);
		    *xp = atxbuf;
		    cc = (int)strlen(atxbuf);
		}
#ifdef DTILDE
/*
  This is ugly, and for UNIX only.
  Normally, we wouldn't call tilde_expand from a place like this anyway,
  but rather let zxpand() take care of it.  But in this case we might want 
  a hybrid result -- a string with the tilde expanded, but with wildcards
  left unexpanded.
*/
		dirp = tilde_expand(*xp); /* Expand tilde, if any, */
		if (*dirp == '~') {	/* Still starts with tilde? */
		    char *tp;		/* Yes, convert to lowercase */
		    tp = *xp;		/* and try again. */
		    while (*tp) {
			if (isupper(*tp)) *tp = tolower(*tp);
			tp++;
		    }
		}
		dirp = tilde_expand(*xp); /* Expand tilde, if any, */
		if (*dirp != '\0') setatm(dirp); /* in the atom buffer. */
		*xp = atmbuf;
#endif /* DTILDE */
		if (iswild(*xp)) return(2);
		else return(x);

            case 2:                     /* ESC */
		putchar(BEL);
		break;

            case 3:                     /* Question mark */
                if (*xhlp == NUL)
                    printf(" Directory name");
                else
                    printf(" %s",xhlp);
                printf("\n%s%s",cmprom,cmdbuf);
		fflush(stdout);
                break;
        }
    x = gtword();
/*  *xp = atmbuf;  */
    }
}
 
/*  C M F L D  --  Parse an arbitrary field  */
/*
 Returns
   -3 if no input present when required,
   -2 if field too big for buffer,
   -1 if reparse needed,
    0 otherwise, xp pointing to string result.
*/
int
cmfld(xhlp,xdef,xp,f) char *xhlp, *xdef, **xp; xx_strp f; {
    int x, xc;
    char *zq;

    inword = cc = xc = 0;		/* Initialize counts & pointers */
    *xp = "";
    if ((x = cmflgs) != 1) {            /* Already confirmed? */
        x = gtword();                   /* No, get a word */
    } else {
        setatm(xdef);			/* If so, use default, if any. */
    }
    *xp = atmbuf;                       /* Point to result. */
 
    while (1) {
        xc += cc;                       /* Count the characters. */
        debug(F111,"cmfld: gtword",atmbuf,xc);
        debug(F101,"cmfld x","",x);
        switch (x) {
            case -4:                    /* EOF */
            case -2:                    /* Out of space. */
            case -1:                    /* Reparse needed */
                return(x);
            case 0:                     /* SP or NL */
            case 1:
                if (xc == 0) 		/* If no input, return default. */
		  setatm(xdef);
		*xp = atmbuf;
		if (f) {		/* If a conversion function is given */
		    zq = atxbuf;	/* ... */
		    atxn = CMDBL;
		    if ((*f)(*xp,&zq,&atxn) < 0) return(-2);
		    setatm(atxbuf);
		    *xp = atmbuf;
		}
                if (**xp == NUL) {	/* If variable evaluates to null */
		    setatm(xdef);	/* Stick in the default again. */
		    if (**xp == NUL) x = -3; /* If still empty, return -3. */
		}
#ifdef COMMENT
/* The following is apparently not necessary. */
/* Remove it if nothing is broken, esp. TAKE file with trailing comments */
		xx = *xp;
		debug(F111,"cmfld before trim",*xp,x);
		for (i = (int)strlen(xx) - 1; i > 0; i--)
		  if (xx[i] != SP)	/* Trim trailing blanks */
		    break;
		  else
		    xx[i] = NUL;
		debug(F111,"cmfld returns",*xp,x); 
#endif /* COMMENT */
		debug(F101,"cmfld: returns","",x);
                return(x);
            case 2:                     /* ESC */
                if (xc == 0 && *xdef != NUL) {
                    printf("%s ",xdef); /* If at beginning of field, */
#ifdef GEMDOS
		    fflush(stdout);
#endif /* GEMDOS */
                    addbuf(xdef);       /* supply default. */
		    inword = cmflgs = 0;
                    setatm(xdef);	/* Return as if whole field */
                    return(0);          /* typed, followed by space. */
                } else {
                    putchar(BEL);       /* Beep if already into field. */
                }                   
                break;
            case 3:                     /* Question mark */
                if (*xhlp == NUL)
                    printf(" Please complete this field");
                else
                    printf(" %s",xhlp);
                printf("\n%s%s",cmprom,cmdbuf);
		fflush(stdout);
                break;
        }
    x = gtword();
/*  *xp = atmbuf; */
    }
}

 
/*  C M T X T  --  Get a text string, including confirmation  */
 
/*
  Print help message 'xhlp' if ? typed, supply default 'xdef' if null
  string typed.  Returns
 
   -1 if reparse needed or buffer overflows.
    1 otherwise.
 
  with cmflgs set to return code, and xp pointing to result string.
*/
int
cmtxt(xhlp,xdef,xp,f) char *xhlp; char *xdef; char **xp; xx_strp f; {
 
    int x, i;
    char *xx, *zq;
    static int xc;
 
    debug(F101,"cmtxt, cmflgs","",cmflgs);
    inword = cc = 0;			/* Start atmbuf counter off at 0 */
    if (cmflgs == -1) {                 /* If reparsing, */
        xc = (int)strlen(*xp);		/* get back the total text length, */
    } else {                            /* otherwise, */
        *xp = "";                       /* start fresh. */
        xc = 0;
    }
    *atmbuf = NUL;                      /* And empty the atom buffer. */
    if ((x = cmflgs) != 1) {
        x = gtword();                   /* Get first word. */
        *xp = pp;                       /* Save pointer to it. */
    }
    debug(F101,"cmtxt (*f)","", f);
    while (1) {				/* Loop for each word in text. */
        xc += cc;                       /* Char count for all words. */
        debug(F111,"cmtxt: gtword",atmbuf,xc);
        debug(F101," x","",x);
        switch (x) {
	  case -9:			/* Buffer overflow */
	  case -4:			/* EOF */
#ifdef MAC
	  case -3:			/* Quit/Timeout */
#endif /* MAC */
	  case -2:			/* Overflow */
	  case -1:			/* Deletion */
	    return(x);
	  case 0:			/* Space */
	    xc++;			/* Just count it */
	    break;
	  case 1:			/* CR or LF */
	    if (xc == 0) *xp = xdef;
	    if (f) {			/* If a conversion function is given */
		zq = atxbuf;		/* Point to the expansion buffer */
		atxn = CMDBL;		/* specify its length */
		debug(F110,"cmtxt calling (*f)",*xp,0);
		if ((x = (*f)(*xp,&zq,&atxn)) < 0) return(-2);
		cc = (int)strlen(atxbuf);
		*xp = atxbuf;		/* and return pointer to it. */
		debug(F111,"cmtxt (*f) returns",*xp,cc);
	    }
	    xx = *xp;
	    for (i = (int)strlen(xx) - 1; i > 0; i--)
	      if (xx[i] != SP)		/* Trim trailing blanks */
		break;
	      else
		xx[i] = NUL;
	    return(x);
	  case 2:			/* ESC */
	    if (xc == 0) {
		printf("%s ",xdef);
		inword = cmflgs = 0;
#ifdef GEMDOS
		fflush(stdout);
#endif /* GEMDOS */
		cc = addbuf(xdef);
	    } else {
		putchar(BEL);
	    }
	    break;
	  case 3:			/* Question Mark */
	    if (*xhlp == NUL)
	      printf(" Text string");
	    else
	      printf(" %s",xhlp);
	    printf("\n%s%s",cmprom,cmdbuf);
	    fflush(stdout);
	    break;
	  default:
	    printf("?Unexpected return code from gtword() - %d\n",x);
	    return(-2);
        }
        x = gtword();
    }
}

 
/*  C M K E Y  --  Parse a keyword  */
 
/*
 Call with:
   table    --  keyword table, in 'struct keytab' format;
   n        --  number of entries in table;
   xhlp     --  pointer to help string;
   xdef     --  pointer to default keyword;
 
 Returns:
   -3       --  no input supplied and no default available
   -2       --  input doesn't uniquely match a keyword in the table
   -1       --  user deleted too much, command reparse required
    n >= 0  --  value associated with keyword
*/
int
cmkey(table,n,xhlp,xdef,f)
/* cmkey */  struct keytab table[]; int n; char *xhlp, *xdef; xx_strp f; {
    return(cmkey2(table,n,xhlp,xdef,"",f));
}
int
cmkey2(table,n,xhlp,xdef,tok,f)
    struct keytab table[]; int n; char *xhlp, *xdef; char *tok; xx_strp f; {

    int i, tl, y, z, zz, xc;
    char *xp, *zq;
 
    tl = (int)strlen(tok);
    inword = xc = cc = 0;		/* Clear character counters. */
 
    if ((zz = cmflgs) == 1)             /* Command already entered? */
      setatm(xdef);			/* Yes, copy default into atom buf */
    else zz = gtword();			/* Otherwise get a command word */
 
debug(F101,"cmkey: table length","",n);
debug(F101," cmflgs","",cmflgs);
debug(F101," zz","",zz);
while (1) {
    xc += cc;
    debug(F111,"cmkey: gtword",atmbuf,xc);
 
    switch(zz) {
        case -4:                        /* EOF */
#ifdef MAC
	case -3:			/* Quit/Timeout */
#endif /* MAC */
        case -2:                        /* Buffer overflow */
        case -1:                        /* Or user did some deleting. */
            return(cmflgs = zz);
 
        case 0:                         /* User terminated word with space */
        case 1:                         /* or newline */
            if (cc == 0) setatm(xdef);	/* Supply default if user typed nada */
	    if (f) {			/* If a conversion function is given */
		zq = atxbuf;		/* apply it */
		atxn = CMDBL;
		if ((*f)(atmbuf,&zq,&atxn) < 0) return(-2);
		debug(F110,"cmkey atxbuf after *f",atxbuf,0);
		setatm(atxbuf);
	    }
            y = lookup(table,atmbuf,n,&z); /* Look up the word in the table */
            switch (y) {
                case -2:		/* Ambiguous */
                    printf("?Ambiguous - %s\n",atmbuf);
		    cmflgs = -2;
                    return(-9);
                case -1:		/* Not found at all */
		    if (tl) {
			for (i = 0; i < tl; i++) /* Check for token */
			  if (tok[i] == *atmbuf) { /* Got one */
			      ungword();  /* Put back the following word */
			      return(-5); /* Special return code for token */
			  }
		    }
		    /* Kludge alert... only print error if */
		    /* we were called as cmkey2, but not cmkey... */
		    /* This doesn't seem to always work. */
		    if (tl == 0) {
			printf("?No keywords match - %s\n",atmbuf); /* cmkey */
			return(cmflgs = -9);
		    } else { 
			if (cmflgs == 1) return(cmflgs = -6); /* cmkey2 */
			else return(cmflgs = -2);
			/* The -6 code is to let caller try another table */
		    }
                default:
                    break;
		}
            return(y);
 
        case 2:                         /* User terminated word with ESC */
            if (cc == 0) {
                if (*xdef != NUL) {     /* Nothing in atmbuf */
                    printf("%s ",xdef); /* Supply default if any */
#ifdef GEMDOS
		    fflush(stdout);
#endif /* GEMDOS */
                    addbuf(xdef);
                    setatm(xdef);
		    inword = cmflgs = 0;
                    debug(F111,"cmkey: default",atmbuf,cc);
                } else {
                    putchar(BEL);       /* No default, just beep */
                    break;
                }
            }
	    if (f) {			/* If a conversion function is given */
		zq = atxbuf;		/* apply it */
		atxn = CMDBL;
		if ((*f)(atmbuf,&zq,&atxn) < 0) return(-2);
		setatm(atxbuf);
	    }
            y = lookup(table,atmbuf,n,&z); /* Something in atmbuf */
            debug(F111,"cmkey: esc",atmbuf,y);
            if (y == -2) {		/* Ambiguous */
                putchar(BEL);
                break;
            }
            if (y == -1) {		/* Not found */
                /* if (tl == 0) */ printf("?No keywords match - %s\n",atmbuf);
		cmflgs = -2;
                return(-9);
            }
/*
  See if the keyword just found has the CM_ABR bit set in its flgs field, and
  if so, search forwards in the table for a keyword that has the same kwval
  but does not have CM_ABR (or CM_INV?) set, and then expand using the full
  keyword.  WARNING: This assumes that (a) keywords are in alphabetical order,
  and (b) the CM_ABR bit is set only if the the abbreviated keyword is a true
  abbreviation (left substring) of the full keyword.
*/
	    if (test(table[z].flgs,CM_ABR)) {
		int zz;
		for (zz = z+1; zz < n; zz++)
		  if ((table[zz].kwval == table[z].kwval) &&
		      (!test(table[zz].flgs,CM_ABR))) {
		      z = zz;
		      break;
		  }
            } 
            xp = table[z].kwd + cc;
            printf("%s ",xp);
#ifdef GEMDOS
	    fflush(stdout);
#endif /* GEMDOS */
            addbuf(xp);
	    inword = cmflgs = 0;
            debug(F110,"cmkey: addbuf",cmdbuf,0);
            return(y);
 
        case 3:                         /* User typed "?" */
	    if (f) {			/* If a conversion function is given */
		zq = atxbuf;		/* do the conversion now. */
		atxn = CMDBL;
		if ((*f)(atmbuf,&zq,&atxn) < 0) return(-2);
		setatm(atxbuf);
	    }
            y = lookup(table,atmbuf,n,&z); /* Look up what we have so far. */

	    if (y == -1) {
                /* if (tl == 0) */ printf(" No keywords match\n");
		cmflgs = -2;
                return(-9);
            }
            if (*xhlp == NUL)
                printf(" One of the following:\n");
            else
                printf(" %s, one of the following:\n",xhlp);
 
	    if ((y > -1) &&
		!test(table[z].flgs,CM_ABR) &&
		((z >= n-1) || strncmp(table[z].kwd,table[z+1].kwd,cc))
		 ) {
		printf(" %s\n",table[z].kwd);
	    } else {
		clrhlp();
		for (i = 0; i < n; i++) {   
		    if (!strncmp(table[i].kwd,atmbuf,cc)
			&& !test(table[i].flgs,CM_INV)
			)
		      addhlp(table[i].kwd);
		}
		dmphlp();
	    }
	    if (*atmbuf == NUL) {
		if (tl == 1)
		  printf("or the token '%c'\n",*tok);
		else if (tl > 1) printf("or one of the tokens '%s'\n",tok);
	    }
            printf("%s%s", cmprom, cmdbuf);
	    fflush(stdout);
            break;
 
        default:            
            printf("\n%d - Unexpected return code from gtword\n",zz);
            return(cmflgs = -2);
        }
        zz = gtword();
    }
}
int
chktok(tlist) char *tlist; {
    char *p;
    p = tlist;
    while (*p != NUL && *p != *atmbuf) p++;
    return((*p) ? (int) *p : 0);
}

/*  C M C F M  --  Parse command confirmation (end of line)  */
 
/*
 Returns
   -2: User typed anything but whitespace or newline
   -1: Reparse needed
    0: Confirmation was received
*/
int
cmcfm() {
    int x, xc;
 
    debug(F101,"cmcfm: cmflgs","",cmflgs);
    debug(F110,"cmcfm: atmbuf",atmbuf,0);
    inword = xc = cc = 0;
    if (cmflgs == 1) return(0);

    setatm("");				/* (Probably unnecessary) */
 
    while (1) {
        x = gtword();
        xc += cc;
        switch (x) {
            case -4:                    /* EOF */
            case -2:
            case -1:
                return(x);
 
            case 1:                     /* End of line */
                if (xc > 0) {
                    printf("?Not confirmed - %s\n",atmbuf);
                    return(-9);
                } else return(0);                   
            case 2:			/* ESC */
		if (xc == 0) {
		    putchar(BEL);	/* beep & continue */
		    continue;		/* or fall thru. */
		}
            case 0:                     /* Space */
		if (xc == 0)		/* If no chars typed, continue, */
		  continue;		/* else fall thru. */
            case 3:			/* Question mark */
                if (xc > 0) {
                    printf("?Not confirmed - %s\n",atmbuf);
                    return(-9);
                }
                printf("\n Type a carriage return to confirm the command\n");
                printf("%s%s",cmprom,cmdbuf);
		fflush(stdout);
                continue;
        }
    }
}

/* Keyword help routines */
 
 
/*  C L R H L P -- Initialize/Clear the help line buffer  */
 
VOID
clrhlp() {                              /* Clear the help buffer */
    hlpbuf[0] = NUL;
    hh = hx = 0;
}
 
 
/*  A D D H L P  --  Add a string to the help line buffer  */
 
VOID
addhlp(s) char *s; {                    /* Add a word to the help buffer */
    int j;
 
    hh++;                               /* Count this column */
 
    for (j = 0; (j < hc) && (*s != NUL); j++) { /* Fill the column */
        hlpbuf[hx++] = *s++;
    }
    if (*s != NUL)                      /* Still some chars left in string? */
        hlpbuf[hx-1] = '+';             /* Mark as too long for column. */
 
    if (hh < (hw / hc)) {               /* Pad col with spaces if necessary */
        for (; j < hc; j++) {
            hlpbuf[hx++] = SP;
        }
    } else {                            /* If last column, */
        hlpbuf[hx++] = NUL;             /* no spaces. */
        dmphlp();                       /* Print it. */
        return;
    }
}
 
 
/*  D M P H L P  --  Dump the help line buffer  */
 
VOID
dmphlp() {                              /* Print the help buffer */
    hlpbuf[hx++] = NUL;
    printf(" %s\n",hlpbuf);
    clrhlp();
}

 
/*  G T W O R D  --  Gets a "word" from the command input stream  */
 
/*
Usage: retcode = gtword();
 
Returns:
 -4 if end of file (e.g. pipe broken)
 -2 if command buffer overflows
 -1 if user did some deleting
  0 if word terminates with SP or tab
  1 if ... CR
  2 if ... ESC
  3 if ... ? (question mark)
 
With:
  pp pointing to beginning of word in buffer
  bp pointing to after current position
  atmbuf containing a copy of the word
  cc containing the number of characters in the word copied to atmbuf
*/

int
ungword() {				/* unget a word */
    if (ungw) return(0);
    cmfsav = cmflgs;
    debug(F101,"ungword cmflgs","",cmflgs);
    ungw = 1;
    cmflgs = 0;
    return(0);
}

int
gtword() {
    int c;                              /* Current char */
    int quote = 0;                      /* Flag for quote character */
    int echof = 0;                      /* Flag for whether to echo */
    int chsrc = 0;			/* Source of character, 1 = tty */
    int comment = 0;			/* Flag for in comment */
    char *cp = NULL;			/* Comment pointer */

#ifdef RTU
    extern int rtu_bug;
#endif /* RTU */

#ifdef datageneral
    extern int termtype;                /* DG terminal type flag */
    extern int con_reads_mt;            /* Console read asynch is active */
    if (con_reads_mt) connoi_mt();      /* Task would interfere w/cons read */
#endif /* datageneral */
 
    if (ungw) {				/* Have a word saved? */
	debug(F110,"gtword ungetting from pp",pp,0);
	while (*pp++ == SP) ;
	setatm(pp);
	ungw = 0;
	cmflgs = cmfsav;
	debug(F111,"gtword returning atmbuf",atmbuf,cmflgs);
	return(cmflgs);
    }
    pp = np;                            /* Start of current field */

    debug(F111,"gtword: cmdbuf",cmdbuf,cmdbuf);
    debug(F111," bp",bp,bp);
    debug(F111," pp",pp,pp);
 
    while (bp < cmdbuf+CMDBL) {         /* Big get-a-character loop */
	echof = 0;			/* Assume we don't echo because */
	chsrc = 0;			/* character came from reparse buf. */

        if ((c = *bp) == NUL) {         /* If no char waiting in reparse buf */
            if (dpx) echof = 1;         /* must get from tty, set echo flag. */
	    c = cmdgetc();		/* Read a character from the tty. */
	    chsrc = 1;			/* Remember character source is tty. */
#ifdef MAC
	    if (c == -3)		/* If null command... */
	      return(-3);
#endif /* MAC */
            if (c == EOF) {		/* This can happen if stdin not tty. */
#ifdef EINTR
		if (errno == EINTR)	/* This is for when bg'd process is */
		  continue;		/* fg'd again. */
#endif /* EINTR */
		return(-4);
	    }
	    c &= cmdmsk;		/* Strip any parity bit */
	}				/* if desired. */
#ifndef MAC
	debug(F000,"gtword char","",c);
#endif /* MAC */

/* Now we have the next character */

        if (quote == 0) {		/* If this is not a quoted character */
            if (c == CMDQ) {		/* Got the quote character itself */
		if (!comment) quote = 1; /* Flag it if not in a comment */
            }
	    if (c == FF) {		/* Formfeed. */
                c = NL;                 /* Replace with newline */
#ifdef COMMENT
/* No more screen clearing... */
		cmdclrscn();		/* Clear the screen */
#endif /* COMMENT */
            }
	    if (c == HT) {		/* Tab */
		if (comment)		/* If in comment, */
		  c = SP;		/* substitute space */
		else			/* otherwise */
		  c = ESC;		/* substitute ESC (for completion) */
	    }
	    if (c == ';' || c == '#') { /* Trailing comment */
		if (inword == 0) {	/* If we're not in a word */
		    comment = 1;	/* start a comment. */
		    cp = bp;		/* remember where it starts. */
		}
	    }
	    if (!comment && c == SP) {	/* Space */
                *bp++ = c;		/* deposit in buffer if not already */
                if (echof) putchar(c);  /* echo it. */
                if (inword == 0) {      /* If leading, gobble it. */
                    pp++;
                    continue;
                } else {                /* If terminating, return. */
                    np = bp;
                    setatm(pp);
                    inword = cmflgs = 0;
		    return(0);
                }
            }
            if (c == NL || c == CR) {   /* CR or LF. */
		if (echof) cmdnewl((char)c); /* Echo it. */
		while (bp > pp && (*(bp-1) == SP || *(bp-1) == HT)) /* Trim */
		  bp--;			/* trailing */
		*bp = NUL;		/* whitespace */
		if (*(bp-1) == '-') {	/* Is this line continued? */
		    if (chsrc) {	/* If reading from tty, */
#ifdef COMMENT
			bp--, pp--;	/* back up the buffer pointer, */
#else
			bp--;
#endif /* COMMENT */
			*bp = NUL;	/* erase the dash, */
			continue;	/* and go back for next char now. */
		    }
		} else {		/* No, a command has been entered. */
		    *bp = NUL;		/* Terminate the command string. */
		    if (comment) {	/* If we're in a comment, */
			comment = 0;	/* Say we're not any more, */
			*cp = NUL;	/* cut it off. */
		    }
		    np = bp;		/* Where to start next field. */
		    setatm(pp);		/* Copy this field to atom buffer. */
		    inword = 0;		/* Not in a word any more. */
		    return(cmflgs = 1);
		}
            }
            if (!comment && echof && (c == '?')) { /* Question mark */
                putchar(c);
                *bp = NUL;
                setatm(pp);
                return(cmflgs = 3);
            }
            if (c == ESC) {		/* ESC */
		if (!comment) {
		    *bp = NUL;
		    setatm(pp);
		    return(cmflgs = 2);
		} else {
		    putchar(BEL);
		    continue;
		}
            }
            if (c == BS || c == RUB) {  /* Character deletion */
                if (bp > cmdbuf) {      /* If still in buffer... */
		    cmdchardel();	/* erase it. */
                    bp--;               /* point behind it, */
                    if (*bp == SP) inword = 0; /* Flag if current field gone */
                    *bp = NUL;          /* Erase character from buffer. */
                } else {                /* Otherwise, */
                    putchar(BEL);       /* beep, */
                    cmres();            /* and start parsing a new command. */
		    *bp = *atmbuf = NUL;
                }
                if (pp < bp) continue;
                else return(cmflgs = -1);
            }
            if (c == LDEL) {            /* ^U, line deletion */
                while ((bp--) > cmdbuf) {
                    cmdchardel();
                    *bp = NUL;
                }
                cmres();                /* Restart the command. */
		*bp = *atmbuf = NUL;
                inword = 0;
                return(cmflgs = -1);
            }
            if (c == WDEL) {            /* ^W, word deletion */
                if (bp <= cmdbuf) {     /* Beep if nothing to delete */
                    putchar(BEL);
                    cmres();
		    *bp = *atmbuf = NUL;
                    return(cmflgs = -1);
                }
                bp--;
                for ( ; (bp >= cmdbuf) && (*bp == SP) ; bp--) {
                    cmdchardel();
                    *bp = NUL;
                }
                for ( ; (bp >= cmdbuf) && (*bp != SP) ; bp--) {
                    cmdchardel();
                    *bp = NUL;
                }
                bp++;
                inword = 0;
                return(cmflgs = -1);
            }
            if (c == RDIS) {            /* ^R, redisplay */
#ifdef COMMENT
                *bp = NUL;
                printf("\n%s%s",cmprom,cmdbuf);
#else
		char *cpx; char cx;
                *bp = NUL;
                printf("\n%s",cmprom);
		cpx = cmdbuf;
		while (cx = *cpx++) {
#ifdef isprint
		    putchar(isprint(cx) ? cx : '^');
#else
		    putchar((cx >= SP && cx < DEL) ? cx : '^');
#endif /* isprint */
		}
#endif /* COMMENT */
		fflush(stdout);
                continue;
            }
	    if (c < SP && quote == 0) {	/* Any other unquoted control char */
		if (!chsrc) bp++;	/* If cmd file, point past it */
		else putchar(BEL);	/* otherwise just beep and */
		continue;		/* continue, don't put in buffer */
	    }
	    if (echof) cmdecho((char) c, 0); /* Echo what was typed. */
        } else {			/* This character was quoted. */
	    int qf = 1;
	    quote = 0;			/* Unset the quote flag. */
	    /* Quote character at this level is only for SP, ?, and controls */
            /* If anything else was quoted, leave quote in, and let */
	    /* the command-specific parsing routines handle it, e.g. \007 */
	    if (c > 32 && c != '?' && c != RUB && chsrc != 0) {
		*bp++ = CMDQ;		/* Deposit \ if it came from tty */
		qf = 0;			/* and don't erase it from screen */
	    }
	    if (echof) cmdecho((char) c, qf); /* Now echo quoted character */
	    debug(F000,"gtword quote",cmdbuf,c);
	}
#ifdef COMMENT
        if (echof) cmdecho((char) c,quote); /* Echo what was typed. */
#endif /* COMMENT */
        if (!comment) inword = 1;	/* Flag we're in a word. */
	if (quote) continue;		/* Don't deposit quote character. */
        if (c != NL) *bp++ = c;		/* Deposit command character. */
    }                                   /* End of big while */
    putchar(BEL);                       /* Get here if... */
    printf("?Command too long, maximum length: %d.\n",CMDBL);
    cmflgs = -2;
    return(-9);
}

/* Utility functions */
 
/* A D D B U F  -- Add the string pointed to by cp to the command buffer  */
 
int
addbuf(cp) char *cp; {
    int len = 0;
    while ((*cp != NUL) && (bp < cmdbuf+CMDBL)) {
        *bp++ = *cp++;                  /* Copy and */
        len++;                          /* count the characters. */
    }   
    *bp++ = SP;                         /* Put a space at the end */
    *bp = NUL;                          /* Terminate with a null */
    np = bp;                            /* Update the next-field pointer */
    return(len);                        /* Return the length */
}
 
/*  S E T A T M  --  Deposit a token in the atom buffer.  */
/*  Break on space, newline, carriage return, or null. */
/*  Null-terminate the result. */
/*  If the source pointer is the atom buffer itself, do nothing. */
/*  Return length of token, and also set global "cc" to this length. */

int
setatm(cp) char *cp; {
    char *ap, *xp;

    cc = 0;				/* Character counter */
    ap = atmbuf;			/* Address of atom buffer */

    if (cp == ap) {			/* In case source is atom buffer */
	xp = atybuf;			/* make a copy */
	strcpy(xp,ap);			/* so we can copy it back, edited. */
	cp = xp;
    }
    *ap = NUL;				/* Zero the atom buffer */

    while (*cp == SP) cp++;		/* Trim leading spaces */
    while ((*cp != SP) && (*cp != NL) && (*cp != NUL) && (*cp != CR)) {
        *ap++ = *cp++;			/* Copy up to SP, NL, CR, or end */
        cc++;				/* and count */
    }
    *ap = NUL;				/* Terminate the string. */
    return(cc);                         /* Return length. */
}
 
/*  R D I G I T S  -- Verify that all the characters in line ARE DIGITS  */
 
int
rdigits(s) char *s; {
    while (*s) {
        if (!isdigit(*s)) return(0);
        s++;
    }
    return(1);
}

/* These functions attempt to hide system dependencies from the mainline */
/* code in gtword().  Ultimately they should be moved to ck?tio.c, where */
/* ? = each and every system supported by C-Kermit. */

int
cmdgetc() {				/* Get a character from the tty. */
    int c;

#ifdef datageneral
    {
	char ch;
	c = dgncinb(0,&ch,1);		/* -1 is EOF, -2 TO, 
                                         * -c is AOS/VS error */
	if (c == -2) {			/* timeout was enabled? */
	    resto(channel(0));		/* reset timeouts */
	    c = dgncinb(0,&ch,1);	/* retry this now! */
	}
	if (c < 0) return(-4);		/* EOF or some error */
	else c = (int) ch & 0177;	/* Get char without parity */
/*	echof = 1; */
    }
#else /* Not datageneral */
#ifdef GEMDOS
    c = isatty(0) ? coninc(0) : getchar();
#else
#ifdef OS2
    c = isatty(0) ? coninc(0) : getchar();
    if (c < 0) return(-4);
#else /* Not OS2 */
    c = getchar();			/* or from tty. */
#ifdef RTU
    if (rtu_bug) {
	c = getchar();			/* RTU doesn't discard the ^Z */
	rtu_bug = 0;
    }
#endif /* RTU */
#endif /* OS2 */
#endif /* GEMDOS */
#endif /* datageneral */
    return(c);				/* Return what we got */
}


#ifdef COMMENT
/*
  No more screen clearing.  If you wanna clear the screen, define a macro
  to do it, like "define cls write screen \27[;H\27[2J".
*/
cmdclrscn() {				/* Clear the screen */

#ifdef aegis
    putchar(FF);
#else
#ifdef AMIGA
    putchar(FF);
#else
#ifdef OSK
    putchar(FF);
#else
#ifdef datageneral
    putchar(FF);
#else
#ifdef OS2
    zsystem("cls");
#else
    zsystem("clear");
#endif /* OS2 */
#endif /* datageneral */
#endif /* OSK */
#endif /* AMIGA */
#endif /* aegis */
}
#endif /* COMMENT */

VOID					/* What to echo at end of command */
#ifdef CK_ANSIC
cmdnewl(char c)
#else
cmdnewl(c) char c;
#endif /* CK_ANSIC */
/* cmdnewl */ {
    putchar(c);				/* c is the terminating character */
#ifdef WINTCP
    if (c == CR) putchar(NL);
#endif /* WINTCP */
#ifdef OS2
    if (c == CR) putchar(NL);
#endif /* OS2 */
#ifdef aegis
    if (c == CR) putchar(NL);
#endif /* aegis */
#ifdef AMIGA
    if (c == CR) putchar(NL);
#endif /* AMIGA */
#ifdef datageneral
    if (c == CR) putchar(NL);
#endif /* datageneral */
#ifdef GEMDOS
    if (c == CR) putchar(NL);
#endif /* GEMDOS */
}

VOID
cmdchardel() {				/* Erase a character from the screen */
    if (!dpx) return;
#ifdef datageneral
    /* DG '\b' is EM (^y or \031) */
    if (termtype == 1)
      /* Erase a character from non-DG screen, */
      dgncoub(1,"\010 \010",3);
    else
#endif
      printf("\b \b");
#ifdef GEMDOS
    fflush(stdout);
#endif /* GEMDOS */
}

VOID
#ifdef CK_ANSIC
cmdecho(char c, int quote)
#else
cmdecho(c,quote) char c; int quote;
#endif /* CK_ANSIC */
{ /* cmdecho */
    if (!dpx) return;
    /* Echo tty input character c */
    if (quote) {
	putchar(BS); putchar(SP); putchar(BS); 
#ifdef isprint
	putchar( isprint(c) ? c : '^' );
#else
	putchar((c >= SP && c < DEL) ? c : '^');
#endif /* isprint */
    } else putchar(c);
#ifdef OS2
    if (quote==1 && c==CR) putchar(NL);
#endif /* OS2 */
}

#endif /* NOICP */

#ifdef NOICP
#include "ckcdeb.h"
#include "ckucmd.h"
#include "ckcasc.h"
/*** #include <ctype.h> (ckcdeb.h already includes this) ***/
#endif /* NOICP */

/*  X X E S C  --  Interprets backslash codes  */
/*  Returns the int value of the backslash code if it is > -1 and < 256 */
/*  and updates the string pointer to first character after backslash code. */
/*  If the argument is invalid, leaves pointer unchanged and returns -1. */

int
xxesc(s) char **s; {			/* Expand backslash escapes */
    int x, y, brace, radix;		/* Returns the int value */
    char hd = '9';			/* Highest digit in radix */
    char *p;

    p = *s;				/* pointer to beginning */
    if (!p) return(-1);			/* watch out for null pointer */
    x = *p++;				/* character at beginning */
    if (x != CMDQ) return(-1);		/* make sure it's a backslash code */

    x = *p;				/* it is, get the next character */
    if (x == '{') {			/* bracketed quantity? */
	p++;				/* begin past bracket */
	x = *p;
	brace = 1;
    } else brace = 0;
    switch (x) {			/* Start interpreting */
      case 'd':				/* Decimal radix indicator */
      case 'D':
	p++;				/* Just point past it and fall thru */
      case '0':				/* Starts with digit */
      case '1':
      case '2':  case '3':  case '4':  case '5':
      case '6':  case '7':  case '8':  case '9':
	radix = 10;			/* Decimal */
	hd = '9';			/* highest valid digit */
	break;
      case 'o':				/* Starts with o or O */
      case 'O':
	radix = 8;			/* Octal */
	hd = '7';			/* highest valid digit */
	p++;				/* point past radix indicator */
	break;
      case 'x':				/* Starts with x or X */
      case 'X':
	radix = 16;			/* Hexadecimal */
	p++;				/* point past radix indicator */
	break;
      default:				/* All others */
#ifdef COMMENT
	*s = p+1;			/* Treat as quote of next char */
	return(*p);
#else
	return(-1);
#endif /* COMMENT */
    }
    /* For OS/2, there are "wide" characters required for the keyboard
     * binding, i.e \644 and similar codes larger than 255 (byte).
     * For this purpose, give up checking for < 256. If someone means
     * \266 should result in \26 followed by a "6" character, he should
     * always write \{26}6 anyway.  Now, return only the lower byte of
     * the result, i.e. 10, but eat up the whole \266 sequence and
     * put the wide result 266 into a global variable.  Yes, that's not
     * the most beautiful programming style but requires the least
     * amount of changes to other routines.
     */
    if (radix <= 10) {			/* Number in radix 8 or 10 */
	for ( x = y = 0;
 	      (*p) && (*p >= '0') && (*p <= hd)
#ifdef OS2
                   && (y < 4) && (x*radix < 768);
              /* the maximum needed value \767 is still only 3 digits long */
              /* while as octal it requires \1377, i.e. 4 digits */
#else
                   && (y < 3) && (x*radix < 256);
#endif /* OS2 */
	      p++,y++) {
	    x = x * radix + (int) *p - 48;
	}
#ifdef OS2
        wideresult = x;			/* Remember wide result */
        x &= 255;
#endif /* OS2 */
	if (y == 0 || x > 255) {	/* No valid digits? */
	    *s = p;			/* point after it */
	    return(-1);			/* return failure. */
	}
    } else if (radix == 16) {		/* Special case for hex */
	if ((x = unhex(*p++)) < 0) { *s = p - 1; return(-1); }
	if ((y = unhex(*p++)) < 0) { *s = p - 2; return(-1); }
	x = ((x << 4) & 0xF0) | (y & 0x0F);
#ifdef OS2
        wideresult = x;
        if ((y = unhex(*p)) >= 0) {
           p++;
	   wideresult = ((x << 4) & 0xFF0) | (y & 0x0F);
           x = wideresult & 255;
        }
#endif /* OS2 */
    } else x = -1;
    if (brace && *p == '}' && x > -1)	/* Point past closing brace, if any */
      p++;
    *s = p;				/* Point to next char after sequence */
    return(x);				/* Return value of sequence */
}

int					/* Convert hex string to int */
#ifdef CK_ANSIC
unhex(char x)
#else
unhex(x) char x;
#endif /* CK_ANSIC */
/* unhex */ {

    if (x >= '0' && x <= '9')		/* 0-9 is offset by hex 30 */
      return(x - 0x30);
    else if (x >= 'A' && x <= 'F')	/* A-F offset by hex 37 */
      return(x - 0x37);
    else if (x >= 'a' && x <= 'f')	/* a-f offset by hex 57 */
      return(x - 0x57);			/* (obviously ASCII dependent) */
    else return(-1);
}

/* See if argument string is numeric */
/* Returns 1 if OK, zero if not OK */
/* If OK, string should be acceptable to atoi() */
/* Allows leading space, sign */

int
chknum(s) char *s; {			/* Check Numeric String */
    int x = 0;				/* Flag for past leading space */
    int y = 0;				/* Flag for digit seen */
    char c;
    debug(F110,"chknum",s,0);
    while (c = *s++) {			/* For each character in the string */
	switch (c) {
	  case SP:			/* Allow leading spaces */
	  case HT:
	    if (x == 0) continue;
	    else return(0);
	  case '+':			/* Allow leading sign */
	  case '-':
	    if (x == 0) x = 1;
	    else return(0);
	    break;
	  default:			/* After that, only decimal digits */
	    if (c >= '0' && c <= '9') {
		x = y = 1;
		continue;
	    } else return(0);
	}
    }
    return(y);
}

/*  L O W E R  --  Lowercase a string  */
 
int
lower(s) char *s; {
    int n = 0;
    while (*s) {
        if (isupper(*s)) *s = tolower(*s);
        s++, n++;
    }
    return(n);
}

/*  L O O K U P  --  Lookup the string in the given array of strings  */
 
/*
 Call this way:  v = lookup(table,word,n,&x);
 
   table - a 'struct keytab' table.
   word  - the target string to look up in the table.
   n     - the number of elements in the table.
   x     - address of an integer for returning the table array index.
 
 The keyword table must be arranged in ascending alphabetical order, and
 all letters must be lowercase.
 
 Returns the keyword's associated value ( zero or greater ) if found,
 with the variable x set to the array index, or:
 
  -3 if nothing to look up (target was null),
  -2 if ambiguous,
  -1 if not found.
 
 A match is successful if the target matches a keyword exactly, or if
 the target is a prefix of exactly one keyword.  It is ambiguous if the
 target matches two or more keywords from the table.
*/
 
int
lookup(table,cmd,n,x) char *cmd; struct keytab table[]; int n, *x; {
 
    int i, v, cmdlen;
 
/* Lowercase & get length of target, if it's null return code -3. */
 
    if ((((cmdlen = lower(cmd))) == 0) || (n < 1)) return(-3);
 
/* Not null, look it up */
 
    for (i = 0; i < n-1; i++) {
        if (!strcmp(table[i].kwd,cmd) ||
           ((v = !strncmp(table[i].kwd,cmd,cmdlen)) &&
             strncmp(table[i+1].kwd,cmd,cmdlen))) {
                *x = i;
                return(table[i].kwval);
             }
        if (v) return(-2);
    }   
 
/* Last (or only) element */
 
    if (!strncmp(table[n-1].kwd,cmd,cmdlen)) {
        *x = n-1;
        return(table[n-1].kwval);
    } else return(-1);
}
