/*
 * @(#)startup.c	1.1	%G%
 *
 * Startup file processing for the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include "gremlin.h"
#include <vfont.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>

/* imports from C */

extern char *malloc();
extern FILE *fopen();

/* imports from path.c */

extern PSetPath();

/* imports from graphics files */

extern char *font_types[];		/* normally "R", "I", "B", "S" */
extern font_sizes[];			/* normally 7, 10, 14, 24 */
extern char fontdir[];			/* font directory */
extern char stippledir[];		/* stipple directory */
extern char stippletype[];		/* stipple font type ("ug" or "cf") */
extern stipple_index[];			/* stipple font numbers */

/* imports from main.c */

extern Artmode;				/* point display size */
extern CBRUSH;				/* current brush */
extern CFONT;				/* current font */
extern CJUST;				/* current text justification */
extern CSIZE;				/* current size */
extern CSTIPPLE;			/* current stipple pattern */
extern Alignment;			/* point alignment indicator */
extern Adjustment;			/* point adjustment mode */
extern GravityOn;			/* gravity mode flag */
extern Gridon;				/* grid mode indicator */
extern SymbolicLines;			/* pr_vector OK for all lines */
extern long timeon_s, timeon_ms;	/* current set flash on rate */
extern long timeoff_s, timeoff_ms;	/* current set flash off rate */

/* locally defined variables */

#define STon(s)  (strncmp(s, "on", 2) == 0)

int STERROR = FALSE;			/* indicates error on startup */

static char *lines[] = { "broken", "dashed", "dotted",
			 "medium", "narrow", "thick", NULL };
static lnum[] = { 2, 4, 1, 6, 5, 3 };

static char *textpos[JUSTMODES] = { "tl", "tc", "tr",
				    "cl", "cc", "cr",
				    "bl", "bc", "br",
				    "lt", "ct", "rt",
				    "lc",       "rc",
				    "lb", "cb", "rb"  };
static textmode[JUSTMODES] = { TOPLEFT,  TOPCENT,  TOPRIGHT,
			       CENTLEFT, CENTCENT, CENTRIGHT,
			       BOTLEFT,  BOTCENT,  BOTRIGHT,
			       TOPLEFT,  TOPCENT,  TOPRIGHT,
			       CENTLEFT,           CENTRIGHT,
			       BOTLEFT,  BOTCENT,  BOTRIGHT  };

static char *fonts[] = { "bold", "italics", "roman", "special", NULL };
static fnum[] = { 3, 2, 1, 4 };

/* The following are defined to allow creation of the command lookup table. */

extern ST1(), ST2(), ST3(), ST4(), STB(), STI(), STR(), STS(),
    STalign(), STbrush(), STflashoff(), STflashon(), STfont(), STfontdir(),
    STgravity(), STgrid(), SThadjust(), STjustify(), STlittlepoint(),
    STmadjust(), STpath(), STsize(), STstipple(),
    STstipple1(), STstipple2(), STstipple3(), STstipple4(),
    STstipple5(), STstipple6(), STstipple7(), STstipple8(),
    STstippledir(), STstippletype(), STsymboliclines(), STvadjust();

/* The following two arrays define the startup commands and the routines
   that process them. */

static char *startupcmds[] = {
    "1",
    "2",
    "3",
    "4",
    "B",
    "I",
    "R",
    "S",
    "align",
    "brush",
    "flashoff",
    "flashon",
    "font",
    "fontdir",
    "gravity",
    "grid",
    "hadjust",
    "justify",
    "l1",
    "l2",
    "l3",
    "l4",
    "l5",
    "l6",
    "l7",
    "l8",
    "littlepoint",
    "madjust",
    "path",
    "size",
    "stipple",
    "stipple1",
    "stipple2",
    "stipple3",
    "stipple4",
    "stipple5",
    "stipple6",
    "stipple7",
    "stipple8",
    "stippledir",
    "stippletype",
    "symboliclines",
    "vadjust",
    NULL
};

static (*(strtns[]))() = {
    ST1,			/* set size 1 point size */
    ST2,			/* set size 2 point size */
    ST3,			/* set size 3 point size */
    ST4,			/* set size 4 point size */
    STB,			/* set B font type */
    STI,			/* set I font type */
    STR,			/* set R font type */
    STS,			/* set S font type */
    STalign,			/* set point alignment */
    STbrush,			/* set brush */
    STflashoff,			/* set current set flash off time */
    STflashon,			/* set current set flash on time */
    STfont,			/* set font */
    STfontdir,			/* font directory */
    STgravity,			/* toggle gravity */
    STgrid,			/* toggle grid display */
    SThadjust,			/* horizontal adjust */
    STjustify,			/* text justification */
    STstipple1,			/* set stipple #1 index equivalent */
    STstipple2,			/* set stipple #2 index equivalent */
    STstipple3,			/* set stipple #3 index equivalent */
    STstipple4,			/* set stipple #4 index equivalent */
    STstipple5,			/* set stipple #5 index equivalent */
    STstipple6,			/* set stipple #6 index equivalent */
    STstipple7,			/* set stipple #7 index equivalent */
    STstipple8,			/* set stipple #8 index equivalent */
    STlittlepoint,		/* point size */
    STmadjust,			/* manhattan adjust */
    STpath,			/* set path or toggle search mode */
    STsize,			/* character size */
    STstipple,			/* set stipple pattern */
    STstipple1,			/* set stipple #1 index */
    STstipple2,			/* set stipple #2 index */
    STstipple3,			/* set stipple #3 index */
    STstipple4,			/* set stipple #4 index */
    STstipple5,			/* set stipple #5 index */
    STstipple6,			/* set stipple #6 index */
    STstipple7,			/* set stipple #7 index */
    STstipple8,			/* set stipple #8 index */
    STstippledir,		/* stipple directory */
    STstippletype,		/* stipple font file name */
    STsymboliclines,		/* toggle symbolic lines */
    STvadjust			/* vertical adjust */
};


/*
 *  Check for .gremlinrc file(s) and process if found.
 *  The only globally accessible routine in this file.
 */
STgremlinrc(altfile)
char *altfile;			/* alternate startup file */
{
    FILE *fp;
    struct stat buf;
    ino_t home_inode, cwd_inode;
    char home[128];

    /* look in home directory */
    sprintf(home, "%s/.gremlinrc", getenv("HOME"));
    fp = fopen(home, "r");
    if (fp != NULL) {
	fstat(fileno(fp), &buf);
	home_inode = buf.st_ino;
	STstartup(fp);
	fclose(fp);
    }   
    else
	home_inode = 0;

    /* look in current directory */
    fp = fopen(".gremlinrc", "r");
    if (fp != NULL) {
	fstat(fileno(fp), &buf);
	cwd_inode = buf.st_ino;
	if (cwd_inode != home_inode)        /* not in home directory */
	    STstartup(fp);
	fclose(fp);
    } 

    /* check for alternate startup file */
    if (*altfile != '\0') {
	fp = fopen(altfile, "r");
	if (fp != NULL) {
	    fstat(fileno(fp), &buf);
	    if ((buf.st_ino != home_inode) && (buf.st_ino != cwd_inode))
		STstartup(fp);
	    fclose(fp);
	}
    }
}  /* end STgremlinrc */


static 
STerror(s)
register char *s;
{
    char msg[256];

    STERROR = 1;
    sprintf(msg, "startup: %s", s);
    TxPutMsg(msg);
}  /* end STerror */


/*
 *  Do startup processing on open file referenced by fp.
 */
static
STstartup(fp)
register FILE *fp; 
{
    register char *s;
    register i;
    char buf[256]; 

    s = fgets(buf, 256, fp);
    while (s != NULL) {
	for (i=0; (buf[i] != '\n'); ++i)
	    if (i > 255)
		break;
	buf[i] = '\0';
	STcommand(buf);
	s = fgets(buf, 400, fp);
    }
}  /* end STstartup */


/*
 *  Make a string lower case.
 */
STtolower(s)
register char *s;
{
    register c;

    while ((c = (int) *s) != 0) {
	if (isupper(c))
	    *s = tolower(c);
	s++;
    }
}


/*
 *  STlookup searches a table of strings to find one that matches a
 *  given string.
 *
 *  If str is an unambiguous abbreviation for one of the entries
 *  in table, then the index of the matching entry is returned.
 *  If str is an abbreviation for more than one entry in table,
 *  then -1 is returned.  If str doesn't match any entry, then
 *  -2 is returned.
 *
 *  The search is carried out by using two pointers, one which moves
 *  forward through table from its start, and one which moves backward
 *  through table from its end.  The two pointers mark the range of
 *  strings that match the portion of str that we have scanned.  When
 *  all of the characters of str have been scanned, then the two
 *  pointers better be identical.
 */
static
STlookup(str, table, next)
char str[];			/* Pointer to a string to be looked up */
char *(table[]);		/* Pointer to an array of string pointers
				   which are the valid commands.  The strings
				   must be ordered monotonically (i.e. all
				   strings whose first characters are identical
				   must be adjacent in the table). */
int *next;			/* Index to delimiter after search */
{
    char **bot, **top;
    int match, index;

    match = 0;
    bot = table;
    for (top = table; *top != NULL; top++)
	;
    if (top == bot) 
	return(-2);
    top--;

    for (index=0; ; index++) {
        *next = index;

	/* Check for the end of string */
	if (Delimiter(str[index]))
	    return(((bot == top) || Delimiter((*bot)[index])) ? match : -1);

	/* 
	 * Move bot up until the string it points to matches str 
	 * in the index'th position.  
	 * Make match refer to the index of bot in table.
	 */

	while ((*bot)[index] != str[index]) {
	    if (bot == top) 
		return(-2);
	    bot++;
	    match++;
	}

	/* Move top down until it matches */

	while ((*top)[index] != str[index]) {
	    if (bot == top) 
		return(-2);
	    top--;
	}
    }
}  /* end STlookup */


/*
 *  This routine looks up and executes a startup command.
 */
static
STcommand(line)
register char *line;
{
    register index;
    register char *save;
    char buf[256];
    int next;

    if ((*line == '\0') || (*line == '#'))
        return; 

    index = STlookup(save = line, startupcmds, &next);
	
    if (index >= 0) {
	line = line + next;

	/* skip to first non-blank */
	while ((*line == '\t') || (*line == ' '))
	    line++;

	(*(strtns[index]))(line);
    }
    else if (index == -1) {
	sprintf(buf, "ambigous command: %s", save);
	STerror(buf);
    }
    else if (index == -2) {
	sprintf(buf, "unknown command: %s", save);
	STerror(buf);
    }
}  /* end STcommand */


/*
 * This routine interprets the string starting at line
 * as a positive integral numeric parameter.  The function
 * returns the numeric equivalent or -1 it there is some
 * error in interpreting the string.
 * NOTE: expects line with at least one non-space character.
 */
static
STgetnum(line)
register char *line;
{
    register i;
    char num[128];
    int result;

    for (i=0; !Delimiter(*line); ++i) {
	num[i] = *line++;
	if (!isdigit(num[i])) 
	    return(-1);
    } 

    num[i] = '\0';
    (void) sscanf(num, "%d", &result);
    return(result);
}  /* end STgetnum */


static
ST1(line)
register char *line;
{
    register size;

    if (*line == '\0')
	return;

    size = STgetnum(line);
    if (size != -1)
	font_sizes[0] = size;
    else
	STerror("bad size 1");
}


static
ST2(line)
register char *line;
{
    register size;

    if (*line == '\0')
	return;

    size = STgetnum(line);
    if (size != -1)
	font_sizes[1] = size;
    else
	STerror("bad size 2");
}


static
ST3(line)
register char *line;
{
    register size;

    if (*line == '\0')
	return;

    size = STgetnum(line);
    if (size != -1)
	font_sizes[2] = size;
    else
	STerror("bad size 3");
}


static
ST4(line)
register char *line;
{
    register size;

    if (*line == '\0')
	return;

    size = STgetnum(line);
    if (size != -1)
	font_sizes[3] = size;
    else
	STerror("bad size 4");
}


static
STB(line)
register char *line;
{
    register char *f;
    register i = 0;

    if (*line == '\0')
	return;

    f = line;
    while (!Delimiter(*f))
	f++, i++;
    
    f = malloc((unsigned) i+1);
    strncpy(f, line, i+1);
    font_types[BFONT] = f;
}


static
STI(line)
register char *line;
{
    register char *f;
    register i = 0;

    if (*line == '\0')
	return;

    f = line;
    while (!Delimiter(*f))
	f++, i++;
    
    f = malloc((unsigned) i+1);
    strncpy(f, line, i+1);
    font_types[IFONT] = f;
}


static
STR(line)
register char *line;
{
    register char *f;
    register i = 0;

    if (*line == '\0')
	return;

    f = line;
    while (!Delimiter(*f))
	f++, i++;
    
    f = malloc((unsigned) i+1);
    strncpy(f, line, i+1);
    font_types[RFONT] = f;
}


static
STS(line)
register char *line;
{
    register char *f;
    register i = 0;

    if (*line == '\0')
	return;

    f = line;
    while (!Delimiter(*f))
	f++, i++;
    
    f = malloc((unsigned) i+1);
    strncpy(f, line, i+1);
    font_types[SFONT] = f;
}


static
STalign(line)
register char *line;
{
    register a;
    register align = 1;

    if (*line == '\0')
	return;

    a = STgetnum(line);
    if (a <= 0)
	STerror("bad alignment");
    else {
	a >>= 1;
	while (a != 0) {
	    align <<= 1;
	    a >>= 1;
	}
	Alignment = (align > 512) ? 512 : align;
    }
}


static
STbrush(line)
register char *line;
{
    register newbrush;
    int index = 0;

    if (*line == '\0')
	return;

    STtolower(line);		/* force argument to lower case */

    if ((newbrush = STlookup(line, lines, &index)) >= 0)
	CBRUSH = lnum[newbrush];
    else
	STerror("bad brush name");
}


static
STflashoff(line)
register char *line;
{
    register flashoff;
    int index = 0;

    if (*line == '\0')
	return;

    flashoff = STgetnum(line);
    if (flashoff == -1)
	STerror("bad flash off time");
    else {
	timeoff_s = flashoff / 1000;
	timeoff_ms = flashoff % 1000;
    }
}


static
STflashon(line)
register char *line;
{
    register flashon;
    int index = 0;

    if (*line == '\0')
	return;

    flashon = STgetnum(line);
    if (flashon == -1)
	STerror("bad flash on time");
    else {
	timeon_s = flashon / 1000;
	timeon_ms = flashon % 1000;
    }
}


static
STfont(line)
register char *line;
{
    int font;
    int index = 0;

    if (*line == '\0')
	return;

    STtolower(line);		/* force argument to lower case */

    if ((font = STlookup(line, fonts, &index)) >= 0)
	CFONT = fnum[font];
    else 
        STerror("bad font number");
}


static
STfontdir(line)
register char *line;
{
    register i = 0;

    if (*line == '\0')
	return;

    while (!Delimiter(*line))
	fontdir[i++] = *line++;

    if (fontdir[--i] != '/')
	fontdir[++i] = '/';

    fontdir[++i] = '\0';
}


static
STgravity(line)
register char *line;
{
    STtolower(line);
    GravityOn = STon(line);
}


static
STgrid(line)
register char *line;
{
    STtolower(line);
    Gridon = STon(line);
}


static
SThadjust(line)
register char *line;
{
    STtolower(line);

    if (STon(line))
	Adjustment = HORZ;
    else if (Adjustment == HORZ)
	Adjustment = NOADJ;
    /*
    Adjustment = (Adjustment == HORZ) ? NOADJ : HORZ;
    */
}


static
STjustify(line)
register char *line;
{
    int new;
    int index = 0;

    if (*line == '\0')
	return;

    STtolower(line);		/* force argument to lower case */

    for (new = 0; (strcmp(line, textpos[new]) != 0); ++new) {
	if (new >= JUSTMODES) {
	   STerror("bad justification mode");
	   return;
	}
    }

    CJUST = textmode[new];
}


static
STlittlepoint(line)
register char *line;
{
    /*
    Artmode = !Artmode;
    */
    STtolower(line);
    Artmode = STon(line);
}


static
STmadjust(line)
register char *line;
{
    STtolower(line);		/* force argument to lower case */

    if (STon(line))
	Adjustment = MAN;
    else if (Adjustment == MAN)
	Adjustment = NOADJ;
    /*
    Adjustment = (Adjustment == MAN) ? NOADJ : MAN;
    */
}


static
STpath(line)
register char *line;
{
    if (*line == '\0')
	return;
    
    PSetPath(line);
}


static
STsize(line)
register char *line;
{
    int new;

    if (*line == '\0')
	return;

    new = STgetnum(line);

    if ((new == -1) || (new > NSIZES))
        STerror("bad font size");
    else
        CSIZE = new;
}


static
STstipple(line)
register char *line;
{
    int newstipple;

    if (*line == '\0')
	return;

    newstipple = STgetnum(line);

    if ((newstipple == -1) || (newstipple > NSTIPPLES))
        STerror("bad stipple number");
    else
	CSTIPPLE = newstipple;
}


static
STstipplenum(line, num)
register char *line;
register num;
{
    register index;

    if (*line == '\0')
	return;

    index = STgetnum(line);

    if ((index == -1) || (index >= NUM_DISPATCH))
        STerror("bad stipple font index");
    else
	stipple_index[num-1] = index;
}


static
STstipple1(line)
register char *line;
{
    STstipplenum(line, 1);
}


static
STstipple2(line)
register char *line;
{
    STstipplenum(line, 2);
}


static
STstipple3(line)
register char *line;
{
    STstipplenum(line, 3);
}


static
STstipple4(line)
register char *line;
{
    STstipplenum(line, 4);
}


static
STstipple5(line)
register char *line;
{
    STstipplenum(line, 5);
}


static
STstipple6(line)
register char *line;
{
    STstipplenum(line, 6);
}


static
STstipple7(line)
register char *line;
{
    STstipplenum(line, 7);
}


static
STstipple8(line)
register char *line;
{
    STstipplenum(line, 8);
}


static
STstippledir(line)
register char *line;
{
    register i = 0;

    if (*line == '\0')
	return;

    while (!Delimiter(*line))
	stippledir[i++] = *line++;

    if (stippledir[--i] != '/')
	stippledir[++i] = '/';

    stippledir[++i] = '\0';
}


static
STstippletype(line)
register char *line;
{
    register i = 0;

    if (*line == '\0')
	return;

    while (!Delimiter(*line))
	stippletype[i++] = *line++;

    stippletype[i] = '\0';
}


static
STsymboliclines(line)
register char *line;
{
    /*
    SymbolicLines = !SymbolicLines;
    */
    STtolower(line);
    SymbolicLines = STon(line);
}


static
STvadjust(line)
register char *line;
{
    STtolower(line);

    if (STon(line))
	Adjustment = VERT;
    else if (Adjustment == VERT)
	Adjustment = NOADJ;
    /*
    Adjustment = (Adjustment == VERT) ? NOADJ : VERT;
    */
}
