/* $Id: rt-util.c,v 3.0 1992/12/14 00:14:12 davison Trn $
*/

#include "EXTERN.h"
#include "common.h"
#include "cache.h"
#include "ngdata.h"
#include "artio.h"
#include "rthread.h"
#include "rt-select.h"
#include "term.h"
#include "INTERN.h"
#include "rt-util.h"

/* Name-munging routines written by Ross Ridge.
** Enhanced by Wayne Davison.
*/

/* Extract the full-name part of an email address, returning NULL if not
** found.
*/
char *
extract_name(name)
char *name;
{
    char *s;
    char *lparen, *rparen;
    char *langle;

    while (isspace(*name)) {
	name++;
    }

    lparen = index(name, '(');
    rparen = rindex(name, ')');
    langle = index(name, '<');
    if (!lparen && !langle) {
	return NULL;
    } else
    if (langle && (!lparen || !rparen || lparen > langle || rparen < langle)) {
	if (langle == name) {
	    return NULL;
	}
	*langle = '\0';
    } else {
	name = lparen;
	*name++ = '\0';
	while (isspace(*name)) {
	    name++;
	}
	if (name == rparen) {
	    return NULL;
	}
	if (rparen != NULL) {
	    *rparen = '\0';
	}
    }

    if (*name == '"') {
	name++;
	while (isspace(*name)) {
	    name++;
	}
	if ((s = rindex(name, '"')) != NULL) {
	    *s = '\0';
	}
    }
    return name;
}

/* If necessary, compress a net user's full name by playing games with
** initials and the middle name(s).  If we start with "Ross Douglas Ridge"
** we try "Ross D Ridge", "Ross Ridge", "R D Ridge" and finally "R Ridge"
** before simply truncating the thing.  We also turn "R. Douglas Ridge"
** into "Douglas Ridge" if it fits, otherwise it goes through the normal
** modification route.
*/
char *
compress_name(name, max)
char *name;
int max;
{
    register char *s, *last, *mid, *d;
    register int len, namelen, midlen;
    int notlast;

    /* First remove white space from both ends. */
    while (isspace(*name)) {
	name++;
    }
    if ((len = strlen(name)) == 0) {
	return name;
    }
    s = name + len - 1;
    while (isspace(*s)) {
	s--;
    }
    s[1] = '\0';
    if (s - name + 1 <= max) {
	return name;
    }

    /* Look for characters that likely mean the end of the name
    ** and the start of some hopefully uninteresting additional info.
    ** Spliting at a comma is somewhat questionalble, but since
    ** "Ross Ridge, The Great HTMU" comes up much more often than 
    ** "Ridge, Ross" and since "R HTMU" is worse than "Ridge" we do
    ** it anyways.
    */
    for (d = name + 1; *d; d++) {
	if (*d == ',' || *d == ';' || *d == '(' || *d == '@'
	 || (*d == '-' && (d[1] == '-' || d[1] == ' '))) {
	    *d-- = '\0';
	    s = d;
	    break;
	}
    }

    /* Find the last name */
    do {
	notlast = 0;
	while (isspace(*s)) {
	    s--;
	}
	s[1] = '\0';
	len = s - name + 1;
	if (len <= max) {
	    return name;
	}
	while (!isspace(*s)) {
	    if (s == name) {	/* only one name */
		name[max] = '\0';
		return name;
	    }
	    if (isdigit(*s)) {	/* probably a phone number */
		notlast = 1;	/* so chuck it */
	    }
	    s--;
	}
    } while (notlast);

    last = s-- + 1;

    /* Look for a middle name */
    while (isspace(*s)) {	/* get rid of any extra space */
	len--;	
	s--;
    }
    mid = name;
    while (!isspace(*mid)) {
	mid++;
    }
    namelen = mid - name + 1;
    if (mid == s+1) {	/* no middle name */
	mid = 0;
    } else {
	*mid++ = '\0';
	while (isspace(*mid)) {
	    len--;
	    mid++;
	}
	midlen = s - mid + 2;
	/* If first name is an initial and middle isn't and it all fits
	** without the first initial, drop it. */
	if (len > max && mid != s && mid[1] != '.'
	 && (!name[1] || (name[1] == '.' && !name[2]))
	 && len - namelen <= max) {
	    len -= namelen;
	    name = mid;
	    mid = 0;
	}
    }
    s[1] = '\0';
    if (mid && len > max) {
	/* Turn middle names into intials */
	len -= s - mid + 2;
	d = s = mid;
	while (*s) {
	    if (isalpha(*s)) {
		if (d != mid) {
		    *d++ = ' ';
		}
		*d++ = *s++;
	    }
	    while (*s && !isspace(*s)) {
		s++;
	    }
	    while (isspace(*s)) {
		s++;
	    }
	}
	if (d != mid) {
	    *d = '\0';
	    midlen = d - mid + 1;
	    len += midlen;
	} else {
	    mid = 0;
	}
    }
    if (len > max) {
	/* If the first name fits without the middle initials, drop them */
	if (mid && len - midlen <= max) {
	    len -= midlen;
	    mid = 0;
	} else {
	    /* Turn the first name into an initial */
	    len -= namelen - 2;
	    name[1] = '\0';
	    namelen = 2;
	    if (len > max) {
		/* Dump the middle initials (if present) */
		if (mid) {
		    len -= midlen;
		    mid = 0;
		}
		if (len > max) {
		    /* Finally just truncate the last name */
		    last[max - 2] = '\0';
		}
	    }
	}
    }

    /* Paste the names back together */
    d = name + namelen;
    if (mid) {
	d[-1] = ' ';
	strcpy(d, mid);
	d += midlen;
    }
    d[-1] = ' ';
    strcpy(d, last);
    return name;
}

/* Compress an email address, trying to keep as much of the local part of
** the addresses as possible.  The order of precence is @ ! %, but
** @ % ! may be better...
*/
static char *
compress_address(name, max)
char *name;
int max;
{
    char *s, *at, *bang, *hack, *start;
    int len;

    /* Remove white space from both ends. */
    while (isspace(*name)) {
	name++;
    }
    if ((len = strlen(name)) == 0) {
	return name;
    }
    s = name + len - 1;
    while (isspace(*s)) {
	s--;
    }
    s[1] = '\0';
    if (*name == '<') {
	name++;
	if (*s == '>') {
	    *s-- = '\0';
	}
    }
    if ((len = s - name + 1) <= max) {
	return name;
    }

    at = bang = hack = NULL;
    for (s = name + 1; *s; s++) {
	/* If there's whitespace in the middle then it's probably not
	** really an email address. */
	if (isspace(*s)) {
	    name[max] = '\0';
	    return name;
	}
	switch (*s) {
	case '@':
	    if (at == NULL) {
		at = s;
	    }
	    break;
	case '!':
	    if (at == NULL) {
		bang = s;
		hack = NULL;
	    }
	    break;
	case '%':
	    if (at == NULL && hack == NULL) {
		hack = s;
	    }
	    break;
	}
    }
    if (at == NULL) {
	at = name + len;
    }

    if (hack != NULL) {
	if (bang != NULL) {
	    if (at - bang - 1 >= max) {
		start = bang + 1;
	    } else if (at - name >= max) {
		start = at - max;
	    } else {
		start = name;
	    }
	} else {
	    start = name;
	}
    } else if (bang != NULL) {
	if (at - name >= max) {
	    start = at - max;
	} else {
	    start = name;
	}
    } else {
	start = name;
    }
    if (len - (start - name) > max) {
	start[max] = '\0';
    }
    return start;
}

/* Fit the author name in <max> chars.  Uses the comment portion if present
** and pads with spaces.
*/
char *
compress_from(ap, size)
ARTICLE *ap;
int size;
{
    char *s;
    int len;

    strcpy(cmd_buf, ap && ap->from? ap->from : nullstr);
    if ((s = extract_name(cmd_buf)) != NULL)
	s = compress_name(s, size);
    else
	s = compress_address(cmd_buf, size);
    len = strlen(s);
    if (!len) {
	strcpy(s,"NO NAME");
	len = 7;
    }
    while (len < size)
	s[len++] = ' ';
    s[size] = '\0';
    return s;
}

#define EQ(x,y) ((isupper(x) ? tolower(x) : (x)) == (y))

/* Parse the subject to skip past any "Re[:^]"s at the start.
*/
char *
get_subject_start(str)
register char *str;
{
    while (*str && (unsigned char)*str <= ' ')
	str++;
    while (EQ(str[0], 'r') && EQ(str[1], 'e')) {	/* check for Re: */
      register char *cp = str + 2;
	if (*cp == '^') {				/* allow Re^2: */
	    while (*++cp <= '9' && *cp >= '0')
		;
	}
	if (*cp != ':')
	    break;
	while (*++cp == ' ')
	    ;
	str = cp;
    }
    return str;
}

/* Output a subject in <max> chars.  Does intelligent trimming that tries to
** save the last two words on the line, excluding "(was: blah)" if needed.
*/
char *
compress_subj(ap, max)
ARTICLE *ap;
int max;
{
    register char *cp;
    register int len;
    ARTICLE *first;

    if (!ap)
	return "<MISSING>";

    /* Put a preceeding '>' on subjects that are replies to other articles */
    cp = buf;
    first = (ThreadedGroup? ap->subj->thread : ap->subj->articles);
    if (ap != first || (ap->flags & AF_HAS_RE)
     || !(!(ap->flags&AF_READ) ^ sel_rereading))
	*cp++ = '>';
    strcpy(cp, ap->subj->str + 4);

    /* Remove "(was: oldsubject)", because we already know the old subjects.
    ** Also match "(Re: oldsubject)".  Allow possible spaces after the ('s.
    */
    for (cp = buf; (cp = index(cp+1, '(')) != Nullch;) {
	while (*++cp == ' ')
	    ;
	if (EQ(cp[0], 'w') && EQ(cp[1], 'a') && EQ(cp[2], 's')
	 && (cp[3] == ':' || cp[3] == ' ')) {
	    *--cp = '\0';
	    break;
	}
	if (EQ(cp[0], 'r') && EQ(cp[1], 'e')
	 && ((cp[2]==':' && cp[3]==' ') || (cp[2]=='^' && cp[4]==':'))) {
	    *--cp = '\0';
	    break;
	}
    }
    len = strlen(buf);
    if (!unbroken_subjects && len > max) {
	char *last_word;
	/* Try to include the last two words on the line while trimming */ 
	if ((last_word = rindex(buf, ' ')) != Nullch) {
	    char *next_to_last;
	    *last_word = '\0';
	    if ((next_to_last = rindex(buf, ' ')) != Nullch) {
		if (next_to_last-buf >= len - max + 3 + 10-1)
		    cp = next_to_last;
		else
		    cp = last_word;
	    } else
		cp = last_word;
	    *last_word = ' ';
	    if (cp-buf >= len - max + 3 + 10-1) {
		sprintf(buf + max - (len-(cp-buf)+3), "...%s", cp + 1);
		len = max;
	    }
	}
    }
    if (len > max)
	buf[max] = '\0';
    return buf;
}

#ifndef HAS_STRCASECMP
static unsigned char casemap[256] = {
    0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,
    0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
    0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,
    0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
    0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,
    0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
    0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,
    0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
    0x40,0x61,0x62,0x63,0x64,0x65,0x66,0x67,
    0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
    0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,
    0x78,0x79,0x7A,0x7B,0x5C,0x5D,0x5E,0x5F,
    0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,
    0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
    0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,
    0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
    0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,
    0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
    0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,
    0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
    0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,
    0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
    0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,
    0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
    0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,
    0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
    0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,
    0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
    0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,
    0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
    0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,
    0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF
};

int
strCASEcmp(s1, s2)
register char *s1, *s2;
{
    do {
	if (casemap[(unsigned)*s1++] != casemap[(unsigned)*s2])
	    return casemap[(unsigned)s1[-1]] - casemap[(unsigned)*s2];
    } while (*s2++ != '\0');
    return 0;
}

int
strnCASEcmp(s1, s2, len)
register char *s1, *s2;
register int len;
{
    while (len--) {
	if (casemap[(unsigned)*s1++] != casemap[(unsigned)*s2])
	    return casemap[(unsigned)s1[-1]] - casemap[(unsigned)*s2];
	if (*s2++ == '\0')
	    break;
    }
    return 0;
}
#endif

/* Modified version of a spinner originally found in Clifford Adams' strn. */

static char spinchars[] = {'|','/','-','\\'};
static int spin_place;		/* represents place in spinchars array */
static int spin_count;		/* counter for when to spin */
static int spin_level INIT(0);	/* used to allow non-interfering nested spins */
static int spin_mode;
static ART_NUM spin_art;
static ART_POS spin_tell;

void
setspin(mode)
int mode;
{
    switch (mode) {
    case SPIN_FOREGROUND:
    case SPIN_BACKGROUND:
	if (!spin_level++) {
	    if ((spin_art = openart) != 0)
		spin_tell = ftell(artfp);
	    spin_count = 1;	/* not 0 to prevent immediate spin display */
	    spin_place = 1;	/* start with slash... */
	}
	spin_mode = mode;
	break;
    case SPIN_POP:
	if (--spin_level > 0)
	    break;
	/* FALL THROUGH */
    case SPIN_OFF:
	spin_level = 0;
	if (spin_place > 1) {	/* we have spun at least once */
	    putchar(spin_char); /* get rid of spin character */
	    backspace();
	    fflush(stdout);
	    spin_place = 0;
	}
	if (spin_art) {
	    artopen(spin_art);
	    fseek(artfp,spin_tell,0);	/* do not screw up the pager */
	    spin_art = 0;
	}
	break;
    }
}

void
spin(count)
int count;		/* modulus for the spin... */
{
    if (!spin_level || (!bkgnd_spinner && spin_mode == SPIN_BACKGROUND))
	return;
    if (!(spin_count++%count)) {
	if (spin_mode == SPIN_FOREGROUND)
	    putchar('.');
	else {
	    putchar(spinchars[spin_place++%4]);
	    backspace();
	}
	fflush(stdout);
    }
}
