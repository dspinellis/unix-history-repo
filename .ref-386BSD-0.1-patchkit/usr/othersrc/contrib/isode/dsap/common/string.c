/* string.c - printable string handling */

#ifndef lint
 static char *rcsid = "$Header: /f/osi/dsap/common/RCS/string.c,v 7.6 91/02/22 09:20:21 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/string.c,v 7.6 91/02/22 09:20:21 mrose Interim $
 *
 *
 * $Log:	string.c,v $
 * Revision 7.6  91/02/22  09:20:21  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/12/11  10:53:56  mrose
 * lock-and-load
 * 
 * Revision 7.4  90/10/17  11:42:54  mrose
 * sync
 * 
 * Revision 7.3  90/07/09  14:35:09  mrose
 * sync
 * 
 * Revision 7.2  90/04/18  08:50:09  mrose
 * 6.2
 * 
 * Revision 7.1  89/12/19  16:19:33  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:44:33  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/ds_search.h"

   /* when/if tidy_string every gets removed from getline you can undef this */
#define TIDY_STRING
#define NICER_ESCAPES

extern LLog * log_dsap;
extern char  * TidyString2();
extern char * srealloc ();

static short exct = 0;
static char char_failed;

int ch_set = 0;

soundex_match ();


static char escapes[16][17] = {
	'A','C','E','I','N','O','U','Y', 
	'a','c','e','i','n','o','u','y', ' ',

/* Grave upper case */
	0xc0, '?', 0xc8, 0xcc, '?', 0xd2, 0xd9, '?',  
/* Grave lower case */
	0xe0, '?', 0xe8, 0xec, '?', 0xf2, 0xf9, '?', '`', 

/* Acute upper case */
	0xc1, '?', 0xc9, 0xcd, '?', 0xd3, 0xda, 0xdd,
/* Acute lower case */
	0xe1, '?', 0xe9, 0xed, '?', 0xf3, 0xfa, 0xfd, '\'',

/* Circumflex upper case */
	0xc2, '?', 0xca, 0xce, '?', 0xd4, 0xdb, '?', 
/* Circumflex lower case */
	0xe2, '?', 0xea, 0xee, '?', 0xf4, 0xfb, '?', '?',

/* Tilde upper case */
	0xc3, '?', 0xcb, 0xcf, 0xd1, 0xd5, 0xdc, '?', 
/* Tilde lower case */
	0xe3, '?', 0xeb, 0xef, 0xf1, 0xf5, 0xfc, '?', '?',

/* Macron upper case */
	'?', '?', '?', '?', '?', '?', '?', '?',   
/* Macron lower case */
	'?', '?', '?', '?', '?', '?', '?', '?',  '?', 

/* ? upper case */
	'?', '?', '?', '?', '?', '?', '?', '?', 
/* ? lower case */
	'?', '?', '?', '?', '?', '?', '?', '?',  '?',

/* Dot upper case */
	0xc5, '?', '?', '?', '?', '?', '?', '?',
/* Dot lower case */
	0xe5, '?', '?', '?', '?', '?', '?', '?', '?', 

/* Diaresis upper case */
	0xc4, '?', 0xc9, 0xcd, '?', 0xd3, 0xda, 0x82, 
/* Diaresis lower case */
	0xe4, '?', 0xe9, 0xed, '?', 0xf3, 0xfa, 0xff, '"', 

/* Umlaut upper case */
	0xc4, '?', 0xc9, 0xcd, '?', 0xd3, 0xda, 0x82,  
/* Umlaut lower case */
	0xe4, '?', 0xe9, 0xed, '?', 0xf3, 0xfa, 0xff, '"', 

/* Ring upper case */
	0xc5, '?', '?', '?', '?', '?', '?', '?', 
/* Ring lower case */
	0xe5, '?', '?', '?', '?', '?', '?', '?', '?',

/* Cedilla upper case */
	'?', 0xc7, '?', '?', '?', '?', '?', '?',
/* Cedilla lower case */
	'?', 0xe7, '?', '?', '?', '?', '?', '?',  '?', 

/* Underline upper case */
	'?', '?', '?', '?', '?', '?', '?', '?',
/* Underline lower case */
	'?', '?', '?', '?', '?', '?', '?', '?',  '_', 

/* Umlaut upper case */
	0xc4, '?', 0xc9, 0xcd, '?', 0xd3, 0xda, 0x82,
/* Umlaut lower case */
	0xe4, '?', 0xe9, 0xed, '?', 0xf3, 0xfa, 0xff, '"', 

/* Cedilla? upper case */
	'?', 0xc7, '?', '?', '?', '?', '?', '?',
/* Cedilla? lower case */
	'?', 0xe7, '?', '?', '?', '?', '?', '?',  '?', 

/* ? upper case */
	'?', '?', '?', '?', '?', '?', '?', '?',
/* ? lower case */
	'?', '?', '?', '?', '?', '?', '?', '?', '?'
};

static char trans[32] = {
	'?', 0xc6, 0xd0, '?', '?', '?', '?', 'L', 
	'L', 0xd8, '?', '?', 0xde, 'T', 'N', 'n',
	'K', 0xe6, 'd', 0xf0, 'h', 'i', '?', 'l',
	'l', 0xf8, '?', 0xdf, 0xfe, 't', 'N', '?'};

int iso8859print(ps, sstr)
PS ps;
char *sstr;
{
unsigned char *str;
int n;
static unsigned char *buff = (unsigned char *)0;
unsigned char *optr;

  if (buff == (unsigned char *)0)
	  buff = (unsigned char *) smalloc (BUFSIZ);

  optr = buff;

  str = (unsigned char *) sstr;
  while (*str != '\0')
  {
  if ((*str > 0x1f) && (*str < 0x80)) {
	*optr++ = *str++;}
  else if ((*str>0xa0) && (*str < 0xc0)) { 
	*optr++ = *str++;}
  else if (*str > 0xdf) {
	*optr++ = trans[((int)(*str++)) - 0xe0];}
  else if ((*str > 0xbf) && (*str < 0xd0)) {
	n = (int) (*str - (unsigned char) 0xc0);
	str++;
	if (*str == '\0')
		return;
	switch (*str) {
	   case 'A':
		*optr++ = escapes[n][0];
		break;
	   case 'C':
		*optr++ = escapes[n][1];
		break;
	   case 'E':
		*optr++ = escapes[n][2];
		break;
	   case 'I':
		*optr++ = escapes[n][3];
		break;
	   case 'N':
		*optr++ = escapes[n][4];
		break;
	   case 'O':
		*optr++ = escapes[n][5];
		break;
	   case 'U':
		*optr++ = escapes[n][6];
		break;
	   case 'Y':
		*optr++ = escapes[n][7];
		break;
	   case 'a':
		*optr++ = escapes[n][8];
		break;
	   case 'c':
		*optr++ = escapes[n][9];
		break;
	   case 'e':
		*optr++ = escapes[n][10];
		break;
	   case 'i':
		*optr++ = escapes[n][11];
		break;
	   case 'n':
		*optr++ = escapes[n][12];
		break;
	   case 'o':
		*optr++ = escapes[n][13];
		break;
	   case 'u':
		*optr++ = escapes[n][14];
		break;
	   case 'y':
		*optr++ = escapes[n][15];
		break;
	   case ' ':
		*optr++ = escapes[n][16];
		break;
	   default:
		*optr++ = '?';
	 };
	str++;
	}
	else str++;
  };
  *optr++ = '\0';
  ps_printf(ps, "%s", buff);
};

static PE ia5enc (x)
char *x;
{
	return (ia5s2prim(x,strlen(x)));
}

static PE nstrenc (x)
char *x;
{
	return (nums2prim(x,strlen(x)));
}


/*
 * Real octet strings encode (r_*)
 */
PE r_octenc (x)
struct qbuf *x;
{
	return (qb2prim(x,PE_CLASS_UNIV, PE_PRIM_OCTS));
}

static PE octenc (x)
char *x;
{
	return (oct2prim(x,strlen(x)));
}

static PE strenc (x)
char *x;
{
	if (*x == T61_MARK) {
		x++;
		return (t61s2prim(x,strlen(x)));
	} else
		return (prts2prim(x,strlen(x)));
}

static char * local_t61 (a)
char * a;
{
char * b;

	if (a == NULLCP)
		return (NULLCP);

	b = smalloc (strlen(a) +2);
	*b++ = T61_MARK;
	(void) strcpy (b,a);
	(void) free (a);
	return (--b);
}

static char * prtsdec (pe)
PE pe;
{
int z;

	if (test_prim_pe (pe,PE_CLASS_UNIV,PE_DEFN_PRTS))
		return (TidyString2(prim2str(pe,&z)));
	else
		return (NULLCP);
}

#include "iso3166.h"

int	check_3166 (a)
char   *a;
{
    int    bitno;

    if (strlen (a) != 2)
	return 0;

    if (islower ((u_char) a[0]))
	a[0] = toupper (a[0]);
    if (islower ((u_char) a[1]))
	a[1] = toupper (a[1]);

    return (isupper ((u_char) a[0]) && isupper((u_char) a[1]) && is3166 (a));
}


static char * cntydec (pe)
PE pe;
{
int   bitno;
char *a;

	if ((a = prtsdec(pe)) == NULLCP)
		return (NULLCP);

	if (strlen (a) != 2) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,
		      ("Country code size wrong: \"%s\"", a));
losing: ;
		free (a);
		return (NULLCP);
	}

	if (islower ((u_char) a[0]))
	    a[0] = toupper (a[0]);
	if (islower ((u_char) a[1]))
	    a[1] = toupper (a[1]);

	if (isupper ((u_char) a[0]) && isupper((u_char) a[1]) && is3166 (a))
	    return (a);

	LLOG (log_dsap, LLOG_EXCEPTIONS,
	      ("Invalid country code: \"%s\"", a));
	goto losing;
}

struct qbuf *
r_octsdec (pe)
PE pe;
{

	if (test_prim_pe (pe,PE_CLASS_UNIV,PE_PRIM_OCTS))
		return (prim2qb(pe));
	else
		return ((struct qbuf *)0);

}
static char * octsdec (pe)
PE pe;
{
int z;

	if (test_prim_pe (pe,PE_CLASS_UNIV,PE_PRIM_OCTS))
		return (TidyString2(prim2str(pe,&z)));
	else
		return (NULLCP);

}

static char * ia5sdec (pe)
PE pe;
{
int z;

	if (test_prim_pe (pe,PE_CLASS_UNIV,PE_DEFN_IA5S))
		return (TidyString2(prim2str(pe,&z)));
	else
		return (NULLCP);
}

static char * numsdec (pe)
PE pe;
{
int z;
	if (test_prim_pe (pe,PE_CLASS_UNIV,PE_DEFN_NUMS))
		return (TidyString2(prim2str(pe,&z)));
	else
		return (NULLCP);
}


static char * t61dec (pe)
PE pe;
{
int z;

	if (pe->pe_form != PE_FORM_PRIM) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Primative string expected"));
		return NULLCP;
	}

	if ( PE_ID (pe -> pe_class, pe -> pe_id) == PE_ID (PE_CLASS_UNIV,PE_DEFN_T61S) ) 
		return (local_t61 (TidyString2(prim2str(pe,&z))));
	else 
		return (prtsdec(pe));
}

static char * quotechar (a,b)
register char a, *b;
{
#ifdef NICER_ESCAPES
#define CONT_CHAR	'\\'

    switch (a & 0xff) {
    case '\n':
	 *b++ = CONT_CHAR;
	 *b++ = 'n';
	 break;

     case '\t':
	 *b++ = CONT_CHAR;
	 *b++ = 't';
	 break;

     case '\r':
	 *b++ = CONT_CHAR;
	 *b++ = 'r';
	 break;

    case '\\':
        *b++ = CONT_CHAR;
        *b++ = CONT_CHAR;
	break;

#ifndef	TIDY_STRING
    case ' '
        *b++ = CONT_CHAR;
        *b++ = ' ';
        break;
#endif

    default:
	(void) sprintf (b,"\\%02x", a & 0xff);
	b += 3;
	break;
    }
    return (b);
#else
	(void) sprintf (b,"\\%02x", a & 0xff);
	b += 3;
	return (b);
#endif
}

static char * unquotechar (a,b)
register char *a, *b;
{
int val;

#ifdef NICER_ESCAPES

        switch (*a) {
        case '\\':
                *b = '\\';
                break;
        case 'n':
            *b = '\n';
            break;

        case 't':
            *b = '\t';

        case 'r':
            *b = '\r';
            break;

#ifndef	TIDY_STRING
       case ' '
           *b = ' ';
	   break;
#endif

        default:
            if (isxdigit((*a) & 0xff) && sscanf (a,"%2x", &val) == 1) {
                *b = val & 0xff;
                a++;
            } else
		parse_error ("Bad Quoted character",NULLCP);
	    break;

        }
#else
	if (*a == '\\') 
		*b = '\\';
	else {
		if (sscanf (a,"%2x", &val) == 1) {
			*b = val & 0xff;
			a++;
		} else
			parse_error ("Bad Quoted character",NULLCP);
	}
#endif
	return (a);
}


check_print_string (str)
register char * str;
{

    for (; *str != 0; str++) {
	if ((isascii((*str)& 0xff)) && (isalnum ((*str) & 0xff)))
		continue;

	switch (*str) {
		case 047:  /* ' */
		case '(':
		case ')':
		case '+':
		case '-':
		case '.':
		case ',':
		case '/':
		case ':':
		case '=':
		case '?':
		case ' ': continue;
		default:  LLOG (log_dsap,LLOG_NOTICE,("character '%c' not in printablestring",*str));
			  char_failed = *str;
			  return (0);
	}
    }
    return (1);
}

#define MINBUF		3
#define PARSE_INCR	240

struct qbuf * 
r_octparse (str)
register char * str;
{
    static char 	*buf;
    static int 		buflen = 0;
    register char	*ptr;
    int			left;
    int			curlen;

    left = buflen;
    ptr = buf;

    for (; *str != 0; str++) {
	if ( left <= MINBUF) {
            if (buflen <= 0) {
                left = buflen = PARSE_INCR;
                ptr = buf = smalloc(PARSE_INCR);
            } else {
                buflen += PARSE_INCR;
                left += PARSE_INCR;
		curlen = ptr - buf;
                buf = srealloc(buf, buflen);
                ptr = buf + curlen;
            }
        }

	if (*str != '\\')
	    *ptr++ = *str;
	else {
	    str++;
	    str = unquotechar (str,ptr);
	    ptr++;
	}
	left--;
    }

    *ptr = 0;	/* just for safety ? */
    return (str2qb(buf, ptr - buf, 1));
}

char * octparse (str)
register char * str;
{
char buffer [BUFSIZ];
register char * ptr=buffer;
register int i;

        for (i=0; *str != 0; str++,i++) {
                if (*str != '\\')
                        *ptr++ = *str;
                else {
			str++, i++;
                        str = unquotechar (str,ptr);
			ptr++;
		}
		if ( i >= BUFSIZ ) {
			parse_error ("String too long", NULLCP);	
			return NULLCP;
		}
	}
	*ptr = 0;
	return (strdup(buffer));
}


#define prtparse_aux(z)	(check_print_string(z) ? strdup(z) : NULLCP)

char * prtparse (str)
char * str;
{
char * ptr;

	if ((ptr = prtparse_aux(str)) != NULLCP)
		return (ptr);
	else {
		parse_error ("character '%c' not in printablestring",(char *)char_failed);
		return (NULLCP);
	}
}

static char * cntyparse(str)
char * str;
{
char * a;

	if ((a=prtparse(str)) == NULLCP)
		return (NULLCP);

	if (check_3166 (a))
	    return a;

	parse_error ("invalid country code: \"%s\"", a);
	free (a);
	return (NULLCP);
}

static char * t61parse (str)
char * str;
{
extern char t61_flag;
char * octparse ();
char * res;

	if (t61_flag) {
		t61_flag = FALSE;  /* recognised it !!! */
		return (local_t61(octparse (str)));   /* need t61 parser */
	} else if ((res=prtparse_aux(str)) == NULLCP) {
		LLOG (log_dsap,LLOG_NOTICE,("auto-convert to T.61 for '%s' ('%c' not allowed)",str,char_failed));
		return (local_t61(octparse (str)));
	} else
		return (res);
}

char * cryptstring (str)
char * str;
{
register char * p;
	/* This is a SIMPLE HACK to prevent passwords being revealed */
	/* at a glance.  It buys virtually no extra security */

#define CRYPT_MASK 0x23 	/* could tailor this */

	for (p=str; *p != 0; p++)
		if (*p != CRYPT_MASK)
			*p ^= CRYPT_MASK; 

	return (str);
}

char * cryptparse (str)
char * str;
{
extern char crypt_flag;
char * octparse ();

	if (crypt_flag) {
		crypt_flag = FALSE;  /* recognised it !!! */
		return (cryptstring(octparse (str)));   
	} else 
		return (octparse (str));
}

sfree (x)
char *x;
{
	free (x);
}

pstrcmp (a,b)
register char * a, *b;
{
    while (*a == *b) {
	if (*a++ == NULL)
	    return (0);
	b++;
    }

    if (*a > *b)
	return (1);
    else
	return (-1);

}

static tpstrcmp (a,b)
register char *a, *b;
{
	if (*a == T61_MARK)
		a++;
	if (*b == T61_MARK)
		b++;

    while (*a == *b) {
	if (*a++ == NULL)
	    return (0);
	b++;
    }

    if (*a > *b)
	return (1);
    else
	return (-1);
}

static tlexequ (a,b)
register char *a, *b;
{

    /* lexequ with T.61 knowledge */

	if (*a == T61_MARK)
		a++;
	if (*b == T61_MARK)
		b++;

    while (chrcnv[*a] == chrcnv[*b]) {
	if (*a++ == NULL)
	    return (0);
	b++;
    }

    if (chrcnv[*a] > chrcnv[*b])
	return (1);
    else
	return (-1);
}

passwdcmp (a,b)
char * a, *b;
{
	if (strcmp (a,b) == 0)
		return (0);
	else
		return (2);

}

telcmp (a, b)
char   *a,
       *b;
{
    register char c1,
		  c2;

    for (;;) {
	while (c1 = *a++)
	    if (c1 != ' ' && c1 != '-')
		break;

	while (c2 = *b++)
	    if (c2 != ' ' && c2 != '-')
		break;

	if (c1 == NULL)
	    return (c2 ? -1 : 0);
	else
	    if (c2 == NULL)
		return 1;

	if (c1 > c2)
	    return 1;
	else
	    if (c1 < c2)
		return -1;
    }
}

strprint (ps,str,format)
PS ps;
char * str;
int format;
{
	if (*str == T61_MARK) {
		if (format != READOUT) {
			ps_print (ps,"{T.61}");
			octprint (ps,++str,format);
		} else if (ch_set == 1) 
			iso8859print(ps,++str);
		else 
			octprint(ps,++str,format);
	} else 
		ps_print (ps,str);
}

cryptprint (ps,str,format)
PS ps;
char * str;
int format;
{
char ptr [LINESIZE];
extern char dsa_mode;

	if (format == READOUT) 
		ps_print (ps,"HIDDEN");
	else {
		if (! dsa_mode) {
			octprint (ps,str,format);
			return;
		}
		ps_print (ps,"{CRYPT}");
		(void) strcpy (ptr,str);
		octprint (ps,cryptstring(ptr),format);
	}
}

#define MAXLINE		75	/* maximum line length */
#define BUFSLOP		5	/* space to allow over run an unexpanded char */

r_octprint (ps, qb, format)
PS ps;
struct qbuf * qb;
int format;
{
    char 	buf[MAXLINE + BUFSLOP];
    register	char *str;
    register 	char *ptr = buf;

    register char	*pend = buf + MAXLINE;
    register char	*optr = buf;
    register int	cnt;
    register struct qbuf *qp;


    for (qp = qb->qb_forw; qp != qb; qp = qp->qb_forw) {
	for (str = qp->qb_data, cnt = qp->qb_len; cnt > 0; str++, cnt--) {
	again:
	    if ((isascii(*str)) && (isprint(*str))) {
		if (format != READOUT)
		switch (*str) {
		    case '&': /* used as seperators */
		    case '#':
		    case '$':
		    case '%':
		    case '@':
#ifdef TIDY_STRING
		    case ' ':
#endif
			ptr = quotechar (*str,ptr);
			break;
		    default:
			*ptr++ = *str;
			if (*str == '\\')
			    *ptr++ = *str;
		}
		    else 
			*ptr++ = *str;
	    } else
		ptr = quotechar (*str,ptr);
	    if (ptr >= pend) {
		if (format != READOUT)
		    *optr++ = '\\';	/* Continuation character */
		*optr++ = '\n';
		(void) ps_write(ps, (PElementData)buf, optr - buf);
		optr = ptr = buf;
		goto again;
	    }
	    optr = ptr;
	}
    }
    if (ptr > buf)
	(void) ps_write(ps, (PElementData)buf, ptr - buf);
}

/* ARGSUSED */
octprint (ps,str,format)
PS ps;
char * str;
int format;
{
    		char	buf[MAXLINE + 4];
    register	char	*ptr = buf;
    register	char	*optr = buf;
    register	char	*pend = buf + MAXLINE;


    while (*str) {
	if (isascii((*str) & 0xff) && isprint((*str) & 0xff)) {
	    if (format != READOUT)
		switch (*str) {
		    case '&': /* used as seperators */
		    case '#':
		    case '$':
		    case '%':
		    case '@':
			ptr = quotechar (*str,ptr);
			break;
		    default:
			*ptr++ = *str;
			if (*str == '\\')
			    *ptr++ = *str;
		}
	    else 
		*ptr++ = *str;
	} else
		ptr = quotechar (*str,ptr);
	if (ptr >= pend) {
	    if (format != READOUT) {
		*optr++ = '\\';
		*optr++ = '\n';
	    }
	    (void) ps_write (ps, (PElementData)buf, optr - buf);
	    ptr = optr = buf;
	    continue;
	}
	optr = ptr;
	str++;
    }

    if (optr > buf)
	(void) ps_write (ps, (PElementData)buf, optr - buf);
}


/*
 * Compare two possible null qbuf lists and return non zero if they are
 * different
 * Pretty complex to allow for all sorts of weird cases
 * Doesn't work for a qbuf which doesn't have a head ! Don't really know what
 * is the proper form of a queue buf. Marshall's doco doesn't say
 */
qb_cmp(qb1, qb2)
struct	qbuf	*qb1, *qb2;
{
    struct	qbuf	*qp1, *qp2;
    register char	*po1, *po2;
    register int	len1, len2;
    register int	i;

    if (qb1 == NULL && qb2 == NULL)
	return (0);
    
    if (qb1 == NULL)
	return (-1);
	
    if (qb2 == NULL)
	return (1);

    qp1 = qb1->qb_forw;
    qp2 = qb2->qb_forw;
    po1 = qp1->qb_data;
    po2 = qp2->qb_data;
    len1 = qp1->qb_len;
    len2 = qp2->qb_len;

    while (qp1 != qb1 && qp2 != qb2) {
	if (len1 < len2) {
	    if ((i = nbcmp(po1, po2, len1)))
		    return (i);
	    len2 -= len1;
	    po2 += len1;
	    qp1 = qp1->qb_forw;
	    po1 = qp1->qb_data;
	    len1 = qp1->qb_len;
	} else {
	    if ((i = nbcmp(po1, po2, len2)))
		    return (i);
	    len1 -= len2;
	    po1 += len2;
	    qp2 = qp2->qb_forw;
	    po2 = qp2->qb_data;
	    len2 = qp2->qb_len;
	}
    }

    if (len1 == 0)
	    qp1 = qp1->qb_forw;
    if (len2 == 0)
	    qp2 = qp2->qb_forw;
    while (qp1 != qb1 && qp1->qb_len == 0)
	    qp1 = qp1->qb_forw;
    while (qp2 != qb2 && qp2->qb_len == 0)
	    qp2 = qp2->qb_forw;
    if (qp1 == qb1 && qp2 == qb2)
	return (0);	/* perfect match */

    if (qp1 == qb1)
	return (-1);

    return (1);
}

/*
 * new bcmp
 * actually compares them and returns 1, 0, -1 depending on wether the 
 * len characters of string1 are greater, equal or less than string2
 */
nbcmp(string1, string2, len)
register char	*string1;
register char	*string2;
register int 	len;
{
    while (len-- > 0) {
	if (*string1++ == *string2++)
	    continue;
	if (*--string1 > *--string2)
	    return (1);
	return (-1);
    }
    return (0);
}

#define SIZEOFQB(qb)  (sizeof (struct qbuf) +  (qb && qb->qb_data ? qb->qb_len \
				: 0))

struct qbuf *
qb_cpy(qb)
struct qbuf	*qb;
{
    struct qbuf	*qp;
    struct qbuf	*nqb;
    struct qbuf	*nqp;
    struct qbuf	*pred;

    if (qb == (struct qbuf *)0)
	return ((struct qbuf *)0);
    nqb = (struct qbuf *) smalloc(SIZEOFQB(qb));
    nqb->qb_len = qb->qb_len;
    if (qb->qb_data) {
	nqb->qb_data = nqb->qb_base;
	bcopy(qb->qb_data, nqb->qb_data, qb->qb_len);
    } else
	nqb->qb_data = NULLCP;
    nqb->qb_forw = nqb;
    nqb->qb_back = nqb;
    pred = nqb;
    for (qp = qb->qb_forw; qp != qb; qp = qp->qb_forw) {
	nqp = (struct qbuf *) smalloc(SIZEOFQB(qp));
	nqp->qb_len = qp->qb_len;
	if (qp->qb_data) {
	    nqp->qb_data = nqp->qb_base;
	    bcopy(qp->qb_data, nqp->qb_data, qp->qb_len);
	} else
	    nqp->qb_data = NULLCP;
	insque(nqp, pred);
	pred = nqp;
    }

    return (nqb);
}

/*
 * output the string to the PS - including a delimiter on the end
 */
part_print (ps, p, len)
PS ps;
char	*p;	/* string to be output (may contain nulls) */
int	len;	/* number of characters in string */
{
    char 	buf[MAXLINE + BUFSLOP];
    register	char *str;
    register 	char *ptr = buf;

    register char	*pend = buf + MAXLINE;
    register char	*optr = buf;


    for (str = p; len > 0; len--, str++) {
    again:
	if (isascii(*str) && isprint(*str)) {
	    switch (*str) {
		case '&': /* used as seperators */
		case '#':
		case '$':
		case '%':
		case '@':
#ifdef TIDY_STRING
		case ' ':
#endif
		    ptr = quotechar (*str,ptr);
		    break;
		default:
		    *ptr++ = *str;
		    if (*str == '\\')
			*ptr++ = *str;
	    }
	} else
	    ptr = quotechar (*str,ptr);
	if (ptr >= pend) {
	    *optr++ = '\\';	/* Continuation character */
	    *optr++ = '\n';
	    (void) ps_write(ps, (PElementData)buf, optr - buf);
	    optr = ptr = buf;
	    goto again;
	}
	optr = ptr;
    }

      /* Add the delimiter */
    *ptr++ = '\\';
    *ptr++ = 'Z';
    if (ptr > buf)
	(void) ps_write(ps, (PElementData)buf, ptr - buf);
}
/*
 * extract the next string that was written using out_print
 * note we return the string in a temporary buffer - don't try to free it
 * it takes the address of a pointer so we can reset the pointer to
 * where we get up to in the string. It also needs to set the length
 * as we support binary strings. But we always terminate out strings with
 * a '\0' for the convience of non binary string users
 */
char *
part_parse (pstr, plen)
char **pstr;		/* address of pointer to string */
int	*plen;		/* address of integer we set the length to */
{
    static char 	*buf;
    static int 		buflen = 0;
    register char	*str;
    register char	*ptr;
    int			left;
    int			curlen;

    str = *pstr;
    left = buflen;
    ptr = buf;

    for (; *str != 0; str++) {
	if ( left <= MINBUF) {
            if (buflen <= 0) {
                left = buflen = PARSE_INCR;
                ptr = buf = smalloc(PARSE_INCR);
            } else {
                buflen += PARSE_INCR;
                left += PARSE_INCR;
		curlen = ptr - buf;
                buf = srealloc(buf, buflen);
                ptr = buf + curlen;
            }
        }

	if (*str != '\\')
	    *ptr++ = *str;
	else if (str[1] == 'Z') { /* Ahha ! found the delimiter */
	    *ptr = '\0';
	    str += 2;
	    *pstr = str;
	    *plen = ptr - buf;
	    return (buf);
	} else {
	    str++;
	    str = unquotechar (str,ptr);
	    ptr++;
	}
	left--;
    }

    *ptr = '\0';	/* for users convience */
    *pstr = ++str;
    *plen = ptr - buf;
    return (buf);
}

case_exact_match (sntx) 
short sntx;
{
	if ((sntx < exct) || (sntx > (exct + 3)))
		return (FALSE);
	else
		return (TRUE);
}

approx_string (sntx)
short sntx;
{
	if ((sntx < exct) || (sntx > (exct + 7)))
		return (FALSE);
	else
		return (TRUE);
}

sub_string (sntx)
short sntx;
{
	if ((sntx < exct) || (sntx > (exct + 8)))
		return (FALSE);
	else
		return (TRUE);
}


string_syntaxes ()
{
	/* Don't change ordering here unless you know 
	   the side effects !!! */

	/* 1-4 Exact string */
	/* 1-7 Approx       */

	exct = add_attribute_syntax ("caseexactstring",
		(IFP) strenc,	(IFP) t61dec,
		(IFP) t61parse,	strprint,
		(IFP) strdup,	tpstrcmp,
		sfree,		NULLCP,
		soundex_match,	TRUE);

	(void) add_attribute_syntax ("TelephoneNumber",
		(IFP) strenc,	(IFP) prtsdec,
		(IFP) prtparse,	strprint,
		(IFP) strdup,	telcmp,
		sfree,		NULLCP,
		soundex_match,	TRUE);

	(void) add_attribute_syntax ("printablestring",
		(IFP) strenc,	(IFP) prtsdec,
		(IFP) prtparse,	strprint,
		(IFP) strdup,	pstrcmp,
		sfree,		NULLCP,
		soundex_match,	TRUE);

	(void) add_attribute_syntax ("ia5string",
		(IFP) ia5enc,	(IFP) ia5sdec,
		(IFP) octparse,	octprint,
		(IFP) strdup,	pstrcmp,
		sfree,		NULLCP,
		soundex_match,	TRUE);

	/* 5-8 ignore strings */

	(void) add_attribute_syntax ("countrystring",
		(IFP) strenc,	(IFP) cntydec,
		(IFP) cntyparse,strprint,
		(IFP) strdup,	lexequ,
		sfree,		NULLCP,
		soundex_match,	TRUE);

	(void) add_attribute_syntax ("DestinationString",
		(IFP) strenc,	(IFP) prtsdec,
		(IFP) prtparse,	strprint,
		(IFP) strdup,	lexequ,
		sfree,		NULLCP,
		soundex_match,	TRUE);

	(void) add_attribute_syntax ("caseignorestring",
		(IFP) strenc,	(IFP) t61dec,
		(IFP) t61parse,	strprint,
		(IFP) strdup,	tlexequ,
		sfree,		NULLCP,
		soundex_match,	TRUE);

	(void) add_attribute_syntax ("caseIgnoreIa5string",
		(IFP) ia5enc,	(IFP) ia5sdec,
		(IFP) octparse,	octprint,
		(IFP) strdup,	lexequ,
		sfree,		NULLCP,
		soundex_match,	TRUE);

	/* 1-9 -> substrings */
	(void) add_attribute_syntax ("numericstring",
		(IFP) nstrenc,	(IFP) numsdec,
		(IFP) strdup,	strprint,
		(IFP) strdup,	pstrcmp,
		sfree,		NULLCP,
		NULLIFP,	FALSE);


	/* Not really strings at all (yet!) */
	(void) add_attribute_syntax ("octetstring",
		(IFP) r_octenc,	(IFP) r_octsdec,
		(IFP) r_octparse, r_octprint,
		(IFP) qb_cpy,	qb_cmp,
		qb_free,		NULLCP,
		NULLIFP,	TRUE);

	(void) add_attribute_syntax ("password",
		(IFP) octenc,	(IFP) octsdec,
		(IFP) cryptparse,	cryptprint,
		(IFP) strdup,	passwdcmp,
		sfree,		NULLCP,
		NULLIFP,	TRUE);
}

