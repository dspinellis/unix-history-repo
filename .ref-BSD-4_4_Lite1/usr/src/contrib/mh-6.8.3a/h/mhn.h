/* mhn.h - definitions for mhn & friends */
/* @(#)$Id: mhn.h,v 1.3 1993/08/20 15:43:43 jromine Exp $ */

#define	VRSN_FIELD	"MIME-Version"
#define	VRSN_VALUE	"1.0"

#define	XXX_FIELD_PRF	"Content-"

#define	TYPE_FIELD	"Content-Type"

#define	ENCODING_FIELD	"Content-Transfer-Encoding"

#define	ID_FIELD	"Content-ID"

#define	DESCR_FIELD	"Content-Description"

#define	MD5_FIELD	"Content-MD5"


#define	isatom(c) \
    	(!isspace (c) \
	    && !iscntrl (c) \
	    && (c) != '(' \
	    && (c) != ')' \
	    && (c) != '<' \
	    && (c) != '>' \
	    && (c) != '@' \
	    && (c) != ',' \
	    && (c) != ';' \
	    && (c) != ':' \
	    && (c) != '\\' \
	    && (c) != '"' \
	    && (c) != '.' \
	    && (c) != '[' \
	    && (c) != ']')

#define	istoken(c) \
    	(!isspace (c) \
	    && !iscntrl (c) \
	    && (c) != '(' \
	    && (c) != ')' \
	    && (c) != '<' \
	    && (c) != '>' \
	    && (c) != '@' \
	    && (c) != ',' \
	    && (c) != ';' \
	    && (c) != ':' \
	    && (c) != '\\' \
	    && (c) != '"' \
	    && (c) != '/' \
	    && (c) != '[' \
	    && (c) != ']' \
	    && (c) != '?' \
	    && (c) != '=')

/* MTR: removed now, since likely to go away in the future
	    && (c) != '.' \
 */

/*  */

#define	CPERLIN	76
#define	BPERLIN	(CPERLIN / 4)
#define	LPERMSG	632
#define	CPERMSG	(LPERMSG * CPERLIN)

/*  */

#if	defined(BSD42) || defined(SOCKETS)
#define	FTP
#endif
