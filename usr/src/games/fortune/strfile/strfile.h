/* $Header: strfile.h,v 1.10 88/07/28 19:11:02 arnold Exp $ */

# ifndef	__STRFILE__

# define	__STRFILE__

/*
 * bits for flag field
 */

# define	STR_RANDOM	0x1
# define	STR_ORDERED	0x2

# define	STR_ENDSTRING(line,tbl)	((line)[0] == (tbl).str_delim && (line)[1] == (tbl).str_delim)

typedef struct {				/* information table */
	unsigned long	str_numstr;		/* # of strings in the file */
	unsigned long	str_longlen;		/* length of longest string */
	unsigned long	str_shortlen;		/* length of shortest string */
	unsigned char	str_flags;		/* bit field for flags */
	char		str_delim;		/* delimiting character */
} STRFILE;

# endif		/* __STRFILE__ */
