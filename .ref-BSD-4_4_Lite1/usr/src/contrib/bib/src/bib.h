/*
 *	@(#)bib.h	2.8	5/27/93
 */
/*   various arguments for bib and listrefs processors */

/* constants */

# define true  1
# define false 0
# define bool unsigned char
# define err  -1
# define REFSIZE 2048                /* maximum size of reference string    */
# define MAXFIELD 512                /* maximum size of any field in referece*/

/* reference citation marker genrated in pass 1 */

# define CITEMARK (char) 02
# define CITEEND  (char) 03
# define FMTSTART (char) 04
# define FMTEND   (char) 05

/* file names */

        /* output of invert, input file for references */
# define INDXFILE "INDEX"
        /* pass1 reference collection file */
# define TMPREFFILE  "/tmp/bibrXXXXXX"
        /* pass2 text collection file */
# define TMPTEXTFILE "/tmp/bibpXXXXXX"
        /* temp file used in invert */
# define INVTEMPFILE "/tmp/invertXXXXXX"
# define SYSINDEX "/usr/dict/papers/INDEX"	/* default system dictionary */

#ifndef BASEDIR
# define BASEDIR   ""
# endif

# define N_BMACLIB "/lib/bmac"       /* where macro libraries live */
# define N_COMFILE "/lib/bmac/common"	/* common words */
# define N_DEFSTYLE "/lib/bmac/bib.stdsn" /* default style of refs */

# define InitDirectory(arr,str) \
   strcpy(arr,BASEDIR); strcat(arr,str);

char BMACLIB[100], COMFILE[100], DEFSTYLE[100];

/* size limits */

	/* maximum number of characters in common file */
# define MAXCOMM 1000

char *malloc();

/* fix needed for systems where open [w]+ doesn't work */
# ifdef READWRITE

# define READ 1
# define WRITE 0

#endif
   /*
    *	Reference information
    */
   struct refinfo{
	char	*ri_ref;	/* actual value, base value */
	char	*ri_cite;	/* citation string */
	char	ri_disambig[2];	/* disambiguation string */
	int	ri_length;	/* length of reference string, plus null */
	long int ri_pos;	/* reference seek position */
	int	ri_n;		/* number of citation in pass1 */
	struct	refinfo	*ri_hp;	/* hash chain */
   };
   struct wordinfo{
	char	*wi_word;	/* actual word */
	char	*wi_def;	/* actual definition */
	int	wi_length;	/* word length */
	bool	wi_expanding;	/* is it being expanded? */
	struct wordinfo *wi_hp;	/* hash chain */
   };
   int	strhash();
#define HASHSIZE	509

#define reg register

