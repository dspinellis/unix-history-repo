/* general.h - general compatibility */

/* 
 * $Header: /f/osi/h/RCS/general.h,v 7.5 91/02/22 09:24:42 mrose Interim $
 *
 *
 * $Log:	general.h,v $
 * Revision 7.5  91/02/22  09:24:42  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/12/23  18:41:46  mrose
 * update
 * 
 * Revision 7.3  90/10/17  14:39:15  mrose
 * update
 * 
 * Revision 7.2  90/07/09  14:37:45  mrose
 * sync
 * 
 * Revision 7.1  90/03/06  14:10:28  mrose
 * jch
 * 
 * Revision 7.0  89/11/23  21:55:44  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef	_GENERAL_
#define	_GENERAL_

#ifndef	_CONFIG_
#include "config.h"		/* system-specific configuration */
#endif


/* target-dependent defines:

	BSDFORK -	target has BSD vfork
	BSDLIBC -	target has real BSD libc
	BSDSTRS -	target has BSD strings
	SWABLIB -	target has byteorder(3n) routines
 */

#ifdef	SOCKETS
#define	SWABLIB
#endif

#ifdef	WINTLI
#define	SWABLIB
#endif

#ifdef	EXOS
#define	SWABLIB
#endif


#ifdef	BSD42
#define	BSDFORK
#define	BSDLIBC
#define	BSDSTRS
#endif

#ifdef	ROS
#undef	BSDFORK
#undef	BSDLIBC
#define	BSDSTRS
#ifndef	BSD42
#define	BSD42
#endif
#undef	SWABLIB
#endif

#ifdef	SYS5
#undef	BSDFORK
#undef	BSDLIBC
#undef	BSDSTRS
#endif

#ifdef	sgi
#undef	BSDFORK
#undef	BSDLIBC
#undef	BSDSTRS
#endif

#ifdef	HPUX
#define	BSDFORK
#undef	BSDLIBC
#undef	BSDSTRS
#undef	SWABLIB
#endif

#ifdef	pyr
#undef	SWABLIB
#endif

#ifdef	XOS
#undef	SWABLIB
#endif

#ifdef	XOS_2
#undef	SWABLIB
#endif

#ifdef  apollo
#undef  SWABLIB
#endif

#ifdef	AUX
#undef	BSDFORK
#define BSDSTRS
#undef SWABLIB
#define BSDLIBC
#endif 

#ifndef	BSDFORK
#define	vfork	fork
#endif

/*    STRINGS */

#ifndef	BSDSTRS
#define	index	strchr
#define	rindex	strrchr
#endif

char   *index ();
char   *mktemp ();
char   *rindex ();
#if	defined(BSDSTRS) && !defined(BSD44) && (!defined(BSD43) || defined(SUNOS4) || defined(vax) || defined(RT) || (defined(mips) && defined(ultrix))) && !defined(XOS_2)
char   *sprintf ();
#else
int     sprintf ();
#endif
char   *strcat ();
int     strcmp ();
char   *strcpy ();
int	strlen ();
char   *strncat ();
int     strncmp ();
char   *strncpy ();

char   *getenv ();
char   *calloc (), *malloc (), *realloc ();

#if	defined(SYS5) && !defined(AIX) && !defined(XOS) && !defined(XOS_2)
#include <memory.h>

#define	bcopy(b1,b2,length)	(void) memcpy ((b2), (b1), (length))
#define	bcmp(b1,b2,length)	memcmp ((b1), (b2), (length))
#define	bzero(b,length)		(void) memset ((b), 0, (length))
#endif

/*    HEXIFY */

int	explode (), implode ();

/*    SPRINTB */

char   *sprintb ();

/*    STR2VEC */

#define	NVEC	100
#define	NSLACK	10


#define	str2vec(s,v)	str2vecX ((s), (v), 0, NULLIP, NULL, 1)

int	str2vecX ();

/*    STR2ELEM */

#define	NELEM	20

int	str2elem ();

/*    STR2SEL */

int	str2sel ();
char   *sel2str ();

/*    GETPASS */

char   *getpassword ();

/*    BADUSER */

int	baduser ();

/*   UTILITIES */

extern char chrcnv[], nochrcnv[];


int	lexequ (), lexnequ ();

int	log_tai ();

int	sstr2arg ();

char    *smalloc (), *strdup ();

/*    MISC */

char   *sys_errname ();

#ifdef	lint
#define	insque(e,p)	INSQUE ((char *) (e), (char *) (p))
#define	remque(e)	REMQUE ((char *) (e))
#endif


void	asprintf (), _asprintf ();

void	isodetailor ();		/* also in tailor.h */


#ifndef	ntohs
unsigned short	ntohs ();
#endif
#ifndef	htons
unsigned short	htons ();
#endif
#ifndef	ntohl
unsigned long	ntohl ();
#endif
#ifndef	htonl
unsigned long	htonl ();
#endif

#endif
