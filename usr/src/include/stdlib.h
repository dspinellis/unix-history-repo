/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdlib.h	5.6 (Berkeley) %G%
 */

#ifndef _STDLIB_H_
#define _STDLIB_H_
#include <machine/types.h>

#ifdef	_SIZE_T_
typedef	_SIZE_T_	size_t;
#undef	_SIZE_T_
#endif

#ifdef	_WCHAR_T_
typedef	_WCHAR_T_	wchar_t;
#undef	_WCHAR_T_
#endif

typedef struct {
	int quot;		/* quotient */
	int rem;		/* remainder */
} div_t;
typedef struct {
	long quot;		/* quotient */
	long rem;		/* remainder */
} ldiv_t;

#define	EXIT_FAILURE	1
#define	EXIT_SUCCESS	0

#define	RAND_MAX	0x7ffffffff

#define	MB_CUR_MAX	1	/* XXX */

#include <sys/cdefs.h>

__BEGIN_DECLS
void	 abort __P((void));
int	 abs __P((int));
int	 atexit __P((void (*)(void)));
double	 atof __P((const char *_nptr));
int	 atoi __P((const char *_nptr));
long	 atol __P((const char *_nptr));
void	*bsearch __P((const void *_key, const void *_base, size_t _nmemb,
	    size_t _size, int (*_compar)(const void *, const void *)));
void	*calloc __P((size_t _nmemb, size_t _size));
div_t	 div __P((int _numer, int _denom));
void	 exit __P((int _status));
void	 free __P((void *_ptr));
char	*getenv __P((const char *_string));
long	 labs __P((long));
ldiv_t	 ldiv __P((long _numer, long _denom));
void	*malloc __P((size_t _size));
void	 qsort __P((void *_base, size_t _nmemb, size_t _size,
	    int (*_compar)(const void *, const void *)));
int	 rand __P((void));
void	*realloc __P((void *_ptr, size_t _size));
void	 srand __P((unsigned _seed));
long	 strtol __P((const char *_nptr, char **_endptr, int _base));
unsigned long
	 strtoul __P((const char *_nptr, char **_endptr, int _base));
int	 system __P((const char *_string));

#ifndef _ANSI_SOURCE
void	 cfree __P((void *_ptr));
int	putenv __P((const char *_string));
int	setenv __P((const char *_string, const char *_value, int _overwrite));
#endif

#ifdef NOT_YET_IMPLEMENTED
int	mblen __P((const char *_s, size_t _n));
size_t	mbstowcs __P((wchar_t *_pwcs, const char *_s, size_t _n));
int	wctomb __P((char *_s, wchar_t _wchar));
int	mbtowc __P((wchar_t *_pwc, const char *_s, size_t _n));
double	strtod __P((const char *_nptr, char **_endptr));
size_t	wcstombs __P((char *_s, const wchar_t *_pwcs, size_t _n));
#endif
__END_DECLS

#endif /* _STDLIB_H_ */
