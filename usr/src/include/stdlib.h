/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdlib.h	5.4 (Berkeley) %G%
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

#if __STDC__ || c_plusplus

void	 abort(void);
int	 abs(int);
int	 atexit(void (*_func)(void));
double	 atof(const char *_nptr);
int	 atoi(const char *_nptr);
long	 atol(const char *_nptr);
void	*bsearch(const void *_key, const void *_base, size_t _nmemb,
	    size_t _size, int (*_compar)(const void *, const void *));
void	*calloc(size_t _nmemb, size_t _size);
div_t	 div(int _numer, int _denom);
void	 exit(int _status);
void	 free(void *_ptr);
char	*getenv(const char *_string);
long	 labs(long);
ldiv_t	 ldiv(long _numer, long _denom);
void	*malloc(size_t _size);
void	 qsort(void *_base, size_t _nmemb, size_t _size,
	    int (*_compar)(const void *, const void *));
int	 rand(void);
void	*realloc(void *_ptr, size_t _size);
void	 srand(unsigned _seed);
long	 strtol(const char *_nptr, char **_endptr, int _base);
unsigned long
	 strtoul(const char *_nptr, char **_endptr, int _base);
int	 system(const char *_string);

#ifndef _ANSI_SOURCE
void	 cfree(void *_ptr);
int	putenv(const char *_string);
int	setenv(const char *_string, const char *_value, int _overwrite);
#endif

#ifdef NOT_YET_IMPLEMENTED
int	mblen(const char *_s, size_t _n);
size_t	mbstowcs(wchar_t *_pwcs, const char *_s, size_t _n);
int	wctomb(char *_s, wchar_t _wchar);
int	mbtowc(wchar_t *_pwc, const char *_s, size_t _n);
double	strtod(const char *_nptr, char **_endptr);
size_t	wcstombs(char *_s, const wchar_t *_pwcs, size_t _n);
#endif

#else

void	 abort();
int	 abs();
int	 atexit();
double	 atof();
int	 atoi();
long	 atol();
void	*bsearch();
void	*calloc();
div_t	 div();
void	 exit();
void	 free();
char	*getenv();
long	 labs();
ldiv_t	 ldiv();
void	*malloc();
void	 qsort();
int	 rand();
void	*realloc();
void	 srand();
long	 strtol();
unsigned long
	 strtoul();
int	 system();

#ifndef _ANSI_SOURCE
void	 cfree();
int	putenv();
int	setenv();
#endif

#ifdef NOT_YET_IMPLEMENTED
int	mblen();
size_t	mbstowcs();
int	wctomb();
int	mbtowc();
double	strtod();
size_t	wcstombs();
#endif

#endif

#endif /* _STDLIB_H_ */
