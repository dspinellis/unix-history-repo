/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Allocator definitions (fast malloc and free)
 */

#if !defined(CALC_MALLOC)

#include "have_malloc.h"
#ifdef HAVE_MALLOC_H
# include <malloc.h>
#else
# if defined(__STDC__)
   extern void *malloc();
   extern void *realloc();
   extern void free();
# else
   extern char *malloc();
   extern char *realloc();
   extern void free();
# endif
#endif

#include "have_string.h"

#ifdef HAVE_STRING_H
# include <string.h>

#else

# ifdef OLD_BSD
extern void bcopy();
extern void bfill();
extern char *index();
# else /* OLD_BSD */
extern void memcpy();
extern void memset();
#  if defined(__STDC__)
extern void *strchr();
#  else
extern char *strchr();
#  endif
# endif /* OLD_BSD */
extern void strcpy();
extern void strncpy();
extern void strcat();
extern int strcmp();
extern long strlen();	/* should be size_t, but old systems don't have it */

#endif

#ifdef OLD_BSD
#undef memcpy
#define memcpy(s1, s2, n) bcopy(s2, s1, n)
#undef memset
#define memset(s, c, n) bfill(s, n, c)
#undef strchr
#define strchr(s, c) index(s, c)
#endif

#ifdef DONT_HAVE_VSPRINTF
/*
 * Hack aleart!!!
 *
 * Systems that do not have vsprintf() need something.  In some cases
 * the sprintf function will deal correctly with the va_alist 3rd arg.
 * Hope for the best!
 */
#define vsprintf sprintf
#endif

#define mem_alloc malloc
#define mem_realloc realloc
#define mem_free free

#else /*!CALC_MALLOC*/

#define malloc(a) mem_alloc((long) a)
#define realloc(a,b) mem_realloc((char *) a, (long) b)
#define free(a) mem_free((char *) a)
extern char *mem_alloc();
extern char *mem_realloc();
extern int mem_free();		/* MUST be int even though no return value */

#endif /*!CALC_MALLOC*/


/*
 * An item to be placed on a free list.
 * These items are overlayed on top of the actual item being managed.
 * Therefore, the managed items must be at least this size!
 * Also, all items on a single free list must be the same size.
 */
struct free_item {
	struct free_item *next;			/* next item on free list */
};
typedef struct free_item FREEITEM;


/*
 * The actual free list header.
 */
typedef struct {
	long		itemsize;	/* size of an item being managed */
	long		maxfree;	/* maximum number of free items */
	long		curfree;	/* current number of free items */
	FREEITEM	*freelist;	/* the free list */
} FREELIST;

#if defined(__STDC__)
typedef void ALLOCITEM;
#else
typedef char ALLOCITEM;
#endif
extern ALLOCITEM * allocitem( /* FREELIST * */ );
extern void freeitem( /* FREELIST *, char * */ );
extern void mem_stats();

/* END CODE */
