/*
  C K C M D B . C  --  malloc debugger.
*/

/*
  Author: Howie Kaye, Columbia University Center for Computing Activities.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/
#include <stdio.h>
#include "ckcdeb.h"
/*
  memdebug:
  variable to control memory debugging.
  if memdebug ==  1, then action is always taken.
  if memdebug ==  0, then no action is taken.
  if memdebug == -1, then the user is asked (works well with gdb).
*/
int memdebug = -1;
int disabled = 0;
int inited = 0;
/*
  To use this package, compile your program with:
  -Dmalloc=dmalloc -Dfree=dfree =Dcalloc=dcalloc ... -DMDEBUG
  and then link it with ckcmdb.c.
*/
#ifdef MDEBUG
/* Use the real ones in this module! */
#ifdef malloc
#undef malloc
#endif /* malloc */
#ifdef calloc
#undef calloc
#endif /* calloc */
#ifdef realloc
#undef realloc
#endif /* realloc */
#ifdef free
#undef free
#endif /* free */

char *malloc(), *realloc();
char *set_range_check();
char *check_range();
char *maybe_check_range();

#define min(x,y) ((x) < (y) ? (x) : (y))
#define RANGE "ABCDEFGHIJKLMNOP"
#define INTSIZE  sizeof(int)
#define LONGSIZE sizeof(long)
#define RSIZE    sizeof(RANGE)
#define RFRONT   min((RSIZE/2),LONGSIZE)
#define RBACK    min((RSIZE-RFRONT),LONGSIZE)

char *
dmalloc(size) int size; {
    char *cp;

    cp = malloc(size + RSIZE + INTSIZE);
    if (cp) {
	cp = set_range_check(cp, size);
	m_insert(cp);
    }
    return(cp);
}

char *
dcalloc(nelem, elsize) int nelem, elsize; {
    char *cp;

    cp = dmalloc(nelem * elsize);
    if (cp)
	bzero(cp, nelem * elsize);
    return(cp);
}

char *
drealloc(bp,size) char *bp; int size; {
    char *cp;

    if (bp == NULL) {
	maybe_quit("Freeing NULL pointer");
    } else {
	m_delete(bp);
	cp = check_range(bp);
    }
    cp = realloc(cp, size + RSIZE + INTSIZE);
    if (cp) {
	cp = set_range_check(cp, size);
	m_insert(cp);
    }
    return(cp);
}

dfree(cp) char *cp; {
    if (cp == NULL)
	maybe_quit("Freeing NULL pointer");
    else {
	switch(m_delete(cp)) {
	case 0:
	    cp = maybe_check_range(cp);
	    break;
	case 1:
	    cp = check_range(cp);
	    break;
	case 2:
	    break;
	}
    }
    return(free(cp));
}

char *
set_range_check(cp,size) char *cp; int size; {
    register int i;
    int tmp = size;

    for(i = 0; i < INTSIZE; i++) {	/* set the size in the string */
	cp[i] = tmp & 0xff;
	tmp >>= 8;
    }
    cp += INTSIZE;			/* skip the size */

    for(i = 0; i < RFRONT; i++)		/* set the front of the range check */
	cp[i] = RANGE[i];		/* string */

    cp += RFRONT;			/* skip the front range check */

    for(i = 0; i < RBACK; i++)		/* set the back odf the range check */
	cp[i+size] = RANGE[i+RFRONT];

    return(cp);
}

/*
  Put calls to this routine in your code any place where you want to
  check whether you've copied too many characters into a malloc'd space.
*/
char *
check_range(cp) char *cp; {
    register char *bp = cp - RFRONT - INTSIZE;
    char *xp = bp;
    register int i;
    int size = 0;

    for(i = 0 ; i < INTSIZE; i++) {	/* get the size out of the string */
	size <<= 8;
	size |= bp[INTSIZE-i-1] & 0xff;
    }
    bp += INTSIZE;

    for(i = 0; i < RFRONT; i++)		/* check front range check */
	if (bp[i] != RANGE[i]) {
	    maybe_quit("leftside malloc buffer overrun");
	    break;
	}
    bp += RFRONT;			/* skip front range check */

    for(i = 0; i < RBACK; i++)		/* check back range check */
	if (bp[i+size] != RANGE[i+RFRONT]) {
	    maybe_quit("rightside malloc buffer overrun");
	    break;
	}
    return(xp);
}

static char *
maybe_check_range(cp) char *cp; {
    register char *bp = cp - RFRONT - INTSIZE;
    char *xp = bp;
    register int i;
    int size = 0;

    for(i = 0 ; i < INTSIZE; i++) {	/* get the size out of the string */
	size <<= 8;
	size |= bp[INTSIZE-i-1] & 0xff;
    }
    bp += INTSIZE;

    for(i = 0; i < RFRONT; i++)		/* check front range check */
	if (bp[i] != RANGE[i]) {
	    return(cp);
	}
    bp += RFRONT;			/* skip front range check */

    for(i = 0; i < RBACK; i++)		/* check back range check */
	if (bp[i+size] != RANGE[i+RFRONT]) {
	    fprintf(stderr,"rightside malloc buffer overrun\n");
	    abort();
	    break;
	}
    return(xp);
}

#define BUCKETS 10000
char *m_used[BUCKETS];
char *m_used2[BUCKETS];

VOID
m_insert(cp) register char *cp; {
    register int i;

    if (disabled)
	return;

    for(i = 0; i < BUCKETS; i++)
	if (m_used[i] == 0) {
	    m_used[i] = cp;
	    return;
	}
    disabled ++;
}

static
m_insert2(cp) register char *cp; {
    register int i;

    if (disabled)
	return;
    for(i = 0; i < BUCKETS; i++)
	if (m_used2[i] == 0) {
	    m_used2[i] = cp;
	    return;
	}
    disabled ++;
}

VOID
m_delete(cp) register char *cp; {
    register int i;

    for(i = 0; i < BUCKETS; i++)
	if (m_used[i] == cp) {
	    m_used[i] = 0;
	    return(1);
	}
    for(i = 0; i < BUCKETS; i++)
	if (m_used2[i] == cp) {
	    m_used2[i] = 0;
	    return(2);
	}
    if (disabled) 
	return(0);

    maybe_quit("Freeing unmalloc'ed pointer");
    return(0);
}

VOID
m_init() {
    register int i;

    inited = 1;
    disabled = 0;
    for(i = 0; i < BUCKETS; i++)
      m_used[i] = 0;
}

VOID
m_done() {
    register int i,j=0;

    if (disabled) 
	return;
    for(i = 0; i < BUCKETS; i++)
	if (m_used[i] != 0) {
	    if (memdebug) {
		if (j == 0)
		    fprintf(stderr,"unfree'ed buffers, indices: ");
		fprintf(stderr,"%d, ", i);
		j++;
	    }
	}
    if (j)
	fprintf(stderr,"\n");
    for(i = 0; i < BUCKETS; i++)
	if (m_used2[i] != 0) {
	    if (memdebug) {
		if (j == 0)
		    fprintf(stderr,"unfree'ed registered buffers, indices: ");
		fprintf(stderr,"%d, ", i);
		j++;
	    }
	}
    if (j)
	fprintf(stderr,"\n");
    if (j)
	maybe_quit("Unfree'ed malloc buffers");
}

VOID
m_checkranges() {
    int i;

    for ( i = 0; i < BUCKETS; i++)
	if (m_used[i])
	    check_range(m_used[i]);
}

static VOID
maybe_quit(str) char *str; {
    debug(F100,"mdebug maybe_quit","",0);
    if (memdebug == 0)
	return;
    fprintf(stderr,"%s\n",str);
    if (memdebug == 1)
	abort();
    if (memdebug == -1)
	if (ask("Quit? "))
	    abort();
}

static int
ask(str) char *str; {
    char buf[100];
    FILE *in;
    int fd;
    
    fd = dup(fileno(stdin));
    in = fdopen(fd, "r");
    while(1) {
	fprintf(stderr,str);
	fflush(stderr);
	if (fgets(buf, 99, in) == NULL)	/* EOF? */
	    return(0);
	if (buf[0] == 'n' || buf[0] == 'N') {
	    fclose(in);
	    return(0);
	}
	if (buf[0] == 'y' || buf[0] == 'Y') {
	    fclose(in);
	    return(1);
	}
	fprintf(stderr,"please answer y/n.\n");
    }
}
#endif /* MDEBUG */
