/* mpattach.c --
 *
 * mpattach.c 2.7 9/14/90 12:18:59	
 * Copyright (c) 1988, Benjamin G. Zorn
 *
 * This defines MPROF intermediate functions that call the actual
 * malloc and free functions that have been renamed.
 */

extern 	char	*__malloc__();
extern	char	*__realloc__();
extern	int	__free__();
extern	int	mprofing;
extern	unsigned int 	*mprof();
extern	void	mprof_note_free();
extern	void	mprof_cleanup();


typedef struct {
    int			nbytes;
    unsigned int 	*creators;
} mpheader;
  
#define	MPROF_HEADER_SIZE	(sizeof(mpheader))


/* malloc -- Allocate header for mprof information on top of user bytes.
 *
 * Note that all objects have the extra header, but not all require
 * profiling if the flag is turned off.
 */
   
char *
malloc(nbytes)
unsigned int	nbytes;
{
    char	*result;
    mpheader	*force;
    
    result = __malloc__(nbytes + MPROF_HEADER_SIZE);
    /*
     * Check for error on allocation.
     */
    if (result == 0) {
	return 0;
    }
    if (mprofing) {
	mprofing = 0;
	force = (mpheader *) result;
	force->nbytes = nbytes;
	force->creators = mprof(nbytes);
	mprofing = 1;
    }
    return (char *) ((int) result + MPROF_HEADER_SIZE);
}


/* realloc -- do the work of free than malloc, but call realloc to do
 * it.  (Provided by Mark Eichin, eichin@athena.mit.edu) Thanks.
 */
char *
realloc(cp, nbytes)
char		*cp; 
unsigned int	nbytes;
{
    mpheader	*force;
    char	*realcp, *result;

/* not really mips specific */
/* the stupid X server I'm working on does this */
    if (!cp) {
      return (malloc(nbytes));
    }

    realcp = (char *) ((int) cp -  MPROF_HEADER_SIZE);
      
    if (mprofing) {
	mprofing = 0;
	force = (mpheader *) realcp;
	mprof_note_free(force->creators, force->nbytes);
	mprofing = 1;
    }

    result = __realloc__(realcp, nbytes + MPROF_HEADER_SIZE);

    if (mprofing) {
	mprofing = 0;
	force = (mpheader *) result;
	force->nbytes = nbytes;
	force->creators = mprof(nbytes);
	mprofing = 1;
    }
    return (char *) ((int) result + MPROF_HEADER_SIZE);
}



/* free -- Free an object, coercing it back to the correct size.
 */

free(cp)
char	*cp;
{
    mpheader	*force;
    char	*realcp;

    realcp = (char *) ((int) cp -  MPROF_HEADER_SIZE);
    force = (mpheader *) realcp;

    if (mprofing) {
	mprofing = 0;
	mprof_note_free(force->creators, force->nbytes);
	mprofing = 1;
    }
    __free__(realcp);
}


/* memalign -- Allocate aligned memory.  This patch was provided by Robert
 * Scheifler and handles calls to memalign and valloc on the Sun as long
 * as the memory allocated is never later freed.  Furthermore, unlike the
 * real Sun memalign, the extra memory allocated is not freed.
 */

#include <errno.h>
extern	int errno;

#define	round_to(i, nearest) \
  		((((i) + (nearest) - 1) / (nearest)) * (nearest))

char *
memalign(alignment, nbytes)
unsigned int	alignment;
unsigned int 	nbytes;
{
    char		*cp;

    /*
     * Check size and alignment parameters.
     */
    if (nbytes == 0 || ((unsigned)(alignment) & 3)) {
	errno = EINVAL;
	return 0;
    } else {
	nbytes = round_to(nbytes, sizeof(int));
	cp = malloc(nbytes + alignment);
	return (char *) round_to((unsigned int) cp, alignment);
    }
}


/* valloc -- Allocate memory on a page-aligned boundary.  This function
 * uses memalign so the same problems occur (i.e. the memory allocated
 * cannot later be freed and the extra memory allocated is lost).
 */

char *
valloc(nbytes)
unsigned int	nbytes;
{
    return memalign(getpagesize(), nbytes);
}
   

/* exit -- mprof has its own version of exit so that the mprof file
 * can be written and closed when mprof finishes.
 * The sun doesn't need this because on_exit allows the user to add
 * functions called on exit.
 */

#if defined(vax) || defined(mips)

exit(code)
	int code;
{
    	void mprof_exit();
    	mprof_exit(code, 0);
	_cleanup();
	_exit(code);
}

#endif

void
mprof_exit(status, dummy)
int	status;
char	*dummy;
{
    	if (mprofing) {
	    	mprofing = 0;
	    	mprof_cleanup();
	}
}

