/* SPARC-specific values for a.out files */

#define PAGE_SIZE	0x2000		/* 8K.  aka NBPG in <sys/param.h> */
/* Note that some SPARCs have 4K pages, some 8K, some others.  */

#define SEG_SIZE_SPARC	PAGE_SIZE
#define	SEG_SIZE_SUN3	0x20000		/* Resolution of r/w protection hw */

#define TEXT_START_ADDR	PAGE_SIZE	/* Location 0 is not accessible */
#define N_HEADER_IN_TEXT(x) 1

/* Non-default definitions of the accessor macros... */

/* Segment size varies on Sun-3 versus Sun-4.  */

#define N_SEGSIZE(x)	(N_MACHTYPE(x) == M_SPARC?	SEG_SIZE_SPARC:	\
			 N_MACHTYPE(x) == M_68020?	SEG_SIZE_SUN3:	\
			/* Guess? */			PAGE_SIZE)

/* Virtual Address of text segment from the a.out file.  For OMAGIC,
   (almost always "unlinked .o's" these days), should be zero.
   Sun added a kludge so that shared libraries linked ZMAGIC get
   an address of zero if a_entry (!!!) is lower than the otherwise
   expected text address.  These kludges have gotta go!
   For linked files, should reflect reality if we know it.  */

#define N_TXTADDR(x) \
    (N_MAGIC(x)==OMAGIC? 0 \
     : (N_MAGIC(x) == ZMAGIC && (x).a_entry < TEXT_START_ADDR)? 0 \
     : TEXT_START_ADDR+EXEC_BYTES_SIZE)
