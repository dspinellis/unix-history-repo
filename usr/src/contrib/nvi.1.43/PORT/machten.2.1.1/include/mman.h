/* Fake <sys/mman.h> included by ex/ex_tag.c. */

#undef	PROT_READ
#define	PROT_READ	0

#undef	MAP_PRIVATE
#define	MAP_PRIVATE	0

#define	mmap	mmap_read
#define	munmap	munmap_read
