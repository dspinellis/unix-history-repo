#ifndef	_FAKE_SYS_MMAN_H
#define	_FAKE_SYS_MMAN_H 1

#define	mmap	mmap_hack	/* fake mmap() using malloc() + read() */
#define	munmap	munmap_hack	/* fake munmap() using free() */

#define	PROT_READ	0x1	/* pages can be read */
#define	PROT_WRITE	0x2	/* pages can be written */
#define	PROT_EXEC	0x4	/* pages can be executed */

#define	MAP_SHARED	1	/* share changes */
#define	MAP_PRIVATE	2	/* changes are private */

#ifdef __STDC__
#include <sys/types.h>
extern void *mmap(void *, size_t, int, int, int, off_t);
extern int munmap(void *, size_t);
#else
extern void *mmap();
extern int munmap();
#endif

#endif /* _FAKE_SYS_MMAN_H */
