#include <fcntl.h>
#include <errno.h>
extern int errno;		/* <errno.h> forgets to do this... */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>
#include <machine/machparam.h>

#ifndef O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif

#define SEEK_SET 0
#define SEEK_CUR 1

extern PTR  EXFUN(malloc, (unsigned));
extern PTR  EXFUN(realloc, (PTR, unsigned));
extern void EXFUN(free, (PTR));

#define	HAVE_STRERROR

#define	HOST_PAGE_SIZE		NBPG
#define	HOST_SEGMENT_SIZE	NBPG
#define	HOST_MACHINE_ARCH	bfd_arch_i386
#define	HOST_TEXT_START_ADDR	0x10000		/* By inspection */
#define	HOST_STACK_END_ADDR	KERNBASE

/* EXACT TYPES */
typedef char int8e_type;
typedef unsigned char uint8e_type;
typedef short int16e_type;
typedef unsigned short uint16e_type;
typedef int int32e_type;
typedef unsigned int uint32e_type;

/* CORRECT SIZE OR GREATER */
typedef char int8_type;
typedef unsigned char uint8_type;
typedef short int16_type;
typedef unsigned short uint16_type;
typedef int int32_type;
typedef unsigned int uint32_type;
#include "fopen-same.h"
