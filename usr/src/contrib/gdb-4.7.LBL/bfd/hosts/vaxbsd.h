#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>
#include <stdlib.h>

#define	NO_CORE_COMMAND		/* No command name in core file */

#ifndef O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif

#undef	ALIGN			/* They use it, we use it too */
#include <machine/param.h>
#undef	ALIGN			/* They use it, we use it too */

/* Note that HOST_PAGE_SIZE -- the page size as far as executable files
   are concerned -- is not the same as NBPG, because of page clustering.  */
#define	HOST_PAGE_SIZE		1024
#define	HOST_MACHINE_ARCH	bfd_arch_vax

#define	HOST_TEXT_START_ADDR	0
#define	HOST_STACK_END_ADDR	(KERNBASE - (UPAGES * NBPG))
#undef	HOST_BIG_ENDIAN_P

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
