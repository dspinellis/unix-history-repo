/* Tahoe running BSD (post-Reno) Unix.  */

#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>
#include <stdlib.h>

#define	NO_CORE_COMMAND

#undef	ALIGN			/* They use it, we use it too */
#include <machine/param.h>
#undef	ALIGN			/* They use it, we use it too */

#define	HOST_PAGE_SIZE		NBPG
#define	HOST_MACHINE_ARCH	bfd_arch_tahoe

#define	HOST_TEXT_START_ADDR	0
#define	HOST_STACK_END_ADDR	(KERNBASE - (UPAGES * NBPG))
#define	HOST_BIG_ENDIAN_P

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
