/* Intel 386 running any BSD Unix */
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>
#include <machine/param.h>
#include <machine/vmparam.h>

#ifndef O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif

#define SEEK_SET 0
#define SEEK_CUR 1

#define	HOST_PAGE_SIZE		NBPG
#define	HOST_MACHINE_ARCH	bfd_arch_i386
#define	HOST_TEXT_START_ADDR		USRTEXT
#define HOST_DATA_START_ADDR    ((bfd_vma)u.u_kproc.kp_eproc.e_vm.vm_daddr)

#if 0	/* This doesn't work in Jolitz release 0.1 */
#define	HOST_STACK_END_ADDR		USRSTACK
#else	/* Found by experimentation. */
#define	HOST_STACK_END_ADDR		(USRSTACK - MAXSSIZ)
#endif

#define TRAD_UNIX_CORE_FILE_FAILING_SIGNAL(core_bfd) \
  ((core_bfd)->tdata.trad_core_data->u.u_sig)
#define u_comm u_kproc.kp_proc.p_comm

#define MINIMIZE		1
#define TRAD_CORE		1
#define DEFAULT_VECTOR		i386bsd_vec
#define SELECT_ARCHITECTURES	bfd_i386_arch

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
