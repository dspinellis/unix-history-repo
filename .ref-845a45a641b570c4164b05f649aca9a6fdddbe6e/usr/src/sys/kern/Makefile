#	@(#)Makefile	7.17 (Berkeley) %G%

# Makefile for kernel tags files, init_sysent, etc.

ARCH=	vax tahoe hp300 i386 pmax sparc

all:
	@echo "make tags, make links or init_sysent.c only"

init_sysent.c syscalls.c ../sys/syscall.h: makesyscalls.sh syscalls.master
	-mv -f init_sysent.c init_sysent.c.bak
	-mv -f syscalls.c syscalls.c.bak
	-mv -f ../sys/syscall.h ../sys/syscall.h.bak
	sh makesyscalls.sh syscalls.master

# Kernel tags:
# Tags files are built in the top-level directory for each architecture,
# with a makefile listing the architecture-dependent files, etc.
# The list of common files is in ./Make.tags.inc.  Links to the
# correct tags file are placed in each source directory.
# We need to have links to tags files from the generic directories
# that are relative to the machine type, even via remote mounts;
# therefore we use symlinks to $SYSTAGS, which points at
# ${SYSDIR}/${MACHINE}/tags.

SYSTAGS=/var/db/sys_tags
SYSDIR=/sys

# Directories in which to place tags links (other than machine-dependent)
DGEN=	kern sys net netimp netinet netns netccitt nfs ufs/ffs ufs/lfs \
	ufs/mfs ufs/ufs vm

tags: FRC
	-for i in ${ARCH}; do \
	    (cd ../$$i && make ${MFLAGS} tags); done

links: ${SYSTAGS}
	-for i in ${DGEN}; do \
	    (cd ../$$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done
	-for i in ${ARCH}; do \
	    (cd ../$$i && make ${MFLAGS} links); done

${SYSTAGS}:
	ln -s ${SYSDIR}/${MACHINE}/tags ${SYSTAGS}

FRC:
