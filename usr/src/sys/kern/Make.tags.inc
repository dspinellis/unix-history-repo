#	@(#)Make.tags.inc	7.2 (Berkeley) %G%

# common files for "make tags"
# included by makefile for each architecture

# Put the ../sys stuff near the end so that subroutine definitions win when
# there is a struct tag with the same name (eg., vmmeter).  The real
# solution would probably be for ctags to generate "struct vmmeter" tags.

COMM=	../kern/*.c ../vm/*.c ../vm/*.h \
	../net/*.[ch] ../netimp/*.[ch] ../netinet/*.[ch] ../netns/*.[ch] \
	../netiso/*.[ch] ../netccitt/*.[ch] \
	../ufs/ufs/*.[ch] ../ufs/ffs/*.[ch] ../ufs/lfs/*.[ch] \
	../ufs/mfs/*.[ch] ../nfs/*.[ch] \
	../sys/*.h
