#	@(#)Make.tags.inc	7.1 (Berkeley) %G%

# common files for "make tags"
# included by makefile for each architecture

COMM=	../kern/*.c ../vm/*.[ch] ../sys/*.h \
	../net/*.[ch] ../netimp/*.[ch] ../netinet/*.[ch] ../netns/*.[ch] \
	../netiso/*.[ch] ../netccitt/*.[ch] \
	../ufs/ufs/*.[ch] ../ufs/ffs/*.[ch] ../ufs/lfs/*.[ch] \
	../ufs/mfs/*.[ch] ../nfs/*.[ch]
