#! /bin/sed -f
#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)strip.sed	5.3 (Berkeley) %G%
#

/%beginstrip%/{
	h
	s/.*/.\\" This version has had comments stripped; an unstripped version is available./p
	g
}
/%beginstrip%/,$s/[. 	][ 	]*\\".*//
/^$/d
/\\n@/d
