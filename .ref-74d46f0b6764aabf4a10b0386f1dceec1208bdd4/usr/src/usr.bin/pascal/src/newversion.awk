# Copyright (c) 1980 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)newversion.awk	5.2 (Berkeley) %G%
#

/^VERSION = /	{
		    n = split( $3, version, ".")
		    print "VERSION = " version[1] "." version[2]+1
		    break
		}
		{
		    print $0
		}
