
#/************************************************************************
# *									*
# *		      Placed in the public domain by			*
# *		Digital Equipment Corporation, Maynard, MA		*
# *									*
# *	The information in this software is subject to change without	*
# *	notice and should not be construed as a commitment by Digital	*
# *	Equipment Corporation.  Digital makes no representations	*
# *	about suitability of this software for any purpose. It is	*
# *	supplied "as is" without express or implied warranty.		*
# *									*
# ************************************************************************/
#
#/*
# * MODIFICATION HISTORY
# *
# * 000 -- M. Gancarz, DEC Ultrix Engineering Group
# */
#
# 
# browse.sh - runs a command in an xterm window until you hit <RETURN>
#
# SCCSID = "@(#)browse.sh	3.8		1/24/86"
$*
echo -n 'Hit <RETURN> to exit...'
read x
exit
#
#	$Header: browse.sh,v 10.2 86/02/01 16:24:10 tony Rel $
#
