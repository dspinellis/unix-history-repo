#
# $Id: Makefile.com,v 5.2 90/06/23 22:21:03 jsp Rel $
#
# Copyright (c) 1990 Jan-Simon Pendry
# Copyright (c) 1990 Imperial College of Science, Technology & Medicine
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Jan-Simon Pendry at Imperial College, London.
#
# Redistribution and use in source and binary forms are permitted provided
# that: (1) source distributions retain this entire copyright notice and
# comment, and (2) distributions including binaries display the following
# acknowledgement:  ``This product includes software developed by the
# University of California, Berkeley and its contributors'' in the
# documentation or other materials provided with the distribution and in
# all advertising materials mentioning features or use of this software.
# Neither the name of the University nor the names of its contributors may
# be used to endorse or promote products derived from this software without
# specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile.com	5.1 (Berkeley) 6/29/90
#

#
# -------- Users may care to override these values --------
#
# Any of these values can be overridden by redefining them
# in a file called Makefile.local or Makefile.local.foo (where
# "foo" is the OS name)
#

# Where to install amd
ETC = /usr/local/etc

# With what to install amd
INSTALL = install
INSTALL_BIN = ${INSTALL} -c -m 711 -o root

# Uncomment the next CC line if you want to use GNU CC
# Better yet - put the definition in Makefile.local.<foo>
#CC = gcc ${GCCOPTS}
GCCOPTS = -fcombine-regs -W -Wunused -fstrength-reduce #-finline-functions

# Basic C compiler options
CCOPTS = -O

# These are for testing/debugging...
# Best to put your own definitions in Makefile.local.<foo>
#CCOPTS =  -g
# Turn on -DDEBUG if you want a running commentary
#DEBUG = -DDEBUG

# Define RPCINC if Sun RPC header files are not in the standard place
RPCINC = #-I../../rpc

# Not currently used but...
# Define RPCGEN as the name of your Sun *RPC/4* RPCGEN program (not RPC/3)
RPCGEN = rpcgen

# System C Compiler - one that is FULLY call compatible with your C libraries
SYSCC = cc
SYSCCFLAGS = ${CFLAGS}

# For old makes
SHELL = /bin/sh

# -------- YOU SHOULD NOT NEED TO CHANGE ANYTHING BELOW THIS LINE --------

# Magic
OS_HDR = os-${OS}.h
OSDEF = -DOS_HDR=\"${OS_HDR}\"
CFLAGS = ${CCOPTS} ${DEBUG} ${OSDEF} -I../rpcx -I../config -I../include

# Basename of the program we are trying to build
AMD = amd
AMQ = amq
MKMAP = mk-amd-map

CC_COMPILE = ${CC} -c ${CFLAGS} ${RPCINC} ${CONFIG}
SYSCC_COMPILE = ${SYSCC} -c ${SYSCCFLAGS} ${RPCINC} ${CONFIG}

#
# Keeps sysV make happy:
#
#VPATH = ..
