# This file was generated automatically by configure.  Do not edit.
VPATH = .
links = 
host_alias = hppa1.0-bsd
host_cpu = hppa1.0
host_vendor = unknown
host_os = bsd
host_canonical = hppa1.0-unknown-bsd
target_alias = hppa1.0-bsd
target_cpu = hppa1.0
target_vendor = unknown
target_os = bsd
target_canonical = hppa1.0-unknown-bsd
ALL=all.internal
#
# Makefile for directory with subdirs to build.
#   Copyright (C) 1990, 1991, 1992 Free Software Foundation
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */
#

srcdir = .

prefix = /usr/local

exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib
tooldir = $(libdir)/hppa1.0-bsd

program_transform_name = 

datadir = $(prefix)/lib
mandir = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
infodir = $(prefix)/info
includedir = $(prefix)/include
docdir = $(datadir)/doc

SHELL = /bin/sh

INSTALL = cp
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AS = as
AR = ar
AR_FLAGS = qc
CFLAGS = -g
CXXFLAGS = -g -O
RANLIB = ranlib
NM = nm

BISON = `if [ -f $${rootme}/byacc/byacc ] ; \
	    then echo $${rootme}/byacc/byacc ; \
	    else echo byacc ; \
	fi`

LEX = `if [ -f $${rootme}/flex/flex ] ; \
	then echo $${rootme}/flex/flex ; \
	else echo flex ; fi`

MAKEINFO = `if [ -f $${rootme}/texinfo/C/makeinfo ] ; \
	then echo $${rootme}/texinfo/C/makeinfo ; \
	else echo makeinfo ; fi`

# C compiler to use to create programs which must be run in the build
# environment.
CC_FOR_BUILD = $(CC)
CXX_FOR_BUILD = gcc

SUBDIRS = m4 autoconf mmalloc libiberty texinfo flex byacc bison opcodes   gcc readline glob    emacs ispell make grep diff rcs cvs patch send_pr libg++ gprof gdbtest libm tgas etc tcl expect tk deja-gnu z8ksim sed fileutils shellutils textutils time wdiff find
OTHERS = 

ALL = all.normal
INSTALL_TARGET = install.all

### for debugging
#GCCVERBOSE=-v

CC_FOR_TARGET = ` \
  if [ -f $${rootme}/gcc/Makefile ] ; then \
    echo $${rootme}/gcc/gcc -B$${rootme}/gcc/; \
  else \
    if [ "$(host_canonical)" = "$(target_canonical)" ] ; then \
      echo $(CC); \
    else \
      t='$(program_transform_name)'; echo gcc | sed -e '' $$t; \
    fi; \
  fi`

CXX_FOR_TARGET = ` \
  if [ -f $${rootme}/gcc/Makefile ] ; then \
    echo $${rootme}/gcc/gcc -B$${rootme}/gcc/; \
  else \
    if [ "$(host_cpu)-$(host_vendor)-$(host_os)" = "$(target_cpu)-$(target_vendor)-$(target_os)" ] ; then \
      echo gcc; \
    else \
      t='$(program_transform_name)'; echo gcc | sed -e '' $$t; \
    fi; \
  fi`

AS_FOR_TARGET = ` \
  if [ -f $${rootme}/gas/Makefile ] ; then \
    echo $${rootme}/gas/as.new ; \
  else \
    if [ "$(host_cpu)-$(host_vendor)-$(host_os)" = "$(target_cpu)-$(target_vendor)-$(target_os)" ] ; then \
      echo $(AS); \
    else \
       t='$(program_transform_name)'; echo as | sed -e '' $$t ; \
    fi \
  fi`

AR_FOR_TARGET = ` \
  if [ -f $${rootme}/binutils/Makefile ] ; then \
    echo $${rootme}/binutils/ar ; \
  else \
    if [ "$(host_canonical)" = "$(target_canonical)" ] ; then \
      echo $(AR); \
    else \
       t='$(program_transform_name)'; echo ar | sed -e '' $$t ; \
    fi \
  fi`

RANLIB_FOR_TARGET = ` \
  if [ -f $${rootme}/binutils/Makefile ] ; then \
    echo $${rootme}/binutils/ranlib ; \
  else \
    if [ "$(host_canonical)" = "$(target_canonical)" ] ; then \
      echo $(RANLIB); \
    else \
       t='$(program_transform_name)'; echo ranlib | sed -e '' $$t ; \
    fi \
  fi`

NM_FOR_TARGET = ` \
  if [ -f $${rootme}/binutils/Makefile ] ; then \
    echo $${rootme}/binutils/nm ; \
  else \
    if [ "$(host_canonical)" = "$(target_canonical)" ] ; then \
      echo $(NM); \
    else \
       t='$(program_transform_name)'; echo nm | sed -e '' $$t ; \
    fi \
  fi`

XTRAFLAGS = ` \
  if [ -f $${rootme}/gcc/Makefile ] ; then \
    if [ -f $${rootme}/newlib/Makefile ] ; then \
      echo -I$${rootme}/newlib/targ-include -I$${srcroot}/newlib/libc/include -I$${rootme}/gcc/include -nostdinc ; \
    else \
      echo -I$${rootme}/gcc/include ; \
    fi ; \
  else \
     echo ; \
  fi`

#### host and target specific makefile fragments come in here.
###

# Flags to pass down to sub-makes -- please keep these in alphabetical order
FLAGS_TO_PASS = \
	"AR=$(AR)" \
	"AR_FLAGS=$(AR_FLAGS)" \
	"BISON=$(BISON)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CXX_FOR_BUILD=$(CXX_FOR_BUILD)" \
	"CC=$(CC)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CFLAGS=$(CFLAGS)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LEX=$(LEX)" \
	"LOADLIBES=$(LOADLIBES)" \
	"MAKEINFO=$(MAKEINFO)" \
	"RANLIB=$(RANLIB)" \
	"exec_prefix=$(exec_prefix)" \
	"prefix=$(prefix)" \
	"tooldir=$(tooldir)" 

# Flags that are concerned with the location of the X11 include files
# and library files
X11_FLAGS_TO_PASS = \
	"X11_INCLUDE_FLAGS=$(X11_INCLUDE_FLAGS)" \
	"X11_LIB_FLAGS=$(X11_LIB_FLAGS)"

# Flags to pass down to makes which are built with the target
# environment (e.g. libg++, xiberty, newlib).  -- keep these in alpha order please
TARGET_FLAGS_TO_PASS = \
	"AR=$(AR_FOR_TARGET)" \
	"AR_FLAGS=$(AR_FLAGS)" \
	"AS=$(AS_FOR_TARGET)" \
	"BISON=$(BISON)" \
	"CXX=$(CXX_FOR_TARGET)" \
	"CXX_FOR_BUILD=$(CXX_FOR_BUILD)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CC=$(CC_FOR_TARGET)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CFLAGS=$(CFLAGS)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LEX=$(LEX)" \
	"LOADLIBES=$(LOADLIBES)" \
	"MAKEINFO=$(MAKEINFO)" \
	"NM=$(NM_FOR_TARGET)" \
	"RANLIB=$(RANLIB_FOR_TARGET)" \
	"XTRAFLAGS=$(XTRAFLAGS)" \
	"exec_prefix=$(exec_prefix)" \
	"prefix=$(prefix)" \
	"tooldir=$(tooldir)" 

# The first rule in the file had better be this one.  Don't put any above it.
all:	$(ALL)

.PHONY: all info install-info clean-info
.NOEXPORT:

info:	
	@rootme=`pwd` ; export rootme ; $(MAKE) $(FLAGS_TO_PASS) subdir_do DO=info "DODIRS=$(SUBDIRS)" 

check:
	@rootme=`pwd` ; export rootme ; $(MAKE) $(FLAGS_TO_PASS) DO=check "DODIRS=$(SUBDIRS)" subdir_do

clean-info:
	@rootme='pwd' ; export rootme ; $(MAKE) $(FLAGS_TO_PASS) DO=clean-info "DODIRS=$(SUBDIRS)" subdir_do

install-info: install-info-dirs force
	-parent=`echo $(infodir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	-if [ -d $(infodir) ] ; then true ; else mkdir $(infodir) ; fi
	@rootme='pwd' ; export rootme ; $(MAKE) $(FLAGS_TO_PASS) DO=install-info "DODIRS=$(SUBDIRS)" subdir_do
	@rootme='pwd' ; export rootme ; $(MAKE) $(FLAGS_TO_PASS) dir.info install-dir.info

install-dir.info:
	-parent=`echo $(infodir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	-if [ -d $(infodir) ] ; then true ; else mkdir $(infodir) ; fi
	$(INSTALL_DATA) dir.info $(infodir)/dir.info

all.normal: all-m4 all-autoconf all-libiberty all-mmalloc all-texinfo \
	all-byacc all-flex all-opcodes all-bfd all-ld all-gas all-tgas \
	all-gcc all-binutils  all-libg++ all-readline all-z8ksim all-gdb \
	all-make all-rcs all-cvs all-diff all-grep \
	all-patch all-emacs all-ispell all-etc \
	all-tcl all-tk all-expect \
	all-newlib all-gprof all-send_pr all-libm all-deja-gnu \
	all-fileutils all-find all-gawk all-sed all-shellutils \
	all-textutils all-time all-wdiff


all.cross: all-libiberty all-mmalloc all-gas all-byacc all-flex all-ld \
	 all-opcodes all-z8ksim all-bfd all-readline all-gdb all-binutils all-gcc \
	all-newlib all-deja-gnu

clean: clean-autoconf clean-libiberty clean-mmalloc clean-texinfo \
	clean-bfd clean-newlib clean-binutils clean-flex \
	clean-byacc clean-ld clean-gas clean-etc clean-opcodes \
	clean-gcc clean-readline clean-glob clean-gdb clean-z8ksim \
	clean-make clean-diff clean-grep clean-rcs \
	clean-cvs clean-patch clean-emacs clean-ispell \
	clean-tcl clean-tk clean-expect \
	clean-libg++ clean-gprof clean-send_pr clean-libm clean-xiberty \
	clean-deja-gnu \
	clean-fileutils clean-find clean-gawk clean-m4 clean-sed clean-shellutils \
	clean-textutils clean-time clean-wdiff
	-rm -rf *.a TEMP errs core *.o *~ \#* TAGS *.E

clean-stamps:
	-rm -f all-*

install: $(INSTALL_TARGET) 
	-parent=`echo $(man1dir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	-if [ -d $(man1dir) ] ; then true ; else mkdir $(man1dir) ; fi

install.all: install-no-fixedincludes
	@if [ -f ./gcc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gcc; \
		$(MAKE) $(FLAGS_TO_PASS) install-headers) ; \
	else \
		true ; \
	fi

install-no-fixedincludes: install-dirs gcc-no-fixedincludes \
	install-autoconf \
	install-bfd \
	install-binutils \
	install-opcodes \
	install-byacc \
	install-cvs \
	install-diff \
	install-deja-gnu \
	install-emacs \
	install-etc \
	install-expect \
	install-flex \
	install-gas \
	install-gdb \
	install-glob \
	install-gprof \
	install-grep \
	install-ispell \
	install-ld \
	install-libg++ \
	install-libiberty \
	install-libm \
	install-make \
	install-mmalloc \
	install-newlib \
	install-patch \
	install-rcs \
	install-readline \
	install-send_pr \
	install-tcl \
	install-texinfo \
	install-tk  \
	install-z8ksim \
	install-fileutils install-find install-gawk install-m4 install-sed install-shellutils \
	install-textutils install-time install-wdiff

gcc-no-fixedincludes:
	@if [ -f ./gcc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gcc; \
		$(MAKE) $(FLAGS_TO_PASS) install install-man \
			"INSTALL_HEADERS=install-common-headers install-float-h install-limits-h") ; \
	else \
		true ; \
	fi

install.cross: install-dirs install-libiberty install-mmalloc \
        install-binutils install-opcodes install-byacc install-flex \
        install-ld install-gas install-readline \
        install-glob install-gdb install-mmalloc \
        install-newlib install-gcc install-etc install-deja-gnu

### deja-gnu
all-deja-gnu: force
	@if [ -f ./deja-gnu/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./deja-gnu; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-deja-gnu: force
	@if [ -f ./deja-gnu/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./deja-gnu;	$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-deja-gnu: force
	@if [ -f ./deja-gnu/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./deja-gnu;	$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### autoconf
all-autoconf: force
	@if [ -f ./autoconf/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./autoconf; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-autoconf: force
	@if [ -f ./autoconf/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./autoconf;	$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-autoconf: force
	@if [ -f ./autoconf/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./autoconf;	$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### etc
all-etc: force
	@if [ -f ./etc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./etc; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-etc: force
	@if [ -f ./etc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./etc; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-etc: force
	@if [ -f ./etc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./etc; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### libiberty
all-libiberty: force
	@if [ -f ./libiberty/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./libiberty; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-libiberty: force
	@if [ -f ./libiberty/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./libiberty; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-libiberty: force
	@if [ -f ./libiberty/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./libiberty; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### xiberty
all-xiberty: all-gcc all-newlib
	@if [ -f ./xiberty/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./xiberty; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-xiberty: force
	@if [ -f ./xiberty/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./xiberty; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-xiberty: force
	@if [ -f ./xiberty/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./xiberty; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### mmalloc
all-mmalloc: force
	@if [ -f ./mmalloc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./mmalloc; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-mmalloc: force
	@if [ -f ./mmalloc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./mmalloc; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-mmalloc: force
	@if [ -f ./mmalloc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./mmalloc; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### texinfo
all-texinfo: all-libiberty
	@if [ -f ./texinfo/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./texinfo; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-texinfo: force
	@if [ -f ./texinfo/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./texinfo; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-texinfo: force
	@if [ -f ./texinfo/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./texinfo; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### bfd
all-bfd: force
	@if [ -f ./bfd/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./bfd; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-bfd: force
	@if [ -f ./bfd/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./bfd; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-bfd: force
	@if [ -f ./bfd/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./bfd; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi


### opcodes
all-opcodes: force
	@if [ -f ./opcodes/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./opcodes; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-opcodes: force
	@if [ -f ./opcodes/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./opcodes; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-opcodes: force
	@if [ -f ./opcodes/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./opcodes; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### binutils
all-binutils: all-opcodes all-libiberty all-bfd all-flex
	@if [ -f ./binutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./binutils; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-binutils: force
	@if [ -f ./binutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./binutils; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-binutils: force
	@if [ -f ./binutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./binutils; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### newlib
all-newlib: all-binutils all-ld all-gas all-gcc
	@if [ -f ./newlib/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd` ; export srcroot ; \
		(cd ./newlib; \
		$(MAKE) $(TARGET_FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-newlib: force
	@if [ -f ./newlib/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd` ; export srcroot ; \
		(cd ./newlib; \
		$(MAKE) $(TARGET_FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-newlib: force
	@if [ -f ./newlib/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd` ; export srcroot ; \
		(cd ./newlib; \
		$(MAKE) $(TARGET_FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### gprof
all-gprof: all-libiberty all-bfd
	@if [ -f ./gprof/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gprof; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-gprof: force
	@if [ -f ./gprof/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gprof; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-gprof: force
	@if [ -f ./gprof/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gprof; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### byacc
all-byacc: force
	@if [ -f ./byacc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./byacc; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-byacc: force
	@if [ -f ./byacc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./byacc; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-byacc: force
	@if [ -f ./byacc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./byacc; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### flex
all-flex: all-libiberty
	@if [ -f ./flex/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./flex; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-flex: force
	@if [ -f ./flex/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./flex; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-flex: force
	@if [ -f ./flex/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./flex; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi
### gcc
all-gcc: all-libiberty all-byacc all-binutils
	@if [ -f ./gcc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gcc; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-gcc: force
	@if [ -f ./gcc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gcc; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-gcc: force
	@if [ -f ./gcc/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gcc; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### readline
all-readline: force
	@if [ -f ./readline/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./readline; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-readline: force
	@if [ -f ./readline/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./readline; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-readline: force
	@if [ -f ./readline/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./readline; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### glob
all-glob: force
	@if [ -f ./glob/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./glob; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-glob: force
	@if [ -f ./glob/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./glob; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-glob: force
	@if [ -f ./glob/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./glob; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### gas
all-gas: all-libiberty all-opcodes all-bfd
	@if [ -f ./gas/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gas; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-gas: force
	@if [ -f ./gas/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gas; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-gas: force
	@if [ -f ./gas/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gas; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### gas
all-tgas: all-libiberty all-bfd
	@if [ -f ./tgas/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./tgas; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi


### ld
all-ld: all-libiberty all-bfd all-byacc all-flex
	@if [ -f ./ld/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./ld; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-ld: force
	@if [ -f ./ld/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./ld; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-ld: force
	@if [ -f ./ld/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./ld; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### gdb
all-gdb: all-bfd all-opcodes all-libiberty all-mmalloc all-readline all-glob all-byacc
	@if [ -f ./gdb/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gdb; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-gdb: force
	@if [ -f ./gdb/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gdb; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-gdb: force
	@if [ -f ./gdb/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gdb; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### make
all-make: all-libiberty
	@if [ -f ./make/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./make; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-make: force
	@if [ -f ./make/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./make; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-make: force
	@if [ -f ./make/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./make; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### diff
all-diff: force
	@if [ -f ./diff/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./diff; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-diff: force
	@if [ -f ./diff/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./diff; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-diff: force
	@if [ -f ./diff/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./diff/; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### grep
all-grep: force
	@if [ -f ./grep/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./grep; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-grep: force
	@if [ -f ./grep/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./grep; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-grep: force
	@if [ -f ./grep/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./grep; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### rcs
all-rcs: force
	@if [ -f ./rcs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./rcs; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-rcs: force
	@if [ -f ./rcs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./rcs; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-rcs: force
	@if [ -f ./rcs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./rcs; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### cvs
all-cvs: force
	@if [ -f ./cvs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./cvs; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-cvs: force
	@if [ -f ./cvs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./cvs; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-cvs: force
	@if [ -f ./cvs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./cvs; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### patch
all-patch: force
	@if [ -f ./patch/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./patch; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-patch: force
	@if [ -f ./patch/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./patch; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-patch: force
	@if [ -f ./patch/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./patch; \
		$(MAKE) $(FLAGS_TO_PASS) \
			bindir=$(bindir) \
			man1dir=$(man1dir) install) ; \
	else \
		true ; \
	fi

### emacs
all-emacs: force
	@if [ -f ./emacs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./emacs; \
		$(MAKE) $(FLAGS_TO_PASS) $(X11_FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-emacs: force
	@if [ -f ./emacs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./emacs; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-emacs: force
	@if [ -f ./emacs/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./emacs; \
		$(MAKE) $(FLAGS_TO_PASS) $(X11_FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### ispell
all-ispell: all-emacs
	@if [ -f ./ispell/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./ispell; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-ispell: force
	@if [ -f ./ispell/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./ispell; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-ispell: force
	@if [ -f ./ispell/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./ispell; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### send_pr
all-send_pr: force
	@if [ -f ./send_pr/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./send_pr; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-send_pr: force
	@if [ -f ./send_pr/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./send_pr; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-send_pr: force
	@if [ -f ./send_pr/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./send_pr; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### libm
all-libm: force
	@if [ -f ./libm/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./libm; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-libm: force
	@if [ -f ./libm/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./libm; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-libm: force
	@if [ -f ./libm/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./libm; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### libg++

all-libg++: all-gas all-ld all-gcc all-xiberty
	@if [ -f ./libg++/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd` ; export srcroot ; \
		(cd ./libg++; \
		$(MAKE) $(TARGET_FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-libg++: force
	@if [ -f ./libg++/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd` ; export srcroot ; \
		(cd ./libg++; \
		$(MAKE) $(TARGET_FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-libg++: force
	@if [ -f ./libg++/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd` ; export srcroot ; \
		(cd ./libg++; \
		$(MAKE) $(TARGET_FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi
### tcl
all-tcl: 
	@if [ -f ./tcl/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./tcl; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-tcl: force
	@if [ -f ./tcl/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./tcl; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-tcl: force
	@if [ -f ./tcl/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./tcl; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi


### tk
all-tk: all-tcl
	@if [ -f ./tk/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./tk; \
		$(MAKE) $(FLAGS_TO_PASS) $(X11_FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-tk: force
	@if [ -f ./tk/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./tk; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-tk: force
	@if [ -f ./tk/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./tk; \
		$(MAKE) $(FLAGS_TO_PASS) $(X11_FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### expect
all-expect: all-tcl
	@if [ -f ./expect/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./expect; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-expect: force
	@if [ -f ./expect/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./expect; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-expect: force
	@if [ -f ./expect/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		srcroot=`cd $(srcdir); pwd`; export srcroot ; \
		(cd ./expect; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### z8ksim
all-z8ksim: all-bfd 
	@if [ -f ./z8ksim/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./z8ksim; \
		$(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-z8ksim: force
	@if [ -f ./z8ksim/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./z8ksim; \
		$(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-z8ksim: force
	@if [ -f ./z8ksim/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./z8ksim; \
		$(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### fileutils
all-fileutils: force
	@if [ -f ./fileutils/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./fileutils; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-fileutils: force
	@if [ -f ./fileutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./fileutils; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-fileutils: force
	@if [ -f ./fileutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./fileutils; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### find
all-find: force
	@if [ -f ./find/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./find; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-find: force
	@if [ -f ./find/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./find; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-find: force
	@if [ -f ./find/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./find; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### gawk
all-gawk: force
	@if [ -f ./gawk/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./gawk; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-gawk: force
	@if [ -f ./gawk/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gawk; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-gawk: force
	@if [ -f ./gawk/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./gawk; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### m4
all-m4: force
	@if [ -f ./m4/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./m4; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-m4: force
	@if [ -f ./m4/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./m4; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-m4: force
	@if [ -f ./m4/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./m4; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### sed
all-sed: force
	@if [ -f ./sed/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./sed; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-sed: force
	@if [ -f ./sed/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./sed; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-sed: force
	@if [ -f ./sed/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./sed; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### time
all-time: force
	@if [ -f ./time/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./time; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-time: force
	@if [ -f ./time/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./time; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-time: force
	@if [ -f ./time/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./time; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### wdiff
all-wdiff: force
	@if [ -f ./wdiff/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./wdiff; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-wdiff: force
	@if [ -f ./wdiff/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./wdiff; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-wdiff: force
	@if [ -f ./wdiff/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./wdiff; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### shellutils
all-shellutils: force
	@if [ -f ./shellutils/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./shellutils; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-shellutils: force
	@if [ -f ./shellutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./shellutils; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-shellutils: force
	@if [ -f ./shellutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./shellutils; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi

### textutils
all-textutils: force
	@if [ -f ./textutils/Makefile ] ; then \
	  rootme=`pwd` ; export rootme ; \
		(cd ./textutils; $(MAKE) $(FLAGS_TO_PASS) all) ; \
	else \
		true ; \
	fi

clean-textutils: force
	@if [ -f ./textutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./textutils; $(MAKE) $(FLAGS_TO_PASS) clean) ; \
	else \
		true ; \
	fi

install-textutils: force
	@if [ -f ./textutils/Makefile ] ; then \
		rootme=`pwd` ; export rootme ; \
		(cd ./textutils; $(MAKE) $(FLAGS_TO_PASS) install) ; \
	else \
		true ; \
	fi



### other supporting targets

subdir_do:
	@for i in $(DODIRS); do \
	  if [ -f ./$$i/localenv ] || [ -f ./$$i/Makefile ] ; then \
	    case $$i in \
	    libg++ | xiberty | newlib) \
	      if (rootme=`pwd` ; export rootme ; \
		  srcroot=`cd $(srcdir); pwd` ; export srcroot ; \
		  cd ./$$i ; \
		  $(MAKE) $(TARGET_FLAGS_TO_PASS) $(DO)) ; then true ; \
	      else exit 1 ; fi \
	      ;; \
	    *) \
	      if (rootme=`pwd` ; export rootme ; \
		  cd ./$$i ; \
		  $(MAKE) $(FLAGS_TO_PASS) $(DO)) ; then true ; \
	      else exit 1 ; fi \
	      ;; \
	    esac ; \
	  else true ; fi ; \
	done

# The "else true" stuff is for Ultrix; the shell returns the exit code
# of the "if" command, if no commands are run in the "then" or "else" part,
# causing Make to quit.

MAKEDIRS= \
	$(prefix) \
	$(exec_prefix) \
	$(tooldir)

#	$(bindir) \
#	$(libdir) \
#	$(includedir) \
#	$(datadir) \
#	$(docdir) \
#	$(mandir) \
#	$(man1dir) \
#	$(man5dir)

#	$(man2dir) \
#	$(man3dir) \
#	$(man4dir) \
#	$(man6dir) \
#	$(man7dir) \
#	$(man8dir)

install-dirs:
	for i in $(MAKEDIRS) ; do \
		echo Making $$i... ; \
		parent=`echo $$i|sed -e 's@/[^/]*$$@@'`; \
		if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi ; \
		if [ ! -d $$i ] ; then \
			if mkdir $$i ; then \
				true ; \
			else \
				exit 1 ; \
			fi ; \
		else \
			true ; \
		fi ; \
	done

MAKEINFODIRS= \
	$(prefix)

install-info-dirs:
	if [ -d $(prefix) ] ; then true ; else mkdir $(prefix) ; fi

dir.info:
	$(srcdir)/texinfo/gen-info-dir $(infodir) $(srcdir)/texinfo/dir.info-template > dir.info.new
	mv -f dir.info.new dir.info

etags tags: TAGS

TAGS:
	etags `$(MAKE) ls`

ls:
	@echo Makefile
	@for i in $(SUBDIRS); \
	do \
		(cd $$i; \
			pwd=`pwd`; \
			wd=`basename $$pwd`; \
			for j in `$(MAKE) ls`; \
			do \
				echo $$wd/$$j; \
			done) \
	done

force:

# with the gnu make, this is done automatically.

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

#
# Build GDB distributions that contain BFD, Include, Libiberty, Readline, etc

DEVO_SUPPORT= README cfg-paper.texi Makefile.in configure configure.in \
	config.sub config configure.man configure.texi move-if-change \
	COPYING.LIB
GDB_SUPPORT_DIRS= bfd include libiberty mmalloc opcodes readline glob
GDB_SUPPORT_FILES= $(GDB_SUPPORT_DIRS) texinfo/fsf/texinfo.tex

setup-dirs: force_update
	./configure sun4
	make clean
	./configure -rm sun4
	chmod og=u `find $(DEVO_SUPPORT) $(GDB_SUPPORT_FILES) -print`

gdb.tar.Z: setup-dirs
	(cd gdb; $(MAKE) -f Makefile.in make-proto-gdb.dir)
	$(MAKE) $(MFLAGS) -f Makefile.in make-gdb.tar.Z

make-gdb.tar.Z: $(DEVO_SUPPORT) $(GDB_SUPPORT_DIRS) gdb texinfo/fsf/texinfo.tex
	rm -rf proto-toplev; mkdir proto-toplev
	ln -s ../gdb/proto-gdb.dir proto-toplev/gdb
	(cd proto-toplev; for i in $(DEVO_SUPPORT) $(GDB_SUPPORT_DIRS); do \
		ln -s ../$$i . ; \
	done)
	# Put only one copy (four hard links) of COPYING in the tar file.
	rm                          proto-toplev/bfd/COPYING
	ln proto-toplev/gdb/COPYING proto-toplev/bfd/COPYING
	rm                          proto-toplev/include/COPYING
	ln proto-toplev/gdb/COPYING proto-toplev/include/COPYING
	rm                          proto-toplev/readline/COPYING
	ln proto-toplev/gdb/COPYING proto-toplev/readline/COPYING
	# Take out texinfo from configurable dirs
	rm proto-toplev/configure.in
	sed '/^configdirs=/s/texinfo //' <configure.in >proto-toplev/configure.in
	# Take out glob from buildable dirs
	rm proto-toplev/Makefile.in

	sed -e '/^SUBDIRS =/s/glob //' \
	    -e '/^all\.normal: /s/\all-texinfo //' \
	    -e '/^clean: /s/clean-texinfo //' \
	    -e '/^install\.all: /s/install-texinfo //' \
	<Makefile.in >proto-toplev/Makefile.in

	mkdir proto-toplev/texinfo
	mkdir proto-toplev/texinfo/fsf
	ln -s ../../../texinfo/fsf/texinfo.tex proto-toplev/texinfo/fsf/
	chmod og=u `find proto-toplev -print`
	(VER=`sed <gdb/Makefile.in -n 's/VERSION = //p'`; \
		echo "==> Making gdb-$$VER.tar.Z"; \
		ln -s proto-toplev gdb-$$VER; \
		tar cfh - gdb-$$VER \
		| compress -v >gdb-$$VER.tar.Z)

force_update:

nothing:

# end of Makefile.in
