everything:	all

what:
		@echo "what do you want to make?"

Icon:
		cd bin;			make
		cd tran;		make
		cd link;		make
		cd operators;		make
		cd functions;		make
		cd lib;			make
		cd rt;			make
		cd iconx;		make
		cp tran/itran link/ilink iconx/iconx bin
		cd bin;			strip icont itran ilink iconx
		cd pilib;		make

Pidemo:
		cd pidemo;		../icon-pi

Alltest:
		cd samples;		make All

Stdtest:
		cd samples;		make Std

Settest:
		cd samples;		make Set

Xpxtest:
		cd samples;		make Xpx

Libtest:
		cd libtest;		make All

Pitest:
		cd libtest;		Functest

Porttest:
		cd port;		make All

Testsuite:
		cd test;		make All

Library:
		cd src;			make All
		cd pifuncs;		make All

Listall:
		@cd bin;		make Listall
		@cd iconx;		make Listall
		@cd h;			make Listall
		@cd tran;		make Listall
		@cd link;		make Listall
		@cd rt;			make Listall
		@cd functions;		make Listall
		@cd operators;		make Listall
		@cd lib;		make Listall

List:
		@cd bin;		make -s List
		@cd iconx;		make -s List
		@cd h;			make -s List
		@cd tran;		make -s List
		@cd link;		make -s List
		@cd rt;			make -s List
		@cd functions;		make -s List
		@cd operators;		make -s List
		@cd lib;		make -s List

Clean:
		cd tran;		make Clean
		cd link;		make Clean
		cd operators;		make Clean
		cd functions;		make Clean
		cd lib;			make Clean
		cd rt;			make Clean
		cd iconx;		make Clean
		cd bin;			make Clean
		cd pifuncs;		make Clean
		cd samples;		make Clean
		cd test;		make Clean
		cd libtest;		make Clean
		cd port;		make Clean

#
# Stuff for 4.3bsd
#
ILIB=$(DESTDIR)/usr/new/lib/icon
IBIN=$(DESTDIR)/usr/new/lib/icon/5.9.0
INEW=$(DESTDIR)/usr/new

all:	Icon

setup-new:
	Icon-setup -vax -interpex -host gethost -vfork -xpx -sets \
		-ibin $(IBIN)

setup-pwd:
	Icon-setup -vax -interpex -host gethost -vfork -xpx -sets
	
install: all
	-mkdir $(ILIB)
	-mkdir $(IBIN)
	cp bin/itran bin/ilink bin/iconx bin/icont $(IBIN)
	cp bin/icont $(INEW)
	cp icon-pi $(INEW)
	cp docs/icont.1 $(DESTDIR)/usr/man/mann/icont.n
	cp docs/icon-pi.1 $(DESTDIR)/usr/man/mann/icon-pi.n
	
clean:	Clean

#  The entries here and in other Makefiles that start with "Dist" are
#  used only at the University of Arizona for preparing the Icon system
#  for distribution. The program gcomp, which is used to specify
#  files not to remove, is from the Icon program library.

Dist-build:
		Icon-setup -vax -interpex -host gethost -vfork -xpx -sets
		make Icon

Dist-test:
		make Alltest
		make Library
		make Pidemo
		make Libtest
		make Pitest
		make Testsuite
		make Porttest
		cd port;		make Linkcheck

Dist-setup:
		cd samples;		make Dist-setup
		cd test;		make Dist-setup
		cd port;		make Dist-setup
		cd libtest;		make Dist-setup

Dist-clean:
		cd tran;		make Dist-clean
		cd link;		make Dist-clean
		cd operators;		make Dist-clean
		cd functions;		make Dist-clean
		cd lib;			make Dist-clean
		cd rt;			make Dist-clean
		cd iconx;		make Dist-clean
		cd h;			make Dist-clean
		cd pifuncs;		make Dist-clean
		cd pilib;		make Dist-clean
		cd samples;		make Dist-clean
		cd port;		make Dist-clean
		cd test;		make Dist-clean
		cd libtest;		make Dist-clean
		cd src;			make Dist-clean
		cd docs;		make Dist-clean
		cd man;			make Dist-clean
		cd book;		make Dist-clean
		rm -f ibin/*
		touch ibin/.placeholder
		rm -f ilib/*
		touch ilib/.placeholder
		rm -rf pidemo/*
		touch pidemo/.placeholder
		rm -f .BK* .CK* .emacs*
		rm -rf `gcomp bin functions h iconx lib link operators \
		   rt tran docs samples Icon-setup Makefile book libtest \
		   port test *.gen pidemo ibin ilib pifuncs src man pilib`
		cd bin;			make Dist-clean
