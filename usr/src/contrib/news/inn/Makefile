##  $Revision: 1.33 $
SHELL	= /bin/sh
MAKE	= make

##  The first two directories must be config and lib.
PROGS	= config lib frontends innd nnrpd backends expire doc
DIRS	= $(PROGS) site

##  We invoke an extra process and set this to be what to make.
WHAT_TO_MAKE	= all

##  Delete the first two lines and all lines that contain (Directory).
##  Print only the first field of all other lines.  This gets us just
##  the list of files from the MANIFEST.
SEDCOMMANDS = -e 1,2d -e '/(Directory)/d' -e 's/ .*//'

##  Major target -- install library, build everything else.
all:
	@for D in $(DIRS) ; do \
	    TARGET=$(WHAT_TO_MAKE); \
	    case $$D in lib) TARGET=install ;; esac ; \
	    echo "" ; \
	    echo "cd $$D ; $(MAKE) $(FLAGS) $$TARGET ; cd .." ; \
	    cd $$D; $(MAKE) $(FLAGS) $$TARGET ; cd .. ; \
	done

##  Install everything.
install:	directories
	$(MAKE) $(FLAGS) WHAT_TO_MAKE=install common
	@echo Do not forget to update your cron entries.
	@echo Also run makehistory if you have to.

##  Directories where files get put.
directories:
	$(SHELL) ./makedirs.sh

##  Other generic targets.
lint depend tags ctags profiled:
	@$(MAKE) $(FLAGS) WHAT_TO_MAKE=$@ common

clean:
	@$(MAKE) $(FLAGS) WHAT_TO_MAKE=$@ common
	rm -f libinn.a libinn_p.a llib-linn.ln FILELIST

##  Common target.
common:
	@for D in $(DIRS) ; do \
	    echo "" ; \
	    echo "cd $$D ; $(MAKE) $(FLAGS) $(WHAT_TO_MAKE) ; cd .." ; \
	    cd $$D; $(MAKE) $(FLAGS) $(WHAT_TO_MAKE) ; cd .. ; \
	done

##  Software update -- install just the programs and documentation.
update:
	@for D in $(PROGS) ; do \
	    echo "" ; \
	    echo "cd $$D ; $(MAKE) $(FLAGS) install ; cd .." ; \
	    cd $$D; $(MAKE) $(FLAGS) install ; cd .. ; \
	done

##  Build subst (for configuration).
subst c sh quiet sedtest:
	cd config ; $(MAKE) $(FLAGS) $@ ; cd ..

##  Build installation document.
Install.ms:	Install.ms.1 Install.ms.2
	@rm -f Install.ms
	cat Install.ms.1 Install.ms.2 >Install.ms
	chmod 444 Install.ms

##  Additional cleanups.
clobber realclean:	clean
	@echo ""
	rm -f Install.ms inn*.tar.Z Part0? MANIFEST.BAK
	rm -rf inews.* rnews.* nntplib.*
	rm -f lib/dbz.c include/dbz.h doc/dbz.3z
	rm -f tags */tags core */core a.out */a.out foo */foo
	@echo ""
	cd site ; make clobber ; cd ..

##  Update syslog.
syslogfix:
	rm -f include/syslog.h lib/syslog.c
	cp syslog/syslog.h include
	cp syslog/syslog.c lib
	cp syslog/syslog.3 doc
	-cd syslog; $(CC) -o syslogd syslogd.c ; cd ..
	@echo "Install syslogd and syslog.conf as appropriate"

##  Configure, compile, and lint.
world:		Install.ms
	cd config ; $(MAKE) $(FLAGS) subst quiet ; cd ..
	cd lib ; $(MAKE) $(FLAGS) lint ; cd ..
	cat lib/lint
	cd lib ; $(MAKE) $(FLAGS) install ; cd ..
	$(MAKE) $(FLAGS) lint

##  Make a distribution.
shar:
	makekit -m -k40 -s70k
tar:
	rm -f inn*.tar.Z
	@rcs -l MANIFEST
	makekit -m -k40 -s70k -x
	tar cf inn.tar `sed $(SEDCOMMANDS) <MANIFEST`
	compress inn.tar
	mv inn.tar.Z inn`date +%m%d`.tar.Z

##  Local convention; for xargs.
list:	FORCE
	@sed $(SEDCOMMANDS) <MANIFEST >FILELIST
FORCE:

# DO NOT DELETE THIS LINE -- make depend depends on it.
