##  $Revision: 1.26 $
SHELL	= /bin/sh
MAKE	= make
IFTRUE	= @$(SHELL) ../iftrue.sh

CONF	= config.data
FILE	= files.list
LIST	= `grep -v '^;' <$(FILE)`
BACKUP	= backup.tar

##  Pick your method of source control:  RCS, SCCS, or NONE
SRCCTL		= NONE
CHECKOUT	= $(SRCCTL)-checkout
CHECKIN		= $(SRCCTL)-checkin

##  If you use the script version of subst, and your version of sed chokes
##  then get FSF sed and edit the next line as appropriate.
#SED	= -e /usr/gnu/bin/sed


all:			substitutions

# Yes, we really want to do nothing here!
install:

clobber clean:
	rm -f subst.tmp config.new subst substitutions
	rm -f all install lint profiled

quiet:
	@$(MAKE) 'FILE=$(FILE)' substitutions | grep -v unchanged

tags ctags:
	cp /dev/null tags

lint profiled:
	cp /dev/null $@

substitutions:		$(CHECKOUT) subst files.list $(CONF)
	-@rm -f ../*/substtmp.new ../*/substtmp.old
	@$(SHELL) ./checkconf.sh $(CONF)
	./subst $(SED) -f $(CONF) $(LIST)
	$(IFTRUE) '$(SRCCTL) != NONE' '$(MAKE) $(CHECKIN)'

RCS-checkout:
	co -l $(LIST)
RCS-checkin:
	ci $(LIST)
SCCS-checkout:
	sccs get -e $(LIST)
SCCS-checkin:
	sccs delta $(LIST)
NONE-checkout:
	-chmod u+w,g+w $(LIST)
NONE-checkin:

backup:
	$(IFTRUE) '-f $(BACKUP)' 'echo $(BACKUP) exists! ; exit 1'
	tar cf $(BACKUP) $(LIST)

restore:
	$(IFTRUE) '! -f $(BACKUP)' 'echo No $(BACKUP)! ; exit 1'
	tar vxf $(BACKUP) $(LIST)

config.data:		config.dist
	$(IFTRUE) '-f config.new' 'echo config.new exists! ; exit 1'
	$(IFTRUE) '-f config.old' 'echo config.old exists! ; exit 1'
	$(IFTRUE) '! -f config.data' '$(MAKE) CLOBBER=YES SCRATCH'
	cat config.data config.dist >subst.tmp
	cp config.dist config.new
	./subst $(SED) -f subst.tmp config.new
	-@rm -f subst.tmp
	@echo "Update config.old from config.new!"
	$(IFTRUE) '-f config.data' \
	 'echo "Renaming config file to config.old"; mv config.data config.old'
	exit 1 ;
SCRATCH:
	$(IFTRUE) '-z $(CLOBBER)' 'echo No way! ; exit 1'
	@echo 'You do not have a config.data file.'
	@echo 'Setting one up using the default file.'
	@echo 'You must edit this file, then remove config.new and config.old.'
	@echo 'Then restart the make.'
	cat config.dist >config.data

subst:		subst.c subst.sh
	-$(MAKE) c || $(MAKE) sh || rm -f subst
	$(IFTRUE) '! -f subst' 'echo Cannot make subst ; exit 1'
c:
	@rm -f subst
	$(CC) -o subst subst.c
sh:
	@rm -f subst
	cp subst.sh subst
	chmod +x subst

sedtest:
	@echo 'This line should not be touched' >sedtest
	@echo '=()<Sed seems OK:  @<NEWSMASTER>@>()=' >>sedtest
	@echo 'ERROR:  do not use the SED version of subst' >>sedtest
	-$(SHELL) ./subst.sh $(SED) -f $(CONF) sedtest
	-@grep ERROR: sedtest
	$(IFTRUE) '! -s sedtest' 'echo Do not use sed ; rm sedtest ; exit 1'
	-@grep OK: sedtest
	rm -f sedtest


.PRECIOUS:	$(CONF) files.list

##  Dependencies.  Default list, below, is probably good enough.
depend:		Makefile $(SOURCES)
	makedepend $(DEFS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.
