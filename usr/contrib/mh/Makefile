##############################################################################
#	Instructions to Make, for compilation of MH modules
##############################################################################

MAKE	=	make DESTDIR=$(DESTDIR) $(MFLAGS) -k
SHELL	=	/bin/sh

# Targets to Make:
#    
#    all:	generate MH system, assumes configuration previously done
#
#    depend:	generate dependencies, for MH master maintainers only!
#
#    lint:	verify code, for MH master maintainers only!
#
#    compat-v7:	verify V7 restrictions, for MH master maintainers only!
#
#    install:	install system and clean up afterwards
#
#    inst-all:	install system
#
#    tar:	generate tar commands to move targets to other systems
#		    e.g., make TFLAGS="cf mh.tar" tar
#    
#    uninstall:	undo installation
#    
#    distribution:
#		revert source tree to MH distribution
#    
#    checkout:	MH maintainers only!
#
#    clean:	remove all objects and targets plus clean up
#    
#    unclean:	remove scratch files and backups


##############################################################################
#	Generate MH system
##############################################################################

ALL	=	config sbr mts zotnet uip support doc

all:;		for d in $(ALL); do (cd $$d; $(MAKE) all); done

depend:;	for d in $(ALL); do (cd $$d; $(MAKE) depend); done

lint:;		for d in $(ALL); do (cd $$d; $(MAKE) lint); done

compat-v7:	all
		miscellany/tools/7limit */x* */*/x*
		miscellany/tools/8limit */*.o */*/*.o


INSTALL	=	zotnet support uip doc

install:	inst-all

inst-all:;
		-mkdir /usr/new/mh /usr/new/lib/mh
		for d in $(INSTALL); do (cd $$d; $(MAKE) inst-all); done

tar:;		@for d in $(INSTALL); \
		    do (cd $$d; $(MAKE) TFLAGS="$(TFLAGS)" tar); done

uninstall:;	for d in $(INSTALL); do (cd $$d; $(MAKE) uninstall); done


##############################################################################
#	Miscellaneous tasks
##############################################################################

DISTRIBUTION=	conf config sbr mts zotnet support uip doc papers dist

distribution:;	rm -f _* :*
		for d in $(DISTRIBUTION); \
		    do (cd $$d; $(MAKE) distribution); done

checkout:	unclean lint compat-v7
		find . \( -name ":*" -o -name "_*" -o -name "#*" \) -a -print

clean:;		rm -f _* :* core
		for d in $(DISTRIBUTION); do (cd $$d; $(MAKE) clean); done

unclean:;	rm -f _* :* core
		for d in $(DISTRIBUTION); do (cd $$d; $(MAKE) unclean); done
