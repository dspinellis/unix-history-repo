#
#       Makefile for UTREE 3.03-um
#       klin, Sun Feb 16 16:35:52 1992
#
#       Steps to create an utree executable:
#       - First check src/Makefile if all definitions are correct for
#         your system. There are some Makefiles for SYSV.2, SYSV.3 and
#         BSD. Check and change the correct Makefile for your system and
#         copy it to Makefile.
#       - Then check src/conf.h if all definitions are correct for
#         your system. There are some conf.h for SYSV.2, SYSV.3 and
#         BSD. Check and change the correct conf.h for your system and
#         copy it to conf.h.
#       - Call make or make utree to create utree. The executable
#         is written into the bin directory. Test utree.
#
#       Steps to create utree man page:
#       - Check all manual definitions in this Makefile and change them if
#         needed.
#       - Call make man to create the utree manual page if your
#         systems needs formatted and/or packed manual pages.
#
#       Steps to install utree:
#       - Check all install definitions in this Makefile and change them if
#         needed.
#       - Call make install to install utree executables and manual
#         page (you must be super user to do this).
#

# --------------------------------------------------------------------- #

# Utree executables
UTREE=  utree           # The utree utility
UTPRL=  utree.prlist    # The tree list formatter

# Utree support shell scripts
UHELP=  utree.help      # Utree help pages
UBACK=  utree.backup    # Utree backup shell script
ULIST=  utree.mklist    # Create directory lists for quicker startup

UTMAN=  utree.1         # Utree man page
UPMAN=  utree.prlist.1  # Utree.prlist man page

# --------------------------------------------------------------------- #

# Create utree executable

all:    $(UTREE) $(UTPRL)

# Compile and link utree executables
$(UTREE):
	@echo "Make $(UTREE)"
	cd src; make $@

$(UTPRL):
	@echo "Make $(UTPRL)"
	cd src; make $@

# Strip off symbols
strip:
	@echo "Make stripped $(UTREE)"
	cd src; make $@

# --------------------------------------------------------------------- #

# Utree manual pages. Edit and change the following lines if needed!

# Commands needed for utree manual page
TBL=    tbl
NROFF=  nroff
CP=     cp
RM=     rm
PACK=   pack
COMP=   compress

man:
# Unformatted manual pages
	$(CP)   doc/utree.m        doc/$(UTMAN).1
	$(CP)   doc/utree.prlist.m doc/$(UPMAN).1
# Formatted manual pages
#        $(TBL)  doc/utree.m        | $(NROFF) -man > doc/$(UTMAN)
#        $(TBL)  doc/utree.prlist.m | $(NROFF) -man > doc/$(UPMAN)
# Packed formatted manual pages
#         $(RM)   -f doc/$(UTMAN).z doc/$(UPMAN).z
#         $(PACK) doc/$(UTMAN) doc/$(UPMAN)
# Compressed formatted manual pages
#        $(RM)   -f doc/$(UTMAN).Z doc/$(UPMAN).Z
#        $(COMP) doc/$(UTMAN) doc/$(UPMAN)

# --------------------------------------------------------------------- #

# Install utree. Change the following lines if needed!
# WARNING: You must be super user to do this!

# Bin, lib and man directories for utree files
BINDIR= /usr/local/bin
LIBDIR= /usr/local/lib
MANDIR= /usr/local/man/man1
#MANDIR= /usr/local/man/cat1

# Commands needed for installation of utree files
CHMOD=  chmod
CHOWN=  chown
CHGRP=  chgrp

# Access modes and owner/group of utree binaries, help and man pages
XMODE=  0755
RMODE=  0644
OWNER=  bin
GROUP=  bin

# Sorry, not all UNIXes have the (BSD) install utility!
install:
	@echo "Install $(UTREE)"
	$(CP)    bin/$(UTREE) $(BINDIR)
	$(CHMOD) $(XMODE)     $(BINDIR)/$(UTREE)
	$(CHOWN) $(OWNER)     $(BINDIR)/$(UTREE)
	$(CHGRP) $(GROUP)     $(BINDIR)/$(UTREE)
	@echo "Install $(UTPRL)"
	$(CP)    bin/$(UTPRL) $(BINDIR)
	$(CHMOD) $(XMODE)     $(BINDIR)/$(UTPRL)
	$(CHOWN) $(OWNER)     $(BINDIR)/$(UTPRL)
	$(CHGRP) $(GROUP)     $(BINDIR)/$(UTPRL)
	@echo "Install $(UHELP) in $(LIBDIR)"
	$(CP)    lib/$(UHELP) $(LIBDIR)
	$(CHMOD) $(RMODE)     $(LIBDIR)/$(UHELP)
	$(CHOWN) $(OWNER)     $(LIBDIR)/$(UHELP)
	$(CHGRP) $(GROUP)     $(LIBDIR)/$(UHELP)
	@echo "Install $(UBACK) in $(BINDIR)"
	$(CP)    bin/$(UBACK) $(BINDIR)
	$(CHMOD) $(XMODE)     $(BINDIR)/$(UBACK)
	$(CHOWN) $(OWNER)     $(BINDIR)/$(UBACK)
	$(CHGRP) $(GROUP)     $(BINDIR)/$(UBACK)
	@echo "Install $(ULIST) in $(BINDIR)"
	$(CP)    bin/$(ULIST) $(BINDIR)
	$(CHMOD) $(XMODE)     $(BINDIR)/$(ULIST)
	$(CHOWN) $(OWNER)     $(BINDIR)/$(ULIST)
	$(CHGRP) $(GROUP)     $(BINDIR)/$(ULIST)
	@echo "Install $(UTMAN) $(UPMAN) in $(MANDIR)"
	$(CP)    doc/$(UTMAN) doc/$(UPMAN) $(MANDIR)
	$(CHMOD) $(RMODE)    $(MANDIR)/$(UTMAN) $(MANDIR)/$(UPMAN)
	$(CHOWN) $(OWNER)    $(MANDIR)/$(UTMAN) $(MANDIR)/$(UPMAN)
	$(CHGRP) $(GROUP)    $(MANDIR)/$(UTMAN) $(MANDIR)/$(UPMAN)

# --------------------------------------------------------------------- #
