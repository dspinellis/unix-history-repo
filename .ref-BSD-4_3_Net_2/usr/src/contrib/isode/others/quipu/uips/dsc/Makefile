###############################################
# Configuration - see READ-ME for more details #
###############################################

# the distinguished name of the "local" country (used by easy interface)
CDN=c=GB

# the distinguished name of the "local" organisation (used by the easy
# interface)
ORGRDN=o=University College London

# the directory where the easy interface deposits some logs.  
LOGPLACE=$(LOGDIR)dsc

# the directory where the dish binaries and sd may be found
LOCALPATH=$(BINDIR)

# the short form name of your organisation - this appears in some of the
# help text
SITESHORTNAME=UCL

# an "english" equivalent of the ORGDN
SITELONGNAME=GB, University College London

# if you want the interface to bind as a user, specify the name of the user
# here.  This is useful as it enables monitoring of the directory using this
# interface.  
# BINDARG=
BINDARG=-u \\"@c=gb@o=university college london@cn=dir@cn=public access dua\\"

# Dou you want greybook order mailboxes?  Anything other than "yes" yields
# rfc822 order
EMAILUKORDER=no

#####################################
# Rules...
#####################################

all:	dsc

inst-all: $(LOGPLACE)
	cp dsc $(LOCALPATH)

install: inst-all clean

lint:;

clean:;	-rm -f dsc

grind:;

dsc: dsc.dist1 localphone dsc.dist2 localmess dsc.dist3
	cat dsc.dist1 localphone dsc.dist2 localmess dsc.dist3 > dsc.dist
	sed 's#DISTORGRDN#$(ORGRDN)#g' dsc.dist |\
	sed 's#DISTLOGPLACE#$(LOGPLACE)#g' |\
	sed 's#DISTLOCPATH#$(LOCALPATH)#g' |\
	sed 's#DISTSITESHORT#$(SITESHORTNAME)#g' |\
	sed 's#DISTSITELONG#$(SITELONGNAME)#g' |\
	sed 's#DISTBINDARG#$(BINDARG)#g' |\
	sed 's#DISTUKORDER#$(EMAILUKORDER)#g' >dsc
	chmod +x dsc

$(LOGPLACE):
	-mkdir $(LOGPLACE)

