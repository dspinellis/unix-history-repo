######################################################################
#
#	Makefile for Sendmail UCB configuration files
#
#		@(#)Makefile	3.18		2/9/83
#
######################################################################


CSSRCS=	csbase.m4 \
	arpa.mc \
	cad.mc \
	calder.mc \
	dali.mc \
	ernie.mc \
	ic.mc \
	icw.mc \
	ingres.mc \
	kim.mc \
	matisse.mc \
	medea.mc \
	miro.mc \
	monet.mc \
	oz.mc \
	statvax.mc \
	turner.mc \
	ucbvax.mc
CCSRCS=	ccbase.m4 \
	amber.mc \
	coral.mc \
	ivory.mc \
	lapis.mc \
	jade.mc \
	ruby.mc \
	topaz.mc
UWSRCS=	dbvax.mc
GENSRCS=arpaproto.mc \
	base.m4 \
	berkhosts.m4 \
	berkm.m4 \
	cchosts.m4 \
	compat.m4 \
	ether.m4 \
	etherm.m4 \
	fudge.m4 \
	ibmm.m4 \
	localm.m4 \
	mmdfm.m4 \
	ncphosts.m4 \
	ncpm.m4 \
	tcpm.m4 \
	uucpm.m4 \
	uucpproto.mc \
	version.m4 \
	zerobase.m4
SRCS=	$(GENSRCS) $(CSSRCS) $(CCSRCS) $(UWSRCS)

CSNALL=	arpa.cf \
	cad.cf \
	calder.cf \
	dali.cf \
	ic.cf \
	icw.cf \
	ingres.cf \
	matisse.cf \
	miro.cf \
	monet.cf \
	turner.cf \
	ucbvax.cf
CSOALL=	ernie.cf \
	esvax.cf \
	kim.cf \
	medea.cf \
	oz.cf \
	statvax.cf
CSALL=	$(CSNALL) $(CSOALL)
CCALL=	amber.cf \
	coral.cf \
	ivory.cf \
	jade.cf \
	lapis.cf \
	ruby.cf \
	topaz.cf
UWALL=	dbvax.cf
PROTOS=	arpaproto.cf uucpproto.cf
ALL=	$(CSALL) $(CCALL) $(UWALL) $(PROTOS)

GET=	sccs get

.SUFFIXES: .mc .cf

.mc.cf:
	m4 $*.mc > $*.cf

all: $(ALL)

############################
#   special dependencies   #
############################

# all computer science department hosts....
$(CSALL): csbase.m4

# all computer center hosts....
$(CCALL): ccbase.m4

# pessimize -- basic dependencies
$(ALL): ether.m4 etherm.m4 base.m4 localm.m4 tcpm.m4 version.m4 zerobase.m4

# hosts using UUCP
arpa.cf ucbvax.cf uucpproto.cf: uucpm.m4 compat.m4 berkhosts.m4

# hosts using MMDF
arpa.cf: mmdfm.m4

# hosts running delivermail also
$(CSOALL): fudge.m4 compat.m4 berkhosts.m4

# hosts knowing about berknet
jade.cf ucbvax.cf: cchosts.m4 berkm.m4 compat.m4 berkhosts.m4

# hosts knowing about the arpanet
ucbvax.cf: ncphosts.m4

# hosts running ncp directly
dbvax.cf: ncpm.m4

#####################
#   administrivia   #
#####################

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

clean:
	rm -f $(ALL) a.out core make.out
	rm -f ,*
