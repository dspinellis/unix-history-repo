#	@(#)Makefile	5.3 (Berkeley) 6/28/90
#
# TERMCAP	Define this if your system is termcap based,
#		otherwise a terminfo based system is assumed.
#
# SRCRT		Includes code to allow you to specify source routes.
#		Format is:
#			[!]@hop1@hop2...[@|:]dst
#		Leading ! means strict source route.
#
# USE_TERMIO	Define this if you have System V termio structures.
#		What is here is how things are on Cray computers.
#
# KLUDGELINEMODE Define this to get the kludged up version of linemode
#		that was in 4.3BSD.  This is a good thing to have
#		around for talking to older systems.
#
# NOSTRNCASECMP	Define this if you do not have strncasecmp() in
#		your C libarary.
#
# strrchr=rindex Define this if you do not have strrchr() in your
#		C libaray (but you do have rindex...)
#
PROG=	telnet
CFLAGS+=-DTERMCAP -DSRCRT -DKLUDGELINEMODE -DUSE_TERMIO -DNEED_GETTOS
#CFLAGS+=-DTERMCAP -DSRCRT -DKLUDGELINEMODE -DUSE_TERMIO -DNEED_GETTOS -DKERBEROS
LDADD=	-ltermcap
#LDADD=	-ltermcap -ldes -lkrb
DPADD=	${LIBTERMCAP}

SRCS=	commands.c main.c network.c ring.c sys_bsd.c telnet.c terminal.c \
	tn3270.c utilities.c

.include <bsd.prog.mk>
