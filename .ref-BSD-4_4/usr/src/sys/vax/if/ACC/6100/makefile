######################################################################
#                                                                    #
#                                                                    #
#   Copyright (c) 1986 by Advanced Computer Communications           #
#   720 Santa Barbara Street, Santa Barbara, California 93101        #
#   (805) 963-9431                                                   #
#                                                                    #
#  filename:  makefile                                               #
#  author:    Clare E. Russ, ACC Santa Barbara                       #
#  purpose:   create executable image of 'acpconfig' configuration   #
#             program to run under UNIX (BSD, TWG, ULTRIX)           #
#                                                                    #
#  use:                                                              #
#        By default, this makefile builds an executable image,       #
#        acpconfig, for a BSD or ULTRIX  system.  If the TWG world   #
#        is desired, it must be defined explicitly in the make       #
#        command line.						     #
#                                                                    #
#           make            (make acpconfig for BSD or ULTRIX)	     #
#           make bsd        (make acpconfig for BSD or ULTRIX)	     #
#           make twg        (make acpconfig for TWG under VAX/VMS)   #
#           make ultrix     (same as "make bsd", now)		     #
#           make clean      (rm -f acpconfig)                        #
#           make print      (print -nh acpconfig.c)                  #
#                                                                    #
#  date:      V0.6    26 June 1986 Clare Russ                        #
#             add ULTRIX V1.2 and ULTRIX V2.0 support to account     #
#             for differences in the internet address data structure #
#             in_addr (see /sys/netinet/in.h)                        #
#                                                                    #
#	      V0.7    16 March 1987 Jeff Berkowitz		     #
#	      Correct the V0.6 change; there is no reason to have an #
#	      ifdef for ULTRIX versus BSD.			     #
#                                                                    #
######################################################################
#
#
#
CFLAGS	=	-O
NAME	=	acpconfig
PRINT	=	print -nh
RM	=	rm -f
#
#  The COMMON_FILES are included regardless of the target environment.
#
COMMON_FILES	=	acpconfig.c \
			/usr/include/sys/types.h \
			/usr/include/sys/socket.h \
			/usr/include/sys/ioctl.h \
			/usr/include/stdio.h \
			/usr/include/errno.h \
			/usr/include/ctype.h \
			/usr/include/netdb.h
#
#  The BSD_FILES are included for the BSD and ULTRIX environments.
#
BSD_FILES	=	/usr/include/netinet/in.h \
			/usr/include/net/if.h
#
#  The TWG_FILES are only included for the TWG environment.
#
TWG_FILES	=	/sys/twgtcp/kernel/netinet/in.h \
			/sys/twgtcp/kernel/net/if.h


#
#  Compile an executable image for BSD (4.2 or 4.3) or ULTRIX (1.2 or 2.0)
#
bsd:	${COMMON_FILES} ${BSD_FILES}
	cc ${CFLAGS} -o ${NAME} ${NAME}.c


#
#  This entry exists so we don't have to change the manual, which says
#  to type "make ultrix"
#
ultrix:	${COMMON_FILES} ${BSD_FILES}
	make bsd



#
#  Compile an executable image for TWG.  Note the -DTWG flag.
#
twg:	${COMMON_FILES} ${TWG_FILES}
	cc -DTWG ${CFLAGS} -o ${NAME} ${NAME}.c

clean:
	${RM} ${NAME}

print:
	${PRINT} ${NAME}.c
