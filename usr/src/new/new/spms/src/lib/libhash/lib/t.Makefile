CFLAGS	      = -I../../../../include

LDFLAGS	      =

LIBS	      = ../../../../lib/libhash.a \
		../../../../lib/libspms.a

LINKER	      = cc

OBJS	      =

PROGRAM	      =

$(PROGRAM):     $(OBJS) $(LIBS)
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
