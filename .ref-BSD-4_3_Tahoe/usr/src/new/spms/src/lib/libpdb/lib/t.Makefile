CFLAGS	      = -I../../../../include

LDFLAGS	      =

LIBS	      = ../../../../lib/libpdb.a \
		../../../../lib/libspms.a

LINKER	      = cc

OBJS	      =

PROGRAM	      =

$(PROGRAM):     $(OBJS) $(LIBS)
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
