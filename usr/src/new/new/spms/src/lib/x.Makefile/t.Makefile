LDFLAGS	      =

LIBS	      =

LINKER	      = cc

OBJS	      =

PROGRAM	      =

$(PROGRAM):     $(OBJS) $(LIBS)
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
