# Makefile for "less"
# Generated Sun Aug 16 09:47:26 PDT 1992 by linstall.
#
# Invoked as:
#	make all
#   or	make install
# Plain "make" is equivalent to "make all".
#
# If you add or delete functions, remake funcs.h by doing:
#	make newfuncs
# This depends on the coding convention of function headers looking like:
#	" \t public <function-type> \n <function-name> ( ... ) "
#
# Also provided:
#	make lint	# Runs "lint" on all the sources.
#	make clean	# Removes "less" and the .o files.
#	make clobber	# Pretty much the same as make "clean".

SHELL = /bin/sh


##########################################################################
# Compilation environment.
##########################################################################

# LIBS is the list of libraries needed.
LIBS = -ltermcap

# INSTALL_LESS is a list of the public versions of less.
# INSTALL_KEY is a list of the public versions of lesskey.
# INSTALL_HELP is a list of the public version of the help file.
# INSTALL_LESSMAN is a list of the public versions of the less manual page.
# INSTALL_KEYMAN is a list of the public versions of the lesskey manual page.
INSTALL_LESS =		$(ROOT)/usr/local/bin/less
INSTALL_KEY =		$(ROOT)/usr/local/bin/lesskey
INSTALL_HELP =		$(ROOT)/usr/local/bin/less.hlp
INSTALL_LESSMAN =	$(ROOT)/usr/local/man/man1/less.1
INSTALL_KEYMAN =	$(ROOT)/usr/local/man/man1/lesskey.1
LESS_MANUAL =		less.nro
KEY_MANUAL =		lesskey.nro
HELPFILE =		/usr/local/bin/less.hlp


# OPTIM is passed to the compiler and the loader.
# It is normally "-O" but may be, for example, "-g".
OPTIM = -O

CFLAGS = $(OPTIM)



##########################################################################
# Files
##########################################################################

SRC1 =	ch.c cmdbuf.c command.c decode.c help.c input.c 
SRC2 =	line.c linenum.c main.c edit.c option.c optfunc.c \
	opttbl.c os.c 
SRC3 =	charset.c filename.c lsystem.c output.c position.c ifile.c \
	brac.c forwback.c jump.c search.c 
SRC4 =	mark.c prompt.c screen.c signal.c tags.c ttyin.c version.c

SRC =	$(SRC1) $(SRC2) $(SRC3) $(SRC4)

OBJ =	brac.o ch.o charset.o cmdbuf.o command.o decode.o edit.o filename.o \
	forwback.o help.o input.o jump.o line.o linenum.o \
	lsystem.o main.o option.o optfunc.o opttbl.o os.o \
	output.o position.o mark.o ifile.o prompt.o screen.o \
	search.o signal.o tags.o ttyin.o version.o


##########################################################################
# Rules for building stuff
##########################################################################

all: less lesskey
install: install_less install_help install_key install_lman install_kman

less: $(OBJ)
	$(CC) $(LDFLAGS) $(OPTIM) -o less $(OBJ) $(LIBS) $(LDLIBS)

lesskey: lesskey.o
	$(CC) $(LDFLAGS) $(OPTIM) -o lesskey lesskey.o $(LDLIBS)

install_less: less
	for f in $(INSTALL_LESS); do  rm -f $$f; cp less $$f;  done
	touch install_less

install_key: lesskey
	for f in $(INSTALL_KEY); do  rm -f $$f; cp lesskey $$f;  done
	touch install_key

install_help: less.hlp
	for f in $(INSTALL_HELP); do  rm -f $$f; cp less.hlp $$f;  done
	touch install_help

install_lman: $(LESS_MANUAL) 
	for f in $(INSTALL_LESSMAN); do  rm -f $$f; cp $(LESS_MANUAL) $$f;  done
	touch install_lman

install_kman: $(KEY_MANUAL)
	for f in $(INSTALL_KEYMAN); do  rm -f $$f; cp $(KEY_MANUAL) $$f;  done
	touch install_kman

##########################################################################
# Maintenance
##########################################################################

lint:
	lint -hp $(SRC)

newfuncs funcs.h:
	if [ -f funcs.h ]; then mv funcs.h funcs.h.OLD; fi
	awk -f mkfuncs.awk $(SRC) >funcs.h

clean:
	rm -f $(OBJ) lesskey.o less lesskey vecho

clobber:
	rm -f *.o less lesskey vecho install_less install_key \
		install_help install_lman install_kman

shar:
	shar -v README CHANGES linstall \
		less.nro lesskey.nro \
		vecho.c mkfuncs.awk > less1.shr
	shar -v less.man lesskey.man \
		less.h position.h cmd.h option.h > less2.shr 
	shar -v lesskey.c $(SRC1) > less3.shr
	shar -v $(SRC2) > less4.shr
	shar -v $(SRC3) less.hlp > less5.shr
	shar -v $(SRC4) funcs.h > less6.shr


##########################################################################
# Dependencies
##########################################################################

$(OBJ): less.h funcs.h defines.h position.h
command.o decode.o: cmd.h
option.o opttbl.o optfunc.o: option.h

lesskey.o: less.h funcs.h defines.h cmd.h

