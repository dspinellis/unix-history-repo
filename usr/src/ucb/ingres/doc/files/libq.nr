.th LIBQ FILES
.sh NAME
libq \- Equel run-time support library
.sh DESCRIPTION
.it Libq is the equel run-time support library containing
all the routines necessary for an equel program to load.
It typically resides in
.bd /usr/lib/libq.a,
and must be specified when loading
equel pre-processed object code.
It may be referenced on the
command line of 
.it cc
by the abbreviation 
.bd \-lq.
.s3
Several useful routines 
which are used by equel processes are included in the library.
These may be employed by the equel programmer to avoid code
duplication.
They are:
.ne 20
.nf

int	IIatoi(buf, i)
char	*buf;
int	i;

char	*IIbmove(source, destination, len)
char	*source, *destination;
int	len;

char	*IIconcatv(buf, arg1, arg2, ..., 0)
char	*buf, *arg1, ...;

char	*IIitos(i)
int	i;

int	IIsequal(s1, s2)
char	*s1, *s2;

int	IIlength(string)
char	*string;

IIsyserr(string, arg1, arg2, ...);
char	*string;

.fi
.in +5
.de xx
.s3
.lp +12 12
\\$1\t\c
..
.xx IIatoi
IIatoi is equivalent to atoi(UTIL).
.xx IIbmove
Moves
.it len
bytes from
.it source
to 
.it destination\c
\&, returning a pointer to the location after the last byte moved.
Does not append a null byte.
.xx IIconcatv
Concatenates into 
.it buf
all of its arguments, returning a pointer to 
the null byte at the end of the concatenation.
.it Buf
may not be equal to any of the arg-n but arg1.
.xx IIitos
IIitos is equivalent to itoa(III).
.xx IIsequal
Returns 1 iff strings s1 is identical to s2.
.xx IIlength
Returns max(length of
.it string
without null byte at end, 255)
.xx IIsyserr
IIsyserr is diferrent from syserr(util) only in that
it will print the name in IIproc_name, and in that there is no 0 mode.
Also, it will always call exit(\*-1) after printing the error message.
.i0
.s3
There are also some global Equel variables which may be manipulated by the
user:
.ne 5
.nf

int	IIerrflag;
char	*IImainpr;
char	(*IIprint_err)();
int	IIret_err();
int	IIno_err();

.fi
.in +5
.xx IIerrflag
Set on an error from \*(II to be the error number (see
the error message section of the ``\*(II Reference Manual'') that ocurred.
This remains valid from the time the error occurrs to the time when
the next equel statement is issued.
This may be used just after an equel statement to see if it succeded.
.xx IImainpr
This is a string which determines which ingres to call when a 
"## ingres" is issued. Initially it is "/usr/bin/ingres".
.xx IIprint_err
This function pointer is used to call a function which determines 
what (if any) error message should be printed when an ingres error
occurs. It is called from IIerror() with the error number as an
argument, and the error message corresponding to the error number
returned will be printed.
If (*IIprint_err)(\c
.it <errno>\c
) returns 0, then no error message will be printed.
Initially IIprint_err is set to IIret_err() to print the error
that ocurred.
.xx IIret_err
Returns its single integer argument. Used to have (*IIprint_err)()
cause printing of the error that ocurred.
.xx IIno_err
Returns 0. Used to have (*IIprint_err)() suppress error message printing.
IIno_err is used when an error in a parametrized equel statemenr occurs
to suppress printing of the corresponding parser error.
.sh "SEE ALSO"
atoi(util),
bmove(util),
cc(I),
equel(unix),
exit(II),
itoa(III),
length(util),
sequal(util),
syserr(util)
