#!/bin/sh
# Output RCS compile-time configuration.
Id='$Id: conf.sh,v 5.14 1991/11/20 18:21:10 eggert Exp $'
#	Copyright 1990, 1991 by Paul Eggert
#	Distributed under license by the Free Software Foundation, Inc.

# This file is part of RCS.
#
# RCS is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# RCS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RCS; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#
# Report problems and direct all questions to:
#
#     rcs-bugs@cs.purdue.edu


# Standard output should already be directed to "a.h";
# later parts of this procedure need it.
# Standard error can be ignored if a.h is OK,
# and can be inspected for clues otherwise.

# The Makefile overrides the following defaults.
: ${CC=cc}
: ${CFLAGS=-O}
: ${COMPAT2=0}
: ${DIFF3=${RCSPREFIX}diff3}
: ${DIFF3_BIN=1}
: ${DIFF=${RCSPREFIX}diff}
: ${DIFF_FLAGS=-an}
: ${DIFF_L=1}
: ${DIFF_SUCCESS=0} ${DIFF_FAILURE=1} ${DIFF_TROUBLE=2}
: ${ED=/bin/ed}
: ${RCSPREFIX=/usr/local/bin/}
: ${SENDMAIL='"/usr/lib/sendmail"'}
# : ${LDFLAGS=} ${LDLIBS=} tickles old shell bug

C="$CC $CFLAGS"
CL="$CC $CFLAGS $LDFLAGS"
L=$LDLIBS
RM='rm -f a.out'

cat <<EOF
/* RCS compile-time configuration */

	/* $Id */

/*
 * This file is generated automatically.
 * If you edit it by hand your changes may be lost.
 * Instead, please try to fix conf.sh,
 * and send your fixes to rcs-bugs@cs.purdue.edu.
 */

EOF

: exitmain
cat >a.c <<EOF
#include "a.h"
int main(argc,argv) int argc; char **argv; { return argc-1; }
EOF
$RM && $CL a.c $L >&2 || exit
e='exit(n), 3 /* lint fodder */'
if ./a.out -
then :
elif ./a.out
then e=n
fi
echo "#define exitmain(n) return $e /* how to exit from main() */"

: _POSIX_SOURCE
cat >a.c <<'EOF'
#include "a.h"
#include <stdio.h>
int main() { exitmain(fileno(stdout) < 0); }
EOF
a='/* ' z='*/ '
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then :
elif $RM || exit; ($CL -D_POSIX_SOURCE a.c $L && ./a.out) >&2
then a= z=
fi
cat <<EOF
$a#define _POSIX_SOURCE $z/* Define this if Posix + strict Standard C.  */

#include <errno.h>
#include <stdio.h>
#include <time.h>
EOF

cat <<'EOF'

/* Comment out #include lines below that do not work.  */
EOF

# Run `$CS a.c $LS' instead of `$CL a.c $L' for compile-time checking only.
# This speeds up the configuration process.
if $C -S a.c >&2
then CS="$C -S" LS=	# Generate assembly language output.
elif $C -c a.c >&2
then CS="$C -c" LS=	# Generate object code.
else CS=$CL LS=$L	# Generate an executable.
fi

# standard include files
# sys/types.h and sys/stat.h must come first because others depend on them.
has_signal=1
for h in \
	sys/types sys/stat \
	dirent fcntl limits pwd signal stdlib string sys/mman sys/wait unistd utime vfork
do
	i="#include <$h.h>"
	: $i
	cat >a.c <<EOF
#include "a.h"
$i
int main(){ exitmain(0); }
EOF
	$RM || exit
	($CL a.c $L && ./a.out) >&2 || {
		case $h in
		string)
			i='#include <strings.h>';;
		*)
			i="/* $i */"
		esac
		case $h in
		signal) has_signal=0
		esac
	}
	echo "$i"
done

cat <<'EOF'

/* Define the following symbols to be 1 or 0.  */
EOF

# has_sys_*_h
for H in dir param
do
	: has_sys_${H}_h
	cat >a.c <<EOF
#include "a.h"
#include <sys/$H.h>
int main() { exitmain(0); }
EOF
	$RM || exit
	if ($CL a.c $L && ./a.out) >&2
	then h=1
	else h=0
	fi
	echo "#define has_sys_${H}_h $h /* Does #include <sys/$H.h> work?  */"
done


# We must do NAME_MAX and has_readlink next, because they might generate
# #include directives that affect later definitions.

: NAME_MAX
cat >a.c <<'EOF'
#include "a.h"
char b[NAME_MAX + 2];
main()
{
#if !defined(NAME_MAX)
	exitmain(1);
#else
	int i;
	b[0] = 'a'; b[1] = '.';
	for (i = 2;  i < NAME_MAX;  i++)
		b[i] = 'a';
	b[i] = 'b';
	exitmain(creat(b, 0) < 0);
#endif
}
EOF
$RM a.*ab || exit
if $CL a.c $L >&2 && ./a.out && test -f a.*ab
then a= z=
else a='/* ' z='*/ '
fi
rm -f a.*ab || exit

: has_readlink
cat >a.c <<'EOF'
#include "a.h"
char b[7];
int main()
{
	exitmain(readlink("a.sym2",b,7) != 6  ||  strcmp(b, "a.sym1") != 0);
}
EOF
$RM a.sym* || exit
if (ln -s a.sym1 a.sym2 && $CL a.c $L && ./a.out) >&2
then h=1
else h=0
fi
cat <<EOF
#define has_readlink $h /* Does readlink() work?  */

$a#undef NAME_MAX $z/* Uncomment this if NAME_MAX is broken.  */

#if !defined(NAME_MAX) && !defined(_POSIX_NAME_MAX)
#	if has_sys_dir_h
#		include <sys/dir.h>
#	endif
#	ifndef NAME_MAX
#		ifndef MAXNAMLEN
#			define MAXNAMLEN 14
#		endif
#		define NAME_MAX MAXNAMLEN
#	endif
#endif
#if !defined(PATH_MAX) && !defined(_POSIX_PATH_MAX)
#	if has_sys_param_h
#		include <sys/param.h>
#		define included_sys_param_h 1
#	endif
#	ifndef PATH_MAX
#		ifndef MAXPATHLEN
#			define MAXPATHLEN 1024
#		endif
#		define PATH_MAX (MAXPATHLEN-1)
#	endif
#endif
#if has_readlink && !defined(MAXSYMLINKS)
#	if has_sys_param_h && !included_sys_param_h
#		include <sys/param.h>
#	endif
#	ifndef MAXSYMLINKS
#		define MAXSYMLINKS 20 /* BSD; not standard yet */
#	endif
#endif
EOF

cat <<'EOF'

/* Comment out the keyword definitions below if the keywords work.  */
EOF

: const, volatile
for i in const volatile
do
	cat >a.c <<EOF
#	include "a.h"
	enum Boolean { false, true };
	static enum Boolean $i zero;
	static enum Boolean $i * $i azero = &zero;
	static enum Boolean $i * $i * $i aazero = &azero;
	int main() { exitmain(!!**aazero); }
EOF
	a= z=
	if $CS a.c $LS >&2
	then
		cat >a.c <<EOF
			typedef unsigned char $i *Iptr_type;
			struct { Iptr_type lim; } s, *f = &s;
			int main() {
				Iptr_type lim;
				lim = f->lim;
				return !!lim;
			}
EOF
		if $CS a.c $LS >&2
		then a='/* ' z=' */'
		fi
	fi
	echo "$a#define $i$z"
done

# *_t
cat <<'EOF'

/* Comment out the typedefs below if the types are already declared.  */
/* Fix any uncommented typedefs that are wrong.  */
EOF
cat >a.c <<'EOF'
#include "a.h"
t x;
int main() { exitmain(0); }
EOF
for t in mode_t pid_t sig_atomic_t size_t ssize_t time_t uid_t
do
	: $t
	case $t in
	size_t) i=unsigned;;
	time_t) i=long;;
	*) i=int;;
	esac
	if $CS -Dt=$t a.c $LS >&2
	then a='/* ' z=' */'
	else a= z=
	fi
	echo "${a}typedef $i $t;$z"
done

: has_prototypes, has_stdarg, has_varargs, va_start_args
cat >a.ha <<'EOF'
#if has_prototypes
#	define P(params) params
#else
#	define P(params) ()
#endif
#if has_stdarg
#	include <stdarg.h>
#else
#	if has_varargs
#		include <varargs.h>
#	else
		typedef char *va_list;
#		define va_dcl int va_alist;
#		define va_start(ap) ((ap) = (va_list)&va_alist)
#		define va_arg(ap,t) (((t*) ((ap)+=sizeof(t)))  [-1])
#		define va_end(ap)
#	endif
#endif
#if va_start_args == 2
#	define vararg_start va_start
#else
#	define vararg_start(ap,p) va_start(ap)
#endif
EOF
cat >a.c <<'EOF'
#include "a.h"
#include "a.ha"
#if has_prototypes
char *f(char **p, ...)
#else
char *f(p, va_alist) char **p; va_dcl
#endif
{
	char *s;
	va_list v;
	vararg_start(v,p);
	s = p[va_arg(v,int)];
	va_end(v);
	return s;
}
int main P((int, char**));
int main(argc, argv) int argc; char **argv; {
	exitmain(f(argv,0) != argv[0]  ||  f(argv,1) != argv[1]);
}
EOF
for has_prototypes in 1 0
do
	for has_stdarg in 1 v 0
	do
		case $has_stdarg in
		v) has_varargs=1 has_stdarg=0;;
		*) has_varargs=0
		esac
		case $has_stdarg in
		0) as='1 2';;
		1) as='2 1'
		esac
		for va_start_args in $as
		do
			$RM || exit
			$CL \
				-Dhas_prototypes=$has_prototypes \
				-Dhas_stdarg=$has_stdarg \
				-Dhas_varargs=$has_varargs \
				-Dva_start_args=$va_start_args \
				a.c $L >&2 && ./a.out && break
		done && break
	done && break
done || {
	echo >&2 "cannot deduce has_prototypes, has_stdarg, va_start_args"
	exit 1
}
cat - a.ha <<EOF

/* Define the following symbols to be 1 or 0.  */
#define has_prototypes $has_prototypes /* Do function prototypes work?  */
#define has_stdarg $has_stdarg /* Does <stdarg.h> work?  */
#define has_varargs $has_varargs /* Does <varargs.h> work?  */
#define va_start_args $va_start_args /* How many args does va_start() take?  */
EOF

: text_equals_binary_stdio, FOPEN_...
cat >a.c <<'EOF'
#include "a.h"
	int
copyto(filename, mode)
	char const *filename, *mode;
{
	int c;
	FILE *f, *g;
	if (!(f = fopen("a.out", "rb")) || !(g = fopen(filename, mode)))
		return 1;
	while (c=getc(f), !feof(f))
		if (ferror(f)  ||  putc(c,g)<0 && ferror(g))
			return 1;
	return fclose(f)!=0 || fclose(g)!=0;
}
int main() { exitmain(copyto("a.d", "w+b") || copyto("a.e", "w+")); }
EOF
e=1
$RM a.d a.e || exit
$CL a.c $L >&2 && ./a.out && cmp a.out a.d && {
	cmp a.out a.e || e=0
}
cat <<EOF

#define text_equals_binary_stdio $e /* Does stdio treat text like binary?  */
#define text_work_stdio 0 /* Text i/o for working file, binary for RCS file?  */
#if text_equals_binary_stdio
	/* Text and binary i/o behave the same, or binary i/o does not work.  */
#	define FOPEN_R "r"
#	define FOPEN_W "w"
#	define FOPEN_WPLUS "w+"
#else
	/* Text and binary i/o behave differently.  */
	/* This is incompatible with Posix and Unix.  */
#	define FOPEN_R "rb"
#	define FOPEN_W "wb"
#	define FOPEN_WPLUS "w+b"
#endif
#if text_work_stdio
#	define FOPEN_R_WORK "r"
#	define FOPEN_W_WORK "w"
#	define FOPEN_WPLUS_WORK "w+"
#else
#	define FOPEN_R_WORK FOPEN_R
#	define FOPEN_W_WORK FOPEN_W
#	define FOPEN_WPLUS_WORK FOPEN_WPLUS
#endif

/* Define or comment out the following symbols as needed.  */
EOF

: bad_fopen_wplus
cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(!fopen("a.d",FOPEN_WPLUS)); }
EOF
$RM || exit
if echo nonempty >a.d && $CL a.c $L >&2 && ./a.out && test ! -s a.d
then b=0
else b=1
fi
echo "#define bad_fopen_wplus $b /* Does fopen(f,FOPEN_WPLUS) fail to truncate f?  */"

: getlogin_is_secure
echo "#define getlogin_is_secure 0 /* Is getlogin() secure?  Usually it's not.  */"

: has_dirent
cat >a.c <<'EOF'
#include "a.h"
int main() {
	DIR *d = opendir(".");
	struct dirent *e;
	while ((e = readdir(d)))
		if (strcmp(e->d_name, "a.c") == 0  &&  closedir(d) == 0)
			exitmain(0);
	exitmain(1);
}
EOF
$RM || exit
if $CL a.c $L >&2 && ./a.out
then h=1
else h=0
fi
echo "#define has_dirent $h /* Do opendir(), readdir(), closedir() work?  */"

: has_fchmod
cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(fchmod(fileno(stdin),0) != 0); }
EOF
$RM || exit
if $CL a.c $L >&2 && ./a.out <a.c && test ! -r a.c
then h=1
else h=0
fi
echo "#define has_fchmod $h /* Does fchmod() work?  */"
rm -f a.c || exit

: has_fputs
cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(fputs("Hello\"\nworld", stdout) != 12); }
EOF
Hello='Hello"
world'
$RM || exit
if $CL a.c $L >&2 && ./a.out >/dev/null && x=`./a.out` && test " $x" = " $Hello"
then h=1
else h=0
fi
echo "#define has_fputs $h /* Does fputs() work?  */"

: has_ftruncate
cat >a.c <<'EOF'
#include "a.h"
int main(argc, argv) int argc; char **argv; {
	int f = creat(argv[1], 0);
	if (f<0 || write(f,"abc",3)!=3 || ftruncate(f,(off_t)0)!=0 || close(f)!=0)
			exitmain(1);
	exitmain(0);
}
EOF
$RM || exit
if $CL a.c $L >&2
then
	h=1
	# Check out /tmp too; it's buggy on some hosts.
	for d in . /tmp
	do
		if test -d $d
		then
			f=$d/a.d
			rm -f $f || exit
			./a.out $f && test ! -s $f && test -f $f  ||  h=0
			rm -f $f || exit
		fi
	done
else h=0
fi
echo "#define has_ftruncate $h /* Does ftruncate() work?  */"

: has_getuid
cat >a.c <<'EOF'
#include "a.h"
#ifndef getuid
	uid_t getuid();
#endif
int main() { exitmain(getuid()!=getuid()); }
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then has_getuid=1
else has_getuid=0
fi
echo "#define has_getuid $has_getuid /* Does getuid() work?  */"

: has_getpwuid
case $has_getuid in
0)
	a='/* ' z='*/ ' h=?;;
*)
	a= z=
	cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(!getpwuid(0)); }
EOF
	$RM || exit
	if ($CL a.c $L && ./a.out) >&2
	then h=1
	else h=0
	fi
esac
echo "$a#define has_getpwuid $h $z/* Does getpwuid() work?  */"

: has_link
cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(link("a.c","a.d") != 0); }
EOF
$RM a.d || exit
if ($CL a.c $L && ./a.out && cmp a.c a.d) >&2
then h=1
else h=0
fi
rm -f a.d || exit
echo "#define has_link $h /* Does link() work?  */"

: has_memcmp
cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(memcmp("beautiful","beautiful",10) != 0); }
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then h=1
else h=0
fi
echo "#define has_memcmp $h /* Does memcmp() work?  */"

: has_memcpy
cat >a.c <<'EOF'
#include "a.h"
char a[3];
int main() {
	memcpy(a,"xy",3);
	exitmain(strcmp(a,"xy")!=0);
}
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then h=1
else h=0
fi
echo "#define has_memcpy $h /* Does memcpy() work?  */"

: has_memmove
cat >a.c <<'EOF'
#include "a.h"
char a[4];
int main() {
	strcpy(a, "xy");
	memmove(a+1, a, 3);
	exitmain(strcmp(a,"xxy")!=0);
}
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then h=1
else h=0
fi
echo "#define has_memmove $h /* Does memmove() work?  */"

: has_mmap, has_madvise
cat >a.c <<'EOF'
#define CHAR1 '#' /* the first character in this file */
#include "a.h"
#ifndef mmap
	caddr_t mmap();
#endif
caddr_t a;
struct stat b;
#ifndef MADVISE_OK
#	define MADVISE_OK (madvise(a,b.st_size,MADV_SEQUENTIAL)==0 && madvise(a,b.st_size,MADV_NORMAL)==0)
#endif
int main()
{
	if (fstat(fileno(stdin), &b) != 0)
		exitmain(1);
	a = mmap(
		(caddr_t)0, b.st_size, PROT_READ, MAP_SHARED,
		fileno(stdin), (off_t)0
	);
	exitmain(
		a == (caddr_t)-1  ||
		!MADVISE_OK ||
		*a != CHAR1  ||
		munmap(a, b.st_size)  !=  0
	);
}
EOF
a=0 has_mmap=0
$RM || exit
if ($CL -DMADVISE_OK=1 a.c $L && ./a.out <a.c) >&2
then
	has_mmap=1
	$RM || exit
	($CL a.c $L && ./a.out <a.c) >&2 && a=1
fi
echo "#define has_madvise $a /* Does madvise() work?  */"
echo "#define has_mmap $has_mmap /* Does mmap() work on regular files?  */"

: has_rename, bad_a_rename, bad_b_rename
cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(rename("a.a","a.b") != 0); }
EOF
echo a >a.a && $RM a.b || exit
if ($CL a.c $L && ./a.out && test -f a.b) >&2
then
	h=1
	rm -f a.a a.b &&
	echo a >a.a && chmod -w a.a || exit
	if ./a.out && test ! -f a.a && test -f a.b
	then a=0
	else a=1
	fi
	rm -f a.a a.b &&
	echo a >a.a && echo b >a.b && chmod -w a.b || exit
	if ./a.out && test ! -f a.a && test -f a.b
	then b=0
	else b=1
	fi
	rm -f a.a a.b || exit
else h=0 a=0 b=0
fi
echo "#define has_rename $h /* Does rename() work?  */"
echo "#define bad_a_rename $a /* Does rename(A,B) fail if A is unwritable?  */"
echo "#define bad_b_rename $b /* Does rename(A,B) fail if B is unwritable?  */"

: void, VOID
cat >a.c <<'EOF'
#include "a.h"
void f() {}
int main() {f(); exitmain(0);}
EOF
if $CS a.c $LS >&2
then
	v='(void) '
else
	v=
	echo 'typedef int void;'
fi
echo "#define VOID $v/* 'VOID e;' discards the value of an expression 'e'.  */"

: has_seteuid
case $has_getuid in
0)
	a='/* ' z='*/ ' has_seteuid=?;;
*)
	a= z=
	cat >a.c <<'EOF'
#include "a.h"
#ifndef geteuid
	uid_t geteuid();
#endif
int main() {
/* Guess, don't test.  Ugh.  Testing would require running conf.sh setuid.  */
/* seteuid() isn't standardized yet, so the guess below may well be wrong.  */
#if !_POSIX_VERSION || _POSIX_VERSION<=199009L&&!defined(sgi)&&!defined(__sgi__)&&!defined(sun)&&!defined(__sun__)
	exitmain(1);
#else
	exitmain(seteuid(geteuid()) != 0);
#endif
}
EOF
	$RM || exit
	if ($CL a.c $L && ./a.out) >&2
	then has_seteuid=1
	else has_seteuid=0
	fi
esac
echo "$a#define has_seteuid $has_seteuid $z/* Does seteuid() work?  See README.  */"

: has_setuid
h=$has_seteuid
case $h in
0)
	cat >a.c <<'EOF'
#include "a.h"
#ifndef getuid
	uid_t getuid();
#endif
int main() { exitmain(setuid(getuid()) != 0); }
EOF
	$RM || exit
	($CL a.c $L && ./a.out) >&2 && h=1
esac
echo "$a#define has_setuid $h $z/* Does setuid() exist?  */"

: has_signal, signal_args, signal_type, sig_zaps_handler
cat >a.c <<'EOF'
#include "a.h"
#ifndef getpid
	pid_t getpid();
#endif
#if !defined(signal) && declare_signal
	signal_type (*signal P((int,signal_type(*)signal_args)))signal_args;
#endif
signal_type nothing(i) int i; {}
int main(argc, argv) int argc; char **argv;
{
	signal(SIGINT, nothing);
	while (--argc)
		kill(getpid(), SIGINT);
	exitmain(0);
}
EOF
for declare_signal in 1 0
do
	for signal_type in void int
	do
		for signal_args in 'P((int))' '()'
		do
			$RM || exit
			($CL \
				-Ddeclare_signal=$declare_signal \
				-Dsignal_args="$signal_args" \
				-Dsignal_type=$signal_type \
					a.c $L && ./a.out 1) >&2 && break
		done && break
	done && break
done || {
	echo >&2 "cannot deduce signal_args, signal_type"
	exit 1
}
if ./a.out 1 2 >&2
then z=0
else z=1
fi
cat <<EOF
#define has_signal $has_signal /* Does signal() work?  */
#define signal_args $signal_args /* arguments of signal handlers */
#define signal_type $signal_type /* type returned by signal handlers */
#define sig_zaps_handler $z /* Must a signal handler reinvoke signal()?  */
EOF

: has_sigaction
cat >a.c <<'EOF'
#include "a.h"
#ifndef getpid
	pid_t getpid();
#endif
static sig_atomic_t volatile gotsig;
static void getsig(i) int i; { gotsig = 1; }
int main(argc, argv) int argc; char **argv;
{
	struct sigaction s;
	sigset_t t;
	if (sigemptyset(&t) != 0  ||  sigaddset(&t, SIGINT) != 0)
		exitmain(1);
	if (sigaction(SIGINT, (struct sigaction const*)0, &s) != 0)
		exitmain(1);
	s.sa_handler = getsig;
	s.sa_mask = t;
	if (sigaction(SIGINT, &s, (struct sigaction*)0) != 0)
		exitmain(1);
	kill(getpid(), SIGINT);
	exitmain(gotsig != 1);
}
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then has_sigaction=1
else has_sigaction=0
fi
echo "#define has_sigaction $has_sigaction /* Does struct sigaction work?  */"

: has_sigblock, sigmask
a='/* ' z='*/ '
b='/* ' y='*/ '
case $has_sigaction in
1)
	h=? n=?;;
*)
	a= z=
	cat >a.c <<'EOF'
#include "a.h"
#include <signal.h>
#if define_sigmask
#	define sigmask(s) (1 << ((s)-1))
#endif
int main()
{
	sigblock(sigmask(SIGHUP));
	exitmain(kill(getpid(), SIGHUP) != 0);
}
EOF
	if $RM || exit; ($CL a.c $L && ./a.out) >&2
	then h=1
	elif $RM || exit; ($CL -Ddefine_sigmask=1 a.c $L && ./a.out) >&2
	then h=1 b= y=
	else h=0
	fi
esac
echo "$a#define has_sigblock $h $z/* Does sigblock() work?  */"
echo "$b#define sigmask(s) (1 << ((s)-1)) $y/* Yield mask for signal number.  */"

: has_sys_siglist
cat >a.c <<'EOF'
#include "a.h"
#ifndef sys_siglist
	extern char const *sys_siglist[];
#endif
int main() { exitmain(!sys_siglist[1][0]); }
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then h=1
else h=0
fi
echo "#define has_sys_siglist $h /* Does sys_siglist[] work?  */"

: fread_type, Fread, Fwrite
cat >a.c <<'EOF'
#define CHAR1 '#' /* the first character in this file */
#include "a.h"
#if !defined(fread) && declare_fread
	fread_type fread P((void*,freadarg_type,freadarg_type,FILE*));
#endif
int main()
{
	char b;
	exitmain(!(
		fread(&b, (freadarg_type)1, (freadarg_type)1, stdin) == 1  &&
		b==CHAR1
	));
}
EOF
for declare_fread in 1 0
do
	for fread_type in ssize_t size_t int unsigned
	do
		for freadarg_type in size_t ssize_t unsigned int
		do
			$RM || exit
			(
				$CL \
					-Ddeclare_fread=$declare_fread \
					-Dfreadarg_type=$freadarg_type \
					-Dfread_type=$fread_type \
					a.c $L &&
				./a.out <a.c
			) >&2 && break
		done && break
	done && break
done || {
	echo >&2 "cannot deduce fread types"
	exit 1
}
cat <<EOF
typedef $fread_type fread_type; /* type returned by fread() and fwrite() */
typedef $freadarg_type freadarg_type; /* type of their size arguments */
EOF

: malloc_type
cat >a.c <<'EOF'
#include "a.h"
typedef void *malloc_type;
#ifndef malloc
	malloc_type malloc();
#endif
int main() { exitmain(!malloc(1)); }
EOF
if $CS a.c $LS >&2
then t=void
else t=char
fi
echo "typedef $t *malloc_type; /* type returned by malloc() */"

: has_getcwd
cat >a.c <<'EOF'
#include "a.h"
#ifndef getcwd
	char *getcwd();
#endif
char buf[10000];
int main() { exitmain(!getcwd(buf,10000)); }
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then has_getcwd=1
else has_getcwd=0
fi
echo "#define has_getcwd $has_getcwd /* Does getcwd() work?  */"

: has_getwd
case $has_getcwd in
1)
	a='/* ' z='*/ ' h=?;;
*)
	a= z=
	cat >a.c <<'EOF'
#include "a.h"
#include <sys/param.h>
#ifndef getwd
	char *getwd();
#endif
char buf[MAXPATHLEN];
int main() { exitmain(!getwd(buf)); }
EOF
	$RM || exit
	if ($CL a.c $L && ./a.out) >&2
	then h=1
	else h=0
	fi
esac
echo "$a#define has_getwd $h $z/* Does getwd() work?  */"

: has_mktemp
cat >a.c <<'EOF'
#include "a.h"
#ifndef mktemp
	char *mktemp();
#endif
int main()
{
	char b[9];
	strcpy(b, "a.XXXXXX");
	exitmain(!mktemp(b));
}
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then h=1
else h=0
fi
echo "#define has_mktemp $h /* Does mktemp() work?  */"

: has_NFS
echo "#define has_NFS 1 /* Might NFS be used?  */"

: strchr
cat >a.c <<'EOF'
#include "a.h"
#ifndef strchr
	char *strchr();
#endif
int main() {exitmain(!strchr("abc", 'c'));}
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then a='/* ' z='*/ '
else a= z=
fi
echo "$a#define strchr index $z/* Use old-fashioned name for strchr()?  */"

: strrchr
cat >a.c <<'EOF'
#include "a.h"
#ifndef strrchr
	char *strrchr();
#endif
int main() {exitmain(!strrchr("abc", 'c'));}
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then a='/* ' z='*/ '
else a= z=
fi
echo "$a#define strrchr rindex $z/* Use old-fashioned name for strrchr()?  */"

: bad_unlink
cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(unlink("a.c") != 0); }
EOF
$RM && chmod -w a.c || exit
if $CL a.c $L >&2 && ./a.out >/dev/null && test ! -f a.c
then b=0
else b=1
fi
rm -f a.c || exit
echo "#define bad_unlink $b /* Does unlink() fail on unwritable files?  */"

: has_vfork, has_fork, has_spawn, has_wait, has_waitpid, RCS_SHELL
cat >a.c <<'EOF'
#include "a.h"
#ifndef getpid
	pid_t getpid();
#endif
#if TRY_VFORK
#	ifndef vfork
		pid_t vfork();
#	endif
#else
#	ifndef fork
		pid_t fork();
#	endif
#	undef vfork
#	define vfork fork
#endif
#if TRY_WAITPID
#	ifndef waitpid
		pid_t waitpid();
#	endif
#else
#	ifndef wait
		pid_t wait();
#	endif
#endif
pid_t child;
int status;
struct stat st;
int main()
{
	pid_t parent = getpid();
	if (!(child = vfork())) {
		/* Tickle vfork/compiler bug (e.g. sparc gcc -O (1.37.1).  */
		pid_t i = getpid(), j = getpid();
		if (i!=getpid() || j!=getpid())
			_exit(!i);
		/* Tickle file descriptor bug (e.g. IRIX 3.3).  */
		_exit(close(1) != 0);
	} else {
#		if TRY_WAITPID
			if (waitpid(child, &status, 0) != child)
				exitmain(1);
#		else
			while (wait(&status) != child)
				;
#		endif
		/* Test for presence of bugs.  */
		exitmain(status  ||  parent != getpid()  ||  fstat(1,&st) != 0);
	}
}
EOF
$RM || exit
if ($CL -DTRY_VFORK=1 a.c $L && ./a.out) >&2
then has_vfork=1
else has_vfork=0
fi
echo "#define has_vfork $has_vfork /* Does vfork() work?  */"
h=$has_vfork
case $h in
0)
	$RM || exit
	($CL a.c $L && ./a.out) >&2 && h=1
esac
echo "#define has_fork $h /* Does fork() work?  */"
$RM || exit
if ($CL -DTRY_VFORK=$has_vfork -DTRY_WAITPID=1 a.c $L && ./a.out) >&2
then h=1
else h=0
fi
echo "#define has_spawn 0 /* Does spawn*() work?  */"
echo "#define has_wait 1 /* Does wait() work?  */"
echo "#define has_waitpid $h /* Does waitpid() work?  */"
echo '#define RCS_SHELL "/bin/sh" /* shell to run RCS subprograms */'

: has_vfprintf
cat >a.c <<'EOF'
#include "a.h"
#if has_prototypes
int p(char const*format,...)
#else
/*VARARGS1*/ int p(format, va_alist) char *format; va_dcl
#endif
{
	int r;
	va_list args;
	vararg_start(args, format);
	r = vfprintf(stderr, format, args);
	va_end(args);
	return r;
}
int main() { exitmain(p("") != 0); }
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then h=1
else h=0
fi
echo "#define has_vfprintf $h /* Does vfprintf() work?  */"

: has__doprintf, has__doprnt
case $h in
1)
	h=? a='/* ' z='*/ ';;
*)
	a= z=
	cat >a.c <<'EOF'
#include "a.h"
#if has_prototypes
int p(char const*format,...)
#else
/*VARARGS1*/ int p(format, va_alist) char *format; va_dcl
#endif
{
	va_list args;
	vararg_start(args, format);
#	if TRY__DOPRINTF
		_doprintf(stderr, format, args);
#	else
		_doprnt(format, args, stderr);
#	endif
	va_end(args);
}
int main() { p(""); exitmain(ferror(stderr) != 0); }
EOF
	$RM || exit
	if ($CL -DTRY__DOPRINTF=1 a.c $L && ./a.out) >&2
	then h=1
	else h=0
	fi
esac
echo "$a#define has__doprintf $h $z/* Does _doprintf() work?  */"
case $h in
0)
	$RM || exit
	if ($CL a.c $L && ./a.out) >&2
	then h=1
	else h=0
	fi
	a= z=;;
*)
	h=? a='/* ' z='*/ '
esac
echo "$a#define has__doprnt $h $z/* Does _doprnt() work?  */"

: EXIT_FAILURE
cat >a.c <<'EOF'
#include "a.h"
int main() { exitmain(EXIT_FAILURE); }
EOF
$RM || exit
if $CL a.c $L >&2 && ./a.out
then a= z=
else a='/* ' z='*/ '
fi
echo "$a#undef EXIT_FAILURE $z/* Uncomment this if EXIT_FAILURE is broken.  */"

: large_memory
echo "#define large_memory $has_mmap /* Can main memory hold entire RCS files?  */"

: ULONG_MAX
cat >a.c <<'EOF'
#include "a.h"
#ifdef ULONG_MAX
	/*
	 * "#if ULONG_MAX/10 <= 0" does not always work,
	 * because some buggy implementations put casts in ULONG_MAX.
	 */
	int main() { exitmain(ULONG_MAX/10 <= 0); }
#else
	int main() { exitmain(1); }
#endif
EOF
$RM || exit
if $CL a.c $L >&2 && ./a.out
then a='/* ' z='*/ '
else a= z=
fi
echo "$a#undef ULONG_MAX $z/* Uncomment this if ULONG_MAX is broken (e.g. < 0).  */"

: struct utimbuf
cat >a.c <<'EOF'
#include "a.h"
struct utimbuf s;
int main() { s.actime = s.modtime = 1; exitmain(utime("a.c", &s) != 0); }
EOF
$RM || exit
if ($CL a.c $L && ./a.out) >&2
then a='/* ' z=' */'
else a= z=
fi
echo "${a}struct utimbuf { time_t actime, modtime; };$z /* Uncomment this if needed.  */"

: CO
echo "#define CO \"${RCSPREFIX}co\" /* name of 'co' program */"

: COMPAT2
echo "#define COMPAT2 $COMPAT2 /* Are version 2 files supported?  */"

: DATEFORM
cat >a.c <<'EOF'
#include "a.h"
int main() { printf("%.2d", 1); exitmain(0); }
EOF
$RM && $CL a.c $L >&2 && r=`./a.out` || exit
case $r in
01)	f=%.2d;;
*)	f=%02d
esac
echo "#define DATEFORM \"$f.$f.$f.$f.$f.${f}\" /* e.g. 01.01.01.01.01.01 */"

: DIFF
echo "#define DIFF \"${DIFF}\" /* name of 'diff' program */"

: DIFF3
echo "#define DIFF3 \"${DIFF3}\" /* name of 'diff3' program */"

echo "#define DIFF3_BIN $DIFF3_BIN /* Is diff3 user-visible (not the /usr/lib auxiliary)?  */"

: DIFF_FLAGS
dfs=
for df in $DIFF_FLAGS
do dfs="$dfs, \"${df}\""
done
echo "#define DIFF_FLAGS $dfs /* Make diff output suitable for RCS.  */"

: DIFF_L
echo "#define DIFF_L $DIFF_L /* Does diff -L work? */"

: DIFF_SUCCESS, DIFF_FAILURE, DIFF_TROUBLE
cat <<EOF
#define DIFF_SUCCESS $DIFF_SUCCESS /* DIFF status if no differences are found */
#define DIFF_FAILURE $DIFF_FAILURE /* DIFF status if differences are found */
#define DIFF_TROUBLE $DIFF_TROUBLE /* DIFF status if trouble */
EOF

: ED
echo "#define ED \"${ED}\" /* name of 'ed' program (used only if !DIFF3_BIN) */"

: MERGE
echo "#define MERGE \"${RCSPREFIX}merge\" /* name of 'merge' program */"

: '*SLASH*', ROOTPATH, TMPDIR, X_DEFAULT
case ${PWD-`pwd`} in
/*) # Posix
	SLASH=/
	qSLASH="'/'"
	SLASHes=$qSLASH
	isSLASH='#define isSLASH(c) ((c) == SLASH)'
	ROOTPATH='isSLASH((p)[0])'
	X_DEFAULT=",v$SLASH";;
?:[/\\]*) # MS-DOS
	SLASH='\'
	qSLASH="'\\\\'"
	SLASHes="$qSLASH: case '/': case ':'"
	isSLASH='int isSLASH P((int));'
	ROOTPATH='((p)[0] && (p)[1]==':' && isSLASH((p)[2]))'
	X_DEFAULT="$SLASH,v";;
*)
	echo >&2 "cannot deduce SLASH"; exit 1
esac
cat <<EOF
#define TMPDIR "${SLASH}tmp" /* default directory for temporary files */
#define SLASH $qSLASH /* principal pathname separator */
#define SLASHes $SLASHes /* \`case SLASHes:' labels all pathname separators */
$isSLASH /* Is arg a pathname separator?  */
#define ROOTPATH(p) $ROOTPATH /* Is p an absolute pathname?  */
#define X_DEFAULT "$X_DEFAULT" /* default value for -x option */
EOF

: DIFF_ABSOLUTE
case $DIFF in
"$SLASH"*) a=1;;
*) a=0
esac
echo "#define DIFF_ABSOLUTE $a /* Is ROOTPATH(DIFF) true?  */"

: ALL_ABSOLUTE
a=1
for i in "$DIFF" "$DIFF3" "$ED" "$RCSPREFIX" "$SENDMAIL"/
do
	case $i in
	"$SLASH"* | "\"$SLASH"*) ;;
	*) a=0 break
	esac
done
echo "#define ALL_ABSOLUTE $a /* Are all subprograms absolute pathnames?  */"

: SENDMAIL
case $SENDMAIL in
'') a='/* ' z='*/ ';;
*) a= z=
esac
echo "$a#define SENDMAIL $SENDMAIL $z/* how to send mail */"

: TZ_must_be_set
echo "#define TZ_must_be_set 0 /* Must TZ be set for gmtime() to work?  */"


: standard function declarations

cat <<'EOF'



/* Adjust the following declarations as needed.  */


#if __GNUC__ && !__STRICT_ANSI__
#	define exiting volatile /* GCC extension: function cannot return */
#else
#	define exiting
#endif
EOF

cat >a.ha <<EOF

#if has_ftruncate
	int ftruncate P((int,off_t));
#endif

/* <sys/mman.h> */
#if has_madvise
	int madvise P((caddr_t,size_t,int));
#endif
#if has_mmap
	caddr_t mmap P((caddr_t,size_t,int,int,int,off_t));
	int munmap P((caddr_t,size_t));
#endif


/* Posix (ISO/IEC 9945-1: 1990 / IEEE Std 1003.1-1990) */
/* These definitions are for the benefit of non-Posix hosts, and */
/* Posix hosts that have Standard C compilers but traditional include files.  */
/* Unfortunately, mixed-up hosts are all too common.  */

/* <fcntl.h> */
#ifdef F_DUPFD
	int fcntl P((int,int,...));
#else
	int dup2 P((int,int));
#endif
#ifndef O_BINARY /* some non-Posix hosts need O_BINARY */
#	define O_BINARY 0 /* no effect on Posix */
#endif
#ifdef O_CREAT
#	define open_can_creat 1
#else
#	define open_can_creat 0
#	define O_RDONLY 0
#	define O_WRONLY 1
#	define O_RDWR 2
#	define O_CREAT 01000
#	define O_TRUNC 02000
	int creat P((char const*,mode_t));
#endif
#ifndef O_EXCL
#	define O_EXCL 0
#endif

/* <pwd.h> */
#if has_getpwuid
	struct passwd *getpwuid P((uid_t));
#endif

/* <signal.h> */
#if has_sigaction
	int sigaction P((int,struct sigaction const*,struct sigaction*));
	int sigaddset P((sigset_t*,int));
	int sigemptyset P((sigset_t*));
#else
#if has_sigblock
	/* BSD */
	int sigblock P((int));
	int sigmask P((int));
	int sigsetmask P((int));
#endif
#endif

/* <stdio.h> */
FILE *fdopen P((int,char const*));
int fileno P((FILE*));

/* <sys/stat.h> */
int chmod P((char const*,mode_t));
int fstat P((int,struct stat*));
int stat P((char const*,struct stat*));
mode_t umask P((mode_t));
#if has_fchmod
	int fchmod P((int,mode_t));
#endif
#ifndef S_IRUSR
#	ifdef S_IREAD
#		define S_IRUSR S_IREAD
#	else
#		define S_IRUSR 0400
#	endif
#	ifdef S_IWRITE
#		define S_IWUSR S_IWRITE
#	else
#		define S_IWUSR (S_IRUSR/2)
#	endif
#endif
#ifndef S_IRGRP
#	if has_getuid
#		define S_IRGRP (S_IRUSR / 0010)
#		define S_IWGRP (S_IWUSR / 0010)
#		define S_IROTH (S_IRUSR / 0100)
#		define S_IWOTH (S_IWUSR / 0100)
#	else
		/* single user OS -- not Posix or Unix */
#		define S_IRGRP 0
#		define S_IWGRP 0
#		define S_IROTH 0
#		define S_IWOTH 0
#	endif
#endif
#ifndef S_ISREG
#	define S_ISREG(n) (((n) & S_IFMT) == S_IFREG)
#endif

/* <sys/wait.h> */
#if has_wait
	pid_t wait P((int*));
#endif
#ifndef WEXITSTATUS
#	define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#	undef WIFEXITED /* Avoid 4.3BSD incompatibility with Posix.  */
#endif
#ifndef WIFEXITED
#	define WIFEXITED(stat_val) (!((stat_val) & 255))
#endif

/* <unistd.h> */
char *getlogin P((void));
int close P((int));
int isatty P((int));
int link P((char const*,char const*));
int open P((char const*,int,...));
int unlink P((char const*));
int _filbuf P((FILE*)); /* keeps lint quiet in traditional C */
int _flsbuf P((int,FILE*)); /* keeps lint quiet in traditional C */
long pathconf P((char const*,int));
ssize_t write P((int,void const*,size_t));
#ifndef STDIN_FILENO
#	define STDIN_FILENO 0
#	define STDOUT_FILENO 1
#	define STDERR_FILENO 2
#endif
#if has_fork
#	if !has_vfork
#		undef vfork
#		define vfork fork
#	endif
	pid_t vfork P((void)); /* vfork is nonstandard but faster */
#endif
#if has_getcwd || !has_getwd
	char *getcwd P((char*,size_t));
#else
	char *getwd P((char*));
#endif
#if has_getuid
	uid_t getuid P((void));
#endif
#if has_readlink
	ssize_t readlink P((char const*,char*,size_t)); /* BSD; not standard yet */
#endif
#if has_setuid
#	if !has_seteuid
#		undef seteuid
#		define seteuid setuid
#	endif
	int seteuid P((uid_t));
	uid_t geteuid P((void));
#endif
#if has_spawn
	int spawnv P((int,char const*,char*const*));
#	if ALL_ABSOLUTE
#		define spawn_RCS spawnv
#	else
#		define spawn_RCS spawnvp
		int spawnvp P((int,char const*,char*const*));
#	endif
#else
	int execv P((char const*,char*const*));
#	if ALL_ABSOLUTE
#		define exec_RCS execv
#	else
#		define exec_RCS execvp
		int execvp P((char const*,char*const*));
#	endif
#endif

/* utime.h */
int utime P((char const*,struct utimbuf const*));


/* Standard C library */
/* These definitions are for the benefit of hosts that have */
/* traditional C include files, possibly with Standard C compilers.  */
/* Unfortunately, mixed-up hosts are all too common.  */

/* <errno.h> */
extern int errno;

/* <limits.h> */
#ifndef ULONG_MAX
	/* This does not work in #ifs, but it's good enough for us.  */
#	define ULONG_MAX ((unsigned long)-1)
#endif

/* <signal.h> */
#if has_signal
	signal_type (*signal P((int,signal_type(*)signal_args)))signal_args;
#endif

/* <stdio.h> */
FILE *fopen P((char const*,char const*));
fread_type fread P((void*,freadarg_type,freadarg_type,FILE*));
fread_type fwrite P((void const*,freadarg_type,freadarg_type,FILE*));
int fclose P((FILE*));
int feof P((FILE*));
int ferror P((FILE*));
int fflush P((FILE*));
int fprintf P((FILE*,char const*,...));
int fputs P((char const*,FILE*));
int fseek P((FILE*,long,int));
int printf P((char const*,...));
int rename P((char const*,char const*));
int sprintf P((char*,char const*,...));
long ftell P((FILE*));
void clearerr P((FILE*));
void perror P((char const*));
#ifndef L_tmpnam
#	define L_tmpnam 32 /* power of 2 > sizeof("/usr/tmp/xxxxxxxxxxxxxxx") */
#endif
#ifndef SEEK_SET
#	define SEEK_SET 0
#endif
#if has_mktemp
	char *mktemp P((char*)); /* traditional */
#else
	char *tmpnam P((char*));
#endif
#if has_vfprintf
	int vfprintf P((FILE*,char const*,va_list));
#else
#if has__doprintf
	void _doprintf P((FILE*,char const*,va_list)); /* Minix */
#else
	void _doprnt P((char const*,va_list,FILE*)); /* BSD */
#endif
#endif

/* <stdlib.h> */
char *getenv P((char const*));
exiting void _exit P((int));
exiting void exit P((int));
malloc_type malloc P((size_t));
malloc_type realloc P((malloc_type,size_t));
void free P((malloc_type));
#ifndef EXIT_FAILURE
#	define EXIT_FAILURE 1
#endif
#ifndef EXIT_SUCCESS
#	define EXIT_SUCCESS 0
#endif
#if !has_fork && !has_spawn
	int system P((char const*));
#endif

/* <string.h> */
char *strcpy P((char*,char const*));
char *strchr P((char const*,int));
char *strrchr P((char const*,int));
int memcmp P((void const*,void const*,size_t));
int strcmp P((char const*,char const*));
size_t strlen P((char const*));
void *memcpy P((void*,void const*,size_t));
#if has_memmove
	void *memmove P((void*,void const*,size_t));
#endif

/* <time.h> */
time_t time P((time_t*));
EOF

cat >a.c <<'EOF'
#include "a.h"
#define a 0
#define b 1
#if h==a
#	include "a.ha"
#else
#	include "a.hb"
#endif
int main() { exitmain(0); }
EOF

# Comment out lines in a.ha that the compiler rejects.
# a.ha may not contain comments that cross line boundaries.
# Leave the result in a.h$h.
h=a l=1
U=`wc -l <a.ha | sed 's| ||g'`
commentOut='s|^[^#/][^/]*|/* & */|'

until  test $U -lt $l  ||  $CS -Dh=$h a.c $LS >&2
do
	case $h in
	a) i=b;;
	*) i=a
	esac

	# The compiler rejects some line in l..U.
	# Use binary search to set l to be the index of the first bad line in l..U.
	u=$U
	while test $l -lt $u
	do
		M=`expr '(' $l + $u ')' / 2`
		M1=`expr $M + 1`
		sed "$M1,\$$commentOut" a.h$h >a.h$i || exit
		if $CS -Dh=$i a.c $LS >&2
		then l=$M1
		else u=$M
		fi
	done

	# Comment out the bad line.
	sed "$l$commentOut" a.h$h >a.h$i || exit

	h=$i
	l=`expr $l + 1`
done

cat a.h$h
