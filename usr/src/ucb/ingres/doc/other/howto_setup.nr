.ds HE 'HOW TO SET UP INGRES''PAGE %'
.so nmacs
.ce
Instructions for setting up INGRES from tape.
.ce
(VAX UNIX*)

.ce
(January 26, 1981)

This set of instructions will help you
read INGRES version 7 in from tape and get it running on
your system.
If you are running any version of UNIX
other than VM/UNIX release four,
read this entire document
before doing anything!,
paying particular attention to section 5.3.

*UNIX is a trademark of Bell Laboratories.

.sh 1
Installing INGRES for the first time.
.PS
If you already have an INGRES system,
go directly to part 1.3.
.dn
Creating a UNIX user called "ingres".
.PS
The first thing you should do is to create a user named "ingres"
on your system
and log in as that user.
The files you read in from 
tape will be owned by "ingres",
an actual user on your system.
INGRES locates files in the system by
looking in the /etc/passwd file for the
user "ingres".
INGRES will
.ul
not work
unless you create this user.
.sh
Creating parent directory for the INGRES system.
.PS
The parent directory of the INGRES system at Berkeley is "/mnt/ingres".
It is possible to run INGRES in a parent directory
of your own choosing,
which must be the parent directory of
the user "ingres" as specified in /etc/passwd.
For the purpose of this document the
parent directory will be denoted by "...".
.sh
Extracting INGRES from the tape.
.PS
You are now ready to read the tape into your
system using tar.
You should be in the home directory for INGRES
and logged in as INGRES.
Type the command:

	tar xf /dev/rmt0

The system is now fully extracted from the tape
and the tape can be unloaded if desired.
.sh
User oriented programs
.PS
In .../bin there is a collection of programs.
Several programs
are type-I commands associated with INGRES and which should be moved or
linked into /usr/bin.
The other programs are executed only by other
INGRES programs and must be left in .../bin.
After moving the files verify that they
are still owned by "ingres",
and are in the correct modes.
Move only the files listed below.
Leave the other files where they are!

.in 18
.ti -10
copydb, creatdb, destroydb, equel, helpr,
.ti -10
ingres, printr, purge, restore, sysmod
.br
(see INGRES reference manual for an
explanation of these commands.

.ti -10
demodb    creates a new database with a name given as an argument
and loads it with sample data relations such as might be used by
a department store for employee and sales records.
The INGRES tutorial uses the relations
in the demo database for its examples.

.in -10
It is imperative that each of these programs be in
the correct mode.
A sample "ls -l" is given below.
Verify that your file modes agree with the listing.

.in +10
.nf
-rwx--x--x 1 ingres	. . .	copydb
-rws--x--x 1 ingres	. . .	creatdb
-rwxr-xr-x 1 ingres	. . .	demodb
-rws--x--x 1 ingres	. . .	destroydb
-rwx--x--x 1 ingres	. . .	equel
-rws--x--x 1 ingres	. . .	helpr
-rws--x--x 1 ingres	. . .	ingres
-rws--x--x 1 ingres	. . .	printr
-rws--x--x 1 ingres	. . .	purge
-rws--x--x 1 ingres	. . .	restore
.fi

.sh
EQUEL run time library.
.PS
The EQUEL run time library should be
moved to "/lib/libq.a"
or "/usr/lib/libq.a". The library
is needed for compiling user EQUEL
programs. It is not necessary for
compiling INGRES itself.
It is
preferable although not essential
that the owner of the library remain "ingres".
To move the library:

.nf
	cp .../bin/libq.a  /usr/lib/libq.a
.fi

Refer to the INGRES Reference Manual for
information about how to use EQUEL and
the EQUEL library.
There is a tutorial on using EQUEL in
".../doc/other/equeltut.q" and there is a
small sample program in ".../doc/other/equeldemo.q".
.sh
Install concurrency device in kernel
.PS
If you want to use concurrency control,
you will have to install the INGRES concurrency device
in the kernel.
This requires adding a device driver,
an entry in /dev,
and a one line change to the exit system call.
You should read the document
"Instructions for Installing the INGRES
Concurrency Device in UNIX"
before proceeding.
.sh 1
General Information about INGRES.
.dn
Some comments on the INGRES directories
.PS
The directories (in ...) necessary
for a minimal running system are:

.nf
bin		the binary programs comprising INGRES
files		files used by various parts of INGRES
data/base/	user-created databases
demo		used by the "demodb" command
.fi

Several additional directories are also created. If space limitations
are a problem on your system you may want to delete some or all
of the material here.
If you need to,
delete the files but be sure to keep
the directories.
The additional directories are:

.nf
doc		documentation
lib		object file libraries
source		system source code
.fi
.sh
Support for separated I & D space.
.PS
On a PDP-11,
you MUST have separated I & D space and floating point hardware
for INGRES to run under version 7.
INGRES will NOT run on an 11/40 or similar machine
under version 7 of UNIX.
.sh 1
Getting started using INGRES.
.dn
The Users File.
.PS
In order to use INGRES a user must be entered into
the "users" file.
The users file contains information
regarding the status and permissions of each
user.
This file resembles the /etc/passwd file,
and includes the user's name, the user
code (used inside INGRES to identify a user),
the UNIX id of the user, some status bits,
and some other fields.
These are described in detail in the document
users(files).

The users file can be initialized by running
the "usersetup" program.
This program installs all
users in the /etc/passwd file as valid
INGRES users.
The user code is assigned sequentially
from "aa".
The status field is initialized to 000000
for all normal users and
177777 for the user "ingres".

To setup the users file in /mnt/ingres execute:

	.../bin/usersetup

Once created the users file can be edited using "ed" to do
the following things:

.in +8
- completely remove all users not authorized to execute INGRES.

- restrict a user from using a particular database.

- set the other status bits for appropriate users.
.in -8

As UNIX users are added or deleted, the users
file must be updated appropriately.
The usersetup program can be executed only once.
A diagnostic is issued if it is executed a second time.
.sh
Creating a Sample Data Base
.PS
The "demodb" command provided can be used to construct the sample
database on
which the examples in the tutorial (located on-line in 
".../doc/other/tutorial") are based.
In order to execute this command
you must be entered into the users
file and have permission to create a database.
To create the sample
database and assign it the name "demo"
type the command:

.nf
	demodb demo
.fi

In order to use INGRES
with this database you need 
merely type

.nf
	ingres demo
.fi

and you will be ready to follow the terminal session described in 
the tutorial or to formulate your own queries.

To create your own copy of the same sample database
use some unique database name.

.nf
	demodb mydemo
.fi

.sh
Creating Other Data Bases.
.PS
You will no doubt wish to create other databases of your own and
for this you will need to use "creatdb".
In order
to use this command you must be authorized in the users file.
.sh
Concurrency Control.
.PS
INGRES is designed to run with either no
concurrency control or with full
concurrency control.
INGRES locks portions of a database by using
a special UNIX device called
"/dev/lock".
Initially you can use INGRES without this
special device.
If you want concurrency control
(if more than one person will be using
INGRES on the same database at the
same time, you MUST have it) then
print a copy of the instructions on
how to install the concurrency device:

.ti +8
nroff .../doc/other/lockdev.nr

[NOTE: this driver has not yet been converted to run under
VM/UNIX.]
.sh 1
Hints and suggestions.
.PS
.dn
Unix Protection Problems.
.PS
If you encounter problems in creating directories or reading in files
from the tape
check the protection status of the parent directories involved to make sure
that permission to write new files exists.

The following summarizes the proper modes for each directory.
All directories are owned by INGRES except the
individual databases.
(The modes in parentheses represent the mode to be used
if you wish to restrict access maximally.)
.nf

	bin		755
	data		700
	data/base	777
	demo		755
	doc		755
	files		755
	geoquel and all its subdirectories	755
	lib		755
	source and all its subdirectories	755 (700)
	any database	777 (typically owned by the dba)
.fi

The programs in .../bin should be in the following modes:

.nf
	-rwsr----- 1 ingres	. . .	ksort
	-rwxr-x--x 1 ingres	. . .	monitor
	-rwsr----- 1 ingres	. . .	vaxingres
.fi

You may need to
become super-user to add files
in "/usr/bin".
.sh
Making Certain Programs "Sticky".
.PS
The INGRES program to sort relations is
frequently executed
during an INGRES session.
Performance can be improved if it is
made "sticky".
If you have sufficient space on your
swapping device,
it is advantageous to make the following
program "sticky".
This can be done by changing its mode
to 5700.

	chmod 5740 .../bin/ksort

.sh
Upper Case Only Terminals
.PS
Remember if you are using an upper-case-only terminal you will have
to use a double backslash "\\\\" with all terminal monitor commands in order to
override the interpretation of "\\" as an escape character.
For example on an upper and lower case terminal you would type:

.nf
	help parts
	\\g
.fi

while on an upper case only terminal you would type:

.nf
	HELP PARTS
	\\\\G
.fi

.sh 1
Source code and system regeneration
.dn
Log on as user "ingres".
.PS
When doing any system work always log on as "ingres".
.sh
Recompilation.
.PS
All source code for the system is located in ".../source"
and various sub-directories therein. Each subdirectory is
named for a functional component or sub-component of the system.
Makefiles for regenerating the various portions of the system
from source are located in the associated directory.
To regenerate a particular module (e.g. parser) say:

.nf
	chdir .../source/parser
	make
.fi

The entire system can be recompiled and installed
in /usr/bin
using the command:

.nf
	chdir .../source/conf
	make allinstall sysinstall
.fi

When recompiling for the first time it is strongly
advised that you use a hard copy terminal or
save the output in some other manner.

Libraries of object code are maintained
in .../lib.

You may find that your C compiler cannot compile INGRES.
If this is true, you will have to increase the size of
the symbol table in cpp.

.sh
Compilation Flags and Funny Flavored UNIXes.
.PS
If you are running any system
other than VM/UNIX release 4 (4BSD),
you may have to recompile the INGRES system
before it is usable.
Examine the file .../source/unix.h closely
for correspondence with your version of reality.
Major problems include representation of user and group id's
and structures returned by various system calls.

Four compile flags in unix.h define the type of user id's
used on your system.
For a vanilla version six system
with 8-bit user id's and 8-bit group id's,
set the xV6_UNIX compilation flag.
For a Berkeley version six system
with a single 16-bit user id
(and no group id),
set the xB_UNIX flag.
For a vanilla version seven system,
chocolate version six system,
or VM/UNIX system,
with 16-bit user id's and 16-bit group id's
set xV7_UNIX.
Also, define either
VAX
or
PDP11
depending on whether you are on a PDP-11 or a VAX.

A large set of flags
in ingres.h
control compilation of trace information
and timing estimates.
Flags of the form "x?TM"
(where "?" is a letter as described below)
turn on timing information.
THESE SHOULD ALWAYS BE LEFT OFF,
since the system you receive on the distribution tape
does not include everything necessary to
successfully use these flags.
Flags of the form "x?TR1", "x?TR2", and "x?TR3"
are trace compilation flags.
Compiling these in does not automatically
make trace information print,
but only makes it available.
These flags must be properly nested;
for example, if xMTR2 is set,
xMTR1 must also be set
(but xMTR3 is optional).
The letter codes for these flags are as follows:

.nf
.in +5
A	Access Methods
M	Terminal Monitor
P	Parser
Q	Qrymod
D	Decomposition
O	One Variable Query Processor
Z	Data Base Utilities
E	Equel
S	Scanner
T	Standalone Routines
.fi
.in -5

A flag named DISTRIB is used in some places
in the code.
This is used in the yet-to-be-born
Distributed INGRES system,
and should always be left off.

Touching any other defined constants
should be considered extremely dangerous
and is done strictly at your own risk.

.sh 1
Documentation.
.dn
Tutorial on INGRES.
.PS
A tutorial on using INGRES can be found in
".../doc/other/tutorial.nr". A copy of the document
can be produced by:

.nf
	nroff .../doc/other/tutorial.nr
.fi

The tutorial guides the reader through
examples of using QUEL the INGRES database
language. A basic familiarity with UNIX is
assumed.
.sh
Reference Manual.
.PS
The reference manual consists of a number of files - 
one for each command or feature in INGRES. The reference
manual can be retrieved by using the
help command
(see help(quel)).
A complete copy of the manual can be
printed on the line printer by:

	chdir .../doc
	make ref

The complete manual is about 80 pages.

.sh
Maintenance Manual.
.PS
A guide to using INGRES has been written and is called
"Creating and Maintaining
a Database Using INGRES".
It includes information about
creating databases, storage structures,
recovery, copy, indices and
other useful information.
A copy of the manual can be produced by:

.nf
	nroff .../doc/other/maintain.nr
.fi

.sh
Design of INGRES
.PS
A draft of the report "The Design and Implementation
of INGRES" by Stonebraker, Kreps and Held can be
found in ".../doc/other/design.roff". It can be printed
by:

.nf
	roff .../doc/other/design.roff
.fi

The report gives a
fairly detailed description of the structure
and capabilities of the entire system as
it existed over three years ago, in January 1976.
.ul
It is out of date in many areas.
It gives implementation details and
algorithms of important system modules.

.sh
Internal Documentation.
.PS
We currently do not provide,
nor have we written any
internal documentation beyond what
is mentioned in the "Design and Implementation"
paper.

This document itself can be reproduced
by doing:

.nf
	nroff .../doc/other/howto_setup.nr
.fi

.sh 1
Consulting
.PS
The INGRES project is not set up to supply
regular consulting.
If you do have problems,
please reread these instructions carefully;
over two thirds of the "bug reports" we receive
can be answered by re-reading the documentation.
For non-technical questions
(e.g., regarding availability of the system
or documentation)
please call the project secretary
at (415) 642-2344.
I can also be reached through this number.


.in 10
.nf
Eric Allman
Electronics Research Laboratory
University of California
Berkeley, California  94720
.fi
.in 0
