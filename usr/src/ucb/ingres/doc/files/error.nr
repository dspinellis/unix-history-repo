.th ERROR FILES
.sh NAME
\&.../files/error7_? \- files with \*(II errors
.sh DESCRIPTION
These files contain the \*(II error messages.
There is one file
for each thousands digit;
e.g.,
error number 2313 will be in file error7_2.
.s3
Each file consists of a sequence of error messages
with associated error numbers.
When an error enters the front end,
the appropriate file is scanned for the correct error number.
If found, the message is printed;
otherwise,
the first message parameter is printed.
.s3
Each message has the format
.br
	errnum <TAB> message tilde.
.br
Messages are terminated
by the tilde character (`~').
The message is scanned before printing.
If the sequence
.bd %\c
.it n
is encountered
(where
.it n
is a digit from 0 to 9),
parameter
.it n
is substituted,
where %0 is the first parameter.
.s3
The parameters can be in any order.
For example,
an error message can reference %2 before it
references %0.
.sh EXAMPLE
.nf
1003	line %0, bad database name %1~
1005	In the purge of %1, a bad %0 caused execution to halt~
1006	No process, try again.~
.fi
