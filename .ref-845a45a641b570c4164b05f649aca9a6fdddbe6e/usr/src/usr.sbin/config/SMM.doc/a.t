.\" Copyright (c) 1983 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)a.t	6.3 (Berkeley) %G%
.\"
.\".ds RH "Configuration File Grammar
.bp
.LG
.B
.ce
APPENDIX A. CONFIGURATION FILE GRAMMAR
.sp
.R
.NL
.PP
The following grammar is a compressed form of the actual
\fIyacc\fP\|(1) grammar used by
.I config
to parse configuration files.
Terminal symbols are shown all in upper case, literals
are emboldened; optional clauses are enclosed in brackets, ``[''
and ``]'';  zero or more instantiations are denoted with ``*''.
.sp
.nf
.DT
Configuration ::=  [ Spec \fB;\fP ]*

Spec ::= Config_spec
	| Device_spec
	| \fBtrace\fP
	| /* lambda */

/* configuration specifications */

Config_spec ::=  \fBmachine\fP ID
	| \fBcpu\fP ID
	| \fBoptions\fP Opt_list
	| \fBident\fP ID
	| System_spec
	| \fBtimezone\fP [ \fB\-\fP ] NUMBER [ \fBdst\fP [ NUMBER ] ]
	| \fBtimezone\fP [ \fB\-\fP ] FPNUMBER [ \fBdst\fP [ NUMBER ] ]
	| \fBmaxusers\fP NUMBER

/* system configuration specifications */

System_spec ::= \fBconfig\fP ID System_parameter [ System_parameter ]*

System_parameter ::=  swap_spec | root_spec | dump_spec | arg_spec

swap_spec ::=  \fBswap\fP [ \fBon\fP ] swap_dev [ \fBand\fP swap_dev ]*

swap_dev ::=  dev_spec [ \fBsize\fP NUMBER ]

root_spec ::=  \fBroot\fP [ \fBon\fP ] dev_spec

dump_spec ::=  \fBdumps\fP [ \fBon\fP ] dev_spec

arg_spec ::=  \fBargs\fP [ \fBon\fP ] dev_spec

dev_spec ::=  dev_name | major_minor

major_minor ::=  \fBmajor\fP NUMBER \fBminor\fP NUMBER

dev_name ::=  ID [ NUMBER [ ID ] ]

/* option specifications */

Opt_list ::=  Option [ \fB,\fP Option ]*

Option ::=  ID [ \fB=\fP Opt_value ]

Opt_value ::=  ID | NUMBER

Mkopt_list ::=  Mkoption [ \fB,\fP Mkoption ]*

Mkoption ::=  ID \fB=\fP Opt_value

/* device specifications */

Device_spec ::= \fBdevice\fP Dev_name Dev_info Int_spec
	| \fBmaster\fP Dev_name Dev_info
	| \fBdisk\fP Dev_name Dev_info
	| \fBtape\fP Dev_name Dev_info
	| \fBcontroller\fP Dev_name Dev_info [ Int_spec ]
	| \fBpseudo-device\fP Dev [ NUMBER ]

Dev_name ::=  Dev NUMBER

Dev ::=  \fBuba\fP | \fBmba\fP | ID

Dev_info ::=  Con_info [ Info ]*

Con_info ::=  \fBat\fP Dev NUMBER
	| \fBat\fP \fBnexus\fP NUMBER

Info ::=  \fBcsr\fP NUMBER
	| \fBdrive\fP NUMBER
	| \fBslave\fP NUMBER
	| \fBflags\fP NUMBER

Int_spec ::=  \fBvector\fP ID [ ID ]*
	| \fBpriority\fP NUMBER
.fi
.sp
.SH
Lexical Conventions
.LP
The terminal symbols are loosely defined as:
.IP ID
.br
One or more alphabetics, either upper or lower case, and underscore,
``_''.
.IP NUMBER
.br
Approximately the C language specification for an integer number.
That is, a leading ``0x'' indicates a hexadecimal value,
a leading ``0'' indicates an octal value, otherwise the number is
expected to be a decimal value.  Hexadecimal numbers may use either
upper or lower case alphabetics.
.IP FPNUMBER
.br
A floating point number without exponent.  That is a number of the
form ``nnn.ddd'', where the fractional component is optional.
.LP
In special instances a question mark, ``?'', can be substituted for
a ``NUMBER'' token.  This is used to effect wildcarding in device
interconnection specifications.
.LP
Comments in configuration files are indicated by a ``#'' character
at the beginning of the line; the remainder of the line is discarded.
.LP
A specification
is interpreted as a continuation of the previous line
if the first character of the line is tab.
