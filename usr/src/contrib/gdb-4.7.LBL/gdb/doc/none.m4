
_divert__(-1)

Switches:

_define__(<_ALL_ARCH__>,<0>)           (Meant as most inclusive; file turning 
					it on is expected to also turn on
					all arch-related switches including
					"_GENERIC__")
_define__(<_GENERIC__>,<1>)            (may not be quite all configs; 
					meant for "most vanilla" manual)
_define__(<_AGGLOMERATION__>,<0>)       is manual part of an agglomeration,
                                        with GPL formatted separately?
_define__(<_PRECONFIGURED__>,<0>)       is manual *only* for preconfigured sw?
_define__(<_FSF__>,<1>)                 set to zero to include things
                                        FSF won't take which Cygnus may want.
_define__(<_INTERNALS__>,<0>)

_define__(<_AOUT__>,<1>)		Object formats.  Note we turn on one.
_define__(<_BOUT__>,<0>)
_define__(<_COFF__>,<0>)
_define__(<_ELF__>,<0>)

_define__(<_LUCID__>,<0>)		A programming environment.

_define__(<_BARE__>,<0>)                Turn on to indicate no OS facilities
					(like shells, user prog args, program
					environment, corefiles)

_define__(<_DOSHOST__>,<0>)		Is this GDB DOS-hosted?

_define__(<_CONLY__>,<0>)		Mention only C debugging if
					turned on.  

_define__(<_REMOTESTUB__>,<1>)		Generic remote serial stub
_define__(<_AMD29K__>,<0>)		Specific architectures.  Note none
_define__(<_H8__>,<0>)
_define__(<_I80386__>,<0>)		starts out on.
_define__(<_I960__>,<0>)
_define__(<_M680X0__>,<0>)
_define__(<_SPARC__>,<0>)
_define__(<_ST2000__>,<0>)
_define__(<_VAX__>,<0>)
_define__(<_VXWORKS__>,<0>)

Text:

Default names; individual configs may override
Assembler:
_define__(<_AS__>,<as>)
C Compiler:
_define__(<_GCC__>,<gcc>)
Linker:
_define__(<_LD__>,<ld>)
Debugger name:
_define__(<_GDBN__>,<GDB>)
Debugger program:
_define__(<_GDBP__>,<gdb>)
Debugger init file:
_define__(<_GDBINIT__>,<.gdbinit>)

Text for host; individual configs *should* override, but this may
catch some flubs
_define__(<_HOST__>,<machine specific>)

"Machine Dependent" nodename
_define__(<_MACH_DEP__>,<Machine Dependent>)

_divert__<>