.TL
Toward a Compatible Filesystem Interface
.AU
Michael J. Karels
Marshall Kirk McKusick
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
.LP
As network or remote filesystems have been implemented for
.UX ,
several stylized interfaces between the filesystem implementation
and the rest of the kernel have been developed.
Notable among these are Sun Microsystems' virtual filesystem interface
using vnodes, Digital Equipment's Generic File System architecture,
and AT&T's File System Switch.
Each design attempts to isolate filesystem-dependent details
below the generic interface and to provide a framework within which
new filesystems may be incorporated.
However, each of these interfaces is different from
and incompatible with the others.
Each of them addresses somewhat different design goals.
Each was based upon a different starting version of
.UX ,
targetted a different set of filesystems with varying characteristics,
and uses a different set of primitive operations provided by the filesystem.
The current study compares the various filesystem interfaces.
Criteria for comparison include generality, completeness, robustness,
efficiency and esthetics.
As a result of this comparison, a proposal for a new filesystem interface
is advanced that includes the best features of the existing implementations.
The proposal adopts the calling convention for name lookup introduced
in 4.3BSD.
A prototype implementation is described.
This proposal and the rationale underlying its development
have been presented to major software vendors
as an early step toward convergence upon a compatible filesystem interface.
