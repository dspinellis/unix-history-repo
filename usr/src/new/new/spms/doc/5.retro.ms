.nr PS 12
.NH
Retrofitting of Software Projects
.nr PS 10
.XS
\*(SN Retrofitting of Software Projects
.XE
.PP
Since SPMS accepts an arbitrary directory arrangement, existing software
packages can be converted into projects with minimal reorganization. For
example, the Fortran 77 compiler project shown in figure 12 was brought
under project control by the following commands.
.KF
.sp 20
.SM
.ce
\fIFigure 12.  \fRThe Fortran 77 compiler project

.NL
.KE
.ID
%  \fBcd /usr/src/usr.bin\fR
%  \fBmkproject  \-T\|project  f77\fR
f77: description? (1 line): \fBFortran 77\fR
%  \fBchproject  f77\fR
%  \fBpmkdir  \-T\|include,src  include\fR
include: description? (1 line): \fBheader files\fR
%  \fBpmkdir src\fR
src: description? (1 line): \fBsource code\fR
%  \fBpd src\fR
%  \fBpmkdir  \-T\|cmdsrc,src,update.300,clean   f77  fpr  fsplit\fR
f77: description? (1 line): \fBf77 \- Fortran 77 compiler\fR
fpr: description? (1 line): \fBfpr \- print Fortran files\fR
fsplit: description? (1 line): \fBfsplit \- split multi-routine Fortran files\fR
%  \fBpmkdir  \-T\|cmdsrc,src,update.300,clean   f77pass1  f1  f2\fR
f77pass1: description? (1 line): \fBf77 parser\fR
f1: description? (1 line): \fBf77 code generator\fR
f2: description? (1 line): \fBf77 peephole optimizer\fR
%  \fBcd /usr/src/usr.lib\fR
%  \fBpmkdir \-T\|libsrc,src,update.200,clean   libF77  libI77  libU77\fR
libF77: description? (1 line): \fBf77 function library\fR
libI77: description? (1 line): \fBf77 I/O library\fR
libU77: description? (1 line): \fBf77 system utility library\fR
%  \fBpmkdir  \-N\|doc  \-T\|doc  /usr/doc/f77\fR
/usr/doc/f77: description? (1 line): \fBf77 documentation\fR
.DE
.PP
The directory aliasing feature of SPMS is also demonstrated by this example.
Sometimes a project will have more than one directory with the same name
as is the case with the Fortran project, where
the name of the `src/f77' compiler driver program directory coincides with the
name of the `doc/f77' documentation directory. Since SPMS insists that
the directories within each project have unique names, the `doc/f77' directory
is aliased to `doc' using \fIpmkdir \-N\fR.


