$! Makefile for VMS
$! DCL-Shell-language. Edit the symbols section at the end.
$!
$ On Control_Y Then Goto The_Exit
$ On Error Then Goto The_Exit
$ define/user sys sys$library
$ cc gzip.c /define=(VAXC)
$ define/user sys sys$library
$ cc zip.c /define=(VAXC)
$ define/user sys sys$library
$ cc deflate.c /define=(VAXC)
$ define/user sys sys$library
$ cc trees.c /define=(VAXC)
$ define/user sys sys$library
$ cc bits.c /define=(VAXC)
$ define/user sys sys$library
$ cc unzip.c /define=(VAXC)
$ define/user sys sys$library
$ cc inflate.c /define=(VAXC)
$ define/user sys sys$library
$ cc util.c /define=(VAXC)
$ define/user sys sys$library
$ cc crypt.c /define=(VAXC)
$ define/user sys sys$library
$ cc lzw.c /define=(VAXC)
$ define/user sys sys$library
$ cc unlzw.c /define=(VAXC)
$ define/user sys sys$library
$ cc unpack.c /define=(VAXC)
$ define/user sys sys$library
$ cc unlzh.c /define=(VAXC)
$ define/user sys sys$library
$ cc getopt.c /define=(VAXC)
$ define/user sys sys$library
$ cc vms.c /define=(VAXC)
$ linkobjs:=gzip.obj zip.obj deflate.obj trees.obj bits.obj unzip.obj -
   inflate.obj util.obj crypt.obj lzw.obj unlzw.obj unpack.obj unlzh.obj -
   getopt.obj vms.obj
$ Schleife:
$ p = f$locate(" ",linkobjs)
$ if p .lt. f$length(linkobjs)
$ then	linkobjs[p,1]:=","
$ goto Schleife
$ endif
$ write sys$output "linking ''linkobjs'"
$ link  /exec=gzip.exe  'linkobjs',sys$input/opt
SYS$LIBRARY:VAXCRTL/SHARE 
$
$ ! Create a hard link.  (To remove both files, delete the copy FIRST, then
$ ! the original.  Otherwise, if original deleted first [copy says "no such
$ ! file"], must use "set file/remove gunzip.exe;#" to get rid of the copy.
$ ! Unlike in Unix, deleting the original ALWAYS destroys the data--but not
$ ! the directory entry of the copy.)  Using a hard link saves disk space, by
$ ! the way.  Note, however, that copying a hard link copies the data, not
$ ! just the link.  Therefore, set up the link in the directory in which the
$ ! executable is to reside, or else rename (move) the executables into the
$ ! directory.
$ !
$ set file/enter=gunzip.exe gzip.exe
$ set file/enter=zcat.exe   gzip.exe
$
$ ! Set up symbols for the gzip executable.  Edit the example below,
$ ! changing "disk:[directory]" as appropriate.
$ !
$ gzip   == "$disk:[directory]gzip.exe"
$ gunzip == "$disk:[directory]gunzip.exe"
$ zcat   == "$disk:[directory]zcat.exe"
$
$The_Exit:
$ Save_Status = $STATUS
$ exit Save_Status
