$set noon
$!
$! go to the vms subdirectory in the libg++ distribution tree
$flnm = f$enviroment("PROCEDURE")	! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$! go to the directory where the test files reside.
$set def [-.tests]
$! make sure that the symbols are defined properly.
$genclass:==@gnu_cc:[000000]genclass
$cxshare:==@gnu_cc:[000000]cxshare
$!
$gloop:
$filenm = f$search("GNU_GXX_INCLUDE:[GEN]*.*",0)
$if filenm.eqs."" then goto gdone
$! if both a .HP and a .CCP file exist, do only one of them
$name = f$parse(filenm,,,"NAME")
$type = f$parse(filenm,,,"TYPE")
$if type.eqs.".HP" then goto generate
$file1 = f$parse(filenm-".CCP",".HP")
$if f$search(file1,1).nes."" then goto gloop
$!
$generate:
$set ver
$genclass -2 "int" val "int" val 'name'
$! 'f$verify(0)
$goto gloop
$gdone:
$!
$lib/create ilib
$create compile_these.list
 ILIST ISLLIST IDLLIST IVEC IAVEC 
 IPLEX  IFPLEX  IXPLEX IRPLEX IMPLEX 
 ISET IBAG IMAP IPQ 
 IXPSET  IOXPSET  ISLSET  IOSLSET  IBSTSET ICHNODE 
 IAVLSET  ISPLAYNODE ISPLAYSET  IVHSET  IVOHSET  ICHSET 
 IXPBAG  IOXPBAG  ISLBAG  IOSLBAG  ISPLAYBAG IVHBAG  ICHBAG 
 IVHMAP  ICHMAP  ISPLAYMAP  IAVLMAP IRAVLMAP 
 ISPLAYPQ  IPHPQ  IXPPQ 
 IVSTACK IVQUEUE ISTACK IQUEUE IDEQUE 
 IXPSTACK ISLSTACK IXPQUEUE  ISLQUEUE IXPDEQUE IDLDEQUE 
$!
$!
$loop:
$filenm = f$search("i*.cc")
$name = f$parse(filenm,,,"NAME")
$if filenm.eqs."" then goto done
$assign nla0: sys$output
$assign nla0: sys$error
$search compile_these.list " ''name' "/OUTPUT=NLA0:
$stat1 = $status
$deassign sys$output
$deassign sys$error
$if stat1.ne.1 then goto loop
$!
$set ver
$gcc/plus/debug/nolist/define="__OPTIMIZE__" 'name'.CC
$lib ilib 'name'
$! 'f$verify(0)
$if $status then delete/nolog 'name'.obj;
$goto loop
$done:
$delete/nolog compile_these.list;
$!
$!
$tloop:
$filenm = f$search("t*.cc")
$name = f$parse(filenm,,,"NAME")
$if filenm.eqs."" then goto tdone
$set ver
$gcc/plus/debug/nolist/define=("unlink=remove") -
		 'name'.CC
$cxshare 'name'+ilib/lib/nomap
$! 'f$verify(0)
$delete/nolog 'name'.obj;
$goto tloop
$tdone:
$!
$!
$open ofile$ run.com/write
$write ofile$ "$set nover"
$write ofile$ "$set default "+f$parse(flnm,,,"DEVICE") -
		+f$parse(flnm,,,"DIRECTORY")-".VMS]"+".TESTS]"
$OPEN IFILE$ [-.vms]EXPECTED.LIST
$rloop:
$  read/end=finish ifile$ line
$  write ofile$ "$write sys$output ""''line'"""
$  write ofile$ "$run ''line'"
$  inp=f$search("''line'.INP",1)
$  if inp.eqs."" then goto rloop
$  open jfile$ 'line'.INP
$rloop1:
$     read jfile$/end=rdone line
$     write ofile$ line
$     goto rloop1
$rdone:
$  close jfile$
$goto rloop
$finish:
$close ofile$
$close ifile$
$write sys$output "Test suite is finished.  Submit the command file RUN.COM"
$write sys$output "to a batch queue, and compare the output to the file"
$write sys$output "EXPECTED.VMS"
$!
$! and go home again.
$!
$set def [-.vms]
