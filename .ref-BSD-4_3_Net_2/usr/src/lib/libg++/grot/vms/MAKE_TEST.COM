$set noon
$! go to the directory where the test files reside.
$set def [-.tests]
$! make sure that the symbols are defined properly.
$genclass:==@gnu_cc:[000000]genclass
$cxshare:==@gnu_cc:[000000]cxshare
$!
$genclass "int" val defs
$genclass -2 "int" val "int" val AVLMap
$genclass "int" val AVLSet
$genclass "int" val AVec
$genclass "int" val BSTSet
$genclass "int" val Bag
$genclass "int" val CHBag
$genclass -2 "int" val "int" val CHMap
$genclass "int" val CHSet
$genclass "int" val DLDeque
$genclass "int" val DLList
$genclass "int" val Deque
$genclass "int" val FPQueue
$genclass "int" val FPStack
$genclass "int" val FPlex
$genclass "int" val List
$genclass "int" val MPlex
$genclass -2 "int" val "int" val Map
$genclass "int" val OSLBag
$genclass "int" val OSLSet
$genclass "int" val OXPBag
$genclass "int" val OXPSet
$genclass "int" val PHPQ
$genclass "int" val Plex
$genclass "int" val PQ
$genclass "int" val Queue
$genclass -2 "int" val "int" val RAVLMap
$genclass "int" val RPlex
$genclass "int" val SLBag
$genclass "int" val SLList
$genclass "int" val SLQueue
$genclass "int" val SLSet
$genclass "int" val SLStack
$genclass "int" val Set
$genclass "int" val SplayBag
$genclass -2 "int" val "int" val SplayMap
$genclass "int" val SplayPQ
$genclass "int" val SplaySet
$genclass "int" val Stack
$genclass "int" val VHBag
$genclass -2 "int" val "int" val VHMap
$genclass "int" val VHSet
$genclass "int" val VOHSet
$genclass "int" val VQueue
$genclass "int" val VStack
$genclass "int" val Vec
$genclass "int" val XPBag
$genclass "int" val XPDeque
$genclass "int" val XPPQ
$genclass "int" val XPQueue
$genclass "int" val XPSet
$genclass "int" val XPStack
$genclass "int" val XPlex
$!
$lib/create ilib
$!
$gxx/debug IAVEC.CC
$lib ilib IAVEC
$!
$gxx/debug IAVLMAP.CC
$lib ilib IAVLMAP
$!
$gxx/debug IAVLSET.CC
$lib ilib IAVLSET
$!
$gxx/debug IBAG.CC
$lib ilib IBAG
$!
$gxx/debug IBSTSET.CC
$lib ilib IBSTSET
$!
$gxx/debug ICHBAG.CC
$lib ilib ICHBAG
$!
$gxx/debug ICHMAP.CC
$lib ilib ICHMAP
$!
$gxx/debug ICHSET.CC
$lib ilib ICHSET
$!
$gxx/debug IDLLIST.CC
$lib ilib IDLLIST
$!
$gxx/debug IFPLEX.CC
$lib ilib IFPLEX
$!
$gxx/debug ILIST.CC
$lib ilib ILIST
$!
$gxx/debug IMAP.CC
$lib ilib IMAP
$!
$gxx/debug IMPLEX.CC
$lib ilib IMPLEX
$!
$gxx/debug IOSLBAG.CC
$lib ilib IOSLBAG
$!
$gxx/debug IOSLSET.CC
$lib ilib IOSLSET
$!
$gxx/debug IOXPBAG.CC
$lib ilib IOXPBAG
$!
$gxx/debug IOXPSET.CC
$lib ilib IOXPSET
$!
$gxx/debug IPHPQ.CC
$lib ilib IPHPQ
$!
$gxx/debug IPLEX.CC
$lib ilib IPLEX
$!
$gxx/debug IPQ.CC
$lib ilib IPQ
$!
$gxx/debug IRAVLMAP.CC
$lib ilib IRAVLMAP
$!
$gxx/debug IRPLEX.CC
$lib ilib IRPLEX
$!
$gxx/debug ISET.CC
$lib ilib ISET
$!
$gxx/debug ISLBAG.CC
$lib ilib ISLBAG
$!
$gxx/debug ISLLIST.CC
$lib ilib ISLLIST
$!
$gxx/debug ISLSET.CC
$lib ilib ISLSET
$!
$gxx/debug ISPLAYBAG.CC
$lib ilib ISPLAYBAG
$!
$gxx/debug ISPLAYMAP.CC
$lib ilib ISPLAYMAP
$!
$gxx/debug ISPLAYPQ.CC
$lib ilib ISPLAYPQ
$!
$gxx/debug ISPLAYSET.CC
$lib ilib ISPLAYSET
$!
$gxx/debug IVEC.CC
$lib ilib IVEC
$!
$gxx/debug IVHBAG.CC
$lib ilib IVHBAG
$!
$gxx/debug IVHMAP.CC
$lib ilib IVHMAP
$!
$gxx/debug IVHSET.CC
$lib ilib IVHSET
$!
$gxx/debug IVOHSET.CC
$lib ilib IVOHSET
$!
$gxx/debug IVQUEUE.CC
$lib ilib IVQUEUE
$!
$gxx/debug IVSTACK.CC
$lib ilib IVSTACK
$!
$gxx/debug IXPBAG.CC
$lib ilib IXPBAG
$!
$gxx/debug IXPLEX.CC
$lib ilib IXPLEX
$!
$gxx/debug IXPPQ.CC
$lib ilib IXPPQ
$!
$gxx/debug IXPSET.CC
$lib ilib IXPSET
$!
$gxx/debug TBAG.CC
$cxshare TBAG+ilib/lib/nomap
$gxx/debug TBITSET.CC
$cxshare TBITSET/nomap
$gxx/debug TBITSTRING.CC
$cxshare TBITSTRING/nomap
$gxx/debug TCOMPLEX.CC
$cxshare TCOMPLEX/nomap
$gxx/debug TCURSES.CC
$cxshare TCURSES/nomap
$gxx/debug TDEQUE.CC
$cxshare TDEQUE+ilib/lib/nomap
$gxx/debug TFILE.CC
$cxshare TFILE/nomap
$gxx/debug TFIX.CC
$cxshare TFIX/nomap
$gxx/debug TFIX16.CC
$cxshare TFIX16/nomap
$gxx/debug TFIX24.CC
$cxshare TFIX24/nomap
$gxx/debug TGETOPT.CC
$cxshare TGETOPT/nomap
$gxx/debug TGWRAPPER.CC
$cxshare TGWRAPPER/nomap
$gxx/debug TINTEGER.CC
$cxshare TINTEGER/nomap
$gxx/debug TLIST.CC
$cxshare TLIST+ilib/lib/nomap
$gxx/debug TLLIST.CC
$cxshare TLLIST+ilib/lib/nomap
$gxx/debug TMAP.CC
$cxshare TMAP+ilib/lib/nomap
$gxx/debug TOBSTACK.CC
$cxshare TOBSTACK/nomap
$gxx/debug TPLEX.CC
$cxshare TPLEX+ilib/lib/nomap
$gxx/debug TPQ.CC
$cxshare TPQ+ilib/lib/nomap
$gxx/debug TQUEUE.CC
$cxshare TQUEUE+ilib/lib/nomap
$gxx/debug TRANDOM.CC
$cxshare TRANDOM/nomap
$gxx/debug TRATIONAL.CC
$cxshare TRATIONAL/nomap
$gxx/debug TSET.CC
$cxshare TSET+ilib/lib/nomap
$gxx/debug TSTACK.CC
$cxshare TSTACK+ilib/lib/nomap
$gxx/debug TSTRING.CC
$cxshare TSTRING/nomap
$gxx/debug TVEC.CC
$cxshare TVEC+ilib/lib/nomap
$gxx/debug TWRAPPER.CC
$cxshare TWRAPPER/nomap
$!
$!
$open ofile$ run.com/write
$write ofile$ "$set nover"
$OPEN IFILE$ EXPECTED.LIST
$loop:
$read/end=finish ifile$ line
$write ofile$ "$write sys$output ""''line'"""
$write ofile$ "$run ''line'"
$inp=f$search("''line'.INP",1)
$if inp.eqs."" then goto lp1
$open jfile$ 'line'.INP
$loop1:
$read jfile$/end=done line
$write ofile$ line
$goto loop1
$done:
$close jfile$
$lp1:
$goto loop
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
