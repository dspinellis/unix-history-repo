: "Create active file and newsgroup hierarchy for new machine"
: "Usage: sh makeactive.sh LIBDIR SPOOLDIR NEWSUSR NEWSGRP"
: '@(#)makeactive	1.24	9/24/87'
LIBDIR=$1
SPOOLDIR=$2
NEWSUSR=$3
NEWSGRP=$4
cat <<"E_O_F" | sort > /tmp/$$groups
general	Articles that should be read by everyone on your local system
comp.ai			Artificial intelligence discussions.
comp.ai.digest		Artificial Intelligence discussions. (Moderated)
comp.arch		Computer architecture.
comp.binaries.amiga	Encoded public domain programs in binary. (Moderated)
comp.binaries.atari.st	Binary-only postings for the Atari ST. (Moderated)
comp.binaries.ibm.pc	Binary-only postings for IBM PC/MS-DOS. (Moderated)
comp.binaries.mac	Encoded Macintosh programs in binary. (Moderated)
comp.bugs.2bsd		Reports of UNIX* version 2BSD related bugs.
comp.bugs.4bsd		Reports of UNIX version 4BSD related bugs.
comp.bugs.4bsd.ucb-fixes	Bug reports/fixes for BSD Unix. (Moderated)
comp.bugs.misc		General UNIX bug reports and fixes (incl V7, uucp)
comp.bugs.sys5		Reports of USG (System III, V, etc.) bugs.
comp.cog-eng		Cognitive engineering.
comp.compilers		Compiler construction, theory, etc. (Moderated)
comp.databases		Database and data management issues and theory.
comp.dcom.lans		Local area network hardware and software.
comp.dcom.modems	Data communications hardware and software.
comp.dcom.telecom	Telecommunications digest. (Moderated)
comp.doc		Archived public-domain documentation. (Moderated)
comp.doc.techreports	Lists of technical reports. (Moderated)
comp.edu		Computer science education.
comp.emacs		EMACS editors of different flavors.
comp.graphics		Computer graphics, art, animation, image processing.
comp.graphics.digest	Graphics software, hardware, theory, etc. (Moderated)
comp.hypercube		Massively parallel systems: hardware & software. (Moderated)
comp.lang.ada		Discussion about Ada*.
comp.lang.apl		Discussion about APL.
comp.lang.c		Discussion about C.
comp.lang.c++		The object-oriented C++ language.
comp.lang.forth		Discussion about Forth.
comp.lang.fortran	Discussion about FORTRAN.
comp.lang.lisp		Discussion about LISP.
comp.lang.misc		Different computer languages not specifically listed.
comp.lang.modula2	Discussion about Modula-2.
comp.lang.pascal	Discussion about Pascal.
comp.lang.prolog	Discussion about PROLOG.
comp.lang.smalltalk	Discussion about Smalltalk 80.
comp.laser-printers	Laser printers, hardware & software. (Moderated)
comp.lsi		Large scale integrated circuits.
comp.mail.elm		Discussion and fixes for ELM mail system.
comp.mail.headers	Gatewayed from the ARPA header-people list.
comp.mail.maps		Various maps, including UUCP maps. (Moderated)
comp.mail.misc		General discussions about computer mail.
comp.mail.uucp		Mail in the uucp network environment.
comp.misc		General topics about computers not covered elsewhere.
comp.newprod		Announcements of new products of interest. (Moderated)
comp.org.decus		DEC* Users' Society newsgroup.
comp.org.fidonet	FidoNews digest, official news of FidoNet Assoc. (Moderated)
comp.org.usenix		USENIX Association events and announcements.
comp.os.cpm		Discussion about the CP/M operating system.
comp.os.eunice		The SRI Eunice system.
comp.os.minix		Discussion of Tanenbaum's MINIX system.
comp.os.misc		General OS-oriented discussion not carried elsewhere.
comp.os.os9		Discussions about the os9 operating system. (Moderated)
comp.os.research	Operating systems and related areas. (Moderated)
comp.os.vms		DEC's VAX* line of computers & VMS.
comp.periphs		Peripheral devices.
comp.protocols.appletalk	Applebus hardware & software.
comp.protocols.kermit	Info about the Kermit package. (Moderated)
comp.protocols.misc	Various forms and types of FTP protocol.
comp.protocols.tcp-ip	TCP and IP network protocols.
comp.risks		Risks to the public from computers & users. (Moderated)
comp.society		The impact of technology on society. (Moderated)
comp.sources.amiga	Source code-only postings for the Amiga. (Moderated)
comp.sources.atari.st	Source code-only postings for the Atari ST. (Moderated)
comp.sources.bugs	Bug reports, fixes, discussion for posted sources
comp.sources.d		For any discussion of source postings.
comp.sources.games	Postings of recreational software. (Moderated)
comp.sources.mac	Software for the Apple Macintosh. (Moderated)
comp.sources.misc	Posting of software . (Moderated)
comp.sources.unix	Postings of public-domain sources. (Moderated)
comp.sources.wanted	Requests for software and fixes.
comp.std.c		Discussion about C language standards. (Moderated)
comp.std.internat	Discussion about international standards.
comp.std.misc		Discussion about various standards. (Moderated)
comp.std.mumps		Discussion for the X11.1 committee on Mumps. (Moderated)
comp.std.unix		Discussion for the P1003 committee on UNIX. (Moderated)
comp.sys.amiga		Commodore Amiga: info&uses, but no programs.
comp.sys.apollo		Apollo computer systems.
comp.sys.apple		Discussion about Apple micros.
comp.sys.atari.8bit	Discussion about 8 bit Atari micros.
comp.sys.atari.st	Discussion about 16 bit Atari micros.
comp.sys.att		Discussions about AT&T microcomputers.
comp.sys.cbm		Discussion about Commodore micros.
comp.sys.dec		Discussions about DEC computer systems.
comp.sys.hp		Discussion about Hewlett-Packard equipment.
comp.sys.ibm.pc		Discussion about IBM personal computers.
comp.sys.ibm.pc.digest	The IBM PC, PC-XT, and PC-AT. (Moderated)
comp.sys.intel		Disucussions about Intel systems and parts.
comp.sys.m6809		Discussion about 6809's.
comp.sys.m68k		Discussion about 68k's.
comp.sys.m68k.pc	Discussion about 68k-based PCs. (Moderated)
comp.sys.mac		Discussions about the Apple Macintosh & Lisa.
comp.sys.mac.digest	Apple Macintosh: info&uses, but no programs. (Moderated)
comp.sys.masscomp	The Masscomp line of computers. (Moderated)
comp.sys.misc		Discussion about computers of all kinds.
comp.sys.nsc.32k	National Semiconductor 32000 series chips.
comp.sys.pyramid	Pyramid 90x computers.
comp.sys.ridge		Ridge 32 computers and ROS. 
comp.sys.sequent	Sequent systems, (esp. Balance 8000). (Moderated)
comp.sys.sun		Sun "workstation" computers. (Moderated)
comp.sys.tandy		Discussion about TRS-80's.
comp.sys.ti		Discussion about Texas Instruments.
comp.sys.workstations	Various workstation-type computers. (Moderated)
comp.terminals		All sorts of terminals.
comp.text		Text processing issues and methods.
comp.text.desktop	Technolgy & techniques of desktop publishing. (Moderated)
comp.unix		Discussion of UNIX* features and bugs. (Moderated)
comp.unix.questions	UNIX neophytes group.
comp.unix.wizards	Discussions, bug reports, and fixes on and for UNIX.
comp.unix.xenix		Discussion about the Xenix OS.
comp.windows.misc	Various issues about windowing systems.
comp.windows.news	Sun Microsystems' NewS window system.
comp.windows.x		Discussion about the X Window System.
misc.consumers		Consumer interests, product reviews, etc.
misc.consumers.house	Discussion about owning and maintaining a house.
misc.forsale		Short, tasteful postings about items for sale.
misc.handicap		Items of interest for/about the handicapped. (Moderated)
misc.headlines		Current interest: drug testing, terrorism, etc.
misc.invest		Investments and the handling of money.
misc.jobs.misc		Discussion about employment, workplaces, careers.
misc.jobs.offered	Announcements of positions available.
misc.jobs.resumes	Postings of resumes and "situation wanted" articles.
misc.kids		Children, their behavior and activities.
misc.legal		Legalities and the ethics of law.
misc.misc		Various discussions not fitting in any other group.
misc.psi		Paranormal abilities and experiences. (Moderated)
misc.taxes		Tax laws and advice.
misc.test		For testing of network software.  Very boring.
misc.wanted		Requests for things that are needed (NOT software).
news.admin		Comments directed to news administrators.
news.announce.conferences	Calls for papers and conference announcements. (Moderated)
news.announce.important	General announcements of interest to all. (Moderated)
news.announce.newusers	Explanatory postings for new users. (Moderated)
news.config		Postings of system down times and interruptions.
news.groups		Discussions and lists of newsgroups.
news.lists		News-related statistics and lists. (Moderated)
news.misc		Discussions of USENET itself.
news.newsites		Postings of new site announcements.
news.software.b		Discussion about B news software.
news.software.notes	Notesfile software from the Univ. of Illinois.
news.stargate		Discussion about satellite transmission of news.
news.sysadmin		Comments directed to system administrators.
rec.arts.books		Books of all genres, shapes, and sizes.
rec.arts.comics		The funnies, old and new.
rec.arts.drwho		Discussion about Dr. Who.
rec.arts.movies		Discussions of movies and movie making.
rec.arts.movies.reviews	Reviews of movies. (Moderated)
rec.arts.poems		For the posting of poems.
rec.arts.sf-lovers	Science fiction lovers' newsgroup.
rec.arts.startrek	Star Trek, the TV show and the movies.
rec.arts.tv		The boob tube, its history, and past and current shows.
rec.arts.tv.soaps	Postings about soap operas.
rec.arts.wobegon	"A Prairie Home Companion" radio show discussion.
rec.audio		High fidelity audio.
rec.autos		Automobiles, automotive products and laws.
rec.autos.tech		Technical aspects of automobiles, et. al.
rec.aviation		Aviation rules, means, and methods.
rec.bicycles		Bicycles, related products and laws.
rec.birds		Hobbyists interested in bird watching.
rec.boats		Hobbyists interested in boating.
rec.food.cooking	Food, cooking, cookbooks, and recipes.
rec.food.drink		Wines and spirits.
rec.food.recipes	Recipes from the USENET Cookbook (troff & text). (Moderated)
rec.food.veg		Vegetarians.
rec.games.board		Discussion and hints on board games.
rec.games.bridge	Hobbyists interested in bridge.
rec.games.chess		Chess & computer chess.
rec.games.empire	Discussion and hints about Empire.
rec.games.frp		Discussion about Fantasy Role Playing games.
rec.games.go		Discussion about Go.
rec.games.hack		Discussion, hints, etc. about the Hack game.
rec.games.misc		Games and computer games.
rec.games.pbm		Discussion about Play by Mail games.
rec.games.rogue		Discussion and hints about Rogue.
rec.games.trivia	Discussion about trivia.
rec.games.video		Discussion about video games.
rec.gardens		Gardening, methods and results.
rec.guns		Discussions about firearms. (Moderated)
rec.ham-radio		Amateur Radio practices, contests, events, rules, etc.
rec.ham-radio.packet	Discussion about packet radio setups.
rec.humor		Jokes and the like.  May be somewhat offensive.
rec.humor.d		Discussions on the content of rec.humor articles.
rec.humor.spc		The Stupid People's Court! (Moderated)
rec.mag			Magazine summaries, tables of contents, etc.
rec.mag.otherrealms	Edited science fiction & fantasy "magazine". (Moderated)
rec.misc		General topics about recreational/participant sports.
rec.motorcycles		Motorcycles and related products and laws.
rec.music.classical	Discussion about classical music.
rec.music.folk		Folks discussing folk music of various sorts.
rec.music.gaffa		Progressive music (e.g., Kate Bush). (Moderated)
rec.music.gdead		A group for (Grateful) Dead-heads.
rec.music.makers	For performers and their discussions.
rec.music.misc		Music lovers' group.
rec.music.synth		Synthesizers and computer music.
rec.nude		Hobbyists interested in naturist/nudist activities.
rec.pets		Pets, pet care, and household animals in general.
rec.photo		Hobbyists interested in photography.
rec.puzzles		Puzzles, problems, and quizzes.
rec.railroad		Real and model train fans' newsgroup.
rec.scuba		Hobbyists interested in SCUBA diving.
rec.skiing		Hobbyists interested in skiing.
rec.skydiving		Hobbyists interested in skydiving.
rec.sport.baseball	Discussion about baseball.
rec.sport.basketball	Discussion about basketball.
rec.sport.football	Discussion about football.
rec.sport.hockey	Discussion about hockey.
rec.sport.misc		Spectator sports.
rec.travel		Traveling all over the world.
rec.video		Video and video components.
rec.woodworking		Hobbyists interested in woodworking.
sci.astro		Astronomy discussions and information.
sci.bio			Biology and related sciences.
sci.crypt		Different methods of data en/decryption.
sci.electronics		Circuits, theory, electrons and discussions.
sci.lang		Natural languages, communication, etc.
sci.math		Mathematical discussions and pursuits.
sci.math.stat		Statistics discussion.
sci.math.symbolic	Symbolic algebra discussion.
sci.med			Medicine and its related products and regulations.
sci.med.aids		AIDS -- treatment, consequences, etc. (Moderated)
sci.misc		Short-lived discussions on subjects in the sciences.
sci.philosophy.tech	Technical philosophy: math, science, logic, etc. 
sci.physics		Physical laws, properties, etc.
sci.research		Research methods, funding, ethics, and whatever.
sci.space		Space, space programs, space related research, etc.
sci.space.shuttle	The space shuttle and the STS program.
soc.college		College, college activities, campus life, etc.
soc.culture.african	Discussions about Africa & things African.
soc.culture.celtic	Group about Celtics (*not* basketball!).
soc.culture.greek	Group about Greeks.
soc.culture.indian	Group for discussion about India & things Indian.
soc.culture.jewish	Group for discussion about Jewish culture & religion.
soc.culture.misc	Group for discussion about other cultures.
soc.human-nets		Computer aided communications digest. (Moderated)
soc.men			Issues related to men, their problems & relationships.
soc.misc		Socially-oriented topics not in other groups.
soc.motss		Issues pertaining to homosexuality.
soc.net-people		Announcements, requests, etc. about people on the net.
soc.politics		Political problems, systems, solutions. (Moderated)
soc.politics.arms-d	Arms discussion digest. (Moderated)
soc.religion.christian	Christianity and related topics. (Moderated)
soc.roots		Genealogical matters.
soc.singles		Newsgroup for single people, their activities, etc.
soc.women		Women's rights, discrimination, etc.
talk.abortion		All sorts of discussions and arguments on abortion.
talk.bizarre		The unusual, bizarre, curious, and often stupid.
talk.origins		Evolution versus creationism (sometimes hot!).
talk.philosophy.misc	Philosophical musings on all topics.
talk.politics.misc	Political discussions and ravings of all kinds.
talk.politics.theory	Theory of politics and political systems.
talk.religion.misc	Religious, ethical, & moral implications.
talk.religion.newage	Esoteric and minority religions & philosophies.
talk.rumors		For the posting of rumors.
E_O_F
: if active file is empty, create it
if test ! -s $LIBDIR/active
then
	sed 's/[ 	].*/ 00000 00001/' /tmp/$$groups > $LIBDIR/active
	cat <<'E_O_F' >>$LIBDIR/active
control 00000 00001
junk 00000 00001
E_O_F
	set - group 0 1
else
: make sure it is in the new format
	set - `sed 1q $LIBDIR/active`
	case $# in
	4)	ed - $LIBDIR/active << 'EOF'
g/^mod\./s/y$/m/
w
q
EOF
		;;
	3)	;;
	2)	ed - $LIBDIR/active << 'EOF'
1,$s/$/ 00001/
w
q
EOF
		echo
		echo Active file updated to new format.
		echo You must run expire immediately after this install
		echo is done to properly update the tables.;;
	*) echo Active file is in unrecognized format. Not upgraded.;;
	esac
fi
if test $# -eq 3 -o $# -eq 2
then
	(sed '/^!net/!d
s/^!//
s!^!/!
s!$! /s/$/ n/!
' $LIBDIR/ngfile
	echo '/ n$/!s/$/ y/') >/tmp/$$sed
	mv $LIBDIR/active $LIBDIR/oactive
	sed -f /tmp/$$sed $LIBDIR/oactive >$LIBDIR/active
	chown $NEWSUSR $LIBDIR/active
	chgrp $NEWSGRP $LIBDIR/active
	chmod 644 $LIBDIR/active
fi
cat << EOF | cat - /tmp/$$groups | $LIBDIR/checkgroups | tee /tmp/checkgroups.out
From: make update

EOF
echo the output of checkgroups has been copied into /tmp/checkgroups.out
rm -f /tmp/$$*
