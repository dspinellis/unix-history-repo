: "Create active file and newsgroup hierarchy for new machine"
: "Usage: sh makeactive.sh LIBDIR SPOOLDIR NEWSUSR NEWSGRP"
: '@(#)makeactive	1.18	3/19/86'
LIBDIR=$1
SPOOLDIR=$2
NEWSUSR=$3
NEWSGRP=$4
cat <<"E_O_F" > /tmp/$$groups
net.abortion		All sorts of discussions on abortion.
net.ai			Artificial intelligence discussions.
net.analog		Analog design developments, ideas, and components.
net.announce		Moderated, general announcements of interest to all.
net.announce.newusers	Moderated, explanatory postings for new users.
net.announce.arpa-internet	Announcements from the Arpa world
net.arch		Computer architecture.
net.astro		Astronomy discussions and information.
net.astro.expert	Discussion by experts in astronomy.
net.audio		High fidelity audio.
net.auto		Automobiles, automotive products and laws.
net.auto.tech		Technical aspects of automobiles, et. al.
net.aviation		Aviation rules, means, and methods.
net.bicycle		Bicycles, related products and laws.
net.bio			Biology and related sciences.
net.books		Books of all genres, shapes, and sizes.
net.bugs		General bug reports and fixes.
net.bugs.2bsd		Reports of UNIX* version 2BSD related bugs.
net.bugs.4bsd		Reports of UNIX version 4BSD related bugs.
net.bugs.usg		Reports of USG (System III, V, etc.) bugs.
net.bugs.uucp		Reports of UUCP related bugs.
net.bugs.v7		Reports of UNIX V7 related bugs.
net.cog-eng		Cognitive engineering.
net.college		College, college activities, campus life, etc.
net.columbia		The space shuttle and the STS program.
net.comics		The funnies, old and new.
net.consumers		Consumer interests, product reviews, etc.
net.cooks		Food, cooking, cookbooks, and recipes.
net.crypt		Different methods of data en/decryption.
net.cse			Computer science education.
net.cycle		Motorcycles and related products and laws.
net.database		Database and data management issues and theory.
net.dcom		Data communications hardware and software.
net.decus		DEC* Users' Society newsgroup.
net.emacs		EMACS editors of different flavors.
net.eunice		The SRI Eunice system.
net.followup		Followups to articles in net.general.
net.games		Games and computer games.
net.games.board		Discussion and hints on board games.
net.games.chess		Chess & computer chess.
net.games.emp		Discussion and hints about Empire.
net.games.frp		Discussion about Fantasy Role Playing games.
net.games.go		Discussion about Go.
net.games.hack		Discussion, hints, etc. about the Hack game.
net.games.pbm		Discussion about Play by Mail games.
net.games.rogue		Discussion and hints about Rogue.
net.games.trivia	Discussion about trivia.
net.games.video		Discussion about video games.
net.garden		Gardening, methods and results.
net.general		*Important*, timely announcements of worldwide interest.
net.graphics		Computer graphics, art, animation, image processing,
net.ham-radio		Amateur Radio practices, contests, events, rules, etc.
net.ham-radio.packet	Discussion about packet radio setups.
net.info-terms		All sorts of terminals.
net.internat		Discussion about international standards
net.invest		Investments and the handling of money.
net.jobs		Job announcements, requests, etc.
net.jokes		Jokes and the like.  May be somewhat offensive.
net.jokes.d		Discussions on the content of net.jokes articles
net.kids		Children, their behavior and activities.
net.lan			Local area network hardware and software.
net.lang		Different computer languages.
net.lang.ada		Discussion about Ada*.
net.lang.apl		Discussion about APL.
net.lang.c		Discussion about C.
net.lang.c++		The object-oriented C++ language.
net.lang.f77		Discussion about FORTRAN.
net.lang.forth		Discussion about Forth.
net.lang.lisp		Discussion about LISP.
net.lang.mod2		Discussion about Modula-2.
net.lang.pascal		Discussion about Pascal.
net.lang.prolog		Discussion about PROLOG.
net.lang.st80		Discussion about Smalltalk 80.
net.legal		Legalities and the ethics of law.
net.lsi			Large scale integrated circuits.
net.mag			Magazine summaries, tables of contents, etc.
net.mail		Proposed new mail/network standards.
net.mail.headers	Gatewayed from the ARPA header-people list.
net.math		Mathematical discussions and puzzles.
net.math.stat		Statistics discussion.
net.math.symbolic	Symbolic algebra discussion.
net.med			Medicine and its related products and regulations.
net.micro		Micro computers of all kinds.
net.micro.16k		National Semiconductor 32000 series chips
net.micro.6809		Discussion about 6809's.
net.micro.68k		Discussion about 68k's.
net.micro.apple		Discussion about Apple micros.
net.micro.amiga		Talk about the new Amiga micro.
net.micro.atari8	Discussion about 8 bit Atari micros.
net.micro.atari16	Discussion about 16 bit Atari micros.
net.micro.att		Discussions about AT&T microcomputers 
net.micro.cbm		Discussion about Commodore micros.
net.micro.cpm		Discussion about the CP/M operating system.
net.micro.hp		Discussion about Hewlett/Packard's.
net.micro.mac		Material about the Apple Macintosh & Lisa
net.micro.pc		Discussion about IBM personal computers.
net.micro.ti		Discussion about Texas Instruments.
net.micro.trs-80	Discussion about TRS-80's.
net.misc		Various discussions too short-lived for other groups.
net.motss		Issues pertaining to homosexuality.
net.movies		Reviews and discussions of movies.
net.music		Music lovers' group.
net.music.classical	Discussion about classical music.
net.music.folk		Folks discussing folk music of various sorts
net.music.gdead		A group for (Grateful) Dead-heads
net.music.synth		Synthesizers and computer music
net.net-people		Announcements, requests, etc. about people on the net.
net.news		Discussions of USENET itself.
net.news.adm		Comments directed to news administrators.
net.news.b		Discussion about B news software.
net.news.config		Postings of system down times and interruptions.
net.news.group		Discussions and lists of newsgroups
net.news.newsite	Postings of new site announcements.
net.news.notes		Notesfile software from the Univ. of Illinois.
net.news.sa		Comments directed to system administrators.
net.news.stargate	Discussion about satellite transmission of news.
net.nlang		Natural languages, cultures, heritages, etc.
net.nlang.africa	Discussions about Africa & things African
net.nlang.celts		Group about Celtics.
net.nlang.greek		Group about Greeks.
net.nlang.india		Group for discussion about India & things Indian
net.origins		Evolution versus creationism (sometimes hot!).
net.periphs		Peripheral devices.
net.pets		Pets, pet care, and household animals in general.
net.philosophy		Philosophical discussions.
net.physics		Physical laws, properties, etc.
net.poems		For the posting of poems.
net.politics		Political discussions.  Could get hot.
net.politics.theory	Theory of politics and political systems.
net.puzzle		Puzzles, problems, and quizzes.
net.railroad		Real and model train fans' newsgroup.
net.rec			Recreational/participant sports.
net.rec.birds		Hobbyists interested in bird watching.
net.rec.boat		Hobbyists interested in boating.
net.rec.bridge		Hobbyists interested in bridge.
net.rec.nude		Hobbyists interested in naturist/nudist activities.
net.rec.photo		Hobbyists interested in photography.
net.rec.scuba		Hobbyists interested in SCUBA diving.
net.rec.ski		Hobbyists interested in skiing.
net.rec.skydive		Hobbyists interested in skydiving.
net.rec.wood		Hobbyists interested in woodworking.
net.religion		Religious, ethical, and moral implications of actions.
net.religion.christian	Discussion about form and nature of Christianity
net.religion.jewish	Information and discussion about Judaism.
net.research		Research and computer research.
net.roots		Genealogical matters.
net.rumor		For the posting of rumors.
net.sci			General purpose scientific discussions.
net.sf-lovers		Science fiction lovers' newsgroup.
net.singles		Newsgroup for single people, their activities, etc.
net.social		Like net.singles, but for everyone.
net.sources		For the posting of software packages & documentation.
net.sources.bugs	For bug fixes and features discussion
net.sources.d		For followup discussion on net.sources postings.
net.sources.games	Postings of recreational software
net.sources.mac		Software for the Apple Macintosh
net.space		Space, space programs, space related research, etc.
net.sport		Spectator sports.
net.sport.baseball	Discussion about baseball.
net.sport.football	Discussion about football.
net.sport.hockey	Discussion about hockey.
net.sport.hoops		Discussion about basketball.
net.startrek		Star Trek, the TV show and the movies.
net.suicide		Suicide, laws, ethics, and its causes and effects (!).
net.taxes		Tax laws and advice.
net.test		For testing of network software.  Very boring.
net.text		Text processing.
net.travel		Traveling all over the world.
net.tv			The boob tube, its history, and past and current shows.
net.tv.drwho		Discussion about Dr. Who.
net.tv.soaps		Postings about soap operas.
net.unix		UNIX neophytes group.
net.unix-wizards	Discussions, bug reports, and fixes on and for UNIX.
net.usenix		USENIX Association events and announcements.
net.veg			Vegetarians.
net.video		Video and video components.
net.wanted		Requests for things that are needed.
net.wanted.sources	Requests for software, termcap entries, etc.
net.wines		Wines and spirits.
net.wobegon		"A Prairie Home Companion" radio show discussion.
net.women		Women's rights, discrimination, etc.
net.works		Assorted workstations.
mod.ai			Discussions about Artificial Intelligence
mod.compilers		Discussion about compiler construction, theory, etc.
mod.computers		Discussion about various computers and related.
mod.computers.apollo		Apollo computer systems.
mod.computers.ibm-pc		The IBM PC, PC-XT, and PC-AT.
mod.computers.laser-printers	Laser printers, hardware and software.
mod.computers.pyramid		Pyramid 90x computers.
mod.computers.ridge		Ridge 32 computers and ROS.
mod.computers.sequent		Sequent systems, (esp. Balance 8000).
mod.computers.sun		Sun "workstation" computers
mod.computers.vax		DEC's VAX* line of computers & VMS.
mod.computers.workstations	Various workstation-type computers.
mod.graphics		Graphics software, hardware, theory, etc.
mod.human-nets		Computer aided communications digest.
mod.legal		Discussions of computers and the law.
mod.mac			Apple Macintosh micros -- info, uses, but no programs.
mod.mac.binaries	Encoded public domain programs in binary form.
mod.mac.sources		Public domain software in source code format.
mod.map			Various maps, including UUCP maps
mod.motss		Moderated newsgroup on gay issues and topics
mod.movies		Moderated reviews and discussion of movies
mod.music		Moderated reviews and discussion of things musical
mod.newprod		Announcements of new products of interest to readers
mod.newslists		Postings of news-related statistics and lists
mod.os			Disussions about operating systems and related areas.
mod.os.os9		Discussions about the os9 operating system.
mod.os.unix		Moderated discussion of UNIX* features and bugs.
mod.politics		Discussions on political problems, systems, solutions.
mod.politics.arms-d		Arms discussion digest.
mod.protocols		Various forms and types of FTP protocol discussions.
mod.protocols.appletalk		Applebus hardware & software discussion.
mod.protocols.kermit		Information about the Kermit package.
mod.protocols.tcp-ip		TCP and IP network protocols.
mod.rec			Discussions on pastimes (not currently active)
mod.rec.guns		Discussions about firearms
mod.recipes		A "distributed cookbook" of screened recipes.
mod.risks		Risks to the public from computers & users.
mod.sources		Moderated postings of public-domain sources.
mod.sources.doc		Archived public-domain documentation.
mod.std			Moderated discussion about various standards
mod.std.c		Discussion about C language standards
mod.std.mumps		Discussion for the X11.1 committee on Mumps
mod.std.unix		Discussion for the P1003 committee on UNIX
mod.techreports		Announcements and lists of technical reports.
mod.telecom		Telecommunications digest.
mod.test		Testing of moderated newsgroups -- no moderator
mod.vlsi		Very large scale integrated circuits.
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
	3|4)	;;
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
sort /tmp/$$groups | $LIBDIR/checkgroups | tee /tmp/checkgroups.out
echo the output of checkgroups has been copied into /tmp/checkgroups.out
rm -f /tmp/$$*
