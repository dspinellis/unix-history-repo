# Unix History Repository
The history and evolution of the Unix operating system is made available
as a revision management repository, covering the period from its
inception in 1970 as a 2.5 thousand line kernel and 26 commands,
to 2017 as a widely-used 27 million line system. The 1.1GB repository
contains about half a million commits and more than two thousand merges.
The repository employs Git system for its storage and is hosted on GitHub.
It has been created by synthesizing with custom software 24 snapshots of
systems developed at Bell Labs, the University of California at Berkeley,
and the 386BSD team, two legacy repositories, and the modern repository
of the open source FreeBSD system.
In total, about one thousand individual contributors are identified,
the early ones through primary research.
The data set can be used for empirical research in software engineering,
information systems, and software archaeology.

You can read more details about the contents, creation, and uses of this
repository through [this link](http://www.dmst.aueb.gr/dds/pubs/jrnl/2016-EMPSE-unix-history/html/unix-history.html).

Two repositories are associated with the project:
* [unix-history-repo](https://github.com/dspinellis/unix-history-repo) is
  a repository representing a
  reconstructed version of the Unix history, based on the currently
  available data. This repository will be often automatically regenerated from
  scratch, so this is not a place to make contributions.
  To ensure replicability its users are encouraged to fork it or archive it.
* [unix-history-make](https://github.com/dspinellis/unix-history-make) is
  a repository containing code and metadata used to build the above repository.
  Contributions to this repository are welcomed.

## Project status
The project has achieved its major goal with the establishment of a continuous timeline from 1970 until today. The repository contains:

* snapshots of PDP-7, V1, V2, V3, V4, V5, V6, and V7 Research Edition,
* Unix/32V,
* all available BSD releases,
* the CSRG SCCS history,
* two releases of 386BSD,
* the 386BSD patchkit,
* the FreeBSD 1.0 to 1.1.5 CVS history,
* an import of the FreeBSD repository starting from its initial imports that led to FreeBSD 2.0, and
* the current FreeBSD repository.

The files appear to be added in the repository in chronological order according to their modification time, and large parts of the source code have been attributed to their actual authors.  Commands like `git blame` and `git log` produce the expected results.

The repository contains a number of two-way merges.

* 3 BSD is merged from Unix/32V and Research Edition 6
* Various BSD releases are merged from the development branch and a time point of BSD-SCCS
* FreeBSD 1.0 is merged from Net/2 BSD and 386BSD-0.1-patchkit
* FreeBSD 2.0 is merged from BSD 4.4/Lite1 and FreeBSD 1.1.5

Blame is apportioned appropriately.

## Tags and Branches
The following tags or branch names mark specific releases, listed in rough chronological order.
* Epoch
* Research-PDP7
* Research-V1
* Research-V2
* Research-V3
* Research-V4
* Research-V5
* Research-V6
* BSD-1
* BSD-2
* Research-V7
* Bell-32V
* BSD-3
* BSD-4
* BSD-4\_1\_snap
* BSD-4\_1c\_2
* BSD-4\_2
* BSD-4\_3
* BSD-4\_3\_Reno
* BSD-4\_3\_Net\_1
* BSD-4\_3\_Tahoe
* BSD-4\_3\_Net\_2
* BSD-4\_4
* BSD-4\_4\_Lite1
* BSD-4\_4\_Lite2
* BSD-SCCS-END
* 386BSD-0.0
* 386BSD-0.1
* 386BSD-0.1-patchkit
* FreeBSD-release/1.0, 1.1, 1.1.5
* FreeBSD-release/2.0 2.0.5, 2.1.0, 2.1.5, 2.1.6, 2.1.6.1, 2.1.7, 2.2.0, 2.2.1, 2.2.2, 2.2.5, 2.2.6, 2.2.7, 2.2.8
* FreeBSD-release/3.0.0, 3.1.0, 3.2.0, 3.3.0, 3.4.0, 3.5.0
* FreeBSD-release/4.0.0 4.1.0, 4.1.1, 4.2.0, 4.3.0, 4.4.0, 4.5.0, 4.6.0, 4.6.1, 4.6.2, 4.7.0, 4.8.0, 4.9.0, 4.10.0, 4.11.0
* FreeBSD-release/5.0.0 5.1.0, 5.2.0, 5.2.1, 5.3.0, 5.4.0, 5.5.0
* FreeBSD-release/6.0.0, 6.1.0, 6.2.0, 6.3.0, 6.4.0
* FreeBSD-release/7.0.0, 7.1.0, 7.2.0, 7.3.0, 7.4.0
* FreeBSD-release/8.0.0, 8.1.0, 8.2.0, 8.3.0, 8.4.0
* FreeBSD-release/9.0.0, 9.1.0, 9.2.0
* FreeBSD-release/10.0.0, 10.1.0, 10.2.0, 10.3.0
* FreeBSD-release/11.0.0, 11.0.1

A detailed description of the major tags is available in the
file [releases.md](https://github.com/dspinellis/unix-history-make/blob/master/releases.md).

More tags and branches are available.
* The `-Snapshot-Development` branches denote commits that have been synthesized from a time-ordered sequence of a snapshot's files.
* The `-VCS-Development` tags denote the point along an imported version control history branch where a particular release occurred.

## Cool things you can do

The easiest thing you can do is to
watch the repository's [Gource Visualization](https://youtu.be/S7JB0mhrGCQ).

If you have a broadband network connection and about 1.5GB of free disk space,
you can download the repository and run Git commands that go back decades.
Run
```sh
git clone https://github.com/dspinellis/unix-history-repo
git checkout BSD-Release
```
to get a local copy of the Unix history repository.
### View log across releases
Running
```sh
git log --reverse --date-order
```
will give you commits such as the following

```
commit 64d7600ea5210a9125bd1a06e5d184ef7547d23d
Author: Ken Thompson <ken@research.uucp>
Date:   Tue Jun 20 05:00:00 1972 -0500

    Research V1 development
    Work on file u5.s

    Co-Authored-By: Dennis Ritchie <dmr@research.uucp>
    Synthesized-from: v1/sys
[...]
commit 4030f8318890a026e065bc8926cebefb71e9d353
Author: Ken Thompson <ken@research.uucp>
Date:   Thu Aug 30 19:30:25 1973 -0500

    Research V3 development
    Work on file sys/ken/slp.c

    Synthesized-from: v3
[...]
commit c4999ec655319a01e84d9460d84df824006f9e2d
Author: Dennis Ritchie <dmr@research.uucp>
Date:   Thu Aug 30 19:33:01 1973 -0500

    Research V3 development
    Work on file sys/dmr/kl.c

    Synthesized-from: v3
[...]
commit 355c543c6840fa5f37d8daf2e2eaa735ea6daa4a
Author: Brian W. Kernighan <bwk@research.uucp>
Date:   Tue May 13 19:43:47 1975 -0500

    Research V6 development
    Work on file usr/source/rat/r.g

    Synthesized-from: v6
[...]
commit 0ce027f7fb2cf19b7e92d74d3f09eb02e8fea50e
Author: S. R. Bourne <srb@research.uucp>
Date:   Fri Jan 12 02:17:45 1979 -0500

    Research V7 development
    Work on file usr/src/cmd/sh/blok.c

    Synthesized-from: v7
[...]
Author: Eric Schmidt <schmidt@ucbvax.Berkeley.EDU>
Date:   Sat Jan 5 22:49:18 1980 -0800

    BSD 3 development

    Work on file usr/src/cmd/net/sub.c
```
### View changes to a specific file
Run
```sh
git checkout Research-Release
git log --follow --simplify-merges usr/src/cmd/c/c00.c
```
to see dates on which the C compiler was modified.
### Annotate lines in a specific file by their version
Run
```
git blame -C -C usr/sys/sys/pipe.c
```
to see how the Unix pipe functionality evolved over the years.
```
3cc1108b usr/sys/ken/pipe.c     (Ken Thompson 1974-11-26 18:13:21 -0500  53) 	rf->f_flag = FREAD|FPIPE;
3cc1108b usr/sys/ken/pipe.c     (Ken Thompson 1974-11-26 18:13:21 -0500  54) 	rf->f_inode = ip;
3cc1108b usr/sys/ken/pipe.c     (Ken Thompson 1974-11-26 18:13:21 -0500  55) 	ip->i_count = 2;
[...]
1f183be2 usr/sys/sys/pipe.c     (Ken Thompson 1979-01-10 15:19:35 -0500 122) 	register struct inode *ip;
1f183be2 usr/sys/sys/pipe.c     (Ken Thompson 1979-01-10 15:19:35 -0500 123) 
1f183be2 usr/sys/sys/pipe.c     (Ken Thompson 1979-01-10 15:19:35 -0500 124) 	ip = fp->f_inode;
1f183be2 usr/sys/sys/pipe.c     (Ken Thompson 1979-01-10 15:19:35 -0500 125) 	c = u.u_count;
1f183be2 usr/sys/sys/pipe.c     (Ken Thompson 1979-01-10 15:19:35 -0500 126) 
1f183be2 usr/sys/sys/pipe.c     (Ken Thompson 1979-01-10 15:19:35 -0500 127) loop:
1f183be2 usr/sys/sys/pipe.c     (Ken Thompson 1979-01-10 15:19:35 -0500 128) 
1f183be2 usr/sys/sys/pipe.c     (Ken Thompson 1979-01-10 15:19:35 -0500 129) 	/*
9a9f6b22 usr/src/sys/sys/pipe.c (Bill Joy     1980-01-05 05:51:18 -0800 130) 	 * If error or all done, return.
9a9f6b22 usr/src/sys/sys/pipe.c (Bill Joy     1980-01-05 05:51:18 -0800 131) 	 */
9a9f6b22 usr/src/sys/sys/pipe.c (Bill Joy     1980-01-05 05:51:18 -0800 132) 
9a9f6b22 usr/src/sys/sys/pipe.c (Bill Joy     1980-01-05 05:51:18 -0800 133) 	if (u.u_error)
9a9f6b22 usr/src/sys/sys/pipe.c (Bill Joy     1980-01-05 05:51:18 -0800 134) 		return;
6d632e85 usr/sys/ken/pipe.c     (Ken Thompson 1975-07-17 10:33:37 -0500 135) 	plock(ip);
6d632e85 usr/sys/ken/pipe.c     (Ken Thompson 1975-07-17 10:33:37 -0500 136) 	if(c == 0) {
6d632e85 usr/sys/ken/pipe.c     (Ken Thompson 1975-07-17 10:33:37 -0500 137) 		prele(ip);
6d632e85 usr/sys/ken/pipe.c     (Ken Thompson 1975-07-17 10:33:37 -0500 138) 		u.u_count = 0;
6d632e85 usr/sys/ken/pipe.c     (Ken Thompson 1975-07-17 10:33:37 -0500 139) 		return;
6d632e85 usr/sys/ken/pipe.c     (Ken Thompson 1975-07-17 10:33:37 -0500 140) 	}
```
## How you can help
You can help if you were there at the time, or if you can locate a
source that contains information that is currently missing.
* If your current GitHub account is not linked to your past contributions,
  (you can search them through
  [this page](http://www.spinellis.gr/cgi-bin/namegrep.pl)),
  associate your past email with your current account through your
  [GitHub account settings](https://github.com/settings/emails).
  (Contact me for instructions on how to add email addresses to which you no
  longer have access.)
* Look for errors and omissions in the
  [files that map file paths to authors](https://github.com/dspinellis/unix-history-make/blob/master/src/author-path).
* Look for parts of the system that have not yet been attributed
  [in these files](https://github.com/dspinellis/unix-history-make/blob/master/src/unmatched)
  and propose suitable attributions.
  Keep in mind that attributions for parts that were developed in one place
  and modified elsewhere (e.g. developed at Bell Labs and modified at Berkeley)
  should be for the person who did the modification, not the original author.
* Look for authors whose identifier starts with ```x-``` in the
  author id to name map files for
  [Bell Labs](https://github.com/dspinellis/unix-history-make/blob/master/src/bell.au),
  and
  [Berkeley](https://github.com/dspinellis/unix-history-make/blob/master/src/berkeley.au),
  and provide or confirm their actual login identifier.
  (The one used is a guess.)
* Contribute a path regular expression to contributor map file
  (see [v7.map](https://github.com/dspinellis/unix-history-make/blob/master/src/author-path/v7)) for
  [4.2BSD](http://www.tuhs.org/Archive/4BSD/Distributions/4.2BSD/),
  [4.3BSD](http://www.tuhs.org/Archive/4BSD/Distributions/4.3BSD/),
  [4.3BSD-Reno](http://www.tuhs.org/Archive/4BSD/Distributions/4.3BSD-Reno/),
  [4.3BSD-Tahoe](http://www.tuhs.org/Archive/4BSD/Distributions/4.3BSD-Tahoe/),
  [4.3BSD-Alpha](http://www.tuhs.org/Archive/4BSD/Distributions/4.3BSD-Alpha/), and
  [Net2](http://www.tuhs.org/Archive/4BSD/Distributions/Net2/).
* Import further branches, such as 2BSD, NetBSD, OpenBSD, and _Plan 9 from Bell Labs_.

## Re-creating the historical repository from scratch
The -make repository is provided to share and document the creation process, rather than as a bullet-proof way to get consistent and repeatable results.  For instance, importing the snapshots on a system that is case-insensitive (NTFS, HFS Plus with default settings) will result in a few files getting lost.

### Prerequisites
* Git
* Perl
* The Perl modules `VCS::SCCS` and `Git::FastExport`
(Install with `sudo cpanm VCS::SCCS Git::FastExport`.)
* If compiling patch under GNU/Linux and library `libbsd`
(e.g. the `libbsd-dev` package).
* Sudo (and authorization to use it to mount ISO images)

### Repository creation
The -repo repository can be created with the following commands.
```sh
make
./import.sh
```

## Adding a single source
If you want to add a new source without running the full import process,
you can do the following.

* Prepare the source's maps and data
* `cd` to the repo directory
* Checkout the repo at the point where the new source will branch out
* Run a Perl command such as the following.

```
perl ../import-dir.pl [-v] -m Research-V7 -c ../author-path/Bell-32V \
-n ../bell.au -r Research-V7 -i ../ignore/Bell-32V \
$ARCHIVE/32v Bell 32V -0500 | gfi
```

## Further reading
- Diomidis Spinellis. A repository of Unix history and evolution. *Empirical Software Engineering*, 2017. doi:10.1007/s10664-016-9445-5.
[HTML](http://www.dmst.aueb.gr/dds/pubs/jrnl/2016-EMPSE-unix-history/html/unix-history.html), [PDF](http://www.dmst.aueb.gr/dds/pubs/jrnl/2016-EMPSE-unix-history/html/unix-history.pdf)
- Diomidis Spinellis. A repository with 44 years of Unix evolution. In *MSR '15: Proceedings of the 12th Working Conference on Mining Software Repositories*, pages 13-16. IEEE, 2015. Best Data Showcase Award. [PDF](http://www.dmst.aueb.gr/dds/pubs/conf/2015-MSR-Unix-History/html/Spi15c.pdf), [HTML](http://www.dmst.aueb.gr/dds/pubs/conf/2015-MSR-Unix-History/html/Spi15c.html), [poster](http://www.dmst.aueb.gr/dds/pubs/conf/2015-MSR-Unix-History/html/poster.pdf).
- Warren Toomey, First Edition Unix: Its Creation and Restoration, in *IEEE Annals of the History of Computing*, vol. 32, no. 3, pp. 74-82, July-Sept. 2010. doi:10.1109/MAHC.2009.55. [PDF](http://www.tuhs.org/Archive/Documentation/Papers/1eUnix_creation_restoration.pdf)
- Warren Toomey,  The Restoration of Early UNIX Artifacts, in *USENIX ATC '09: 2009 USENIX Annual Technical Conference*. 2009. [PDF](https://www.usenix.org/legacy/events/usenix09/tech/full_papers/toomey/toomey.pdf)
- Diomidis Spinellis, Panagiotis Louridas, and Maria Kechagia. An exploratory study on the evolution of C programming in the Unix operating system. In Qing Wang and Guenther Ruhe, editors, *ESEM '15: 9th International Symposium on Empirical Software Engineering and Measurement*, pages 54–57. IEEE, October 2015. [HTML](http://www.dmst.aueb.gr/dds/pubs/conf/2015-ESEM-CodeStyle/html/SLK15.html), [PDF](http://www.dmst.aueb.gr/dds/pubs/conf/2015-ESEM-CodeStyle/html/SLK15.pdf)
- Diomidis Spinellis, Panos Louridas, and Maria Kechagia. The evolution of C programming practices: A study of the Unix operating system 1973–2015. In Willem Visser and Laurie Williams, editors, *ICSE '16: Proceedings of the 38th International Conference on Software Engineering*, May 2016. Association for Computing Machinery. doi:10.1145/2884781.2884799. [PDF](http://www.dmst.aueb.gr/dds/pubs/conf/2016-ICSE-ProgEvol/html/SLK16.pdf), [HTML](http://www.dmst.aueb.gr/dds/pubs/conf/2016-ICSE-ProgEvol/html/SLK16.html)
- Wikipedia: The Free Encyclopedia
    - [History of Unix](https://en.wikipedia.org/wiki/History_of_Unix)
    - [List of Unix systems](https://en.wikipedia.org/wiki/List_of_Unix_systems)
    - [Research Unix](https://en.wikipedia.org/wiki/Research_Unix)
    - [Berkeley Software Distribution](http://en.wikipedia.org/wiki/BSD_Unix)
- TUHS: The Unix Heritage Society
    - [The Unix Tree](http://minnie.tuhs.org/cgi-bin/utree.pl)
- Historical documents and data
    - [PDP-7 Unix restoration project](https://github.com/DoctorWkt/pdp7-unix)
    - [First Edition Unix printout - 1972](http://bitsavers.trailing-edge.com/pdf/bellLabs/unix/PreliminaryUnixImplementationDocument_Jun72.pdf)
    - [Unix 32/V Report - 1978](http://cm.bell-labs.com/cm/cs/who/dmr/otherports/32vscan.pdf)
    - [Berkeley CSRG Archive CD-ROMs](https://www.mckusick.com/csrg/index.html)
- Studies
    - [M. Douglas McIlroy. A Research UNIX Reader: Annotated Excerpts from the Programmer's Manual, 1971-1986](https://archive.org/details/a_research_unix_reader)
    - [Michael S. Mahoney. Unix at the Bell Labs - People](http://www.princeton.edu/~hos/Mahoney/unixpeople.htm)

## Acknowledgements
* The following people helped with Bell Labs login identifiers.
 * Brian W. Kernighan
 * Doug McIlroy
 * Arnold D. Robbins
* The following people helped with *BSD login identifiers.
 * Clem Cole
 * Era Eriksson
 * Mary Ann Horton
 * Warner Losh
 * Kirk McKusick
 * Jeremy C. Reed
 * Ingo Schwarze
 * Anatole Shaw
* The BSD SCCS import code is based on work by
 * [H. Merijn Brand](http://search.cpan.org/~hmbrand/VCS-SCCS-0.20/SCCS.pm)
 * [Jonathan Gray](https://github.com/jonathangray/csrg-git-patches/)

## Build identification
* Software URL: https://github.com/dspinellis/unix-history-make
* Software SHA: 6944aefdab2757f133a35a15554e5c900931195a
* Build timestamp: 2017-03-23 18:30:32 UTC
