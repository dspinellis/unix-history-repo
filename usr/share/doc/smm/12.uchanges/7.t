.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)7.t	6.3 (Berkeley) 4/14/86
.\"
.SH
.LG
.ce
Section 7
.SM
.sp
.BP hier
Has been updated to reflect the reorganization
to the user and system source.
.BP me
Some new macros were added:
.B \&.sm
(smaller)
and
.B \&.bu
(bulleted paragraph).
The \fIpic\fP, \fIideal\fP, and \fIgremlin\fP preprocessors are
now supported.
.BP words
Two new word lists have been add to \fI/usr/dict\fP.
The 1935 Webster's word list is available as web2
with a supplemental list in web2a.
.IP
Several hundred words have been added to \fI/usr/dict/words\fP,
both general words (``abacus, capsize, goodbye, Hispanic, ...'') and
important technical terms (all the amino acids, many mathematical
terms, a few dinosaurs, ...).
About 10 spelling errors in \fI/usr/dict/words\fP have been corrected.
.IP
Several hundred words that \fIspell\fP derives without
difficulty from existing words (\fIe.g.\fP ``getting'' from ``get''), or that
\fIspell\fP would accept anyway, \fIe.g.\fP ``1st, 2nd'' etc., have been removed
from \fI/usr/dict/words\fP.
