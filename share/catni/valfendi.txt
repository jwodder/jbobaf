Morphology Algorithm
Revision 5.3pre2, 25 Dec 2004

The following algorithm for resolution of Lojban text into individual words
from sounds, stress, and pause is an alternative to the Parsing Expression
Grammar which will become official. Should discrepancies occur between this
algorithm and the PEG, please contact me or xorxes. While the algorithm looks
very complicated, almost all of it is resolving special cases, and performing
what error detection and correction may be possible.


We have a string representing the speech stream, marked with stress and
pauses.  We want to break it up into words.

1.  Scan the line from left to right. Convert all spaces to pauses
    unless preceded by comma; convert space to comma if preceded by comma.
2.  Break at all pauses (cannot pause in the middle of a word).
3.  Pick the first piece that has not been resolved.
  A.  If the piece ends in a consonant:
    I.  Make a decapitalized copy of the string with commas removed.
    II. Search backward in the string for a place in the string that is
        preceded by "la", "lai", "la'i", or "doi" where the 'l' or 'd' is not
        immediately preceded by a consonant and such that the character at
        that place is a consonant.
    III.If you found such a place:
      a.  Split before the place and call the second part a cmene.
      b.  Search backward in the first part for a consonant. If it is not
          the first character, split before it and resolve the second part as
          a cmavo.
    IV. If you did not find such a place, resolve the piece as a cmene.
  B.  If the piece ends in 'y':
    I.  Search backward for a consonant.
    II. If you find one:
      a.  If it is preceded by a consonant, resolve the piece as an error.
      b.  If it is not preceded by a consonant, break before the consonant
          and resolve the second piece as a cmavo.
    III.If you do not find one, resolve the piece as a cmavo.
  C.  If the piece does not end in 'y' or a consonant and has no consonant
      that is adjacent to a consonant when 'y' is removed:
    I.  Number the consonants starting with 1 and find the last one whose
        number is a power of 2.
    II. If this consonant is the first letter in the piece or there are no
        consonants, resolve the string as a cmavo.
    III.If this consonant is not the first letter, split before it.
  D.  If the piece contains 'y' and no consonant following the last 'y' is
      followed two letters later, not counting apostrophes and commas, by a
      vowel, split it after 'y'. (e.g. ly.Ebucy.Obukybu.DENpabu)
  E.  If the piece contains a consonant followed two letters later, not
      counting apostrophes and commas, by a vowel, and there is no 'y' after
      the letter between the consonant and the vowel, then there is a (possibly
      invalid) brivla in the piece.
    I.  Make a copy of the string, decapitalize all consonants, remove all
        commas adjacent to consonants, and insert commas before consonant
        clusters, between adjacent nondiphthong vowels, and after each pair
        of vowels without a comma between them.
    II. If the stress option is set and no vowel in the piece is stressed,
        stress the vowel in the next-to-last syllable not counting syllables
        which have 'y' in them.
    III.Capitalize all letters in all syllables which have at least one capital
        letter in them.
    IV. Scan forward for a stressed vowel other than 'Y' after or at the first
        CC or CyC consonant cluster, then scan forward to the end of the next
        syllable, ignoring syllables with 'y' in them. If the next syllable is
        itself stressed, reset the count.
    V.  If you reached the end of the word looking for a stressed vowel or the
        next syllable, resolve the piece as an error. If the next syllable
        begins with a non-initial consonant cluster, a vowel, or an apostrophe,
        go back to IV and keep looking. If the next syllable begins with a
        valid consonant cluster or single consonant, break before it and
        consider the first part, which is a brivlavau. If there is no next
        syllable, the whole piece is a brivlavau.
    VI. If the piece does not have a CC or CyC consonant cluster in the first
        five letters, not counting apostrophe, comma, or 'y', find the first
        consonant after the first letter and break before it.
    VI. If the piece has a consonant cluster in the first five letters, find
        the first CC or CyC consonant cluster and check whether it is a valid
        initial cluster (if it contains 'y' it is not) and whether the part
        beginning there is a slinku'i (see below) or monosyllabic. If the CyC
        contains a stressed 'Y', it is not a consonant cluster; thus
        /ledYcIlta/ is {le dy cilta} "D's thread", but /ledycIlta/ is
        {ledycilta} "hypha". If the part contains a diphthong and no other
        vowels, it is monosyllabic even if there is a comma in the diphthong,
        e.g. {pru,a}.
      a.  If the brivlavau begins with a consonant cluster, it is a valid
          initial cluster, and the brivlavau is not a slinku'i and is not
          monosyllabic, resolve it as a brivla.
      b.  If the brivlavau begins with a consonant cluster but the cluster is
          not a valid initial cluster or the brivlavau is a slinku'i or
          monosyllabic, resolve it as an error.
      c.  If the brivlavau does not begin with a consonant cluster, the cluster
          is a valid initial cluster, and the part beginning there is not a
          slinku'i and is not monosyllabic, break before the consonant cluster
          and resolve the second part as a brivla.
      d.  If the brivlavau does not begin with a consonant cluster and the
          cluster is not a valid initial cluster or the part beginning there
          is a slinku'i, resolve the brivlavau as a brivla.
4.  If there are any more pieces unresolved, return to step 3.

We have a piece that has been resolved as a cmene. We want to verify that
it is a valid cmene.
1. Make a copy of the string and remove all commas from the copy.
2. Scan the string:
  A. If an apostrophe is followed by anything other than a vowel, it's
     invalid.
  B. If a consonant is followed by an apostrophe or a consonant making an
     invalid pair, or the consonant is 'n', the following letter is 't' or
     'd', and the letter after that is 'c', 'j', 's', or 'z', it's invalid.
  C. If there is an invalid character in the string, the string is invalid.
3. If "la" or "doi" occurs in the string not preceded by a consonant, it's
   invalid.
4. If the last character is not a consonant, it's invalid.
5. Else it's valid.
(Alahum version: remove step 3.)

We have a piece that has been resolved as a cmavo. We want to verify that
it is a valid cmavo.
1. Make a copy of the string and remove all commas from the copy.
2. Scan the string:
  A. If an apostrophe is followed by anything other than a vowel, it's
     invalid.
  B. If a consonant is followed by an apostrophe or a consonant, or the
     consonant is not the first letter, it's invalid.
  C. If there is an invalid character in the string, the string is invalid.
  D. If a vowel is followed by a vowel, check whether the diphthong is in
     {ai au ei oi}, {ia ie ii io iu ua ue ui uo uu}, or the rest, and
     increment the corresponding counter.
3. If there is a diphthong in the rest, or there is a diphthong in {ia .. uu}
   and there are more than two letters in the cmavo, it's invalid.
4. If the last character is not a vowel, it's invalid.
5. Else it's valid.

We have a piece that has been resolved as a brivla. We want to verify that
it is in greater brivla space (e.g. {dadysabodre} is in greater brivla space,
but is not an actual brivla since {sabodre} is neither a concatenation of
rafsi nor a brivla).
1.  Make a copy of the string and remove all commas from the copy.
2.  Scan the string:
  A.  If an apostrophe is followed by anything other than a vowel, it's
      invalid.
  B.  If a consonant is followed by an apostrophe or a consonant making an
      invalid pair or a consonant making an invalid initial pair at the start
      of a word, or the consonant is 'n', the following letter is 't' or
      'd', and the letter after that is 'c', 'j', 's', or 'z', it's invalid.
  C.  If there is an invalid character in the string, the string is invalid.
  D.  If a vowel is followed by a vowel, and either or both of the vowels
      is 'y', it's invalid.
3.  Insert commas as in 3.E.I of the word-breaking algorithm.
4.  Scan the string backward, assigning the last syllable the number 1:
  A.  If an apostrophe, increment the syllable number if the previous vowel
      is not 'y'.
  B.  If a consonant, do nothing.
  C.  If an invalid character, the string is invalid. (This is detected here
      if the only invalid character is the last.)
  D.  If a vowel:
    I.  If the previous vowel was 'y' and this vowel is 'y', the string is
        invalid. (This catches {ratymykiu}; {fagyycpi} was caught in 2.D.)
    II. If 'Y' is stressed, it's invalid.
    III.If the vowel is stressed, note whether the syllable number is 2.
5.  If the syllable numbered 2 is unstressed, the string is invalid; else
    it is valid.

We have a piece that is in greater brivla space. We want to verify that it is
a brivla.
1.  Split the piece before and after each 'y'.
2.  Check each piece to see whether it has an r-hyphen or n-hyphen. If an
    r-hyphen occurs after 'y', the word is invalid.
3.  Split each piece that consists of rafsi into rafsi, putting the r-hyphen,
    if any, in a separate piece.
4.  Check each y-hyphen to see if it is needed.
  A.  If the concatenation of the preceding rafsi and the following rafsi
      contains an invalid consonant cluster, the y-hyphen is needed.
  B.  If the preceding rafsi is long, the y-hyphen is needed.
  C.  If this y-hyphen is the second piece, check for tosmabru. If the word is
      a tosmabru, the hyphen is needed.
    I.  If the preceding rafsi is not of form CVC, it's not a tosmabru.
    II. Scan the following rafsi until one of these rules triggers.
      a.  If the rafsi is not a gismu, 4-letter rafsi, or 3-letter rafsi,
          it's not a tosmabru.
      b.  If the third letter of the rafsi, not counting apostrophes, is a
          vowel or the beginning of a non-initial cluster, it's not a tosmabru.
      c.  If the rafsi is a gismu or 4-letter rafsi, or the following piece is
          a y-hyphen, it's a tosmabru.
  D.  If the long rafsi option is enabled and the following piece is a fu'ivla
      rafsi (including CCVVC) or a rafsi fu'ivla, the y-hyphen is needed.
  E.  Else the y-hyphen is not needed and the word is invalid.
5.  Check each piece to see if it's the entire brivla, a valid rafsi, or a
    hyphen.
  A.  Gismu, 4-letter rafsi, and 3-letter rafsi are always valid.
  B.  If the CCVVC option is enabled, pieces of the form CCVVC, where CC is a
      valid initial and VV is a valid diphthong or has an apostrophe, are
      valid, but a piece of the form CCVVCV is valid only if it's the only one.
  C.  If the piece is not yet validated and the long rafsi option is enabled:
    I.  If the piece ends in a consonant, append 'a' to a copy. The copy will
        be henceforth referred to as the piece, until you reassemble the pieces
        into the brivla, at which time use the piece without the appended 'a'.
    II. If the piece has a consonant cluster in the first five letters, find
        the first CC consonant cluster and check whether it is a valid initial
        cluster and whether the part beginning there is a slinku'i (see below)
        or monosyllabic.
      a.  If the piece begins with a consonant cluster, it is a valid
          initial cluster, and the piece is not a slinku'i and is not
          monosyllabic, it may be valid.
      b.  If the piece begins with a consonant cluster but the cluster is
          not a valid initial cluster or the piece is a slinku'i or
          monosyllabic, it is invalid.
      c.  If the piece does not begin with a consonant cluster, the cluster
          is a valid initial cluster, and the part beginning there is not a
          slinku'i and is not monosyllabic, it is invalid.
      d.  If the piece does not begin with a consonant cluster and the
          cluster is not a valid initial cluster or the part beginning there
          is a slinku'i, it may be valid.
    III.If the piece may be valid, count the vowels at the end. If it ends with
        more than one vowel, it is invalid.
    IV. If the piece consists of 0 or 1 consonant followed by at least 0
        3-letter rafsi followed by 0 or 1 4-letter rafsi followed by 0 or 1
        vowel, it's invalid.
    V.  If the piece begins with a CVV rafsi and an r-hyphen or n-hyphen, and
        removing the rafsi and the hyphen leaves a string consisting of at
        least 0 3-letter rafsi followed by 0 or 1 4-letter rafsi followed by 0
        or 1 vowel, it's invalid.

A slinku'i, as far as word breaking is concerned, is anything that matches
the regex
^C[raf3]*([gim]?$|[raf4]?y)
but does not match the regex
^[raf3]*([gim]?$|[raf4]?y)
where
C matches any consonant
[raf3] matches any 3-letter rafsi, meaningful or not (any CCV where CC is a
valid initial pair, CVC, or CVV where the VV is a diphthong allowed in lujvo)
[raf4] matches any 4-letter rafsi, meaningful or not (any CCVC where CC is a
valid initial pair, or CVCC for any CC)
[gim] matches any gismu, meaningful or not ([raf4]V).
Anything after the first 'y' is ignored. It has no effect on where to break the
word, only on whether the word is valid.


Lemma: All brivla contain a consonant followed two letters later, ignoring
apostrophes, by a vowel. If a brivla contains 'y', it contains such a
consonant after the 'y'.

Proof: If a brivla contains no 'y', it contains two adjacent consonants
in the first five letters, ignoring apostrophes. Find the first vowel
after this consonant cluster. Two letters before it is a consonant.
If a brivla contains 'y', there is at least one rafsi after 'y'. Consider
the last rafsi. Either it is a CVV or CCV rafsi of a gismu, in which case
the first and last letters of the rafsi are the consonant and the vowel
two letters later, or it is the final long rafsi of a gismu or fu'ivla,
in which case, being identical to the selrafsi, it has such a consonant
by the first part.

Theorem: If two lerpoi R and S which both lack 'y' are such that for all i
R[i:i+1] is a valid initial consonant pair, valid consonant pair, valid lujvo
diphthong, fa'u valid fu'ivla diphthong iff S[i:i+1], and R[i] is a vowel,
consonant, fa'u y'ybu iff S[i] is, then R is a valid brivla iff S is,
regardless of whether for some i R[i] is 'n', 'r', or 'l' and S[i] isn't.

Proof: Denote the length of R and S (they are obviously equal in length) by n.
The theorem is vacuously true for n<=3 and trivially true for n<=5. Suppose
that n>5 and R and S have the following beginnings:
1. CVC. They must begin CVCCV, CVC/CV, or CVC-C-C (where C-C means any valid
   consonant pair, CC means a valid initial pair, and C/C means a valid pair
   that is not valid initially). In any case, if all pairs are valid, R is a
   valid brivla iff R-CV isn't. If R-CVC is a lujvo without an r-hyphen or
   n-hyphen, or a final long or short rafsi of a gismu, and R-CV isn't a lujvo,
   and all pairs are valid, then R is a lujvo. The only case that can produce
   trouble is that R-CVC is a lujvo with a hyphen letter and S-CVC has a
   different letter there. Since a hyphen letter can occur only after the first
   rafsi, both R and S are fu'ivla in this case.
2. CVV. They must begin CVVC-C. R-CVV is valid iff R isn't, and likewise for S.
   If R-CVV begins with rC or nr, but S-CVV doesn't, then R may be a lujvo but
   S a fu'ivla; otherwise they are of the same type.
3. CCV. Either R is a slinku'i or it's a brivla, and similarly for S. The regex
   for slinku'i makes no distinction for hyphen-letters, so both are slinku'i
   or neither is.
4. V. If R has a consonant cluster in the first five letters, it is valid iff
   the lerpoi resulting from removing all initial vowels is invalid, and
   similarly for S.

