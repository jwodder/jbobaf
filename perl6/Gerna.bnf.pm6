grammar Lojban {
 regex text {
  <NAI>* (<CMENE>+ <free>* | <indicators> <free>* | <free>+)? <joik_jek>?
  <text_1>
 }

 regex text_1 {
  ((<I> (<jek> | <joik>)? (<stag>? <BO>)? <free>*)+ | <NIhO>+ <free>*)?
  <paragraphs>?
 }

 regex paragraphs { <paragraph> (<NIhO>+ <free>* <paragraphs>)? }

 regex paragraph {
  (<statement> | <fragment>) (<I> <free>* (<statement> | <fragment>)?)*
 }

 regex statement { <statement_1> | <prenex> <statement> }

 regex statement_1 { <statement_2> (<I> <joik_jek> <statement_2>?)* }

 regex statement_2 {
  <statement_3> (<I> (<jek> | <joik>)? <stag>? <BO> <free>* <statement_2>?)?
 }

 regex statement_3 {
  <sentence>
  | <tag>? <TUhE> <free>* <text_1> (<TUhU><free>*)?
 }

 regex fragment {
  <ek> <free>*
  | <gihek> <free>*
  | <quantifier>
  | <NA> <free>*
  | <terms> (<VAU><free>*)?
  | <prenex>
  | <relative_clauses>
  | <links>
  | <linkargs>
 }

 regex prenex { <terms> <ZOhU> <free>* }

 regex sentence { (<terms> (<CU> <free>*)?)? <bridi_tail> }

 regex subsentence { <sentence> | <prenex> <subsentence> }

 regex bridi_tail {
  <bridi_tail_1>
   (<gihek> <stag>? <KE> <free>* <bridi_tail> (<KEhE><free>*)? <tail_terms>)?
 }

 regex bridi_tail_1 {
  <bridi_tail_2> (<gihek> <free>* <bridi_tail_2> <tail_terms>)*
 }

 regex bridi_tail_2 {
  <bridi_tail_3> (<gihek> <stag>? <BO> <free>* <bridi_tail_2> <tail_terms>)?
 }

 regex bridi_tail_3 { <selbri> <tail_terms> | <gek_sentence> }

 regex gek_sentence {
  <gek> <subsentence> <gik> <subsentence> <tail_terms>
  | <tag>? <KE> <free>* <gek_sentence> (<KEhE><free>*)?
  | <NA> <free>* <gek_sentence>
 }

 regex tail_terms { <terms>? (<VAU><free>*)? }

 regex terms { <terms_1> + }

 regex terms_1 { <terms_2> (<PEhE> <free>* <joik_jek> <terms_2>)* }

 regex terms_2 { <term> (<CEhE> <free>* <term>)* }

 regex term {
  <sumti>
  | (<tag> | <FA> <free>*) (<sumti> | (<KU><free>*)?)
  | <termset>
  | <NA> <KU> <free>*
 }

 regex termset {
  <NUhI> <free>* <gek> <terms> (<NUhU><free>*)? <gik> <terms> (<NUhU><free>*)?
  | <NUhI> <free>* <terms> (<NUhU><free>*)?
 }

 regex sumti { <sumti_1> (<VUhO> <free>* <relative_clauses>)? }

 regex sumti_1 {
  <sumti_2> ((<ek> | <joik>) <stag>? <KE> <free>* <sumti> (<KEhE><free>*)?)?
 }

 regex sumti_2 { <sumti_3> (<joik_ek> <sumti_3>)* }

 regex sumti_3 { <sumti_4> ((<ek> | <joik>) <stag>? <BO> <free>* <sumti_3>)? }

 regex sumti_4 { <sumti_5> | <gek> <sumti> <gik> <sumti_4> }

 regex sumti_5 {
  <quantifier>? <sumti_6> <relative_clauses>?
  | <quantifier> <selbri> (<KU><free>*)? <relative_clauses>?
 }

 regex sumti_6 {
  (<LAhE> <free>* | <NAhE> <BO> <free>*) <relative_clauses>? <sumti>
   (<LUhU><free>*)?
  | <KOhA> <free>*
  | <lerfu_string> (<BOI><free>*)?
  | <LA> <free>* <relative_clauses>? <CMENE>+ <free>*
  | (<LA> | <LE>) <free>* <sumti_tail> (<KU><free>*)?
  | <LI> <free>* <mex> (<LOhO><free>*)?
  | <ZO> <any_word> <free>*
  | <LU> <text> (<LIhU><free>*)?
  | <LOhU> <any_word>+ <LEhU> <free>*
  | <ZOI> <any_word> <anything> <any_word> <free>*
 }

 regex sumti_tail {
  (<sumti_6> <relative_clauses>?)? <sumti_tail_1>
  | <relative_clauses> <sumti_tail_1>
 }

 regex sumti_tail_1 {
  <quantifier>? <selbri> <relative_clauses>?
  | <quantifier> <sumti>
 }

 regex relative_clauses {
  <relative_clause> (<ZIhE> <free>* <relative_clause>)*
 }

 regex relative_clause {
  <GOI> <free>* <term> (<GEhU><free>*)?
  | <NOI> <free>* <subsentence> (<KUhO><free>*)?
 }

 regex selbri { <tag>? <selbri_1> }

 regex selbri_1 { <selbri_2> | <NA> <free>* <selbri> }

 regex selbri_2 { <selbri_3> (<CO> <free>* <selbri_2>)? }

 regex selbri_3 { <selbri_4> + }

 regex selbri_4 {
  <selbri_5>
  (<joik_jek> <selbri_5>
  | <joik> <stag>? <KE> <free>* <selbri_3> (<KEhE><free>*)?
  )*
 }

 regex selbri_5 {
  <selbri_6> ((<jek> | <joik>) <stag>? <BO> <free>* <selbri_5>)?
 }

 regex selbri_6 {
  <tanru_unit> (<BO> <free>* <selbri_6>)?
  | (<NAhE> <free>*)? <guhek> <selbri> <gik> <selbri_6>
 }

 regex tanru_unit { <tanru_unit_1> (<CEI> <free>* <tanru_unit_1>)* }

 regex tanru_unit_1 { <tanru_unit_2> <linkargs>? }

 regex tanru_unit_2 {
  <BRIVLA> <free>*
  | <GOhA> <RAhO>? <free>*
  | <KE> <free>* <selbri_3> (<KEhE><free>*)?
  | <ME> <free>* <sumti> (<MEhU><free>*)? (<MOI> <free>*)?
  | (<number> | <lerfu_string>) <MOI> <free>*
  | <NUhA> <free>* <mex_operator>
  | <SE> <free>* <tanru_unit_2>
  | <JAI> <free>* <tag>? <tanru_unit_2>
  | <any_word> (<ZEI> <any_word>)+
  | <NAhE> <free>* <tanru_unit_2>
  | <NU> <NAI>? <free>* (<joik_jek> <NU> <NAI>? <free>*)* <subsentence>
   (<KEI><free>*)?
 }

 regex linkargs { <BE> <free>* <term> <links>? (<BEhO><free>*)? }

 regex links { <BEI> <free>* <term> <links>? }

 regex quantifier {
  <number> (<BOI><free>*)?
  | <VEI> <free>* <mex> (<VEhO><free>*)?
 }

 regex mex { <mex_1> (<operator> <mex_1>)* | <FUhA> <free>* <rp_expression> }

 regex mex_1 { <mex_2> (<BIhE> <free>* <operator> <mex_1>)? }

 regex mex_2 {
  <operand> | (<PEhO> <free>*)? <operator> <mex_2>+ (<KUhE><free>*)?
 }

 regex rp_expression { <rp_operand> <rp_operand> <operator> }

 regex rp_operand { <operand> | <rp_expression> }

 regex operator {
  <operator_1>
  (<joik_jek> <operator_1>
  | <joik> <stag>? <KE> <free>* <operator> (<KEhE><free>*)?
  # Check whether the <joik> above should be a <joik_jek>.
  )*
 }

 regex operator_1 {
  <operator_2>
  | <guhek> <operator_1> <gik> <operator_2>
  | <operator_2> (<jek> | <joik>) <stag>? <BO> <free>* <operator_1>
 }

 regex operator_2 { <mex_operator> | <KE> <free>* <operator> (<KEhE><free>*)? }

 regex mex_operator {
  <SE> <free>* <mex_operator>
  | <NAhE> <free>* <mex_operator>
  | <MAhO> <free>* <mex> (<TEhU><free>*)?
  | <NAhU> <free>* <selbri> (<TEhU><free>*)?
  | <VUhU> <free>*
 }

 regex operand {
  <operand_1> ((<ek> | <joik>) <stag>? <KE> <free>* <operand> (<KEhE><free>*)?)?
 }

 regex operand_1 { <operand_2> (<joik_ek> <operand_2>)* }

 regex operand_2 {
  <operand_3> ((<ek> | <joik>) <stag>? <BO> <free>* <operand_2>)?
 }

 regex operand_3 {
  <quantifier>
  | <lerfu_string> (<BOI><free>*)?
  | <NIhE> <free>* <selbri> (<TEhU><free>*)?
  | <MOhE> <free>* <sumti> (<TEhU><free>*)?
  | <JOhI> <free>* <mex_2>+ (<TEhU><free>*)?
  | <gek> <operand> <gik> <operand_3>
  | (<LAhE> <free>* | <NAhE> <BO> <free>*) <operand> (<LUhU><free>*)?
 }

 regex number { <PA> (<PA> | <lerfu_word>)* }

 regex lerfu_string { <lerfu_word> (<PA> | <lerfu_word>)* }

 regex lerfu_word {
  <BY> | <any_word> <BU> | <LAU> <lerfu_word> | <TEI> <lerfu_string> <FOI>
 }

 regex ek { <NA>? <SE>? <A> <NAI>? }

 regex gihek { <NA>? <SE>? <GIhA> <NAI>? }

 regex jek { <NA>? <SE>? <JA> <NAI>? }

 regex joik { <SE>? <JOI> <NAI>? | <interval> | <GAhO> <interval> <GAhO> }

 regex interval { <SE>? <BIhI> <NAI>? }

 regex joik_ek { <joik> <free>* | <ek> <free>* }

 regex joik_jek { <joik> <free>* | <jek> <free>* }

 regex gek { <SE>? <GA> <NAI>? <free>* | <joik> <GI> <free>* | <stag> <gik> }

 regex guhek { <SE>? <GUhA> <NAI>? <free>* }

 regex gik { <GI> <NAI>? <free>* }

 regex tag { <tense_modal> (<joik_jek> <tense_modal>)* }

 regex stag { <simple_tense_modal> ((<jek> | <joik>) <simple_tense_modal>)* }

 regex tense_modal {
  <simple_tense_modal> <free>* | <FIhO> <free>* <selbri> (<FEhU><free>*)?
 }

 regex simple_tense_modal {
  <NAhE>? <SE>? <BAI> <NAI>? <KI>?
  | <NAhE>? (<time> <space>? | <space> <time>?) <CAhA>? <KI>?
  | <NAhE>? <CAhA> <KI>?
  | <KI>
  | <CUhE>
 }

 regex time {
  <ZI> <time_offset>* (<ZEhA> (<PU> <NAI>?)?)? <interval_property>*
  | <time_offset>+ (<ZEhA> (<PU> <NAI>?)?)? <interval_property>*
  | <ZEhA> (<PU> <NAI>?)? <interval_property>*
  | <interval_property>+
 }

 regex time_offset { <PU> <NAI>? <ZI>? }

 regex space {
  <VA> <space_offset>* <space_interval>? (<MOhI> <space_offset>)?
  | <space_offset>+ <space_interval>? (<MOhI> <space_offset>)?
  | <space_interval> (<MOhI> <space_offset>)?
  | <MOhI> <space_offset>
 }

 regex space_offset { <FAhA> <NAI>? <VA>? }

 regex space_interval {
  (<VEhA> <VIhA>? | <VIhA>) (<FAhA> <NAI>?)? <space_int_props>?
  | <space_int_props>
 }

 regex space_int_props { (<FEhE> <interval_property>)+ }

 regex interval_property { (<number> <ROI> | <TAhE> | <ZAhO>) <NAI>? }

 regex free {
  <SEI> <free>* (<terms> (<CU> <free>*)?)? <selbri> <SEhU>?
  | <SOI> <free>* <sumti> <sumti>? <SEhU>?
  | <vocative> <relative_clauses>? <selbri> <relative_clauses>? <DOhU>?
  | <vocative> <relative_clauses>? <CMENE>+ <free>* <relative_clauses>? <DOhU>?
  | <vocative> <sumti>? <DOhU>?
  | (<number> | <lerfu_string>) <MAI>
  | <TO> <text> <TOI>?
  | <XI> <free>* (<number> | <lerfu_string>) <BOI>?
  | <XI> <free>* <VEI> <free>* <mex> <VEhO>?
 }

 regex vocative { (<COI> <NAI>?)+ <DOI>? | <DOI> }

 regex indicators { <FUhE>? <indicator>+ }

 regex indicator { (<UI> | <CAI>) <NAI>? | <Y> | <DAhO> | <FUhO> }

 #`{ The following rules are non-formal:
  regex word { <BAhE>? <any-word> <indicators>? }
  any-word = "any single word (no compound cmavo)"
  anything = "any text at all, whether Lojban or not"
  regex null { <any-word> <SI> | <utterance> <SA> | <text> <SU> }
  FAhO is a universal terminator and signals the end of parsable input.
 }
}
