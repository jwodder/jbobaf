grammar Vlatai {
 regex C :i { <[lrmnbdgjzvptkcsfx]> }
 regex V :i { <[aeiou]> }
 regex Vy :i { <[aeiouy]> }

 regex CC :i { <[bcfgkmpsvx]><[lr]> | <[td]>r | <[cs]><[pftkmn]> |
  <[jz]><[bvdgm]> | t<[cs]> | d<[jz]> }

 regex C_C :i {
  <[bdgjvzcfkpstx]><[lrmn]>
  | <[lrn]><[bdgjvzcfkpstx]>
  | b<[dgjvz]> | d<[bgjvz]> | g<[bdjvz]> | j<[bdgv]> | v<[bdgjz]> | z<[bdgv]>
  | c<[fkpt]> | f<[ckpstx]> | k<[cfpst]> | p<[cfkstx]> | s<[fkptx]>
   | t<[cfkpsx]> | x<[fpst]>
  | l<[rmn]> | r<[lmn]> | m<[lrnbdgjvcfkpstx]> | n<[lrm]>
 }

 regex CxC :i {
  <[lmnr]><[bcdfgjkpstvx]> | l<[mnrz]> | mn | n<[lmrz]> | r<[lmnz]>
  | b<[dgjmnvz]> | d<[bglmnv]> | g<[bdjmnvz]> | <[jz]><[lnr]> | v<[bdgjmnz]>
  | f<[ckmnpstx]> | k<[cfmnpst]> | p<[cfkmnstx]> | sx | t<[fklmnpx]>
   | x<[fmnpst]>
 }

 regex CVV :i { <C> [ <[aeo]>i | au | <V> \' <V> ] }
 regex CCV { <CC><V> }
 regex CVC { <C><V><C> }
 regex gism { <CC><V><C> | <C><V><C_C> }

 regex CyC :i { (<C>)\1 | <[bdgjvz]><[cfkpstx]> | <[cfkpstx]><[bdgjvz]> |
  <[cjsz]> ** 2 | <[ck]>x | x<[ck]> | mz
 }
}
