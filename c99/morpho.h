#ifndef JBOBAF_MORPHO
#define JBOBAF_MORPHO

/* Should these three be public or not?
 *
 * const enum {srera, fadni, lidne} ccpairs[17][17];
 * int c2i(char c);
 * char i2c(int i);
 */

_Bool isCC(char c1, char c2);
_Bool isC_C(char c1, char c2);
_Bool isC(char c);
_Bool isV(char v);
_Bool isVy(char v);
#endif
