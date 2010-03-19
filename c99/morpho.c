const enum {srera, fadni, lidne} consPairs[17][17] = {
 {srera, srera, fadni, srera, fadni, fadni, srera, lidne, fadni, fadni, srera, lidne, srera, srera, fadni, srera, fadni},
 {srera, srera, srera, lidne, srera, srera, lidne, lidne, lidne, lidne, lidne, lidne, srera, lidne, srera, srera, srera},
 {fadni, srera, srera, srera, fadni, lidne, srera, fadni, fadni, fadni, srera, lidne, srera, srera, fadni, srera, lidne},
 {srera, fadni, srera, srera, srera, srera, fadni, lidne, fadni, fadni, fadni, lidne, fadni, fadni, srera, fadni, srera},
 {fadni, srera, fadni, srera, srera, fadni, srera, lidne, fadni, fadni, srera, lidne, srera, srera, fadni, srera, fadni},
 {lidne, srera, lidne, srera, lidne, srera, srera, fadni, lidne, fadni, srera, fadni, srera, srera, lidne, srera, srera},
 {srera, fadni, srera, fadni, srera, srera, srera, lidne, fadni, fadni, fadni, lidne, fadni, fadni, srera, srera, srera},
 {fadni, fadni, fadni, fadni, fadni, fadni, fadni, srera, fadni, fadni, fadni, fadni, fadni, fadni, fadni, fadni, fadni},
 {fadni, fadni, fadni, fadni, fadni, fadni, fadni, lidne, srera, fadni, fadni, lidne, fadni, fadni, fadni, fadni, srera},
 {fadni, fadni, fadni, fadni, fadni, fadni, fadni, fadni, fadni, srera, fadni, fadni, fadni, fadni, fadni, fadni, fadni},
 {srera, fadni, srera, fadni, srera, srera, fadni, lidne, fadni, fadni, srera, lidne, fadni, fadni, srera, fadni, srera},
 {fadni, fadni, fadni, fadni, fadni, fadni, fadni, fadni, fadni, fadni, fadni, srera, fadni, fadni, fadni, fadni, fadni},
 {srera, srera, srera, lidne, srera, srera, lidne, lidne, lidne, lidne, lidne, lidne, srera, lidne, srera, fadni, srera},
 {srera, lidne, srera, fadni, srera, srera, fadni, fadni, fadni, fadni, fadni, lidne, lidne, srera, srera, fadni, srera},
 {fadni, srera, fadni, srera, fadni, fadni, srera, lidne, fadni, fadni, srera, lidne, srera, srera, srera, srera, fadni},
 {srera, srera, srera, fadni, srera, srera, srera, lidne, fadni, fadni, fadni, lidne, fadni, fadni, srera, srera, srera},
 {lidne, srera, lidne, srera, lidne, srera, srera, fadni, lidne, fadni, srera, fadni, srera, srera, lidne, srera, srera}
};

int c2i(char c) {
 switch (c) {
  case 'b': case 'B': return 0;
  case 'c': case 'C': return 1;
  case 'd': case 'D': return 2;
  case 'f': case 'F': return 3;
  case 'g': case 'G': return 4;
  case 'j': case 'J': return 5;
  case 'k': case 'K': return 6;
  case 'l': case 'L': return 7;
  case 'm': case 'M': return 8;
  case 'n': case 'N': return 9;
  case 'p': case 'P': return 10;
  case 'r': case 'R': return 11;
  case 's': case 'S': return 12;
  case 't': case 'T': return 13;
  case 'v': case 'V': return 14;
  case 'x': case 'X': return 15;
  case 'z': case 'Z': return 16;
  default: return -1;
 }
}

char i2c(int i) {
 switch (i) {
  case 0: return 'b';
  case 1: return 'c';
  case 2: return 'd';
  case 3: return 'f';
  case 4: return 'g';
  case 5: return 'j';
  case 6: return 'k';
  case 7: return 'l';
  case 8: return 'm';
  case 9: return 'n';
  case 10: return 'p';
  case 11: return 'r';
  case 12: return 's';
  case 13: return 't';
  case 14: return 'v';
  case 15: return 'x';
  case 16: return 'z';
  default: return '?';
 }
}

_Bool isCC(char c1, char c2) {
 int i1 = c2i(c1), i2 = c2i(c2);
 return i1 != -1 && i2 != -1 && consPairs[i1][i2] == lidne;
}

_Bool isC_C(char c1, char c2) {
 int i1 = c2i(c1), i2 = c2i(c2);
 return i1 != -1 && i2 != -1 && consPairs[i1][i2] != srera;
}

/* vim:set nowrap: */
