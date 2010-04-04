/* Add functions for:
 - getting the index of a {selrafsi}, rather than the {valsi} itself
 - moving to a {valsi} by index?
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "vlaste.h"
#include "stejudri.h"

static FILE* vlaste = NULL;

int kvbinsearch(const struct kvpair* porsi, const char* key, int qty) {
 int low = 0, high = qty;  /* low {ga'o bi'o ke'i} high */
 while (low < high) {
  int i = (low+high) / 2;
  int cmp = strcmp(key, porsi[i].key);
  if (cmp < 0) high = i;
  else if (cmp > 0) low = i+1;
  else return i;
 }
 return -1;
}

void freeStecmi(struct stecmi* cmima) {
 if (cmima != NULL) {
  free(cmima->valsi);
  free(cmima);
 }
}

const char* getSelrafsi(const char* rafsi) {
 int i = kvbinsearch(selrafsi, rafsi, SELRAFSI_QTY);
 return i == -1 ? NULL : vlastecmi[i].key;
}

int vlajudri(const char* valsi) {
 return kvbinsearch(vlastecmi, valsi, VLASTECMI_QTY);
}

struct stecmi* vlacpa(const char* valsi) {
 if (vlaste == NULL) return NULL;  /* Should errno be set to something? */
 return judrycpa(vlajudri(valsi));
}

#define readField(field) \
 p = strchr(buf, '\t'); \
 if (p == NULL) { \
  if (*buf != '\0') cmima->field = buf; \
  return cmima; \
 } \
 if (p > buf) cmima->field = buf; \
 *p = '\0'; \
 buf = p+1;

struct stecmi* lamcpa(void) {
 if (vlaste == NULL) return NULL;  /* Should errno be set to something? */
 char* buf = calloc(CMIMA_LEN+1, sizeof(char));
 if (buf == NULL) return NULL;
 if (fgets(buf, CMIMA_LEN+1, vlaste) == NULL) {free(buf); return NULL; }
 char* p = strchr(buf, '\n');
 if (p != NULL) *p = '\0';
 struct stecmi* cmima = malloc(sizeof(struct stecmi));
 if (cmima == NULL) {free(buf); return NULL; }
 memset(cmima, 0, sizeof(struct stecmi));
 cmima->valsi = buf;
 p = strchr(buf, '\t');
 if (p == NULL) return cmima;
 *p = '\0';
 buf = p+1;
 readField(selmaho);
 /* Read rafsi: */
 int r=0;
 switchLoop:
 switch (*buf) {
  case '\0': return cmima;
  case '\t': *buf = '\0'; buf++; break;
  case ' ': *buf = '\0'; buf++; goto switchLoop;
  default:
   if (*(buf-1) == '\0') {
    cmima->rafsi[r] = buf;
    if (++r >= 3) {
     p = strchr(buf, '\t');
     if (p == NULL) return cmima;
     else {*p = '\0'; buf = p+1; break; }
    }
   }
   buf++;
   goto switchLoop;
 }
 readField(ralvla);
 readField(djuvla);
 readField(selvla);
 readField(notci);
 return cmima;
}

int vlastecfa(void) {
 if (vlaste == NULL && (vlaste = fopen(VLASTE, "r")) == NULL) return EOF;
 else return 0;
}

int vlastesti(void) {
 if (vlaste != NULL && fclose(vlaste) == 0) {vlaste = NULL; return 0; }
 else {vlaste = NULL; return EOF; }
}

void mleciha(const char* str, int indent, int width) {
 size_t len = strlen(str);
 if (len <= width) {puts(str); return; }
 while (len > width) {
  int i = width;
  while (i > 0 && !isspace(str[i])) i--;
  if (i <= 0) i = width;
  fwrite(str, sizeof(char), i, stdout);
  putchar('\n');
  str += i;
  len -= i;
  while (isspace(*str)) {str++; len--; }
  if (len > 0) for (int j=0; j<indent; j++) putchar(' ');
 }
 if (len > 0) puts(str);
}

void vlaciska(struct stecmi* cmima) {
 printf("VALSI:    %s\n", cmima->valsi);
 if (cmima->selmaho != NULL) printf("SELMAhO:  %s\n", cmima->selmaho);
 if (cmima->rafsi[0] != NULL) {
  printf("RAFSI:   ");
  for (int i=0; i<3; i++) {
   if (cmima->rafsi[i] == NULL) break;
   printf(" %s", cmima->rafsi[i]);
  }
  putchar('\n');
 }
 if (cmima->ralvla != NULL) printf("RALVLA:   %s\n", cmima->ralvla);
 if (cmima->djuvla != NULL) printf("DJUVLA:   %s\n", cmima->djuvla);
 if (cmima->selvla != NULL) {
  printf("SELVLA:   ");
  mleciha(cmima->selvla, 10, 69);
 }
 if (cmima->notci != NULL) {
  printf("NOTCI:    ");
  mleciha(cmima->notci, 10, 69);
 }
 putchar('\n');
}

struct stecmi* judrycpa(int i) {
 if (vlaste == NULL) return NULL;  /* Should errno be set to something? */
 if (i < 0 || i >= VLASTECMI_QTY) {errno = EINVAL; return NULL; }
 if (fseek(vlaste, vlastecmi[i].val, SEEK_SET) < 0) return NULL;
 return lamcpa();
}
