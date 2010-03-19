#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "vlaste.h"
#include "stejudri.h"

int main(int argc, char** argv) {
 if (vlastecfa() == EOF) {perror("na'e ka'e cfari"); return 4; }
 if (argc == 1) {
  srandom(time(NULL));
  struct stecmi* cunso = judrycpa(random() % VLASTECMI_QTY);
  if (cunso == NULL) perror("ti na drani");
  else {vlaciska(cunso); freeStecmi(cunso); }
 } else {
  for (int i=1; i<argc; i++) {
   int j = vlajudri(argv[i]);
   if (j == -1) printf("mi na djuno fi zoi xy. %s .xy\n", argv[i]);
   else {
    struct stecmi* valsi = judrycpa(j);
    if (valsi == NULL) perror("ti na drani");
    else {vlaciska(valsi); freeStecmi(valsi); }
   }
  }
 }
 vlastesti();
 return 0;
}
