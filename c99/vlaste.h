#ifndef JBOBAF_VLASTE
#define JBOBAF_VLASTE

struct stecmi {
 char *valsi, *selmaho, *rafsi[3], *ralvla, *djuvla, *selvla, *notci;
 /* Unused/empty fields (other than `valsi') are NULL. */
 /* Should klesi, krarafsi, veljvo, & termre be added in? */
 /* All of the data for a stecmi is stored in a single memory block, which is
  * pointed to by `valsi'. */
};

void freeStecmi(struct stecmi* cmima);
const char* getSelrafsi(const char* rafsi);
int vlajudri(const char* valsi);
struct stecmi* vlacpa(const char* valsi);
struct stecmi* lamcpa(void);
int vlastecfa(void);
int vlastesti(void);
void vlaciska(struct stecmi* cmima);
struct stecmi* judrycpa(int i);
#endif
