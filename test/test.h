#define ASSERT(x, y) assert(x, y, #y)

// c60
int printf(char *fmt, ...);
// c126
int sprintf(char *buf, char *fmt, ...);
// c68
void assert(int expected, int actual, char *code);
// c107
int strcmp(char *p, char *q);
int memcmp(char *p, char *q, long n);
// c136
void exit(int n);
// c193
int strncmp(char *p, char *q, long n);
// c204
int vsprintf(char *buf, char *fmt, void *ap);
// c205
int vsprintf();
// c220
long strlen(char *s);
// c271
void *memcpy(void *dest, void *src, long n);