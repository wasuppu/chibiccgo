#include "test.h"

int main() {
  // c65
  ASSERT(1, sizeof(char));
  ASSERT(2, sizeof(short));
  ASSERT(2, sizeof(short int));
  ASSERT(2, sizeof(int short));
  ASSERT(4, sizeof(int));
  ASSERT(8, sizeof(long));
  ASSERT(8, sizeof(long int));
  ASSERT(8, sizeof(long int));
  ASSERT(8, sizeof(char *));
  ASSERT(8, sizeof(int *));
  ASSERT(8, sizeof(long *));
  ASSERT(8, sizeof(int **));
  ASSERT(8, sizeof(int(*)[4]));
  ASSERT(32, sizeof(int*[4]));
  ASSERT(16, sizeof(int[4]));
  ASSERT(48, sizeof(int[3][4]));
  ASSERT(8, sizeof(struct {int a; int b;}));

  // c68
  ASSERT(8, sizeof(-10 + (long)5));
  ASSERT(8, sizeof(-10 - (long)5));
  ASSERT(8, sizeof(-10 * (long)5));
  ASSERT(8, sizeof(-10 / (long)5));
  ASSERT(8, sizeof((long)-10 + 5));
  ASSERT(8, sizeof((long)-10 - 5));
  ASSERT(8, sizeof((long)-10 * 5));
  ASSERT(8, sizeof((long)-10 / 5));

  // c78
  ASSERT(1, ({ char i; sizeof(++i); }));

  // c86
  ASSERT(8, sizeof(int(*)[10]));
  ASSERT(8, sizeof(int(*)[][10]));

  // c112
  ASSERT(4, sizeof(struct { int x, y[]; }));

  // c130
  ASSERT(1, sizeof(char));
  ASSERT(1, sizeof(signed char));
  ASSERT(1, sizeof(signed char signed));
  // c131
  ASSERT(1, sizeof(unsigned char));
  ASSERT(1, sizeof(unsigned char unsigned));

  // c130
  ASSERT(2, sizeof(short));
  ASSERT(2, sizeof(int short));
  ASSERT(2, sizeof(short int));
  ASSERT(2, sizeof(signed short));
  ASSERT(2, sizeof(int short signed));
  // c131
  ASSERT(2, sizeof(unsigned short));
  ASSERT(2, sizeof(int short unsigned));

  // c130
  ASSERT(4, sizeof(int));
  ASSERT(4, sizeof(signed int));
  ASSERT(4, sizeof(signed));
  ASSERT(4, sizeof(signed signed));
  // c131
  ASSERT(4, sizeof(unsigned int));
  ASSERT(4, sizeof(unsigned));
  ASSERT(4, sizeof(unsigned unsigned));

  // c130
  ASSERT(8, sizeof(long));
  ASSERT(8, sizeof(signed long));
  ASSERT(8, sizeof(signed long int));
  // c131
  ASSERT(8, sizeof(unsigned long));
  ASSERT(8, sizeof(unsigned long int));

  // c130
  ASSERT(8, sizeof(long long));
  ASSERT(8, sizeof(signed long long));
  ASSERT(8, sizeof(signed long long int));
  // c131
  ASSERT(8, sizeof(unsigned long long));
  ASSERT(8, sizeof(unsigned long long int));

  // c131
  ASSERT(1, sizeof((char)1));
  ASSERT(2, sizeof((short)1));
  ASSERT(4, sizeof((int)1));
  ASSERT(8, sizeof((long)1));

  ASSERT(4, sizeof((char)1 + (char)1));
  ASSERT(4, sizeof((short)1 + (short)1));
  ASSERT(4, sizeof(1?2:3));
  ASSERT(4, sizeof(1?(short)2:(char)3));
  ASSERT(8, sizeof(1?(long)2:(char)3));

  // c133
  ASSERT(1, sizeof(char) << 31 >> 31);
  ASSERT(1, sizeof(char) << 63 >> 63);

  // c140
  ASSERT(4, sizeof(float));
  ASSERT(8, sizeof(double));

  // c142
  ASSERT(4, sizeof(1f+2));
  ASSERT(8, sizeof(1.0+2));
  ASSERT(4, sizeof(1f-2));
  ASSERT(8, sizeof(1.0-2));
  ASSERT(4, sizeof(1f*2));
  ASSERT(8, sizeof(1.0*2));
  ASSERT(4, sizeof(1f/2));
  ASSERT(8, sizeof(1.0/2));

  // c149
  ASSERT(16, sizeof(long double));

  // c257
  ASSERT(1, sizeof(main));

  printf("OK\n");
  return 0;
}
