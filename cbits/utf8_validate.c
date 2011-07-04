#include <stdio.h>
#include <string.h>

#define INVALID_CONTINUATION_BYTE(x) ((x) < 0x80 || (x) > 0xBF)

typedef unsigned char uchar;

/* Static global variable holding the ascii mask */
long ascii_mask = 0;

/* Mask size in bytes */
long ascii_mask_length = sizeof(long);

/* Initialize bit mask to have fast matching of ASCII characters */
void make_ascii_mask(void) {
  if(!ascii_mask) {
    int i;
    for(i = 0; i < ascii_mask_length; i++) {
      ascii_mask = (ascii_mask << 8) | 0x80;
    }
  }
}

/* Validate an UTF-8 encoded string.
 *
 * - str:    pointer to a character array
 * - offset: offset of the string we want to validate
 * - length: length of the string we want to validate
 *
 * Return value: the index of the first invalid byte in the string
 */
int _hs_utf8_validate(uchar *str, int offset, int length) {
  /*
  int i;
  for(i = offset; i < length; i++) printf("%u ", str[i]);
  printf("\n");
  */

  uchar *p = str + offset;

  uchar *end1 = p + length;
  uchar *end2 = end1 - 1;
  uchar *end3 = end2 - 1;
  uchar *end4 = end3 - 1;

  /* Assign ascii mask if we haven't done so yet */
  make_ascii_mask();
  uchar *end_ascii_mask = end1 - ascii_mask_length;

  while(p < end1) {
    uchar n1 = *p;

    /* Ascii mask */
    if(p < end_ascii_mask && (*((long *) p) & ascii_mask) == 0) {
      p += ascii_mask_length;

    /* One-byte character */
    } if(n1 <= 0x7F) {
      p++;

    /* Two-byte character */
    } else if(n1 >= 0xC2 && n1 <= 0xDF && p < end2) {
      uchar n2 = *(p + 1);

      if(n2 < 0x80 || n2 > 0xBF) break;

      p += 2;

    /* Three-byte character */
    } else if(n1 < 0xF0 && p < end3) {
      uchar n2 = *(p + 1);
      uchar n3 = *(p + 2);

      if(n1 < 0xED) {
        if(n1 == 0xE0) {
          if(n2 < 0xA0 || n2 > 0xBF ||
              INVALID_CONTINUATION_BYTE(n3)) break;
        } else {
          if(INVALID_CONTINUATION_BYTE(n2) ||
              INVALID_CONTINUATION_BYTE(n3)) break;
        }
      } else {
        if(n1 == 0xED) {
          if(n2 < 0x80 || n2 > 0x9F ||
              INVALID_CONTINUATION_BYTE(n3)) break;
        } else {
          if(INVALID_CONTINUATION_BYTE(n2) ||
              INVALID_CONTINUATION_BYTE(n3)) break;
        }
      }

      p += 3;

    /* Four-byte character */
    } else if(p < end4) {
      uchar n2 = *(p + 1);
      uchar n3 = *(p + 2);
      uchar n4 = *(p + 3);

      if(n1 == 0xF0) {
        if(n2 < 0x90 || n2 > 0xBF ||
            INVALID_CONTINUATION_BYTE(n3) ||
            INVALID_CONTINUATION_BYTE(n4)) break;
      } else if(n1 == 0xF4) {
        if(n2 < 0x80 || n2 > 0x8F ||
            INVALID_CONTINUATION_BYTE(n3) ||
            INVALID_CONTINUATION_BYTE(n4)) break;
      } else if(n1 >= 0xF1 && n1 <= 0xF3) {
        if(INVALID_CONTINUATION_BYTE(n2) ||
            INVALID_CONTINUATION_BYTE(n3) ||
            INVALID_CONTINUATION_BYTE(n4)) break;
      }

      p += 4;

    /* Impossible */
    } else {
      break;
    }
  }

  return p - str - offset;
}

/*
void test_is_valid(char *str)
{
  int length = strlen(str);
  int v = _hs_utf8_validate((uchar *) str, 0, length);
  if(v >= length)
  {
    printf("%s is valid\n", str);    
  }
  else
  {
    printf("%s is invalid at byte %d\n", str, v);    
  }
}

int main(int argc, char **argv)
{
  test_is_valid("Hello world");    
  test_is_valid("O hai lambda \xCE\xBB");    
  test_is_valid("\xC0\xC0");    
  test_is_valid("\x82\xe5\xaa\xc9\xd3\x10\xd2\x82\xc7\x67\x4f\x93\xff\xa4\xa1");
  test_is_valid("\xa6\x44\x42\x28\x3a\x69\xb8\xa4\x5a\xe1\x55\x47\xf\xe8\xb1\x14\xc\xe2\xff\xd5");

  printf("Mask: %lx\n", ascii_mask);

  return 0;
}
*/
