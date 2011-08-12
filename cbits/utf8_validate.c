#include <stdio.h>
#include <string.h>

#include "cbits.h"

typedef unsigned char uchar;

/* Initialize bit mask to have fast matching of ASCII characters */
inline long make_ascii_mask(void) {
  int i;
  long mask = 0;
  for(i = 0; i < sizeof(long); i++) {
    mask = (mask << 8) | 0x80;
  }

  return mask;
}

/* Validate an UTF-8 encoded string.
 *
 * - str:    pointer to a character array
 * - offset: offset of the string we want to validate
 * - length: length of the string we want to validate
 *
 * Return value: the index of the first invalid byte in the string
 */
int _hs_text_utf8_validate(uchar *str, int offset, int length) {
  uchar *p = str + offset;

  const uchar *end1 = p + length;
  const uchar *end2 = end1 - 1;
  const uchar *end3 = end2 - 1;
  const uchar *end4 = end3 - 1;

  /* Assign ascii mask if we haven't done so yet */
  const long ascii_mask = make_ascii_mask();
  const uchar *end_ascii_mask = end1 - sizeof(long);

#if HAVE_UNALIGNED_LOADS
  /* Fast ascii loop */
  while(p < end_ascii_mask && (*((const long *) p) & ascii_mask) == 0) {
    p += sizeof(long);
  }
#endif

  /* Slow, careful loop */
  while(p < end1) {
    const uchar n1 = *p;

    /* One-byte character */
    if(n1 <= 0x7F) {
      p++;

    /* Two-byte character */
    } else if(n1 >= 0xC2 && n1 <= 0xDF && p < end2) {
      const uchar n2 = *(p + 1);

      if(n2 < 0x80 || n2 > 0xBF) break;

      p += 2;

    /* Three-byte character */
    } else if(n1 < 0xF0 && p < end3) {
      const uchar n2 = *(p + 1);
      const uchar n3 = *(p + 2);

      if(n1 < 0xED) {
        if(n1 == 0xE0) {
          if(n2 < 0xA0 || n2 > 0xBF ||
              NOT_CONTINUATION_BYTE(n3)) break;
        } else {
          if(NOT_CONTINUATION_BYTE(n2) ||
              NOT_CONTINUATION_BYTE(n3)) break;
        }
      } else {
        if(n1 == 0xED) {
          if(n2 < 0x80 || n2 > 0x9F ||
              NOT_CONTINUATION_BYTE(n3)) break;
        } else {
          if(NOT_CONTINUATION_BYTE(n2) ||
              NOT_CONTINUATION_BYTE(n3)) break;
        }
      }

      p += 3;

    /* Four-byte character */
    } else if(p < end4) {
      const uchar n2 = *(p + 1);
      const uchar n3 = *(p + 2);
      const uchar n4 = *(p + 3);

      if(n1 == 0xF0) {
        if(n2 < 0x90 || n2 > 0xBF ||
            NOT_CONTINUATION_BYTE(n3) ||
            NOT_CONTINUATION_BYTE(n4)) break;
      } else if(n1 == 0xF4) {
        if(n2 < 0x80 || n2 > 0x8F ||
            NOT_CONTINUATION_BYTE(n3) ||
            NOT_CONTINUATION_BYTE(n4)) break;
      } else if(n1 >= 0xF1 && n1 <= 0xF3) {
        if(NOT_CONTINUATION_BYTE(n2) ||
            NOT_CONTINUATION_BYTE(n3) ||
            NOT_CONTINUATION_BYTE(n4)) break;
      }

      p += 4;

    /* Impossible */
    } else {
      break;
    }
  }

  return p - str - offset;
}
