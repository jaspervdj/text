/*
 * Copyright (c) 2011 Bryan O'Sullivan <bos@serpentine.com>.
 *
 * Portions copyright (c) 2008-2010 Björn Höhrmann <bjoern@hoehrmann.de>.
 *
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 */

#include <string.h>
#include <stdint.h>
#include <stdio.h>

void _hs_text_memcpy(void *dest, size_t doff, const void *src, size_t soff,
		     size_t n)
{
  memcpy(dest + doff, src + soff, n);
}

int _hs_text_memcmp(const void *a, size_t aoff, const void *b, size_t boff,
		    size_t n)
{
  return memcmp(a + aoff, b + boff, n);
}

/* Compares two arrays of equal length, and return the index at which they
 * differ. If the length of the array chunks is returned, they are equal.
 */
size_t _hs_text_diff(void *a, size_t a_off, void *b, size_t b_off, size_t n)
{
  char *a_p = ((char *) a) + a_off;
  char *b_p = ((char *) b) + b_off;

  const char *a_end = a_p + n;
  const char *a_end_long = a_end - sizeof(long);

  /* Fast loop */
  while(a_p < a_end_long && *((long *) a_p) == *((long *) b_p))
  {
    a_p += sizeof(long);
    b_p += sizeof(long);
  }

  /* Slow loop */
  while(a_p < a_end && *a_p == *b_p)
  {
    a_p++;
    b_p++;
  }

  /* If we ended up at a continuation byte, we need to go left again */
  while(a_p < a_end && *a_p >= 0x80 && *a_p <= 0xBF)
  {
    a_p--;
  }

  return a_p - ((char *) a) - a_off;
}
