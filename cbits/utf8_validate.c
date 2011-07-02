#include <string.h>
#include <stdio.h>

#define INVALID_CONTINUATION_BYTE(x) ((x) < 0x80 || (x) > 0xBF)

typedef unsigned char uchar;

int _hs_utf8_validate(uchar *str, int offset, int length)
{
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

  while(p < end1)
  {
    uchar n1 = *p;

    /* One-byte character */
    if(n1 <= 0x7F)
    {
      p++;
    }

    /* Two-byte character */
    else if(n1 >= 0xC2 && n1 <= 0xDF && p < end2)
    {
      uchar n2 = *(p + 1);
      if(n2 < 0x80 || n2 > 0xBF) break;
      p += 2;
    }

    /* Three-byte character */
    else if(n1 < 0xF0 && p < end3)
    {
      uchar n2 = *(p + 1);
      uchar n3 = *(p + 2);

      if(n1 < 0xED)
      {
        if(n1 == 0xE0)
        {
          if(n2 < 0xA0 || n2 > 0xBF ||
              INVALID_CONTINUATION_BYTE(n3)) break;
        }
        else
        {
          if(INVALID_CONTINUATION_BYTE(n2) ||
              INVALID_CONTINUATION_BYTE(n3)) break;
        }
      }
      else
      {
        if(n1 == 0xED)
        {
          if(n2 < 0x80 || n2 > 0x9F ||
              INVALID_CONTINUATION_BYTE(n3)) break;
        }
        else
        {
          if(INVALID_CONTINUATION_BYTE(n2) ||
              INVALID_CONTINUATION_BYTE(n3)) break;
        }
      }

      p += 3;
    }
        
    /* Four-byte character */
    else if(p < end4)
    {
      uchar n2 = *(p + 1);
      uchar n3 = *(p + 2);
      uchar n4 = *(p + 3);

      if(n1 == 0xF0)
      {
        if(n2 < 0x90 || n2 > 0xBF ||
            INVALID_CONTINUATION_BYTE(n3) ||
            INVALID_CONTINUATION_BYTE(n4)) break;
      }
      else if(n1 == 0xF4)
      {
        if(n2 < 0x80 || n2 > 0x8F ||
            INVALID_CONTINUATION_BYTE(n3) ||
            INVALID_CONTINUATION_BYTE(n4)) break;
      }
      else if(n1 >= 0xF1 && n1 <= 0xF3)
      {
        if(INVALID_CONTINUATION_BYTE(n2) ||
            INVALID_CONTINUATION_BYTE(n3) ||
            INVALID_CONTINUATION_BYTE(n4)) break;
      }

      p += 4;
    }

    /* Impossible */
    else
    {
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

  return 0;
}
*/
