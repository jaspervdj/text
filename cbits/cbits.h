/*
 * Common macro's for the C code
 */
#ifndef CBITS_H
#define CBITS_H

/* The given byte is *not* a continuation byte */
#define NOT_CONTINUATION_BYTE(x) ((x) < 0x80 || (x) > 0xBF)

/* The given byte is a continuation byte */
#define CONTINUATION_BYTE(x) ((x) >= 0x80 && (x) <= 0xBF)

/* If the architecture supports unaligned loads */
#define HAVE_UNALIGNED_LOADS (defined(__i386__) || defined(__x86_64__))

#endif
