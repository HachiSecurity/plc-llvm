#ifndef PLC_RTS_H
#define PLC_RTS_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sodium.h>
#include <stdbool.h>
#include <gmp.h>

#include "tiny_sha3/sha3.h"

struct bytestring {
    size_t length;
    unsigned char* arr[];
} typedef bytestring;

#endif
