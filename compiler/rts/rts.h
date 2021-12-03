#ifndef PLC_RTS_H
#define PLC_RTS_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sodium.h>
#include <stdbool.h>
#include <gmp.h>

#include "tiny_sha3/sha3.h"

struct closure typedef closure;

typedef closure* (*closure_entry)();
typedef void (*closure_print)();

struct closure {
    closure_entry entry;
    closure_print print;
    size_t flags;
    void** free_vars;
} typedef closure;

struct bytestring {
    size_t length;
    unsigned char* arr[];
} typedef bytestring;

struct pair {
    closure* fst;
    closure* snd;
} typedef pair;

struct list {
    closure* head;
    closure* tail;
} typedef list;

struct data {
    unsigned char tag;
    closure** fields;
} typedef data;

#endif
