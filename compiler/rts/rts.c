#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define FALSE 0
#define TRUE 1

int min(int x, int y) {
    if(x < y) return x;
    return y;
}

struct bytestring {
    size_t length;
    char arr[];
} typedef bytestring;

void print_bytestring(bytestring* str) {
    for(size_t i=0; i<str->length; i++) {
        printf("%02X", str->arr[i]);
    }
    printf("\n");
}

char index_bytestring(bytestring* str, size_t n) {
    if(n < 0 || n > str->length) {
        exit(-1); //todo: error message?
    }

    return str->arr[n];
}

char equals_bytestring(bytestring* s0, bytestring* s1) {
    if(s0->length != s1->length) return FALSE;

    for(size_t i=0; i<s0->length; i++) {
        if(s0->arr[i] != s1->arr[i]) return FALSE;
    }

    return TRUE;
}

char less_than_bytestring(bytestring* s0, bytestring* s1) {
    int r = memcmp(s0->arr, s1->arr, min(s0->length, s1->length));

    if(r == 0) {
        return s0->length < s1->length;
    }

    return r < 0;
}

char less_than_equals_bytestring(bytestring* s0, bytestring* s1) {
    int r = memcmp(s0->arr, s1->arr, min(s0->length, s1->length));

    if(r == 0) {
        return s0->length <= s1->length;
    }

    return r < 0;
}
