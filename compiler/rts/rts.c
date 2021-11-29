#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sodium.h>
#include <stdbool.h>
#include <gmp.h>

#include "tiny_sha3/sha3.h"

int min(int x, int y) {
    return x < y ? x : y;
}

struct bytestring {
    size_t length;
    unsigned char* arr[];
} typedef bytestring;

void rts_init() {
    if(sodium_init() < 0) {
        printf("Unable to initialise libsodium!\n");
        exit(-1);
    }
}

void print_bytestring(bytestring* str) {
    for(size_t i=0; i<str->length; i++) {
        printf("%02X", (*str->arr)[i]);
    }
}

char index_bytestring(bytestring* str, size_t n) {
    if(n < 0 || n > str->length) {
        printf("Trying to access element at index %zu of bytestring with length %zu!\n", n, str->length);
        exit(-1);
    }

    return (*str->arr)[n];
}

bool equals_bytestring(bytestring* s0, bytestring* s1) {
    if(s0->length != s1->length) return false;

    for(size_t i=0; i<s0->length; i++) {
        if((*s0->arr)[i] != (*s1->arr)[i]) return false;
    }

    return true;
}

bool less_than_bytestring(bytestring* s0, bytestring* s1) {
    int r = memcmp((*s0->arr), (*s1->arr), min(s0->length, s1->length));

    if(r == 0) {
        return s0->length < s1->length;
    }

    return r < 0;
}

bool less_than_equals_bytestring(bytestring* s0, bytestring* s1) {
    int r = memcmp((*s0->arr), (*s1->arr), min(s0->length, s1->length));

    if(r == 0) {
        return s0->length <= s1->length;
    }

    return r < 0;
}

unsigned char* sha2_256(bytestring* str) {
    unsigned char* result = (unsigned char*)malloc(crypto_hash_sha256_BYTES);
    crypto_hash_sha256(result, (unsigned char*)(*str->arr), str->length);

    return result;
}

unsigned char* sha3_256(bytestring* str) {
    unsigned char* result = (unsigned char*)malloc(32U);
    sha3((unsigned char*)(*str->arr), str->length, result, 32);

    return result;
}

unsigned char* blake2b_256(bytestring* str) {
    unsigned char* result = (unsigned char*)malloc(32);
    crypto_generichash(result, 32, (*str->arr), str->length, NULL, 0);

    return result;
}

bool verify_signature(bytestring* pubKey, bytestring* message, bytestring* signature) {
    return crypto_sign_verify_detached((*signature->arr), (*message->arr), message->length, (*pubKey->arr)) == 0;
}
