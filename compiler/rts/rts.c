#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sodium.h>

#define FALSE 0
#define TRUE 1

int min(int x, int y) {
    if(x < y) return x;
    return y;
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
    printf("\n");
}

char index_bytestring(bytestring* str, size_t n) {
    if(n < 0 || n > str->length) {
        printf("Trying to access element at index %zu of bytestring with length %zu!\n", n, str->length);
        exit(-1);
    }

    return (*str->arr)[n];
}

char equals_bytestring(bytestring* s0, bytestring* s1) {
    if(s0->length != s1->length) return FALSE;

    for(size_t i=0; i<s0->length; i++) {
        if((*s0->arr)[i] != (*s1->arr)[i]) return FALSE;
    }

    return TRUE;
}

char less_than_bytestring(bytestring* s0, bytestring* s1) {
    int r = memcmp((*s0->arr), (*s1->arr), min(s0->length, s1->length));

    if(r == 0) {
        return s0->length < s1->length;
    }

    return r < 0;
}

char less_than_equals_bytestring(bytestring* s0, bytestring* s1) {
    int r = memcmp((*s0->arr), (*s1->arr), min(s0->length, s1->length));

    if(r == 0) {
        return s0->length <= s1->length;
    }

    return r < 0;
}

unsigned char* sha2_256(bytestring* str) {
    rts_init();
    unsigned char* result = (unsigned char*)malloc(crypto_hash_sha256_BYTES);
    crypto_hash_sha256(result, (unsigned char*)(*str->arr), str->length);

    return result;
}

unsigned char* blake2b_256(bytestring* str) {
    rts_init();
    unsigned char* result = (unsigned char*)malloc(32);
    crypto_generichash(result, 32, (*str->arr), str->length, NULL, 0);

    return result;
}

unsigned char verify_signature(bytestring* pubKey, bytestring* message, bytestring* signature) {
    rts_init();
    if(crypto_sign_verify_detached((*signature->arr), (*message->arr), message->length, (*pubKey->arr)) == 0) {
        return TRUE;
    }
    return FALSE;
}
