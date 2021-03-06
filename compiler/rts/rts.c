#include "rts.h"

int min(int x, int y) {
    return x < y ? x : y;
}

void rts_init() {
    if(sodium_init() < 0) {
        printf("Unable to initialise libsodium!\n");
        exit(-1);
    }
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
