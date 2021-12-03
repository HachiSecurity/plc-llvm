#include "data4.h"

int main() {
    char bytes[] = { 0x54, 0x68, 0x65, 0x43, 0x61, 0x6B, 0x65, 0x49, 0x73, 0x41, 0x4C, 0x69, 0x65 };
    closure *val = plc_new_bytestring(sizeof(bytes), bytes);
    closure *arg = plc_new_data_bytestring(val);
    closure *ptr = plc_entry();
    closure *res = plc_apply(ptr, arg);
    plc_print_closure(res);
    putchar('\n');
    return 0;
}
