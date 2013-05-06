void foo(int argc) {
    int a = 0;
    if (argc < 1) { a = 0; }
    else
    #ifdef A
    if (argc < 2) { a = 1; }
    else
    #endif
    #ifdef B
    if (argc < 3) { a = 2; }
    else
    #endif
    {}
}