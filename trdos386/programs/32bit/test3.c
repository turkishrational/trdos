/* =========================================================================
   TRDOS 386 ADVANCED COMPILER TEST - ARGUMENT PARSING ENGINE (TEST3.C)
   ========================================================================= */

/* Native syscall write prototype embedded directly to bypass stdio hooks */
extern int write(int fd, const void *buf, unsigned int count);

/* String utility: Computes the standalone length of a text array character stream */
static unsigned int trdos_strlen(const char *str)
{
    unsigned int len = 0;
    while (str[len] != '\0') {
        len++;
    }
    return len;
}

/* Master Entry Point matching the pure flat execution assembly requirements */
int main(int argc, char **argv)
{
    int i;
    unsigned int len;
    char *arg_str;

    write(1, "====================================================\r\n", 54);
    write(1, "TRDOS 386 - NATIVE FLAT ARGUMENT SCANNING MOTOR\r\n", 49);
    write(1, "====================================================\r\n", 54);

    /* Enforce check: Verify if arguments are passed from the command line prompt */
    if (argc < 2) {
        write(1, "Usage status: No custom arguments detected inside prompt.\r\n", 59);
        write(1, "Try passing parameters like: test3 arg1 arg2 trdos386\r\n", 55);
        return 10; /* Safe exit code marker targeting tccexec monitor */
    }

    write(1, "Processing verified command line variables loops:\r\n", 51);

    /* Iterate through every verified command-line token argument array */
    for (i = 1; i < argc; i++) {
        arg_str = argv[i];
        len = trdos_strlen(arg_str);

        write(1, "  -> Argument [", 15);
        
        /* Direct write stream pumping raw string inputs directly to system screen buffers */
        write(1, arg_str, len);
        
        write(1, "] synchronized cleanly.\r\n", 26);
    }

    write(1, "----------------------------------------------------\r\n", 54);
    write(1, "Execution pipeline completed safely without memory leak leaks.\r\n", 60);

    return 45; /* Success return value kilitlendi: tccexec should catch 45 exactly */
}
