extern void trdos_print_msg_advanced(const char *msg, unsigned int len, unsigned int color) ;

void trdos_print_msg_advanced(const char *msg, unsigned int len, unsigned int color) {
    int res;
    
    __asm__ __volatile__(
        "movl %1, %%ebx\n\t"    /* msg pointer -> ebx (%1) */
        "movl %2, %%ecx\n\t"    /* len dword değeri -> ecx (%2) */
        "movl %3, %%edx\n\t"    /* color dword değeri -> edx (%3) */
        "movl $35, %%eax\n\t"   /* _msg sistem çağrısı */
        "int $0x40\n\t"
        "movl %%eax, %0"        /* Dönen sonucu res değişkenine yaz (%0) */
        : "=r" (res)            /* %0: Çıktı register kısıtlaması */
        : "r" (msg),            /* %1: Girdi genel amaçlı kısıtlama */
          "r" (len),            /* %2: Girdi genel amaçlı kısıtlama (Katili çekecek olan yer!) */
          "r" (color)           /* %3: Girdi register kısıtlaması */
        : "eax", "ebx", "ecx", "edx"
    );
}
