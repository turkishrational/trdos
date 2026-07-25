/* =========================================================================
   asmtest4.c - TRDOS 386 Yerel TCC Çoklu Operant İzolasyon Testi (24/07/2026)
   ========================================================================= */

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
        : "g" (msg),            /* %1: Girdi genel amaçlı kısıtlama */
          "g" (len),            /* %2: Girdi genel amaçlı kısıtlama (Katili çekecek olan yer!) */
          "r" (color)           /* %3: Girdi register kısıtlaması */
        : "eax", "ebx", "ecx", "edx"
    );
}

int main(void) {
    const char *text = "-> [TEST 4]: Coklu operant kalkanı basariyla asildi!\r\n";
    
    /* Mesajın tam uzunluğunu (strlen emülasyonu) ve CGA rengini (0x0E = Parlak Sarı) paslıyoruz */
    trdos_print_msg_advanced(text, 55, 0x0E);
    
    return 0;
}
