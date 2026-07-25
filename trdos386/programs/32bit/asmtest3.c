/* =========================================================================
   asmtest3.c - TRDOS 386 Yerel TCC Inline ASM İzrasyon Testi (23/07/2026)
   ========================================================================= */

/* TRDOS 386 Ring 3 _msg (EAX=35) Sistem Çağrısı Köprüsü */
void trdos_print_msg(const char *msg) {
    __asm__ __volatile__(
        "movl %0, %%ebx\n\t"    /* Girdi parametresini (msg) ebx'e yükle */
        "movl $255, %%ecx\n\t"  /* Tampon boyutu sınırı */
        "movl $7, %%edx\n\t"    /* Renk niteliği (Beyaz) */
        "movl $35, %%eax\n\t"   /* _msg kernel fonksiyon numarası */
        "int $0x40"             /* TRDOS Ring 0 Donanım Kesmesi */
        :
        : "r" (msg)             /* Girdi kısıtlaması: Herhangi bir register */
        : "eax", "ebx", "ecx", "edx" /* Clobber listesi: Değişen register'lar */
    );
}

int main(void) {
    /* Test mesajımızı TRDOS terminaline gönderiyoruz */
    trdos_print_msg("-> TEBRiKLER: TCC 0.9.23 ASM Motoru TRDOS Flat Bellekte Calisiyor!\r\n");
    return 0;
}
