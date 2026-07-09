/* =========================================================================
   GWD.C - TCC Inline ASM Sayısal Format Uyumlu Working Directory Aracı
   ========================================================================= */

#define NULL ((void*)0)

int main()
{
    /* TRDOS 386 Çekirdek Kuralı: Tampon bellek alanı en az 92 bayt olmak zorundadır */
    char dir_buffer[96]; 
    char output_str[128];
    unsigned int drive_num = 0;
    int i, len;

    /* Tampon bellek alanındaki eski çöpleri temizliyoruz */
    for (i = 0; i < 96; i++) {
        dir_buffer[i] = 0;
    }

    /* 1. ADIM: _dir (28) sistem çağrısını TCC Inline ASM Standartlarında Tetikliyoruz */
    __asm__ __volatile__ (
        "movl %1, %%ebx\n\t"    /* ebx = Kullanıcı tampon bellek adresi */
        "movl $28, %%eax\n\t"   /* sys_dir fonskiyon numarası (28) */
        "int $0x40\n\t"         /* TRDOS Kernel Kesmesi */
        "movl %%eax, %0\n\t"    /* AL = Aktif sürücü numarası (0=A .. 3=D) */
        : "=r" (drive_num)
        : "r" (dir_buffer)
        : "eax", "ebx", "ecx", "edx"
    );

    /* 2. ADIM: Dönen sürücü indeksini (0->A, 3->D) ASCII karakter koduna dönüştürüyoruz */
    output_str[0] = 'A' + (drive_num & 0xFF);
    output_str[1] = ':';

    /* 3. ADIM: Kernel'ın tampona transfer ettiği dizin stringini arkasına ekliyoruz */
    len = 2;
    i = 0;
    
    /* Eğer kök dizindeysek (dir_buffer boş ise) mutlak suretle bir separator çakıyoruz */
    if (dir_buffer[0] == '\0') {
        output_str[len++] = '/'; /* TRDOS Tarzı Separator */
    } else {
        /* Eğer kernel dizinin başına slash koymadıysa biz ekliyoruz */
        if (dir_buffer[0] != '\\' && dir_buffer[0] != '/') {
            output_str[len++] = '/';
        }
        
        while (dir_buffer[i] != '\0' && len < 120) {
            /* Karakter standardizasyonu: Ters slash'ları düz slash yapıyoruz */
            if (dir_buffer[i] == '\\') {
                output_str[len++] = '/';
            } else {
                output_str[len++] = dir_buffer[i];
            }
            i++;
        }
    }
    
    /* Satır sonu işaretçileri (CRLF) ve string sonlandırıcı (null-terminator) */
    output_str[len++] = '\r';
    output_str[len++] = '\n';
    output_str[len] = '\0';

    /* 4. ADIM: _msg (35) çağrısını TCC Inline ASM formatında ateşliyoruz */
    /* 0x0E (Bright Yellow) rengi saf onluk tabanda 14 sayısına denktir ($14) */
    __asm__ __volatile__ (
        "movl %0, %%ebx\n\t"    /* Yazdırılacak mesaj adresi */
        "movl %1, %%ecx\n\t"    /* Karakter uzunluğu */
        "movl $14, %%edx\n\t"   /* Parlak Sarı Renk Özniteliği (14 = 0x0E) */
        "movl $35, %%eax\n\t"   /* sys_msg fonksiyon numarası (35) */
        "int $0x40\n\t"         /* TRDOS Kernel Kesmesi */
        :
        : "r" (output_str), "r" (len)
        : "eax", "ebx", "ecx", "edx"
    );

    /* 5. ADIM: _exit (1) kesmesi ile kabuğa (shell) firesiz geri dönüş */
    __asm__ __volatile__ (
        "movl $0, %%ebx\n\t"
        "movl $1, %%eax\n\t"
        "int $0x40\n\t"
    );

    return 0;
}
