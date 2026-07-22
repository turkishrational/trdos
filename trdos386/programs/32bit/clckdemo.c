/* 22/07/2026 */
/* 21/07/2026 */ 
/* 20/07/2026 - Google AI - ref: aclock5.s - Erdogan Tan */
/* =========================================================================
   clock_demo.c - TRDOS 386 C-Tabanlı VESA VBE Analog Saat Demosu
   ========================================================================= */
#include "trdosgfx.h"

int main(void) {
    TRDOSTime curr_time;
    unsigned int start_ticks;
    unsigned char no_beep = 0;
    int i;

    trdos_print_msg("TRDOS 386 TCC Port - VESA Analog Saat Baslatiliyor...\r\n");

    /* VESA Mod 105h Geçişi (1024x768, 256 Renk) */
    if (trdos_set_video_mode(0x08FF, 0x105) == 0) {
        trdos_print_msg("Hata: VESA Mod 105h yuklenemedi!\r\n");
        return 1;
    }

    /* İlk zaman parametrelerini al ve önceki değerleri eşitle */
    trdos_get_time(&curr_time);
    start_ticks = trdos_get_ticks();
    
    unsigned int phour = curr_time.hour;
    unsigned int pminute = curr_time.minute;
    unsigned int psecond = curr_time.second;

    /* ARKA PLANI SADECE BİR KEZ ÇİZ (Döngü Öncesi Mühür) */
    draw_background();

    /* 22/07/2026 */
    goto initial; 

    /* ANA SAAT DÖNGÜSÜ (Sil - Çiz Algoritması) */
    while (1) {
        trdos_get_time(&curr_time);

        if (curr_time.second != psecond) {
            /* A. ESKİ İBRELERİ SİYAHLA SİL (Erase Stage) */
            color = 0; /* Siyah */
            radius = 140; draw_line((450 - (phour % 12 * 30 + pminute / 2)) % 360);
            radius = 175; draw_line((450 - (pminute * 6)) % 360);
            radius = 165; draw_line((450 - (psecond * 6)) % 360);

            /* Zamanı Güncelle */
            phour = curr_time.hour;
            pminute = curr_time.minute;
            psecond = curr_time.second;
initial:
            /* B. YENİ İBRELERİ RENKLİ ÇİZ (Draw Stage) */
            radius = 140; color = 0x0F; draw_line((450 - (phour % 12 * 30 + pminute / 2)) % 360); /* Beyaz Akrep */
            radius = 175; color = 0x0F; draw_line((450 - (pminute * 6)) % 360);                 /* Beyaz Yelkovan */
            radius = 165; color = 0x0C; draw_line((450 - (psecond * 6)) % 360);                 /* Kırmızı Saniye */

            /* Merkez beyaz göbeği tazele */
            draw_centermark();

            if (!no_beep) {
                trdos_beep(16, 1331); /* Donanımsal Tik Sesi */
            }
        }

        /* Donanımsal Yarım Saniye Tik Beklemesi */
        while ((trdos_get_ticks() - start_ticks) < 9) {}
        start_ticks = trdos_get_ticks();

        /* Klavye Kesme Kontrolü */
        int key = trdos_getchar_nowait();
        if (key != 0) {
            if (key == 0x20) no_beep = !no_beep; /* Spacebar */
            // else break; /* Herhangi bir tuş çıkış yapar */
        else { 
            /* 22/07/2026 - Google AI */
            trdos_set_video_mode(0x0803, 0);
            trdos_sysexit();
             }
        }
    }

    /* Standart Metin Moduna (Mode 3) Geri Dönüş */
    ;trdos_set_video_mode(0x0803, 0);
    ;return 0;
}