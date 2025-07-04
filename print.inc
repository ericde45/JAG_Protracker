;-*-asm-*-
; ---------------------------------------
; print pads status
; Pads : mask = xxxxxxCx xxBx2580 147*oxAP 369#RLDU
print_pads_status:

  move.l    DSP_pad1,d1
  lea      string_pad_status,a0
  move.l    #31,d6

.boucle:
  moveq    #0,d0
  btst.l    d6,d1
  beq.s    .print_space
  move.b    (a0)+,d0
  bsr      print_caractere
  bra.s    .ok
.print_space:
  move.b    #'.',d0
  bsr      print_caractere
  lea      1(a0),a0
.ok:
  dbf      d6,.boucle

; ligne suivante
  moveq    #10,d0
  bsr      print_caractere

print_pads_status_pad2:
; pad2
  move.l    DSP_pad2,d1
  lea      string_pad_status,a0
  move.l    #31,d6

.boucle2:
  moveq    #0,d0
  btst.l    d6,d1
  beq.s    .print_space2
  move.b    (a0)+,d0
  bsr      print_caractere
  bra.s    .ok2
.print_space2:
  move.b    #'.',d0
  bsr      print_caractere
  lea      1(a0),a0
.ok2:
  dbf      d6,.boucle2

; ligne suivante
  moveq    #10,d0
  bsr      print_caractere


  rts

string_pad_status:    dc.b    "......CE..BD2580147*oFAp369#RLDU"
    even

; ---------------------------------------
; imprime une chaine terminée par un zéro
; a0=pointeur sur chaine
print_string:
  movem.l d0-d7/a0-a6,-(a7)

print_string_boucle:
  moveq  #0,d0
  move.b  (a0)+,d0
  cmp.w  #0,d0
  bne.s  print_string_pas_fin_de_chaine
  movem.l (a7)+,d0-d7/a0-a6
  rts
print_string_pas_fin_de_chaine:
  bsr    print_caractere
  bra.s  print_string_boucle

; ---------------------------------------
; imprime un nombre HEXA de 2 chiffres
print_nombre_hexa_2_chiffres:
  movem.l d0-d7/a0-a6,-(a7)
  lea    convert_hexa,a0
  move.l    d0,d1
  divu    #16,d0
  and.l    #$F,d0      ; limite a 0-15
  move.l    d0,d2
  mulu    #16,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere
  move.l    d1,d0
  and.l    #$F,d0      ; limite a 0-15
  move.b    (a0,d0.w),d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts

convert_hexa:
  dc.b    48,49,50,51,52,53,54,55,56,57
  dc.b    65,66,67,68,69,70
  even

; ---------------------------------------
; imprime un nombre de 2 chiffres
print_nombre_2_chiffres:
  movem.l d0-d7/a0-a6,-(a7)
  move.l    d0,d1
  divu    #10,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #10,d2
  sub.l    d2,d1
  cmp.l    #0,d0
  beq.s    .zap
  add.l    #48,d0
  bsr      print_caractere
.zap:
  move.l    d1,d0
  add.l    #48,d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts

; ---------------------------------------
; imprime un nombre de 3 chiffres
print_nombre_3_chiffres:
  movem.l d0-d7/a0-a6,-(a7)
  move.l    d0,d1

  divu    #100,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #100,d2
  sub.l    d2,d1
  cmp.l    #0,d0
  beq.s    .zap
  add.l    #48,d0
  bsr      print_caractere
.zap:
  move.l    d1,d0
  divu    #10,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #10,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere

  move.l    d1,d0
  add.l    #48,d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts


; ---------------------------------------
; imprime un nombre de 2 chiffres , 00
print_nombre_2_chiffres_force:
  movem.l d0-d7/a0-a6,-(a7)
  move.l    d0,d1
  divu    #10,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #10,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere
  move.l    d1,d0
  add.l    #48,d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts

; ---------------------------------------
; imprime un nombre de 4 chiffres HEXA
print_nombre_hexa_4_chiffres:
  movem.l d0-d7/a0-a6,-(a7)
  move.l    d0,d1
  lea    convert_hexa,a0

  divu    #4096,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #4096,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere

  move.l    d1,d0
  divu    #256,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #256,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere


  move.l    d1,d0
  divu    #16,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #16,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere
  move.l    d1,d0
  move.b    (a0,d0.w),d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts

; ---------------------------------------
; imprime un nombre de 6 chiffres HEXA ( pour les adresses memoire)
print_nombre_hexa_6_chiffres:
  movem.l d0-d7/a0-a6,-(a7)

  move.l    d0,d1
  lea    convert_hexa,a0

  move.l    d1,d0
  swap    d0
  and.l    #$F0,d0
  divu    #16,d0
  and.l    #$F,d0
  move.b    (a0,d0.w),d0
  and.l    #$FF,d0
  bsr      print_caractere

  move.l    d1,d0
  swap    d0
  and.l    #$F,d0
  move.b    (a0,d0.w),d0
  and.l    #$FF,d0
  bsr      print_caractere

  and.l    #$FFFF,d1
  move.l    d1,d0
  divu    #4096,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #4096,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere

  move.l    d1,d0
  divu    #256,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #256,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere


  move.l    d1,d0
  divu    #16,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #16,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere
  move.l    d1,d0
  move.b    (a0,d0.w),d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts

; ---------------------------------------
; imprime un nombre de 8 chiffres HEXA ( pour les adresses memoire et les données en 16:16)
print_nombre_hexa_8_chiffres:
  movem.l d0-d7/a0-a6,-(a7)

  move.l    d0,d1
  lea    convert_hexa,a0

  move.l    d1,d0
  swap    d0
  and.l    #$F000,d0
  divu    #4096,d0
  and.l    #$F,d0
  move.b    (a0,d0.w),d0
  and.l    #$FF,d0
  bsr      print_caractere



  move.l    d1,d0
  swap    d0
  and.l    #$F00,d0
  divu    #256,d0
  and.l    #$F,d0
  move.b    (a0,d0.w),d0
  and.l    #$FF,d0
  bsr      print_caractere


  move.l    d1,d0
  swap    d0
  and.l    #$F0,d0
  divu    #16,d0
  and.l    #$F,d0
  move.b    (a0,d0.w),d0
  and.l    #$FF,d0
  bsr      print_caractere

  move.l    d1,d0
  swap    d0
  and.l    #$F,d0
  move.b    (a0,d0.w),d0
  and.l    #$FF,d0
  bsr      print_caractere

  and.l    #$FFFF,d1
  move.l    d1,d0
  divu    #4096,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #4096,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere

  move.l    d1,d0
  divu    #256,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #256,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere


  move.l    d1,d0
  divu    #16,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #16,d2
  sub.l    d2,d1
  move.b    (a0,d0.w),d0
  bsr      print_caractere
  move.l    d1,d0
  move.b    (a0,d0.w),d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts


; ---------------------------------------
; imprime un nombre de 4 chiffres
print_nombre_4_chiffres:
  movem.l d0-d7/a0-a6,-(a7)
  move.l    d0,d1

  divu    #1000,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #1000,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere

  move.l    d1,d0
  divu    #100,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #100,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere


  move.l    d1,d0
  divu    #10,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #10,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere
  move.l    d1,d0
  add.l    #48,d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts

; ---------------------------------------
; imprime un nombre de 5 chiffres
print_nombre_5_chiffres:
  movem.l d0-d7/a0-a6,-(a7)
  move.l    d0,d1

  divu    #10000,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #10000,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere

  move.l    d1,d0
  divu    #1000,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #1000,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere

  move.l    d1,d0
  divu    #100,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #100,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere


  move.l    d1,d0
  divu    #10,d0
  and.l    #$FF,d0
  move.l    d0,d2
  mulu    #10,d2
  sub.l    d2,d1
  add.l    #48,d0
  bsr      print_caractere
  move.l    d1,d0
  add.l    #48,d0
  bsr      print_caractere
  movem.l (a7)+,d0-d7/a0-a6
  rts


; -----------------------------
; copie un caractere a l ecran
; d0.w=caractere

print_caractere:
  movem.l d0-d7/a0-a6,-(a7)



  cmp.b  #00,d0
  bne.s  print_caractere_pas_CLS
  move.l  #ecran1,A1_BASE      ; = DEST
  move.l  #$0,A1_PIXEL
  move.l  #PIXEL16|XADDPHR|PITCH1,A1_FLAGS
  move.l  #ecran1+320*100,A2_BASE      ; = source
  move.l  #$0,A2_PIXEL
  move.l  #PIXEL16|XADDPHR|PITCH1,A2_FLAGS

  move.w  #$00,B_PATD


  moveq  #0,d0
  move.w  #nb_octets_par_ligne,d0
  lsr.w  #1,d0
  move.w  #nb_lignes,d1
  mulu  d1,d0
  swap  d0
  move.w  #1,d0
  swap  d0
  ;move.w  #65535,d0
  move.l  d0,B_COUNT
  move.l  #LFU_REPLACE|SRCEN|PATDSEL,B_CMD


  movem.l (a7)+,d0-d7/a0-a6
  rts

print_caractere_pas_CLS:

  cmp.b  #10,d0
  bne.s  print_caractere_pas_retourchariot
  move.w  #0,curseur_x
  add.w  #8,curseur_y
  movem.l (a7)+,d0-d7/a0-a6
  rts

print_caractere_pas_retourchariot:
  cmp.b  #09,d0
  bne.s  print_caractere_pas_retourdebutligne
  move.w  #0,curseur_x
  movem.l (a7)+,d0-d7/a0-a6
  rts

print_caractere_pas_retourdebutligne:
  cmp.b  #08,d0
  bne.s  print_caractere_pas_retourdebutligneaudessus
  move.w  #0,curseur_x
  sub.w  #8,curseur_y
  movem.l (a7)+,d0-d7/a0-a6
  rts
print_caractere_pas_retourdebutligneaudessus:

  lea    ecran1,a1
  moveq  #0,d1
  move.w  curseur_x,d1
  add.l  d1,a1
  moveq  #0,d1
  move.w  curseur_y,d1
  mulu  #nb_octets_par_ligne,d1
  add.l  d1,a1

  lsl.l  #3,d0    ; * 8
  lea    fonte,a0
  add.l  d0,a0


; copie 1 lettre
  move.l  #8-1,d0
copieC_ligne:
  moveq  #8-1,d1
  move.b  (a0)+,d2
copieC_colonne:
  moveq  #0,d4
  btst  d1,d2
  beq.s  pixel_a_zero
  moveq  #0,d4
  move.w  couleur_char,d4
pixel_a_zero:
  move.b  d4,(a1)+
  dbf    d1,copieC_colonne
  lea    nb_octets_par_ligne-8(a1),a1
  dbf    d0,copieC_ligne

  move.w  curseur_x,d0
  add.w  #8,d0
  cmp.w  #320,d0
  blt    curseur_pas_fin_de_ligne
  moveq  #0,d0
  add.w  #8,curseur_y
curseur_pas_fin_de_ligne:
  move.w  d0,curseur_x

  movem.l (a7)+,d0-d7/a0-a6

  rts
