; version 2.1 : passage en 21:11

; --------------- TODO ---------------
; gestion du STOP :
  ; - registre pour appel I2S
  ; - 4 étapes pour arret : I2S, timer1, timer 2, main

; - ne pas relire 2 fois ce que j'ai lu juste avant le mixage : volume et note
; - registres disponibles en alt/interrupt pour mixage / repeat ?


; --------------- POSTPONED ---------------
; - utiliser la ram DSP dispo pour stocker des samples ? problème : 21 bits pour la valeur entiere de l'adresse en ram ne suffit plus
        ; - evaluer l'utilisation 2 fois de suite du meme sample : meme location, meme taille
        ; - // ou le meme sample sur 2 voies dans la meme frame ?
        ; stats song 1 :
            ; nb ecritures sample location sans optimisation : 19366
            ; nb ecritures  location+length  test precedent identique : 10029
            ; nb ecritures  location+length  identique sur plusieurs canaux : 11113


; ---------- done: ----------
; OK - ajouter 1 volume SFX et 1 volume music
; OK - ajouter 2 voies FX
; OK - solution asynchrone entre replay 68000 et DSP : 12 * 4 voies * 10 = 480 octets // 2 pointeurs sur une table de pointeur, 1 pointeur de lecture, 1 pointeur ecriture // si pointeur ecriture = pointeur lecture, on ne fait rien, sinon on genere du son / APMIX
        ; table/liste de buffers de 4*10 octets / 8 entrées / index pour le remplissage / index pour la lecture     ;      ===========> dmacon immédiat , avec les buffers à délais...
; KO - re-tester interruption DSP=>68000 : https://www.yaronet.com/topics/194108-fonctionnement-des-interruptions-entre-68000-et-dsp#post-12
; OK - interlacer les voies pour le mixage / optimisation
; OK : lecture 4 octets par 4 octets / en verifiant que la source n'est pas deja égale à l'existant pour ne pas recharger les octets
    ; NON - stocker les 4 ocets en ram dsp ( position de l'ancienne virgule sur 32 bits )
; OK : resignage en I2S ou pas : INUTILE
; OK : pitch un peu trop high/aigu
; OK : bug song 2/5
; OK : frequence minimale possible avec un increment de 1, note mini = 108 : (3546895 / note) / frequence I2S // 3546895/108 = 32841,62037037037
;    => version toutes fréquences... : 4 registres de plus pour increment entier....
;          ou alors version 21:11 :
              ; - timer 1 : OK
              ; - interruption68000 : OK
              ; - mixage : OK
              ; - lecture des valeurs avant mixage : OK
; OK - reprendre la virgule en 21:11
; OK - revenir a une version sans relecture des samples .B



;------------------------------------------------------------
; emulateur Paula version buffer
; version lecture octet par octet, meme si on avance pas
; par la suite :
;        - lecture 32 bits mais shlq , 2 voies 16 bits d'un coup
;        - lecture octet par octet mais acces ram uniquement si on avance
;        - lecture 4 octets a chaque fois
;
; - 1 buffer joué + 1 buffer à remplir en .L
;
; - si frequence = 35000 : 700*2=1400 octets
; - il faut calculer la taille du buffer en fonction de la fréquence réelle
; - pour remplir 1 buffer, 1 voix =

; registres programme principal : 31 registres : R0-R30
;          - 2 * source
;          - 2 * source partie a virgule
;          - 2 * fin
;          - 2 * volume
;          - 2 * increment
;          - eventuellement 2 * source repeat
;          - eventuellement 2 * fin repeat
;          - octet en cours droite ou gauche
;          - octet en cours de calcul
;          - 1 registre pointeur adresse stockage resultat, gauche ou droite
; = 14+3=17

; - location + increment
; - comparaison fin
; - si fin, repeat location + repeat fin remplace, en gardant la virgule



; TODO V1
; - mixer 2 voies, par 2 voies.
; - timer 1 : switcher les 2 buffers
; - calculer la difference entre debut et position actuelle du buffer pour savoir combien d'octets/samples il faut produire
; - mettre l'adresse du buffer a remplir dans un registre
; - mettre le nb d'octets à remplir dans un registre
; - dans la boucle principale, pour avoir un maximum de registres disponibles, surveiller le registre ALT d'adresse, et remplir le buffer




;CC (Carry Clear) = %00100
;CS (Carry Set)   = %01000
;EQ (Equal)       = %00010
;MI (Minus)       = %11000
;NE (Not Equal)   = %00001
;PL (Plus)        = %10100
;HI (Higher)      = %00101
;T (True)         = %00000


nb_bits_virgule_offset          .equ      11          ; 11 ok DRAM/ 8 avec samples en ram DSP
mix_SFX = 1

; valeur pour Paula_flag_Tick_50Hz
flag_timer_50HZ_OK 	= 0
flag_en_attente_timer_50HZ = 1
flag_prise_en_compte_valeurs_PAULA = 2

; DSP_flag_STOP
; 0-1 = running
DSP_STOP_flag_STOP_NOW		= 2
DSP_STOP_flag_arret_I2S		= 3
DSP_STOP_flag_arret_Timer1	= 4
DSP_STOP_flag_arret_Timer2	= 5
DSP_STOP_flag_arret_main	= 6
PAULA_valeur_minimale_length_en_words = 1

LSP_DSP_Audio_frequence equ 30000        ; real hardware needs lower sample frequencies than emulators
routine_interrupt_68000 equ 0

PAULA_corretion_frequence = 27

DSP_STACK_SIZE	equ  4	; long words
DSP_USP		equ    (D_ENDRAM-(4*DSP_STACK_SIZE))
DSP_ISP		equ    (DSP_USP-(4*DSP_STACK_SIZE))

; index DSP, tout en .LONG
; 1-32
DSP_n_note          								EQU 	0
DSP_n_cmd           								EQU 	1
DSP_n_cmdlo         								EQU 	2
DSP_n_start         									EQU 	3
DSP_n_length        								EQU 	4
DSP_n_loopstart     								EQU 	5
DSP_n_replen        								EQU 	6
DSP_n_period        								EQU 	7
DSP_n_finetune      								EQU 	8
DSP_n_volume        							EQU 	9
DSP_n_dmabit        								EQU 	10
DSP_n_toneportdirec 							EQU 	11
DSP_n_toneportspeed 						EQU 	12
DSP_n_wantedperiod 	 					EQU	13
DSP_n_vibratocmd    							EQU 	14
DSP_n_vibratopos  	  							EQU 	15
DSP_n_tremolocmd  							EQU 	16
DSP_n_tremolopos   							EQU 	17
DSP_n_wavecontrol  							EQU 	18
DSP_n_glissfunk     								EQU 	19
DSP_n_sampleoffset  						EQU	20
DSP_n_pattpos       								EQU 	21
DSP_n_funkoffset    							EQU	22
DSP_n_wavestart     							EQU	23
DSP_n_loopcount    							EQU	24
DSP_n_reallength    							EQU	25
DSP_n_realvolume    							EQU	26

DSP_size_mt_chanXtemp				equ		27*4

	.68000
PAULA_init:
	moveq	#0,d0
	move.l  d0,D_CTRL
; copie du code DSP dans la RAM DSP

	lea    PAULA_DSP_debut(pc),A0
	lea    D_RAM,A1
	move.w  #(PAULA_DSP_fin-DSP_base_memoire)>>2-1,d0
boucle_copie_bloc_DSP:
	move.l  (A0)+,(A1)+
	dbf    D0,boucle_copie_bloc_DSP

	move.l    #silence<<nb_bits_virgule_offset,d0
	move.l    d0,DSP_pointeur_silence
	lea      PAULA_sample_location0,a0
	move.l    d0,(a0)
	move.l    d0,20(a0)
	move.l    d0,(28*1)(a0)
	move.l    d0,(28*1)+20(a0)
	move.l    d0,(28*2)(a0)
	move.l    d0,(28*2)+20(a0)
	move.l    d0,(28*3)(a0)
	move.l    d0,(28*3)+20(a0)
	move.l    d0,(28*4)(a0)
	move.l    d0,(28*4)+20(a0)
	move.l    d0,(28*5)(a0)
	move.l    d0,(28*5)+20(a0)

	move.l    #(silence+2)<<nb_bits_virgule_offset,d0
	move.l    d0,DSP_pointeur_fin_silence
	move.l    d0,8(a0)
	move.l    d0,24(a0)
	move.l    d0,(28*1)+8(a0)
	move.l    d0,(28*1)+24(a0)
	move.l    d0,(28*2)+8(a0)
	move.l    d0,(28*2)+24(a0)
	move.l    d0,(28*3)+8(a0)
	move.l    d0,(28*3)+24(a0)
	move.l    d0,(28*4)+8(a0)
	move.l    d0,(28*4)+24(a0)
	move.l    d0,(28*5)+8(a0)
	move.l    d0,(28*5)+24(a0)

; launch DSP
	move.l  #REGPAGE,D_FLAGS
	move.l  #DSP_routine_init_DSP,D_PC
	move.l  #DSPGO,D_CTRL
	rts

	.long
silence:
    dc.b      $0,$0,$0,$0
    dc.b      $0,$0,$0,$0
	.long


  .text

;-------------------------------------
;
;     DSP
;
;-------------------------------------

	.long
PAULA_DSP_debut:
	.dsp

REG_interrupt_adresse_routine_I2S          .equr     R13

	.org  D_RAM

DSP_base_memoire:

; CPU interrupt
 .if      routine_interrupt_68000=1
	movei    #DSP_PAULA_routine_interruption_CPU68000,r28            ; 6 octets
	movei    #D_FLAGS,r30                      ; 6 octets
	jump      (r28)                          ; 2 octets
	load      (r30),R29                ; read flags                ; 2 octets = 16 octets
 .else
	.rept  8
	  nop
	.endr
 .endif
; I2S interrupt
	movei  #D_FLAGS,r30
	load	(r31),r28	; return address
	addqt    #4,r31		; pop from stack
	load    (r30),r29	; read flags
	jump    (REG_interrupt_adresse_routine_I2S)
	bset    #10,r29    	; clear latch 1 = I2S

; Timer 1 interrupt
	movei    #DSP_LSP_routine_interruption_Timer1,R28            ; 6 octets
	movei    #D_FLAGS,r30                      ; 6 octets
	jump      (R28)                          ; 2 octets
	load      (r30),r29  ; read flags                ; 2 octets = 16 octets

; Timer 2 interrupt
	movei  #DSP_LSP_routine_interruption_Timer2,r28            ; 6 octets
	movei  #D_FLAGS,r30                      ; 6 octets
	jump    (r28)                          ; 2 octets
	load    (r30),r29  ; read flags                ; 2 octets = 16 octets

; ----------------------------------
; DSP : routine en interruption I2S
; ----------------------------------
; R30/R29/R28/R27/R25/R24/R23/R22
REG_interrupt_buffer_source_gauche         .equr     R27
REG_interrupt_DEST_DAC_gauche          .equr    R26
REG_interrupt_DEST_DAC_droite            .equr    R25
REG_interrupt_TMP1                      .equr    R24
REG_interrupt_TMP2                      .equr    R23

REG_interrupt_Adress_Buffer_a_remplir      .equr    R22
REG_interrupt_nb_octets_a_remplir          .equr    R21
REG_interrupt_adresse_buffer_originale        .equr    R20
	// r19 free

REG_interrupt_utilise_par_main__octets_voie0          .equr    R18
REG_interrupt_utilise_par_main__octets_voie1          .equr    R17
REG_interrupt_utilise_par_main__octets_voie2          .equr    R16
REG_interrupt_utilise_par_main__octets_voie3          .equr    R15
; R14


; dispos:
; alt R12
; possible de libérer alt R11

; simple lecture d'un buffer
DSP_LSP_routine_interruption_I2S:
; 2 voies + 1 voie SFX = 14 bits + 14 bits + 14 bits = 15,5 bits
	load	(REG_interrupt_buffer_source_gauche),REG_interrupt_TMP1        ; valeur 16 bits sur 32 bits buffer gauche+droite
	addq	#4,REG_interrupt_buffer_source_gauche
	sharq	#16,REG_interrupt_TMP1            ; 16 bits du haut = gauche
	move	REG_interrupt_TMP1,REG_interrupt_TMP2
	store	REG_interrupt_TMP1,(REG_interrupt_DEST_DAC_gauche)
	store	REG_interrupt_TMP2,(REG_interrupt_DEST_DAC_droite)

;------------------------------------
; return from interrupt I2S
DSP_return_from_IRQ:
	addqt	#2,R28    	; next instruction
	bclr    #3,r29    	; clear IMASK
	jump    (r28)    	; return
	store	R29,(r30)  	; restore flags

DSP_LSP_routine_interruption_I2S_STOP:
	movei	#DSP_flag_STOP,REG_interrupt_TMP2
	bclr	#5,R29    	; clear I2S enabled = I2S Interrupt Enable Bit : stop I2S
	moveq	#DSP_STOP_flag_arret_Timer1,REG_interrupt_TMP1
	jr	DSP_return_from_IRQ
	store	REG_interrupt_TMP1,(REG_interrupt_TMP2)  ; launch stop Timer 1
;--------------------------------------------
; ---------------- Timer 1 ------------------
;--------------------------------------------
;
; copie les valeurs dans les variables, vers les registres
;
;

DSP_LSP_routine_interruption_Timer1_STOP:

	moveq    #DSP_STOP_flag_arret_Timer2,REG_interrupt_TMP2      ; launch timer 2 STOP
	bclr      #6,R29  ; clear Timer 1 Interrupt Enable Bit
	store      REG_interrupt_TMP2,(REG_interrupt_TMP1)

;------------------------------------
; return from interrupt Timer 1
	load    (r31),R28  ; return address
	bset    #11,R29    ; clear latch 1 = timer 1
	addq    #4,R31    ; pop from stack
	bclr    #3,R29    ; clear IMASK
	addqt  #2,r28    ; next instruction
	jump    (r28)    ; return
	store    r29,(r30)  ; restore flags

DSP_LSP_routine_interruption_Timer1:

; test STOP
	movei    #DSP_flag_STOP,REG_interrupt_TMP1
	load      (REG_interrupt_TMP1),REG_interrupt_TMP2
	cmpq    #DSP_STOP_flag_arret_Timer1,REG_interrupt_TMP2
	jr        eq,DSP_LSP_routine_interruption_Timer1_STOP
; for the sound engine:
	moveq    #2,REG_interrupt_nb_octets_a_remplir

; utilisés : R0/R1/R2/R3
; calculer nb octets remplis
; - calculer la difference entre debut et position actuelle du buffer pour savoir combien d'octets/samples il faut produire
	move     REG_interrupt_buffer_source_gauche,REG_interrupt_nb_octets_a_remplir
	movei    #DSP_pointeur_BUFFER_a_remplir,REG_interrupt_TMP1
	sub      REG_interrupt_adresse_buffer_originale,REG_interrupt_nb_octets_a_remplir
	movei   #DSP_pointeur_BUFFER_a_jouer,REG_interrupt_TMP2
; - timer 1 : switcher les 2 buffers
	load    (REG_interrupt_TMP1),R0
	load    (REG_interrupt_TMP2),R1
	movei   #Paula_flag_Tick_50Hz,R2
	store   R1,(REG_interrupt_TMP1)
	store   R0,(REG_interrupt_TMP2)
	move    R1,REG_interrupt_Adress_Buffer_a_remplir        ; celui que la routine en main doit remplir
	move    R0,REG_interrupt_adresse_buffer_originale        ; celui qui va etre joué
	moveq   #flag_timer_50HZ_OK ,R3
	move	R0,REG_interrupt_buffer_source_gauche
	store	R3,(R2)

	movei     #Paula_compteur_frames_Timer1,R0
	load      (r0),R1
	addq      #1,R1
	store     R1,(R0)

; - Mettre l'adresse du buffer a remplir dans un registre

;------------------------------------
; return from interrupt Timer 1
	load    (r31),R28  ; return address
	bset    #11,R29    ; clear latch 1 = timer 1
	addq    #4,R31    ; pop from stack
	addqt	#2,r28    ; next instruction
	bclr    #3,R29    ; clear IMASK
	jump    (r28)    ; return
	store   r29,(r30)  ; restore flags

DSP_LSP_routine_interruption_Timer2_STOP:
	moveq    #DSP_STOP_flag_arret_main,REG_interrupt_TMP2      ; launch timer 2 STOP
	bclr      #7,R29                                            ; clear Timer 2 Interrupt Enable Bit
	store      REG_interrupt_TMP2,(REG_interrupt_TMP1)
;------------------------------------
; return from interrupt Timer 2
	load    (r31),R28  ; return address
	bset      #12,R29    ; clear latch 2 = timer 2
	addq    #4,R31    ; pop from stack
	addqt	#2,r28    ; next instruction
	bclr    #3,R29    ; clear IMASK
	jump    (r28)    ; return
	store   r29,(r30)  ; restore flags

; ------------------- N/A ------------------
DSP_LSP_routine_interruption_Timer2:
; ------------------- N/A ------------------
; test STOP
	movei	#DSP_flag_STOP,REG_interrupt_TMP1
	load	(REG_interrupt_TMP1),REG_interrupt_TMP2
	cmpq    #DSP_STOP_flag_arret_Timer2,REG_interrupt_TMP2
	jr	eq,DSP_LSP_routine_interruption_Timer2_STOP
	nop
;------------------------------------

	include "readjoy.inc"

;------------------------------------
; return from interrupt Timer 2
	load    (r31),R28  ; return address
	bset    #12,R29    ; clear latch 3 = timer 1
	addq    #4,R31    ; pop from stack
	addqt	#2,r28    ; next instruction
	bclr    #3,R29    ; clear IMASK
	jump    (r28)    ; return
	store    r29,(r30)  ; restore flags

      .if      routine_interrupt_68000=1
;------------------------------------
; interruption provoquée par le 68000
;------------------------------------
DSP_PAULA_routine_interruption_CPU68000:
  movei      #PAULA_sample_location0,R14
; gere le dmacon
  movei      #dmacon_current_frame,R10
  movei      #DSP_main_DMACON_CLR,R4
  load       (R10),R8
  btst       #15,R8
  jump        eq,(R4)
  nop

; DMACON SET => force externe vers interne avec remise à zéro de la virgule
  bclr        #15,R8
  moveq      #0,R0
  movei      #channela,R11
  store        R8,(R10)

; voix 0/A
  btst        #0,R8
  jr          eq,DSP_main_DMACON_no_SET_0
  nop
  load        (R11),R1        ; location paula 68000
  shlq        #nb_bits_virgule_offset,R1
  store        R1,(R14+5)
  addq        #4,R11
  store        R1,(R14)        ; location interne << nb_bits_virgule_offset
  loadw      (R11),R2        ; length 68000
  or          R2,R2
  add        R2,R2
  shlq        #nb_bits_virgule_offset,R2
  subq        #4,R11
  add        R1,R2
  store        R2,(R14+6)          ; fin repeat << nb_bits_virgule_offset
  store        R0,(R14+1)          ; virgule
  store        R2,(R14+2)          ; end interne


DSP_main_DMACON_no_SET_0:
  addq        #16,R11
; voix 1/B
  addq        #28,R14
  btst        #1,R8
  jr          eq,DSP_main_DMACON_no_SET_1
  nop
  load        (R11),R1        ; location paula 68000
  shlq        #nb_bits_virgule_offset,R1
  store        R1,(R14+5)
  store        R1,(R14)            ; location interne
  addq        #4,R11
  loadw      (R11),R2        ; length 68000
  add        R2,R2
  shlq        #nb_bits_virgule_offset,R2
  subq        #4,R11
  add        R1,R2
  store        R2,(R14+6)
  store        R0,(R14+1)          ; virgule
  store        R2,(R14+2)          ; end interne

DSP_main_DMACON_no_SET_1:
  addq        #16,R11
; voix 2/C
  addq        #28,R14
  btst        #2,R8
  jr          eq,DSP_main_DMACON_no_SET_2
  nop
  load        (R11),R1        ; location paula 68000
  shlq        #nb_bits_virgule_offset,R1
  store        R1,(R14+5)
  store        R1,(R14)            ; location interne
  addq        #4,R11
  loadw      (R11),R2        ; length 68000
  add        R2,R2
  shlq        #nb_bits_virgule_offset,R2
  subq        #4,R11
  add        R1,R2
  store        R2,(R14+6)
  store        R0,(R14+1)          ; virgule
  store        R2,(R14+2)          ; end interne

DSP_main_DMACON_no_SET_2:
  addq        #16,R11
; voix 3/D
  addq        #28,R14
  btst        #3,R8
  jr          eq,DSP_main_DMACON_no_SET_3
  nop
  load        (R11),R1        ; location paula 68000
  shlq        #nb_bits_virgule_offset,R1
  store        R1,(R14+5)
  store        R1,(R14)            ; location interne
  addq        #4,R11
  loadw      (R11),R2        ; length 68000
  add        R2,R2
  shlq        #nb_bits_virgule_offset,R2
  subq        #4,R11
  add        R1,R2
  store        R2,(R14+6)
  store        R0,(R14+1)          ; virgule
  store        R2,(R14+2)          ; end interne
DSP_main_DMACON_no_SET_3:

DSP_main_DMACON_CLR:

DSP_PAULA_routine_interruption_CPU68000__retour:
;------------------------------------
; return from interrupt 68000 interrupt
  load    (r31),R28  ; return address
  bset    #9,R29    ; clear latch 1 = timer 1
  addq    #4,R31    ; pop from stack
  bclr    #3,R29    ; clear IMASK
  addqt  #2,r28    ; next instruction
  jump    (r28)    ; return
  store    r29,(r30)  ; restore flags
    .endif

; ------------- main DSP ------------------
DSP_routine_init_DSP:
; assume run from bank 1
	movei	#DSP_ISP+(DSP_STACK_SIZE*4),r0      ; init isp
	moveta  r0,r31                  ; ISP (bank 0)
	movei	#DSP_USP+(DSP_STACK_SIZE*4),r31      ; init usp

; calculs des frequences deplacé dans DSP
; sclk I2S
	movei  #$00F14003,r0
	loadb	(r0),r3
	btst	#4,r3
	movei	#415530<<8,r1  ;frequence_Video_Clock_divisee*128
	jr	eq,initPAL
	nop
	movei  #415483<<8,r1  ;frequence_Video_Clock_divisee*128
initPAL:
	movei    #LSP_DSP_Audio_frequence,R0
	div      R0,R1
	movei    #128,R2
	add      R2,R1    ; +128 = +0.5
	shrq     #8,R1
	subq     #1,R1
	movei    #DSP_parametre_de_frequence_I2S,r2
	store    R1,(R2)
;calcul inverse
	addq    #1,R1

	btst  #4,r3
	movei  #26593900,r0  ;frequence_Video_Clock
	jr	eq,initPAL2
	shlq	#6,r1		; *64
	movei  #26590906,r0  ;frequence_Video_Clock
initPAL2:
	div      R1,R0
	movei    #DSP_frequence_de_replay_reelle_I2S,R2

; correction fréquence, trop aigue
	movei    #PAULA_corretion_frequence,R11
	add      R11,R0

	store    R0,(R2)
; init I2S
	movei  #SCLK,r10
	movei  #SMODE,r11
	movei  #DSP_parametre_de_frequence_I2S,r12
	movei  #%001101,r13      ; SMODE bascule sur RISING
	load  (r12),r12        ; SCLK
	store  r12,(r10)
	store  r13,(r11)

; init Timer 1 = 50 HZ

; 26593900 / 50 = 531 878 => 2 × 73 × 3643 => 146*3643
	movei  #JPIT1,r10        ; F10000
	movei  #(145<<16)|(3643-1),r12        ; Timer 1 Pre-scaler
	store  r12,(r10)        ; JPIT1 & JPIT2

; init timer 2
	movei  #JPIT3,r10        ; F10004
	movei  #(145<<16)|(955-1),r12      ; Timer 1 Pre-scaler 951=200hz
	store  r12,(r10)        ; JPIT1 & JPIT2

; enable interrupts
	movei  #D_FLAGS,r30

; prod version
	movei  #D_I2SENA|D_TIM1ENA|D_TIM2ENA|REGPAGE|D_CPUENA,r29      ; I2S+Timer 1+timer 2+CPU
; prod version
;----------------------------
; registres pour replay reel samples dans I2S
	movei	#L_I2S,R1
	moveta	R1,REG_interrupt_DEST_DAC_gauche
	addq	#4,r1
	moveta	r1,REG_interrupt_DEST_DAC_droite

	movei	#DSP_LSP_routine_interruption_I2S,R2
	moveta	R2,REG_interrupt_adresse_routine_I2S

; buffers pour I2S
	movei	#DSP_frequence_de_replay_reelle_I2S,R2
	movei	#DSP_pointeur_BUFFER_a_jouer,R0
	load	(R2),R3		   ; frequence replay reel
	movei	#50,R4
	movei	#PAULA_DSP_fin,R1
	div	R4,R3
	move	R1,R5
	moveta	R1,REG_interrupt_Adress_Buffer_a_remplir	  ; buffer a remplir = 1er
	addq	#4,R3
	shlq	#2,R3	 ; *4 / 4 octets par sample : droite+gauche en 16 bits
	add	R3,R1
	store	R1,(R0)
	add	R3,R1

; clear les buffers, de R5 à fin de buffers
	sub	r5,r1
	moveq	#0,R3
.clear_ram_dsp:
	subq	#4,r1
	store	R3,(r5)
	jr	ne,.clear_ram_dsp
	addq	#4,R5

	movei	#PAULA_DSP_fin,R1
	moveta	R3,REG_interrupt_nb_octets_a_remplir        ; init REG_interrupt_nb_octets_a_remplir=0
	moveta	R1,REG_interrupt_buffer_source_gauche
	moveta	R1,REG_interrupt_adresse_buffer_originale        ; buffer a jouer = 2eme, +N octets

; demarre les timers
	store  r29,(r30)
;-----------------------------------------------------------
;
;
;      main loop
;
;
;-----------------------------------------------------------
DSP_boucle_centrale:
	movei	#DSP_flag_STOP,R0
	load	(R0),R1
	cmpq	#DSP_STOP_flag_STOP_NOW,R1
	jr	ne,.pas_STOP
	moveq	#DSP_STOP_flag_arret_I2S,R2
	movei	#DSP_LSP_routine_interruption_I2S_STOP,R3
	store	R2,(R0)
	moveta	R3,REG_interrupt_adresse_routine_I2S	; STOP routine I2S
.pas_STOP:
	cmpq	#DSP_STOP_flag_arret_main,R1
	jr	ne,.DSP_pas_stop_final
	nop
	movei	#D_CTRL,R20
	moveq	#0,R6
	store	R6,(R0)					; DSP_FLAG_STOP_DSP=0
.wait:
	jr	.wait
	store	R6,(R20)

.DSP_pas_stop_final:
	movei      #Paula_flag_Tick_50Hz,R0

boucle_centrale_wait_for_pmix_hippel:
	load        (R0),R1
	cmpq      #flag_timer_50HZ_OK,R1
	jr          ne,boucle_centrale_wait_for_pmix_hippel
	nop

  .if          1=0
; copie de buffers_paula_asynchrones
	movei      #table_buffers_paula_asynchrones__READ,R2
	movei      #table_buffers_paula_asynchrones,R7
	load        (R2),R3
	move      R3,R5
	moveq      #%111,R6
	addq        #1,R3
	shlq        #2,R5
	and        R6,R3        ; loop on 8
	add        R5,R7
	store        R3,(R2)        ; update table_buffers_paula_asynchrones__READ
	load        (R7),R20      ; buffer
	.endif

	movei      #buffers_paula_asynchrones,R20

; écraser Paula_flag_Tick_50Hz
;  moveq      #flag_en_attente_timer_50HZ,R2
;  store        R2,(R0)


;prendre en compte location en +12
;et length en +10
;si location pas vide, pour mettre en interne avec virgule a zero


; copie et transformation des valeurs Paula en variables DSP
  movei    #DSP_frequence_de_replay_reelle_I2S,R4
  movei    #3546895,R12
  load      (R4),R6        ; R6 = frequence réelle de replay, après calcul

  ;movei      #Paula_custom,R20
  movei      #PAULA_sample_location0,R14      ; 0+3 / ecart de 7


  .if        lecture_valeurs_paula=1
;-A-
	load   (R20),R1        ; location 0/A =00
	addq   #4,R20
	loadw  (R20),R2        ; length en .w =04
	cmpq 	#0,R2
	jr      ne,.pas_length_a_zero
	nop
	movei	#silence,R1
	moveq	#1,R2
.pas_length_a_zero:
	shlq       #nb_bits_virgule_offset,R1
	shlq       #nb_bits_virgule_offset,R2
	store      R1,(R14+5)        ; PAULA_sample_location0 externe
	add        R2,R2
	add        R1,R2          ; location + (2*length) = end
	store      R2,(R14+6)      ; end externe
	addq       #2,R20

	loadw      (R20),R2        ; period/note = 06
	addq       #2,R20
; period
	cmpq    #0,R2
	jr      ne,.1
	nop
	moveq    #0,R4
	jr      .2
	nop
.1:
	move    R12,R4
	div      R2,R4      ; (3546895 / note) note minimale = 108
	or        R4,R4
	shlq      #nb_bits_virgule_offset,R4
	div      R6,R4      ; (3546895 / note) / frequence I2S en 0:15
	or        R4,R4
.2:
	store        R4,(R14+4)    ; increment
	loadw      (R20),R2        ; volume =08
 .if channel_1 = 0
	moveq      #0,R5
	addq        #2+2,R20
	store        R5,(R14+3)      ; volume DSP
 .endif
 .if channel_1 = 1
	addq        #2+2,R20
	store        R2,(R14+3)      ; volume DSP
 .endif

	load        (R20),R1        ; location interne
	cmpq      #0,R1
	jr          eq,.pas_de_dmacon_force0
	subqt      #2,R20

	loadw      (R20),R2        ; length interne
	shlq        #nb_bits_virgule_offset,R1
	shlq        #nb_bits_virgule_offset,R2
	store        R1,(R14)        ; PAULA_sample_location0 interne
	add        R2,R2
	add        R1,R2          ; location + (2*length) = end
	store        R2,(R14+2)      ; end interne
.pas_de_dmacon_force0:
	addq        #6,R20
	addq        #28,R14

DSP_main_recupe_voie1:
;-B-
	load        (R20),R1        ; location 0/A
	addq        #4,R20
	loadw      (R20),R2        ; length en .w
	or          R2,R2
	cmpq      #0,R2
	jr          ne,.pas_length_a_zero
	nop
	movei      #silence,R1
	moveq      #1,R2
.pas_length_a_zero:
	shlq        #nb_bits_virgule_offset,R1
	shlq        #nb_bits_virgule_offset,R2
	store        R1,(R14+5)        ; PAULA_sample_location0 externe
	add        R2,R2
	add        R1,R2          ; location + (2*length) = end
	store        R2,(R14+6)      ; externe
	addq        #2,R20
	loadw      (R20),R2        ; period/note
	addq        #2,R20
; period
    cmpq    #0,R2
    jr      ne,.1
    nop
    moveq    #0,R4
    jr      .2
    nop
.1:
    move    R12,R4
    div      R2,R4      ; (3546895 / note) note minimale = 108
    or        R4,R4
    shlq      #nb_bits_virgule_offset,R4
    div      R6,R4      ; (3546895 / note) / frequence I2S en 0:15
    or        R4,R4
.2:
  store        R4,(R14+4)    ; increment
  loadw      (R20),R2        ; volume
  .if        channel_2=0
  moveq      #0,R5
  addq        #2+2,R20
  store        R5,(R14+3)      ; volume DSP
  .endif
  .if        channel_2=1
  addq        #2+2,R20
  store        R2,(R14+3)      ; volume DSP
  .endif
  load        (R20),R1        ; location interne
  cmpq      #0,R1
  jr          eq,.pas_de_dmacon_force1
  subqt      #2,R20

  loadw      (R20),R2        ; length interne
  shlq        #nb_bits_virgule_offset,R1
  shlq        #nb_bits_virgule_offset,R2
  store        R1,(R14)        ; PAULA_sample_location0 interne
  add        R2,R2
  add        R1,R2          ; location + (2*length) = end
  store        R2,(R14+2)      ; end interne
.pas_de_dmacon_force1:
  addq        #6,R20
  addq        #28,R14

DSP_main_recupe_voie2:
;-C-
  load        (R20),R1        ; location 0/A
  addq        #4,R20
  loadw      (R20),R2        ; length en .w
  or          R2,R2
  cmpq      #0,R2
  jr          ne,.pas_length_a_zero
  nop
  movei      #silence,R1
  moveq      #1,R2
.pas_length_a_zero:
  shlq        #nb_bits_virgule_offset,R1
  shlq        #nb_bits_virgule_offset,R2
  store        R1,(R14+5)        ; PAULA_sample_location0 externe
  add        R2,R2
  add        R1,R2          ; location + (2*length) = end
  store        R2,(R14+6)      ; externe
  addq        #2,R20
  loadw      (R20),R2        ; period/note
  addq        #2,R20
; period
    cmpq    #0,R2
    jr      ne,.1
    nop
    moveq    #0,R4
    jr      .2
    nop
  .1:
    move    R12,R4
    div      R2,R4      ; (3546895 / note) note minimale = 108
    or        R4,R4
    shlq      #nb_bits_virgule_offset,R4
    div      R6,R4      ; (3546895 / note) / frequence I2S en 0:15
    or        R4,R4
  .2:
  store        R4,(R14+4)    ; increment
  loadw      (R20),R2        ; volume
  .if        channel_3=0
  moveq      #0,R5
  addq        #2+2,R20
  store        R5,(R14+3)      ; volume DSP
  .endif
  .if        channel_3=1
  addq        #2+2,R20
  store        R2,(R14+3)      ; volume DSP
  .endif
  load        (R20),R1        ; location interne
  cmpq      #0,R1
  jr          eq,.pas_de_dmacon_force2
  subqt      #2,R20

  loadw      (R20),R2        ; length interne
  shlq        #nb_bits_virgule_offset,R1
  shlq        #nb_bits_virgule_offset,R2
  store        R1,(R14)        ; PAULA_sample_location0 interne
  add        R2,R2
  add        R1,R2          ; location + (2*length) = end
  store        R2,(R14+2)      ; end interne
.pas_de_dmacon_force2:
  addq        #6,R20
  addq        #28,R14

DSP_main_recupe_voie3:
;-D-
  load        (R20),R1        ; location 0/A
  addq        #4,R20
  loadw      (R20),R2        ; length en .w
  or          R2,R2
  cmpq      #0,R2
  jr          ne,.pas_length_a_zero
  nop
  movei      #silence,R1
  moveq      #1,R2
.pas_length_a_zero:
  shlq        #nb_bits_virgule_offset,R1
  shlq        #nb_bits_virgule_offset,R2
  store        R1,(R14+5)        ; PAULA_sample_location0 externe
  add        R2,R2
  add        R1,R2          ; location + (2*length) = end
  store        R2,(R14+6)      ; externe
  addq        #2,R20
  loadw      (R20),R2        ; period/note
  addq        #2,R20
; period
    cmpq    #0,R2
    jr      ne,.1
    nop
    moveq    #0,R4
    jr      .2
    nop
  .1:
    move    R12,R4
    div      R2,R4      ; (3546895 / note) note minimale = 108
    or        R4,R4
    shlq      #nb_bits_virgule_offset,R4
    div      R6,R4      ; (3546895 / note) / frequence I2S en 0:15
    or        R4,R4
  .2:
  store        R4,(R14+4)    ; increment
  loadw      (R20),R2        ; volume
  .if        channel_4=0
  moveq      #0,R5
  addq        #2+2,R20
  store        R5,(R14+3)      ; volume DSP
  .endif
  .if        channel_4=1
  addq        #2+2,R20
  store        R2,(R14+3)      ; volume DSP
  .endif
  load        (R20),R1        ; location interne
  cmpq      #0,R1
  jr          eq,.pas_de_dmacon_force3
  subqt      #2,R20

  loadw      (R20),R2        ; length interne
  shlq        #nb_bits_virgule_offset,R1
  shlq        #nb_bits_virgule_offset,R2
  store        R1,(R14)        ; PAULA_sample_location0 interne
  add        R2,R2
  add        R1,R2          ; location + (2*length) = end
  store        R2,(R14+2)      ; end interne
.pas_de_dmacon_force3:
  ;addq        #6,R20
  ;addq        #28,R14


  .endif


; remplissage du buffer
;
; - lire 1 octet
; - ajouter increment a location virgule
; - si carry=1, increment location entier
; - multiplier par le volume
; -

REG_main_sample1                    .equr          R0
REG_main_sample2                    .equr          R1

REG_main_location_entier0              .equr          R2
REG_main_location_end0                .equr          R3
REG_main_increment0                  .equr          R4
REG_main_volume0                    .equr          R5

REG_main_location_entier1              .equr          R6
REG_main_location_end1                .equr          R7
REG_main_increment1                  .equr          R8
REG_main_volume1                    .equr          R9

REG_main_location_entier2              .equr          R10
REG_main_sample_voie0                .equr          R11
REG_main_location_end2                .equr          R12
REG_main_increment2                  .equr          R13
;R14=pointeur source des valeurs des registres
REG_main_volume2                    .equr          R15

REG_main_location_entier3              .equr          R16
REG_main_location_end3                .equr          R17
REG_main_increment3                  .equr          R18
REG_main_volume3                    .equr          R19

REG_main_and_mask_FFFF              .equr          R20

REG_main_location_integer_old            .equr          R21
REG_main_location_integer_new          .equr          R22
REG_main_location_virgule_a_conserver    .equr          R23

REG_main_sample_voie1                .equr          R24
REG_main_sample_voie2                .equr          R25

REG_main_buffer_destination              .equr          R26
REG_main_volume_music                .equr          R27
REG_main_sample_voie3                .equr          R28
REG_main_nb_octets_a_remplir          .equr          R29
REG_main_mask_and_FFFFFFFC        .equr          R30


; 0+3=left
  movei      #PAULA_sample_location0,R14
  movefa    REG_interrupt_nb_octets_a_remplir,REG_main_nb_octets_a_remplir

  load        (R14),REG_main_location_entier0
  movei      #$FFFFFFFC,REG_main_mask_and_FFFFFFFC
    load        (R14+7),REG_main_location_entier1
  load        (R14+14),REG_main_location_entier2
      shrq        #2,REG_main_nb_octets_a_remplir      ; /4
    load        (R14+21),REG_main_location_entier3



  load        (R14+2),REG_main_location_end0
      movefa    REG_interrupt_Adress_Buffer_a_remplir,REG_main_buffer_destination
    load        (R14+9),REG_main_location_end1
  load        (R14+16),REG_main_location_end2
    load        (R14+23),REG_main_location_end3

  load        (R14+3),REG_main_volume0
    load        (R14+10),REG_main_volume1
  load        (R14+17),REG_main_volume2
    load        (R14+24),REG_main_volume3

  load        (R14+4),REG_main_increment0
  movei      #DSP_volume_music,REG_main_volume_music
    load        (R14+11),REG_main_increment1
  load        (REG_main_volume_music),REG_main_volume_music
  load        (R14+18),REG_main_increment2
  movei      #$FFFF,REG_main_and_mask_FFFF
    load        (R14+25),REG_main_increment3

; preload samples
      .if      mixage=1
  move      REG_main_location_entier0,REG_main_sample1
  move      REG_main_location_entier1,REG_main_sample2
  shrq        #nb_bits_virgule_offset,REG_main_sample1          ; enleve la partie a virgule
  shrq        #nb_bits_virgule_offset,REG_main_sample2
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample1
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample2
  load        (REG_main_sample1),REG_main_sample_voie0      ; load 4 octets d'un coup
  load        (REG_main_sample2),REG_main_sample_voie1
  moveta    REG_main_sample_voie0,REG_interrupt_utilise_par_main__octets_voie0
  move      REG_main_location_entier2,REG_main_sample1
  moveta    REG_main_sample_voie1,REG_interrupt_utilise_par_main__octets_voie1
  move      REG_main_location_entier3,REG_main_sample2
  shrq        #nb_bits_virgule_offset,REG_main_sample1
  shrq        #nb_bits_virgule_offset,REG_main_sample2
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample1
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample2
  load        (REG_main_sample1),REG_main_sample_voie2
  load        (REG_main_sample2),REG_main_sample_voie3
  moveta    REG_main_sample_voie2,REG_interrupt_utilise_par_main__octets_voie2
  moveta    REG_main_sample_voie3,REG_interrupt_utilise_par_main__octets_voie3
      .endif

; mixage
DSP_main_boucle_remplissage_4voies:
      .if      mixage=1


; V2 : sans relecture
; voie 0
  move      REG_main_location_entier0,REG_main_location_integer_old
  add        REG_main_increment0,REG_main_location_entier0
;  tester le bouclage
  cmp        REG_main_location_end0,REG_main_location_entier0
  jr          mi,.pas_bouclage0

  move      REG_main_location_entier0,REG_main_location_virgule_a_conserver
  load        (R14+6),REG_main_location_end0
  shlq        #32-nb_bits_virgule_offset,REG_main_location_virgule_a_conserver
  load        (R14+5),REG_main_location_entier0
  shrq        #32-nb_bits_virgule_offset,REG_main_location_virgule_a_conserver
  or          REG_main_location_virgule_a_conserver,REG_main_location_entier0

.pas_bouclage0:
; refresh du sample
  shrq        #nb_bits_virgule_offset,REG_main_location_integer_old
  and        REG_main_mask_and_FFFFFFFC,REG_main_location_integer_old        ; old adresse location modulo 4
  move      REG_main_location_entier0,REG_main_sample1
  shrq        #nb_bits_virgule_offset,REG_main_sample1
  move      REG_main_sample1,REG_main_sample2                    ; REG_main_sample2 = actuelle location sans virgule
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample1        ; actuelle adresse location modulo 4
  cmp        REG_main_location_integer_old,REG_main_sample1
  jr          eq,.pas_de_nouveau_octet_a_lire0
  nop

  load        (REG_main_sample1),REG_main_sample_voie0
  moveta    REG_main_sample_voie0,REG_interrupt_utilise_par_main__octets_voie0
.pas_de_nouveau_octet_a_lire0:
; il faut recuperer le bon octet et le multiplier par le volume
  not        REG_main_mask_and_FFFFFFFC        ; => %11
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample2      ; adresse and %11 = numéro octet a lire
  shlq        #3,REG_main_sample2
  not        REG_main_mask_and_FFFFFFFC        ; => $FFFFFFFC
  neg        REG_main_sample2                    ;  - (numéro d'octet a utiliser  << 3) => -0 -8 -16 -24
  movefa    REG_interrupt_utilise_par_main__octets_voie0,REG_main_sample_voie0
  sh        REG_main_sample2,REG_main_sample_voie0
    move      REG_main_location_entier1,REG_main_location_integer_old      ; voie 1
  sharq      #24,REG_main_sample_voie0
    add        REG_main_increment1,REG_main_location_entier1                ; voie 1
  imult        REG_main_volume0,REG_main_sample_voie0



; voie 1
;  tester le bouclage
  cmp        REG_main_location_end1,REG_main_location_entier1
  jr          mi,.pas_bouclage1

  move      REG_main_location_entier1,REG_main_location_virgule_a_conserver
  load        (R14+13),REG_main_location_end1
  shlq        #32-nb_bits_virgule_offset,REG_main_location_virgule_a_conserver
  load        (R14+12),REG_main_location_entier1
  shrq        #32-nb_bits_virgule_offset,REG_main_location_virgule_a_conserver
  or          REG_main_location_virgule_a_conserver,REG_main_location_entier1

.pas_bouclage1:
; refresh du sample
  shrq        #nb_bits_virgule_offset,REG_main_location_integer_old
  and        REG_main_mask_and_FFFFFFFC,REG_main_location_integer_old        ; old adresse location modulo 4
  move      REG_main_location_entier1,REG_main_sample1
  shrq        #nb_bits_virgule_offset,REG_main_sample1
  move      REG_main_sample1,REG_main_sample2                    ; REG_main_sample2 = actuelle location sans virgule
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample1        ; actuelle adresse location modulo 4
  cmp        REG_main_location_integer_old,REG_main_sample1
  jr          eq,.pas_de_nouveau_octet_a_lire1
  nop

  load        (REG_main_sample1),REG_main_sample_voie1
  moveta    REG_main_sample_voie1,REG_interrupt_utilise_par_main__octets_voie1
.pas_de_nouveau_octet_a_lire1:
; il faut recuperer le bon octet et le multiplier par le volume
  not        REG_main_mask_and_FFFFFFFC        ; => %11
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample2      ; adresse and %11 = numéro octet a lire
  shlq        #3,REG_main_sample2
  not        REG_main_mask_and_FFFFFFFC        ; => $FFFFFFFC
  neg        REG_main_sample2                    ;  - (numéro d'octet a utiliser  << 3) => -0 -8 -16 -24
  movefa    REG_interrupt_utilise_par_main__octets_voie1,REG_main_sample_voie1
  sh        REG_main_sample2,REG_main_sample_voie1
    move      REG_main_location_entier2,REG_main_location_integer_old      ; voie 2
  sharq      #24,REG_main_sample_voie1
    add        REG_main_increment2,REG_main_location_entier2      ; voie 2
  imult        REG_main_volume1,REG_main_sample_voie1



; voie 2
;  tester le bouclage
  cmp        REG_main_location_end2,REG_main_location_entier2
  jr          mi,.pas_bouclage2

  move      REG_main_location_entier2,REG_main_location_virgule_a_conserver
  load        (R14+20),REG_main_location_end2
  shlq        #32-nb_bits_virgule_offset,REG_main_location_virgule_a_conserver
  load        (R14+19),REG_main_location_entier2
  shrq        #32-nb_bits_virgule_offset,REG_main_location_virgule_a_conserver
  or          REG_main_location_virgule_a_conserver,REG_main_location_entier2

.pas_bouclage2:
; refresh du sample
  shrq        #nb_bits_virgule_offset,REG_main_location_integer_old
  and        REG_main_mask_and_FFFFFFFC,REG_main_location_integer_old        ; old adresse location modulo 4
  move      REG_main_location_entier2,REG_main_sample1
  shrq        #nb_bits_virgule_offset,REG_main_sample1
  move      REG_main_sample1,REG_main_sample2                    ; REG_main_sample2 = actuelle location sans virgule
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample1        ; actuelle adresse location modulo 4
  cmp        REG_main_location_integer_old,REG_main_sample1
  jr          eq,.pas_de_nouveau_octet_a_lire2
  nop

  load        (REG_main_sample1),REG_main_sample_voie2
  moveta    REG_main_sample_voie2,REG_interrupt_utilise_par_main__octets_voie2
.pas_de_nouveau_octet_a_lire2:
; il faut recuperer le bon octet et le multiplier par le volume
  not        REG_main_mask_and_FFFFFFFC        ; => %11
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample2      ; adresse and %11 = numéro octet a lire
  shlq        #3,REG_main_sample2
  not        REG_main_mask_and_FFFFFFFC        ; => $FFFFFFFC
  neg        REG_main_sample2                    ;  - (numéro d'octet a utiliser  << 3) => -0 -8 -16 -24
  movefa    REG_interrupt_utilise_par_main__octets_voie2,REG_main_sample_voie2
  sh        REG_main_sample2,REG_main_sample_voie2
    move      REG_main_location_entier3,REG_main_location_integer_old      ; voie 3
  sharq      #24,REG_main_sample_voie2
    add        REG_main_increment3,REG_main_location_entier3              ; voie 3
  imult        REG_main_volume2,REG_main_sample_voie2


; voie 3
;  tester le bouclage
  cmp        REG_main_location_end3,REG_main_location_entier3
  jr          mi,.pas_bouclage3

  move      REG_main_location_entier3,REG_main_location_virgule_a_conserver
  load        (R14+27),REG_main_location_end3
  shlq        #32-nb_bits_virgule_offset,REG_main_location_virgule_a_conserver
  load        (R14+26),REG_main_location_entier3
  shrq        #32-nb_bits_virgule_offset,REG_main_location_virgule_a_conserver
  or          REG_main_location_virgule_a_conserver,REG_main_location_entier3

.pas_bouclage3:
; refresh du sample
  shrq        #nb_bits_virgule_offset,REG_main_location_integer_old
  and        REG_main_mask_and_FFFFFFFC,REG_main_location_integer_old        ; old adresse location modulo 4
  move      REG_main_location_entier3,REG_main_sample1
  shrq        #nb_bits_virgule_offset,REG_main_sample1
  move      REG_main_sample1,REG_main_sample2                    ; REG_main_sample2 = actuelle location sans virgule
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample1        ; actuelle adresse location modulo 4
  cmp        REG_main_location_integer_old,REG_main_sample1
  jr          eq,.pas_de_nouveau_octet_a_lire3
  nop

  load        (REG_main_sample1),REG_main_sample_voie3
  moveta    REG_main_sample_voie3,REG_interrupt_utilise_par_main__octets_voie3
.pas_de_nouveau_octet_a_lire3:
; il faut recuperer le bon octet et le multiplier par le volume
  not        REG_main_mask_and_FFFFFFFC        ; => %11
  and        REG_main_mask_and_FFFFFFFC,REG_main_sample2      ; adresse and %11 = numéro octet a lire
  shlq        #3,REG_main_sample2
  not        REG_main_mask_and_FFFFFFFC        ; => $FFFFFFFC
  neg        REG_main_sample2                    ;  - (numéro d'octet a utiliser  << 3) => -0 -8 -16 -24
  movefa    REG_interrupt_utilise_par_main__octets_voie3,REG_main_sample_voie3
  sh        REG_main_sample2,REG_main_sample_voie3
  sharq      #24,REG_main_sample_voie3
  imult        REG_main_volume3,REG_main_sample_voie3





  move      REG_main_sample_voie0,REG_main_sample1
  move      REG_main_sample_voie1,REG_main_sample2
  add        REG_main_sample_voie3,REG_main_sample1        ; 0+3=left
  add        REG_main_sample_voie2,REG_main_sample2        ; 1+2=right
  imult        REG_main_volume_music,REG_main_sample1
  imult        REG_main_volume_music,REG_main_sample2
  sharq      #8,REG_main_sample1
  sharq      #8,REG_main_sample2


  shlq        #16,REG_main_sample1
  and        REG_main_and_mask_FFFF,REG_main_sample2    ; le sample est signé, il faut éliminer les 16 bits du haut
  movei      #DSP_main_boucle_remplissage_4voies,REG_main_location_virgule_a_conserver
  or          REG_main_sample2,REG_main_sample1

  subq        #1,REG_main_nb_octets_a_remplir
  store        REG_main_sample1,(REG_main_buffer_destination)

  jump        hi,(REG_main_location_virgule_a_conserver)
  addqt      #4,REG_main_buffer_destination



  store        REG_main_sample1,(REG_main_buffer_destination)

  .endif

; stocke l'avancée des registres
  store        REG_main_location_entier0,(R14)
  store        REG_main_location_entier1,(R14+7)
  store        REG_main_location_entier2,(R14+14)
  store        REG_main_location_entier3,(R14+21)

  store        REG_main_location_end0,(R14+2)
  store        REG_main_location_end1,(R14+9)
  store        REG_main_location_end2,(R14+16)
  store        REG_main_location_end3,(R14+23)

; gestion SFX / mixage SFX
; 2 voies : left / right
; bouclage sur pointeur_silence / pointeur_fin_silence / volume = 0 / increment = $99990000


.EQURUNDEF        REG_main_sample1
.EQURUNDEF        REG_main_sample2

.EQURUNDEF        REG_main_location_entier0
.EQURUNDEF        REG_main_location_end0
.EQURUNDEF        REG_main_increment0
.EQURUNDEF        REG_main_volume0

.EQURUNDEF        REG_main_location_entier1
.EQURUNDEF        REG_main_location_end1
.EQURUNDEF        REG_main_increment1
.EQURUNDEF        REG_main_volume1

.EQURUNDEF        REG_main_location_entier2
.EQURUNDEF        REG_main_sample_voie0
.EQURUNDEF        REG_main_location_end2
.EQURUNDEF        REG_main_increment2
;R14=pointeur source des valeurs des registres
.EQURUNDEF        REG_main_volume2

.EQURUNDEF        REG_main_location_entier3
.EQURUNDEF        REG_main_location_end3
.EQURUNDEF        REG_main_increment3
.EQURUNDEF        REG_main_volume3

.EQURUNDEF        REG_main_and_mask_FFFF

.EQURUNDEF        REG_main_location_integer_old
.EQURUNDEF        REG_main_location_integer_new
.EQURUNDEF        REG_main_location_virgule_a_conserver

.EQURUNDEF        REG_main_sample_voie1
.EQURUNDEF        REG_main_sample_voie2

.EQURUNDEF        REG_main_buffer_destination
.EQURUNDEF        REG_main_boucle
.EQURUNDEF        REG_main_sample_voie3
.EQURUNDEF        REG_main_nb_octets_a_remplir
.EQURUNDEF        REG_main_mask_and_FFFFFFFC














REG_main_SFX__pointeur_silence_repeat      .equr        R0
REG_main_SFX__pointeur_fin_silence_repeat  .equr        R1
REG_main_SFX__volume_repeat              .equr        R2
REG_main_SFX__increment_repeat            .equr        R3

; left
REG_main_SFX__location_entier0            .equr        R4
REG_main_SFX__location_end0              .equr        R5
REG_main_SFX__increment0                .equr        R6
REG_main_SFX__volume0                  .equr        R7

; right
REG_main_SFX__location_entier1            .equr        R8
REG_main_SFX__location_end1              .equr        R9
REG_main_SFX__increment1                .equr        R10
REG_main_SFX__volume1                  .equr        R11

REG_main_sample1                    .equr          R12
REG_main_sample2                    .equr          R13
; R14
; R15

REG_main_SFX__volume_SFX              .equr        R18
REG_main_SFX__tmp1                    .equr        R19
REG_main_and_mask_FFFF                  .equr        R20
REG_main_location_integer_old                .equr        R21
REG_main_SFX__tmp2                    .equr        R22

REG_main_SFX__sample1                  .equr        R23
REG_main_SFX__sample2                  .equr        R24
REG_main_SFX__sample_voie0              .equr        R25
REG_main_SFX__sample_voie1              .equr        R26
REG_main_SFX__destination_buffer          .equr        R27
REG_main_SFX__nb_octets_a_remplir        .equr        R28
REG_main_SFX__loop                      .equr        R29
REG_main_mask_and_FFFFFFFC            .equr        R30

  .if      mix_SFX=1

  movei    #DSP_frequence_de_replay_reelle_I2S,R4
  movei    #3546895,REG_main_SFX__tmp1
  load      (R4),REG_main_SFX__tmp2        ; R6 = frequence réelle de replay, après calcul


    movei      #DSP_pointeur_silence,REG_main_SFX__pointeur_silence_repeat
    movei      #DSP_volume_SFX,REG_main_SFX__volume_SFX
    movei      #DSP_pointeur_fin_silence,REG_main_SFX__pointeur_fin_silence_repeat
    load        (REG_main_SFX__pointeur_silence_repeat),REG_main_SFX__pointeur_silence_repeat
    moveq      #0,REG_main_SFX__volume_repeat
    load        (REG_main_SFX__pointeur_fin_silence_repeat),REG_main_SFX__pointeur_fin_silence_repeat
    movei      #$4800,REG_main_SFX__increment_repeat
    load        (REG_main_SFX__volume_SFX),REG_main_SFX__volume_SFX

    movefa    REG_interrupt_nb_octets_a_remplir,REG_main_SFX__nb_octets_a_remplir
    movefa    REG_interrupt_Adress_Buffer_a_remplir,REG_main_SFX__destination_buffer
    shrq        #2,REG_main_SFX__nb_octets_a_remplir      ; /4

; lecture valeurs SFX
; check si nouvelles valeurs
    movei      #PAULA_SFX_left,R14
    load        (R14),REG_main_SFX__location_entier0
    cmpq      #0,REG_main_SFX__location_entier0
    jr		eq,DSP_pas_valeur_SFX_left

    load        (R14+2),REG_main_SFX__location_end0
    movei      #PAULA_SFX_left_private,R15

    store        REG_main_SFX__location_entier0,(R15)
    load        (R14+3),REG_main_SFX__volume0
    store        REG_main_SFX__location_end0,(R15+2)
    load        (R14+4),REG_main_SFX__sample2
    store        REG_main_SFX__volume0,(R15+3)
    moveq      #0,REG_main_sample1
    store        REG_main_SFX__sample2,(R15+4)
    store        REG_main_sample1,(R14)
DSP_pas_valeur_SFX_left:

    movei      #PAULA_SFX_right,R14
    load        (R14),REG_main_SFX__location_entier0
    cmpq      #0,REG_main_SFX__location_entier0
    jr		eq,DSP_pas_de_nouvelle_valeur_SFX_right

    load        (R14+2),REG_main_SFX__location_end0
    movei      #PAULA_SFX_right_private,R15

    store        REG_main_SFX__location_entier0,(R15)
    load        (R14+3),REG_main_SFX__volume0
    store        REG_main_SFX__location_end0,(R15+2)
    load        (R14+4),REG_main_SFX__sample2
    store        REG_main_SFX__volume0,(R15+3)
    moveq      #0,REG_main_sample1
    store        REG_main_SFX__sample2,(R15+4)
    store        REG_main_sample1,(R14)
DSP_pas_de_nouvelle_valeur_SFX_right:

; SFX 0
    movei      #PAULA_SFX_left_private,R14      ; SFX left
    load        (R14),REG_main_SFX__location_entier0

    load        (R14+4),REG_main_SFX__sample2
; period
    cmpq    #0,REG_main_SFX__sample2
    load        (R14+3),REG_main_SFX__volume0
    jr      ne,.1
    load        (R14+2),REG_main_SFX__location_end0

    jr      .2
    move    REG_main_SFX__increment_repeat,REG_main_SFX__increment0

  .1:
    move    REG_main_SFX__tmp1,REG_main_SFX__increment0
    div      REG_main_SFX__sample2,REG_main_SFX__increment0      ; (3546895 / note) note minimale = 108
    or        REG_main_SFX__increment0,REG_main_SFX__increment0	 ; XXX
    shlq      #nb_bits_virgule_offset,REG_main_SFX__increment0
    div      REG_main_SFX__tmp2,REG_main_SFX__increment0      ; (3546895 / note) / frequence I2S en 0:15
    or        REG_main_SFX__increment0,REG_main_SFX__increment0 ; XXX
  .2:

; SFX 1
    load        (R14+4+7),REG_main_SFX__sample2
    load        (R14+7),REG_main_SFX__location_entier1

; period
    cmpq    #0,REG_main_SFX__sample2
    load        (R14+3+7),REG_main_SFX__volume1
    jr      ne,.3
    load        (R14+2+7),REG_main_SFX__location_end1

    jr      .4
    move    REG_main_SFX__increment_repeat,REG_main_SFX__increment1

  .3:
    move    REG_main_SFX__tmp1,REG_main_SFX__increment1
    div      REG_main_SFX__sample2,REG_main_SFX__increment1      ; (3546895 / note) note minimale = 108
    or        REG_main_SFX__increment1,REG_main_SFX__increment1	 ; XXX
    shlq      #nb_bits_virgule_offset,REG_main_SFX__increment1
    div      REG_main_SFX__tmp2,REG_main_SFX__increment1      ; (3546895 / note) / frequence I2S en 0:15
    or        REG_main_SFX__increment1,REG_main_SFX__increment1 ; XXX
  .4:

; preload samples 4 octets
  move      REG_main_SFX__location_entier0,REG_main_SFX__sample1
  move      REG_main_SFX__location_entier1,REG_main_SFX__sample2
  shrq        #nb_bits_virgule_offset,REG_main_SFX__sample1          ; enleve la partie a virgule
  shrq        #nb_bits_virgule_offset,REG_main_SFX__sample2
  and        REG_main_mask_and_FFFFFFFC,REG_main_SFX__sample1
  and        REG_main_mask_and_FFFFFFFC,REG_main_SFX__sample2
  load        (REG_main_SFX__sample1),REG_main_SFX__sample_voie0      ; load 4 octets d'un coup
  load        (REG_main_SFX__sample2),REG_main_SFX__sample_voie1
  moveta    REG_main_SFX__sample_voie0,REG_interrupt_utilise_par_main__octets_voie0
  moveta    REG_main_SFX__sample_voie1,REG_interrupt_utilise_par_main__octets_voie1

    movei      #loop_mixage_SFX,REG_main_SFX__loop

loop_mixage_SFX:
; voie SFX 0
  move      REG_main_SFX__location_entier0,REG_main_location_integer_old
  add        REG_main_SFX__increment0,REG_main_SFX__location_entier0
;  tester le bouclage
  cmp        REG_main_SFX__location_end0,REG_main_SFX__location_entier0
  jr          mi,.pas_bouclage_SFX0
  nop


; bouclage/repeat => silence
  move      REG_main_SFX__pointeur_silence_repeat,REG_main_SFX__location_entier0
  move      REG_main_SFX__pointeur_fin_silence_repeat,REG_main_SFX__location_end0
  move      REG_main_SFX__volume_repeat,REG_main_SFX__volume0
  move      REG_main_SFX__increment_repeat,REG_main_SFX__increment0

.pas_bouclage_SFX0:
; refresh du sample
  shrq        #nb_bits_virgule_offset,REG_main_location_integer_old
  move      REG_main_SFX__location_entier0,REG_main_SFX__sample1
  and        REG_main_mask_and_FFFFFFFC,REG_main_location_integer_old        ; old adresse location modulo 4
  shrq        #nb_bits_virgule_offset,REG_main_SFX__sample1
  move      REG_main_SFX__sample1,REG_main_SFX__sample2                    ; REG_main_SFX__sample2 = actuelle location sans virgule
  and        REG_main_mask_and_FFFFFFFC,REG_main_SFX__sample1        ; actuelle adresse location modulo 4
  cmp        REG_main_location_integer_old,REG_main_SFX__sample1
  jr          eq,.pas_de_nouveau_octet_a_lire_SFX0
  nop

  load        (REG_main_SFX__sample1),REG_main_SFX__sample_voie0
  moveta    REG_main_SFX__sample_voie0,REG_interrupt_utilise_par_main__octets_voie0

.pas_de_nouveau_octet_a_lire_SFX0:
; il faut recuperer le bon octet et le multiplier par le volume
  not        REG_main_mask_and_FFFFFFFC        ; => %11
  and        REG_main_mask_and_FFFFFFFC,REG_main_SFX__sample2      ; adresse and %11 = numéro octet a lire
  shlq        #3,REG_main_SFX__sample2
  not        REG_main_mask_and_FFFFFFFC        ; => $FFFFFFFC
  neg        REG_main_SFX__sample2                    ;  - (numéro d'octet a utiliser  << 3) => -0 -8 -16 -24
  movefa    REG_interrupt_utilise_par_main__octets_voie0,REG_main_SFX__sample_voie0
  sh        REG_main_SFX__sample2,REG_main_SFX__sample_voie0
  sharq      #24,REG_main_SFX__sample_voie0
  imult        REG_main_SFX__volume0,REG_main_SFX__sample_voie0


; voie SFX 1
  move      REG_main_SFX__location_entier1,REG_main_location_integer_old
  add        REG_main_SFX__increment1,REG_main_SFX__location_entier1
;  tester le bouclage
  cmp        REG_main_SFX__location_end1,REG_main_SFX__location_entier1
  jr          mi,.pas_bouclage_SFX1
  nop


; bouclage/repeat => silence
  move      REG_main_SFX__pointeur_silence_repeat,REG_main_SFX__location_entier1
  move      REG_main_SFX__pointeur_fin_silence_repeat,REG_main_SFX__location_end1
  move      REG_main_SFX__volume_repeat,REG_main_SFX__volume1
  move      REG_main_SFX__increment_repeat,REG_main_SFX__increment1

.pas_bouclage_SFX1:
; refresh du sample
  shrq        #nb_bits_virgule_offset,REG_main_location_integer_old
  move      REG_main_SFX__location_entier1,REG_main_SFX__sample1
  and        REG_main_mask_and_FFFFFFFC,REG_main_location_integer_old        ; old adresse location modulo 4
  shrq        #nb_bits_virgule_offset,REG_main_SFX__sample1
  move      REG_main_SFX__sample1,REG_main_SFX__sample2                    ; REG_main_SFX__sample2 = actuelle location sans virgule
  and        REG_main_mask_and_FFFFFFFC,REG_main_SFX__sample1        ; actuelle adresse location modulo 4
  cmp        REG_main_location_integer_old,REG_main_SFX__sample1
  jr          eq,.pas_de_nouveau_octet_a_lire_SFX1
  nop

  load        (REG_main_SFX__sample1),REG_main_SFX__sample_voie1
  moveta    REG_main_SFX__sample_voie1,REG_interrupt_utilise_par_main__octets_voie1

.pas_de_nouveau_octet_a_lire_SFX1:
; il faut recuperer le bon octet et le multiplier par le volume
  not        REG_main_mask_and_FFFFFFFC        ; => %11
  and        REG_main_mask_and_FFFFFFFC,REG_main_SFX__sample2      ; adresse and %11 = numéro octet a lire
  shlq        #3,REG_main_SFX__sample2
  not        REG_main_mask_and_FFFFFFFC        ; => $FFFFFFFC
  neg        REG_main_SFX__sample2                    ;  - (numéro d'octet a utiliser  << 3) => -0 -8 -16 -24
  movefa    REG_interrupt_utilise_par_main__octets_voie1,REG_main_SFX__sample_voie1
  sh        REG_main_SFX__sample2,REG_main_SFX__sample_voie1
  sharq      #24,REG_main_SFX__sample_voie1
  imult        REG_main_SFX__volume1,REG_main_SFX__sample_voie1

  load        (REG_main_SFX__destination_buffer),REG_main_SFX__sample1        ; left.w/right.w
  imult        REG_main_SFX__volume_SFX,REG_main_SFX__sample_voie0
  move      REG_main_SFX__sample1,REG_main_SFX__sample2
  imult        REG_main_SFX__volume_SFX,REG_main_SFX__sample_voie1
  shlq        #16,REG_main_SFX__sample2
  sharq      #8,REG_main_SFX__sample_voie0
  sharq      #16,REG_main_SFX__sample1            ; left signé
  sharq      #8,REG_main_SFX__sample_voie1
  sharq      #16,REG_main_SFX__sample2            ; right signé

  add        REG_main_SFX__sample_voie0,REG_main_SFX__sample1
  add        REG_main_SFX__sample_voie1,REG_main_SFX__sample2


  shlq        #16,REG_main_SFX__sample1
  and        REG_main_and_mask_FFFF,REG_main_SFX__sample2    ; le sample est signé, il faut éliminer les 16 bits du haut

  or          REG_main_SFX__sample2,REG_main_SFX__sample1

  subq        #1,REG_main_SFX__nb_octets_a_remplir
  store        REG_main_SFX__sample1,(REG_main_SFX__destination_buffer)

  jump        hi,(REG_main_SFX__loop)
  addqt      #4,REG_main_SFX__destination_buffer

  store        REG_main_SFX__sample1,(REG_main_SFX__destination_buffer)

    store        REG_main_SFX__location_entier0,(R14)
    store        REG_main_SFX__location_end0,(R14+2)
    store        REG_main_SFX__volume0,(R14+3)

    store        REG_main_SFX__location_entier1,(R14+7)
    store        REG_main_SFX__location_end1,(R14+7+2)
    store        REG_main_SFX__volume1,(R14+7+3)


  .endif        ; mix_SFX

; back to main loop
    movei    #Paula_flag_Tick_50Hz,REG_main_sample1
  movei    #DSP_boucle_centrale,REG_main_SFX__increment0
    moveq    #flag_en_attente_timer_50HZ,REG_main_sample2
  jump      (REG_main_SFX__increment0)
    store      REG_main_sample2,(REG_main_sample1)


  .long
dmacon_current_frame:  dc.l        0
Paula_compteur_frames_Timer1:      dc.l            0
Paula_flag_Tick_50Hz:  dc.l    flag_en_attente_timer_50HZ
    ; 0 = le dps a envoyé le signal 50 Hz
    ; 1 = modification des canaux ON/OFF dmacon
    ; 2 = application des valeurs

DSP_pointeur_silence:                      dc.l      silence<<nb_bits_virgule_offset
DSP_pointeur_fin_silence:                    dc.l      (silence+2)<<nb_bits_virgule_offset

DSP_frequence_de_replay_reelle_I2S:          dc.l      0
DSP_parametre_de_frequence_I2S:            dc.l      0

DSP_pointeur_BUFFER_a_remplir:            dc.l        PAULA_DSP_fin
DSP_pointeur_BUFFER_a_jouer:              dc.l        PAULA_DSP_fin+(700*4)

DSP_volume_SFX:    dc.l      volume_SFX    ; 0-256
DSP_volume_music:  dc.l      volume_music    ; 0-256

DSP_flag_STOP:    dc.l        0


; registres PAULA
; left =  0+3
; right = 1+2
; Channel 0/A
PAULA_sample_location0:        dc.l      silence<<nb_bits_virgule_offset                ;  0 : interne
PAULA_sample_location_virgule0:  dc.l      0                                        ;  1 : interne
PAULA_sample_end0:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  2 : interne
PAULA_volume0:                dc.l      0                                        ;  3 : volume
PAULA_increment0:              dc.l      $11110000                                ;  4 :  period  ; uniquement a virgule car frequence de replay > 28800 ( maxi Amiga )
PAULA_repeat_location0:        dc.l      silence<<nb_bits_virgule_offset                ;  5 : externe
PAULA_repeat_end0:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  6 : externe

; Channel 1/B
PAULA_sample_location1:        dc.l      silence<<nb_bits_virgule_offset                ;  7 : interne
PAULA_sample_location_virgule1:  dc.l      0                                        ;  8 : interne
PAULA_sample_end1:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  9 : interne
PAULA_volume1:                dc.l      0                                        ;  10 : volume
PAULA_increment1:              dc.l      $22220000                                ;  11 :  period  ; uniquement a virgule car frequence de replay > 28800 ( maxi Amiga )
PAULA_repeat_location1:        dc.l      silence<<nb_bits_virgule_offset                ;  12 : externe
PAULA_repeat_end1:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  13 : externe

; Channel 2/C
PAULA_sample_location2:        dc.l      silence<<nb_bits_virgule_offset                ;  14 : interne
PAULA_sample_location_virgule2:  dc.l      0                                        ;  15 : interne
PAULA_sample_end2:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  16 : interne
PAULA_volume2:                dc.l      0                                        ;  17 : volume
PAULA_increment2:              dc.l      $33330000                                ;  18 :  period  ; uniquement a virgule car frequence de replay > 28800 ( maxi Amiga )
PAULA_repeat_location2:        dc.l      silence<<nb_bits_virgule_offset                ;  19 : externe
PAULA_repeat_end2:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  20 : externe

; Channel 3/D
PAULA_sample_location3:        dc.l      silence<<nb_bits_virgule_offset                ;  21 : interne
PAULA_sample_location_virgule3:  dc.l      0                                        ;  22 : interne
PAULA_sample_end3:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  23 : interne
PAULA_volume3:                dc.l      0                                        ;  24 : volume
PAULA_increment3:              dc.l      $44440000                                ;  25 :  period  ; uniquement a virgule car frequence de replay > 28800 ( maxi Amiga )
PAULA_repeat_location3:        dc.l      silence<<nb_bits_virgule_offset                ;  26 : externe
PAULA_repeat_end3:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  27 : externe

; SFX channels
PAULA_SFX_left_private:
PAULA_sample_location_sfx1:        dc.l      silence<<nb_bits_virgule_offset                ;  0 : interne
PAULA_sample_location_virgule_sfx1:  dc.l      0                                        ;  1 : interne
PAULA_sample_end_sfx1:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  2 : interne
PAULA_volume_sfx1:                dc.l      0                                        ;  3 : volume
PAULA_period_sfx1:                dc.l      $110                                      ;  4 :  period  ; uniquement a virgule car frequence de replay > 28800 ( maxi Amiga )
PAULA_repeat_location_sfx1:          dc.l      silence<<nb_bits_virgule_offset                ;  5 : externe
PAULA_repeat_end_sfx1:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  6 : externe

PAULA_SFX_right_private:
PAULA_sample_location_sfx2:        dc.l      silence<<nb_bits_virgule_offset                ;  0 : interne
PAULA_sample_location_virgule_sfx2:  dc.l      0                                        ;  1 : interne
PAULA_sample_end_sfx2:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  2 : interne
PAULA_volume_sfx2:                dc.l      0                                        ;  3 : volume
PAULA_period_sfx2:                dc.l      $110                                  ;  4 :  period  ; uniquement a virgule car frequence de replay > 28800 ( maxi Amiga )
PAULA_repeat_location_sfx2:          dc.l      silence<<nb_bits_virgule_offset                ;  5 : externe
PAULA_repeat_end_sfx2:            dc.l      (silence+2)<<nb_bits_virgule_offset            ;  6 : externe


PAULA_SFX_left:
  dc.l      silence<<nb_bits_virgule_offset                ;  0 : interne
  dc.l      0                                        ;  1 : interne
  dc.l      (silence+2)<<nb_bits_virgule_offset            ;  2 : interne
  dc.l      0                                        ;  3 : volume
  dc.l      $110                                      ;  4 :  period  ; uniquement a virgule car frequence de replay > 28800 ( maxi Amiga )

PAULA_SFX_right:
  dc.l      silence<<nb_bits_virgule_offset                ;  0 : interne
  dc.l      0                                        ;  1 : interne
  dc.l      (silence+2)<<nb_bits_virgule_offset            ;  2 : interne
  dc.l      0                                        ;  3 : volume
  dc.l      $110                                  ;  4 :  period  ; uniquement a virgule car frequence de replay > 28800 ( maxi Amiga )

; xxxxxxCx xxBx2580 147*oxAP 369#RLDU
DSP_pad1:        dc.l    0
DSP_pad2:        dc.l    0


  .long

;---------------------
; FIN DE LA RAM DSP
PAULA_DSP_fin:
;---------------------


SOUND_DRIVER_SIZE      .equ      PAULA_DSP_fin-DSP_base_memoire
  .print  "--- Sound driver code size (DSP): ", /u SOUND_DRIVER_SIZE, " bytes / 8192 ---"
