    TITLE   "Source for CAN control panel encoder using CBUS"

;      Date Rev   By  Notes
; 16-Jun-14 v3a   PJW V3a Release. Returns physical PIC in node properties
; 12-May-14     PJW Now using "cbusdefs8h.inc"
; 23-Feb-14 v3A8  PJW Fixed issue with monitored push buttons in blocks 2..8, other bug fixes
; 22-Feb-14     PJW Record toggle switch state at startup.
; 20-Feb-14 v3A7  PJW Fixed bugs with processing RX SOD event
; 19-Feb-14 v3A6  BV  Revised AUTOID logic
; 16-Feb-14 v3A5  PJW Allow one event to set both lockout and do a SOD
; 14-Feb-14 v3A4  PJW Don't send SOD events for locked out block
; 10-Feb-14 v3a3  PJW CANID enumeration now optional (see AUTOID)
;  3-Feb-14 v3a2  PJW Enumerated DatMode, added Bob's auto-self-enumerate for CANID from ACE8C code
;  1-Feb-14     PJW In FLiM mode, if J5 is in upper position, use short events
;           by default when initialising the flash event table
; 29-Jan-14     PJW Corrected flash erase/fill
; 28-Jan-14     PJW Corrected ON/OFF polarity on incoming events
; 22-Jan-14     PJW Updated to use cbusdefs8g.inc; hard coded opcodes now symbolic
; 22-Jan-14     PJW Added SOD generation routines
; 21-Dec-13     PJW Added lockout and SOD, temporarily using switch 127 and 128
; 20-Dec-13     PJW Added monitor mode
;  1-Dec-13     PJW Now called CANACE3C
; 25-Nov-13     PJW Starting to add monitor code
;  6-Oct-13     PJW No compatibility with V2f anymore
;  4-Oct-13     PJW Basically functional. Not using interrupt scan mode yet
;  1-Oct-13 v3a   PJW Total rewrite with more flexible switch control, toggle option and lots more (PJW)
;       v2f   RH  Fix bug in read parameters, add node number to response
;       v2e     Self enum as separate subroutine. Added OpCodes 0x5D and 0x75. PB changes
;       v2d     Change parameters to new format
;       v2c     Add check fo zero parameter index and correct error code
;       v2b     Change response to QNN to OPC_PNN
;       v2a     first release build
;       102h    remove degging code
;       102g    remove extended fram test and add new code in CAN initialisation
;       102f    add test for extended fram in receive routine
;       102b-e    various fault find builds
;     102_a     First version wrt CBUS Developers Guide
;           Add code to support 0x11 (RQMN)
;           Add code to return 8th parameter by index - Flags
;           Ignore extended frames in packet receive routine
; 10-May-11 t     Correction to putNN for EEPROM write
;     s     Correction so responds to RTR in SLiM mode
; 15-Mar-11 r     SLiM mode: Boot cmd uses SLiM NN,
;             OPC 0x73 RQNPN works with SLiM NN
;           Add FLiM mode test to packet routine
;           Remove FLiM mode tests from individual routines
;     q     As rev p but distinguishes table Index from Pointno. 
;           Correction to para1rd for node number
;           Correction to restore on double teach in SLiM
;           Doesn't restore default in FLiM if same event sent twice
;           Added restore all events to default using OPC NNCLR (0x55) in learn mode
;     p     As rev m but events are sent as base 1 although stored with index 0
;           Incoming events also base 1 but stored as base 0.
;           Applies to both long and short events
;           Added WRACK frame following any FLASH wrirtes.  (OPC 0x59)
;  2-Mar-11 n     Default switch numbers put back to starting at 1
;     m     As rev k with mods to F5 handling and Pointno in snd_inx
; 12-Jan-11 k     Added FLiM mode to rev j
; 11-Aug-10 j     No rev i.
;           A major rewrite of rev h to allow event learning, including short events
;           for 'many to many' mode. Working in SLiM mode OK.
;     h     Changed HPINT error handling for RX buf overflow
; 24-Mar-10 g     New enum sequence
;     f     TXB2CON test in newid_f
; 15-Feb-10 e     ignore zero length frames in hpint routine
;           test TXB2CON.TXREQ is zero before transmitting
; 27-Jan-10 d     Added back NNACK after enum.
; 24-Jan-10 c     Fix to event number offset in modes 2 and 3
; 16-Jan-10 b       Mods for different switch cominations in FLiM mode
;           Mode 0  All toggles
;           Mode 1  All PB pairs
;           Mode 2  Top 4 rows  Toggles,  bottom 4 rows PB pairs
;           Mode 3  Top 4 rows  Toggles,  bottom 4 rows PB ON only
;           Mode 4  Top 4 rows  PB pairs, bottom 4 rows PB ON only
;           Mode 5  All PB ON only
; 30-Dec-09 a     This is the combined FLiM / SLiM version with bootloader.
; 10-May-09       Add ACE3 Id to params, Fix buffer flow control bug in SendTx1 routine
;  6-Sep-08       Tx error enabled
;  5-Aug-08       Tx error interrupt disabled, mods to error handling in interupt
;  2-Aug-09       mods to hpint error routine, mods to scan so uses sendTX and not sendTX1

; A control panel encoder for the SLiM or FLiM model 
; Scans up to 128 toggles/pushbuttons in various modes

; Works with toggle switches or pushbuttons
; Sends ON and OFF events
; Event numbers start at 1 and are sequential with columms and first 4 rows
; then columns and second 4 rows. Makes panel wiring simpler.
; 
; Present code works with the SLiM PCB.
; Based on the CANACE3 code

; The setup timer is TMR3. Used during self enumeration.
; CAN bit rate of 125 Kbits/sec
; Standard frame only
; Flash timer is TMR0.  
; Timer 1 used for low priority interrupt

; End of comments for ACE3

; 
; Assembly options
  LIST  P=18F2480,r=dec,N=75,C=120,T=ON

  include   "p18f2480.inc"
  include   "cbuslib/cbusdefs.inc"


LOCKSW  equ 128  ;Lockout 'switch' number (128..255)
SODSW equ 129  ;SOD 'switch' number (128..255)

  ;definitions  Change these to suit hardware.
  
;The switch scan interval defines how often EACH switch is scanned.
;It also controls SOD generation delay, so keep it so that
;16*SCANTM = 500000uS, or change subsequent code in sodgen
SCANTM  equ 31250  ;Switch scan interval (uS)

;Calculate timer 1 constant when using a 4MHz resonator
TMR1CN  equ 0x10000-((.4*SCANTM)/.16) ;Timer 1 count (counts UP)

S_PORT  equ PORTB ;setup switch  Change as needed
S_BIT0  equ 4     ; bits used for node number and CAN ID
S_BIT1  equ 5
M_PORT  equ PORTA
M_BIT equ 5   ;mode toggle or PB
S_BIT equ 4
Modstat equ 1   ;address in EEPROM

MAN_NO      equ MANU_MERG    ;manufacturer number
MAJOR_VER   equ 3    ;Firmware major version (numeric)
MINOR_VER   equ "b"   ;Firmware minor version (alpha)
MODULE_ID   equ MTYP_CANACE3C ; id to identify this type of module
EVT_NUM     equ 0           ; Number of events
EVperEVT    equ 0           ; Event variables per event
NV_NUM      equ 9          ; Number of node variables
NODEFLGS    equ PF_COMBI + PF_BOOT
CPU_TYPE    equ P18F2480
BETA_VER  equ 0    ;Firmware beta version (numeric, 0 = Release)

J5_SHORT  equ 0   ;Load short events in default flash table if J5 in upper position
AUTOID    equ 0   ;Include automatic CAN ID enumeration (this may cause problems with CANCAN)


;Self enumeration bits stored in Datmode
MD_NEWFRM equ 0 ;new frame received
MD_SETUP  equ 1 ;in setup mode
MD_NNWAIT equ 2 ;waiting for NN
MD_FLRUN  equ 3 ;FLiM Run mode
MD_EMSUP  equ 4 ;suppress error message
MD_EVULN  equ 5 ;unlearn event
MD_EVRD   equ 6 ;read event variable
MD_IDCONF equ 7 ;ID conflict detected

SCSWITCH  equ 0x00    ;Normal switch mode
;Scan mode bits
SCONONLY  equ 0      ;Bit set to send on event only
SCOFFONLY equ 1      ;Bit set to send off event only
SCPBPAIR  equ 2      ;Bit set if Pushbutton pair mode
SCPBTOGG  equ 3      ;Bit set if Pushbutton toggle mode with event monitor
SCPBTOGN  equ 4      ;Bit set if Pushbutton toggle + NO event monitor
SCSOD   equ 5      ;Bit set to send status on receipt of SOD (only useful for SCSWITCH mode)
SCLOCKOUT equ 6      ;Bit set to lockout switch block
SCUNUSED  equ 7      ;Unused bit

;Scan flag bits
SFONOFF   equ 0      ;Event On/Off
SFSLIM1   equ 1      ;SLiM mode 1
SFNSCAN   equ 2      ;Time for next scan

;SOD control bits (NV9)
SODTIME   equ 0x1F    ;Mask for SOD delay time (seconds)
SODGN1    equ 6      ;Generate ON SOD event if set
SODGN0    equ 7      ;Generate OFF SOD event if set

;Other flags, stored in Oflag
OFINIT    equ 0      ;First scan after init

;set config registers

; note. there seem to be differences in the naming of the CONFIG parameters between
; versions of the p18F2480.inf files

  CONFIG  FCMEN = OFF, OSC = HSPLL, IESO = OFF
  CONFIG  PWRT = ON,BOREN = BOHW, BORV=0
  CONFIG  WDT=OFF
  CONFIG  MCLRE = ON
  CONFIG  LPT1OSC = OFF, PBADEN = OFF
  CONFIG  DEBUG = OFF
  CONFIG  XINST = OFF,LVP = OFF,STVREN = ON,CP0 = OFF
  CONFIG  CP1 = OFF, CPB = OFF, CPD = OFF,WRT0 = OFF,WRT1 = OFF, WRTB = OFF
  CONFIG  WRTC = OFF,WRTD = OFF, EBTR0 = OFF, EBTR1 = OFF, EBTRB = OFF

;********************************************************************************
#define HIGH_INT_VECT 0x0808  ;HP interrupt vector redirect. Change if target is different
#define LOW_INT_VECT  0x0818  ;LP interrupt vector redirect. Change if target is different.
#define RESET_VECT  0x0800  ;start of target
#define TYPE_PARAM  0x0810
#define NODE_PARAM  0x0820
#define NUM_PARAM   24
#define META_PARAM  NODE_PARAM + NUM_PARAM

;*******************************************************************
  include   "cbuslib/boot_loader.inc"
;****************************************************************
; define RAM storage
  
  CBLOCK  0   ;file registers - access bank
          ;interrupt stack for low priority
          ;hpint uses fast stack
  W_tempL
  St_tempL
  Bsr_tempL
  PCH_tempH   ;save PCH in hpint
  PCH_tempL   ;save PCH in lpint (if used)

  Fsr_temp0L
  Fsr_temp0H 
  Fsr_temp1L
  Fsr_temp1H 
  Fsr_temp2L
  Fsr_hold
  Fsr_holx

  TempCANCON
  TempCANSTAT
  TempINTCON
  CANid1    ;SLiM node CAN ID
  CanID_tmp ;temp for CAN Node ID
  IDtemph   ;used in ID shuffle
  IDtempl
  NN_temph  ;node number in RAM
  NN_templ
  NodeID_l  ;used in SLiM mode
  NV_temp   ;temp store of NV
  IDcount   ;used in self allocation of CAN ID.
  Datmode     ;flag for data waiting and other states
  Count     ;counter for loading
  Count1
  Count2
  Dcount      ;used in delay only
  Dcount1
  Latcount    ;latency counter
  Keepcnt     ;keep alive counter
  Roll      ;rolling bit for enum
  SodCnt      ;SOD counter
  EEtemp      ;temp store for eeprom I/O  

  Temp      ;temps
  Atemp     ;Port temp value
  Dlc       ;data length
  Mode0     ;for FLim / Slim
  Mode2     ;mode. toggle or PB in SLiM

  ScMode0     ;NV1 Scan mode, block 0
  ScMode1     ;NV2 Scan mode, block 1
  ScMode2     ;NV3 Scan mode, block 2
  ScMode3     ;NV4 Scan mode, block 3
  ScMode4     ;NV5 Scan mode, block 4
  ScMode5     ;NV6 Scan mode, block 5
  ScMode6     ;NV7 Scan mode, block 6
  ScMode7     ;NV8 Scan mode, block 7
  SodMode     ;NV9 SOD Control

  Lockout     ;Bit 0 set for block lockout

          ;the above variables must be in access space (00 to 5F)
            
  Buffer0     ;temp buffer in access bank for data  
  Buffer1
  Buffer2
  Buffer3
  Buffer4   
  Buffer5
  Buffer6
  Buffer7
  Buffer8   
  Buffer9
  Buffer10
  Buffer11
  Buffer12    
  Buffer13
  Buffer14
  Buffer15
    
  Rx0con      ;start of receive packet 0
  Rx0sidh
  Rx0sidl
  Rx0eidh
  Rx0eidl
  Rx0dlc
  Rx0d0
  Rx0d1
  Rx0d2
  Rx0d3
  Rx0d4
  Rx0d5
  Rx0d6
  Rx0d7
  
  Tx1con      ;start of transmit frame  1
  Tx1sidh
  Tx1sidl
  Tx1eidh
  Tx1eidl
  Tx1dlc
  Tx1d0
  Tx1d1
  Tx1d2
  Tx1d3
  Tx1d4
  Tx1d5
  Tx1d6
  Tx1d7
  
  ;***************************************************************
  ; the following are used in the switch scanning
  Sflag   ;Scan flags
  Oflag   ;Other flags OF****
  Ccount    ;column counter for switch scan
  Row     ;row data for switch scan
  Bitcng    ;bits changed in scan
  Bitcnt    ;bit counter in scan (0-7)
  Bitmask   ;bit mask in scan
  Block   ;Current block (0-7) for scan
  ScMode    ;Scan mode for block
  Index   ;pointer to FLASH event table
  
  Fsr_tmp1Le  ;temp store for FSR1
  Fsr_tmp1He 
  Enum0   ;bits for new enum scheme.
  Enum1
  Enum2
  Enum3
  Enum4
  Enum5
  Enum6
  Enum7
  Enum8
  Enum9
  Enum10
  Enum11
  Enum12
  Enum13

  Flcount     ;flash memory counter
  Flcount1    ;flash memory counter low
  Flcounth    ;flash memory counter high
  Blk_st      ;first block for unlearn
  Blk_lst     ;last block for unlearn
  Blk_num     ;number of blocks to shift up
  ENcount     ;used in flash write
  ENtempl
  ENtemph
  
  ;add variables to suit

  ENDC  

  CBLOCK  0x100   ;buffer for FLASH write (64 bytes?) 
  ; Probably needs to be aligned on a 256 byte boundary
  Flash_buf
  ENDC

;Switch State Bits
SS_STATE  equ 0      ;Bit set if switch is ON
  CBLOCK  0x180   ;Switch State (128 bytes, SS_**** bits)
  Switch_State
  ENDC  
  
  CBLOCK  0x200   ;fast lookup table for incoming events (256 bytes)
  ; Each entry corresponds to the first 256 event codes (0..255)
  ; and contains the switch number if this event is monitored,
  ; or 0xFF if unused
  ; MUST be aligned on a 256 byte boundary for the code to work
  Moni_Table
  ENDC  



;************************************************************************************

;
;   start of program code

    ORG   RESET_VECT
loadadr
    nop           ;for debug
    goto  setup

    ORG   HIGH_INT_VECT
    goto  hpint     ;high priority interrupt

    ORG   TYPE_PARAM     ;node type parameters
myName  db  "ACE3C  "     ;module name (7 characters)

    ORG   LOW_INT_VECT 
    goto  lpint     ;low priority interrupt

    ORG   NODE_PARAM     ;Main parameters
nodeprm db  MAN_NO, MINOR_VER ;1-2
    db  MODULE_ID, EVT_NUM  ;3-4
    db  EVperEVT, NV_NUM  ;5-6
    db  MAJOR_VER,NODEFLGS  ;7-8
    db  CPU_TYPE,PB_CAN     ;9-10 
        dw  RESET_VECT        ;11-12 Load address for module code above bootloader
        dw  0               ;13-14 Top 2 bytes of 32 bit address not used
    dw  0         ;15-16 CPU Manufacturers ID low
    dw  0         ;17-18 CPU Manufacturers ID high
    db  0,BETA_VER      ;19-20 CPU Manufacturers code, Beta revision
sparprm     fill 0,prmcnt-$ ; Unused parameter space set to zero

PRMCOUNT    equ sparprm-nodeprm ; Number of parameter bytes implemented

             ORG META_PARAM

prmcnt      dw  PRMCOUNT    ; Number of parameters implemented
nodenam     dw  myName      ; Pointer to module type name
            dw  0 ; Top 2 bytes of 32 bit address not used


PRCKSUM equ MAN_NO+MINOR_VER+MODULE_ID+EVT_NUM+EVperEVT+NV_NUM+MAJOR_VER+NODEFLGS+CPU_TYPE+PB_CAN+HIGH myName+LOW myName+HIGH loadadr+LOW loadadr+PRMCOUNT+BETA_VER

cksum       dw  PRCKSUM     ; Checksum of parameters

;*******************************************************************

; 
;
;   high priority interrupt. Used for CAN receive and transmit error.

hpint movff CANCON,TempCANCON
    movff CANSTAT,TempCANSTAT
  
;   movff PCLATH,PCH_tempH    ;save PCLATH
  
  
    movff FSR0L,Fsr_temp0L    ;save FSR0
    movff FSR0H,Fsr_temp0H
    movff FSR1L,Fsr_temp1L    ;save FSR1
    movff FSR1H,Fsr_temp1H
    

    movlw 8           ;for relocated code
    movwf PCLATH
    movf  TempCANSTAT,W     ;Jump table
    andlw B'00001110'
    addwf PCL,F     ;jump
    bra   back
    bra   errint      ;error interrupt
    bra   back
    bra   back
    bra   back
    bra   rxb1int     ;only receive interrupts used
    bra   rxb0int
    bra   back
    
rxb1int bcf   PIR3,RXB1IF   ;uses RB0 to RB1 rollover so may never use this
    
    lfsr  FSR0,Rx0con   ;
    
    goto  access
    
rxb0int bcf   PIR3,RXB0IF
    btfsc Datmode,MD_SETUP  ;setup mode?
    bra   setmode 
    lfsr  FSR0,Rx0con
    
    goto  access
    
    ;error routine here. Only acts on lost arbitration in TX1
    ;used for increasing priority if send is delayed
  
errint  movlb 15         ;change bank      
    btfss TXB1CON,TXLARB
    bra   errbak        ;not lost arb in TX1
  
    movf  Latcount,F      ;is it already at zero?
    bz    errbak
    decfsz  Latcount,F
    bra   errbak
    bcf   TXB1CON,TXREQ   ;abort current transmit
    movlw B'00111111'
    andwf TXB1SIDH,F      ;change priority
txagain     bsf TXB1CON,TXREQ   ;try again
          
errbak    bcf   RXB1CON,RXFUL
    movlb 0
    bcf   RXB0CON,RXFUL   ;ready for next
    
    bcf   COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf   COMSTAT,RXB1OVFL    
    bra   back1

access  movf  CANCON,W        ;switch buffers
    andlw B'11110001'
    movwf CANCON
    movf  TempCANSTAT,W
    andlw B'00001110'
    iorwf CANCON
    lfsr  FSR1,RXB0CON  ;this is switched bank
load  movf  POSTINC1,W
    movwf POSTINC0
    movlw 0x6E      ;end of access buffer lo byte
    cpfseq  FSR1L
    bra   load
    

    btfsc Rx0dlc,RXRTR    ;is it RTR?
    bra   isRTR
;   btfsc Datmode,1     ;setup mode?
;   bra   setmode 
    movf  Rx0dlc,F      ;ignore any zero data frames
    bz    back
;   btfss Rx0sidl,3     ;ignore extended frames
    bsf   Datmode,MD_NEWFRM ;valid message frame
    
    btfss Mode0,1       ;FLim Mode?
    bra   back        ; dont do Can ID check if SLiM mode
#if AUTOID    
    ;check for ID conflict
    movf  Rx0sidh,w   ;get received IDHi
    xorwf Tx1sidh,w   ;compare with our IDHi
    andlw 0x0f      ;Bob's update
    bnz   back      ;skip if not matched
    movf  Rx0sidl,w   ;get received IDLo
    xorwf Tx1sidl,w   ;compare with our IDLo
    andlw 0xe0      ;Bob's revised update
    bnz   back      ;skip if not matched
    bsf   Datmode,MD_IDCONF ;flag ID conflict   
#endif    
back  bcf   RXB0CON,RXFUL ;ready for next
  
back1 clrf  PIR3      ;clear all interrupt flags
    movf  CANCON,W
    andlw B'11110001'
    iorwf TempCANCON,W
    
    movwf CANCON
;   movff PCH_tempH,PCLATH
    movff Fsr_temp0L,FSR0L    ;recover FSR0
    movff Fsr_temp0H,FSR0H

    movff Fsr_temp1L,FSR1L    ;recover FSR1
    movff Fsr_temp1H,FSR1H

    
    retfie  1       ;use shadow registers

isRTR btfsc Datmode,MD_SETUP;setup mode?
    bra   back      ;back
;   btfss Mode0,1     ;FLiM?  corrected in rev s
;   bra   back
    movlb 15
isRTR1  btfsc TXB2CON,TXREQ ;wait till sent
    bra   isRTR1    
    bsf   TXB2CON,TXREQ ;send ID frame - preloaded in TXB2

    movlb 0
    bra   back

setmode tstfsz  RXB0DLC
    bra   back        ;only zero length frames for setup
    
    swapf RXB0SIDH,W      ;get ID into one byte
    rrcf  WREG
    andlw B'01111000'     ;mask
    movwf Temp
    swapf RXB0SIDL,W
    rrncf WREG
    andlw B'00000111'
    iorwf Temp,W
    movwf IDcount       ;has current incoming CAN_ID

    lfsr  FSR1,Enum0      ;set enum to table
enum_st clrf  Roll        ;start of enum sequence
    bsf   Roll,0
    movlw 8
enum_1  cpfsgt  IDcount
    bra   enum_2
    subwf IDcount,F     ;subtract 8
    incf  FSR1L       ;next table byte
    bra   enum_1
enum_2  dcfsnz  IDcount,F
    bra   enum_3
    rlncf Roll,F
    bra   enum_2
enum_3  movf  Roll,W
    iorwf INDF1,F
    bra   back


;   org   0xA00     ;set to page boundary   
;*************************************************************
main0 btfsc Mode0,1     ;is it SLiM?
    bra   mainf
    btfss Mode0,2     ;SLiM learn?
    bra   main_a      ;no
    btfss INTCON,TMR0IF   ;is it flash?
    bra   main_a
    btg   PORTB,7     ;flash green
    bcf   INTCON,TMR0IF

main_a  movlw B'00110000'   ;get NN from switches
    andwf S_PORT,W
    movwf Temp
    swapf Temp,W
    addlw 1
    cpfseq  Atemp
    bra   setnew
  
    bra   no_new

setnew  movwf Atemp

    movwf NN_templ
    call  newid1
    call  clear_events  ;reinstate new NN in FLASH buffer
    call  wrack     ;may not need in SLiM?
no_new  btfsc M_PORT,M_BIT
    bra   mset
    btfss Mode2,0
    bra   noflash       ;do nothing
    bcf   Mode2,0       ;mode change
    call  buf_init      ;re-initialise
    call  scan_init
    call  moni_build      ;build monitor table
    bra   noflash

mset  btfsc Mode2,0
    bra   noflash       ;do nothing
    bsf   Mode2,0       ;mode change
    call  buf_init      ;re-initialise
    call  scan_init
    call  moni_build      ;build monitor table
    call  clear_events    ;reinstate new switch numbers in FLASH buffer
    bra   noflash

; here if FLiM mde

mainf btfss Datmode,MD_IDCONF
    bra   mainf_1
    call  self_enA
    movlw B'00001000'   ;back to normal running
    movwf Datmode   

mainf_1
    btfss INTCON,TMR0IF   ;is it flash?
    bra   noflash
    btfss Datmode,MD_NNWAIT
    bra   nofl1
    
    btg   PORTB,6     ;flash yellow LED
    
nofl1 bcf   INTCON,TMR0IF
    btfss Datmode,MD_FLRUN  ;running mode
    bra   noflash
    decfsz  Keepcnt     ;send keep alive?
    bra   noflash
    movlw 10
    movwf Keepcnt
    movlw OPC_NNACK
;   call  nnrel     ;send keep alive frame (works OK, turn off for now)


noflash btfsc M_PORT,S_BIT  ;setup button?
    bra   main3
    movlw 100
    movwf Count
    clrf  Count1
    clrf  Count2
wait  decfsz  Count2
    goto  wait
    btfss Datmode,MD_NNWAIT
    bra   wait2
    btfss INTCON,TMR0IF   ;is it flash?
    bra   wait2
    btg   PORTB,6     ;flash LED
    bcf   INTCON,TMR0IF
wait2 decfsz  Count1
    goto  wait
    btfsc M_PORT,S_BIT
    bra   main4     ;not held long enough
    decfsz  Count
    goto  wait
    btfss Mode0,1     ;is it in FLiM?
    bra   go_FLiM
    clrf  Datmode     ;back to virgin
    bcf   PORTB,6     ;yellow off
    bsf   PORTB,7     ;Green LED on
    clrf  INTCON      ;interrupts off
    movlw 1
    movwf IDcount     ;back to start
    movlw Modstat
    movwf EEADR
    movlw   0
    call  eewrite     ;status to reset
    movlw OPC_NNREL   ;send node release frame
    call  nnrel
    clrf  NN_temph
    clrf  NN_templ
wait1 btfss M_PORT,S_BIT
    bra   wait1     ;wait till release
    call  ldely
    btfss M_PORT,S_BIT
    bra   wait1
    
    movlw LOW NodeID      ;put NN back to 0000
    movwf EEADR
    movlw 0
    call  eewrite
    incf  EEADR
    movlw 0
    call  eewrite
    btfss Mode0,1
    bra   main5       ;FLiM setup
    movlw Modstat
    movwf EEADR
    movlw 0
    call  eewrite       ;mode back to SLiM
    clrf  Datmode
    bcf   Mode0,1
    bcf   PORTB,6
    bsf   PORTB,7       ;green LED on
    clrf  Atemp       ;for new NN and ID
    clrf  Mode2
    btfsc M_PORT,M_BIT    ;check for SLiM scan mode
    bsf   Mode2,0
    call  buf_init    
    call  scan_init
    call  moni_build      ;build monitor table
    movlw B'11000000'
    movwf INTCON
    goto  main0       ;setloop

main5 movlw Modstat
    movwf EEADR
    movlw 1
    call  eewrite       ;mode to FLiM in EEPROM
    bsf   Mode0,1       ;to FLiM
    call  self_en
    bcf   Datmode,MD_SETUP
    call  nnack       ;send request for NN
    bsf   Datmode,MD_NNWAIT
    bra   main1

main4 btfsc Mode0,1       ;is it SLiM?
    bra   main4a
    btfss Mode0,2
    bra   main4b
    bcf   Mode0,2       ;out of learn
    bcf   Mode0,3
    bsf   PORTB,7       ;green back on
    bra   main1
main4b  bsf   Mode0,2       ;into learn
    bra   main1

main4a  ;btfss  Datmode,MD_FLRUN    
    ;bra    main3
    btfss Datmode,MD_NNWAIT
    bra   mset2
    bcf   Datmode,MD_NNWAIT
    bsf   PORTB,6     ;LED on
    movlw OPC_NNACK
    call  nnrel
    movlw Modstat
    movwf EEADR
    movlw B'00001000'
    movwf Datmode     ;normal
    call  eewrite
    bra   main3

mset2 bsf   Datmode,MD_NNWAIT
    call  self_en
    bcf   Datmode,MD_SETUP
    call  nnack
    bra   main1

main3 btfss Datmode,MD_SETUP    ;setup mode ?
    bra   main1
;   call  self_en

    bcf   Datmode,MD_SETUP    ;out of setup
    bsf   Datmode,MD_NNWAIT   ;wait for NN
;   call  nnack     ;send blank NN for config
  
;   bsf   PORTB,7     ;on light
    bra   main0     ;continue normally
    
    
go_FLiM bsf   Datmode,MD_SETUP    ;FLiM setup mode
    bcf   PORTB,7     ;green off
    bra   wait1
    
; common to FLiM and SLiM   
  
main1
    btfsc Datmode,MD_NEWFRM   ;any new CAN frame received?
    bra   packet      ;yes

    bra   do        ;look for inputs

;set of jumps here as branches too long

params1 goto  params
fl_lrn1 goto  fl_lrn
fl_ulrn1  goto  fl_ulrn

teach0  clrf  Rx0d1     ; Clear node from short events
    clrf  Rx0d2
teach1  btfss Mode0,1     ; Skip if FLiM mode
    goto  teach     ; SLIM Mode
    btfss Mode0,4     ; Skip if FLIM learn
    call  do_event    ; Process event
    bra   main2

readEV1 goto  readEV
undo1 goto  undo
    
name
    btfss Datmode,MD_NNWAIT   ;only in setup mode
    bra   main2
    call  namesend
    bra   main2

doQnn
    movf  NN_temph,w    ;respond if NN is not zero
    addwf NN_templ,w
    btfss STATUS,Z
    call  whoami
    bra   main2   
    
                ;main packet handling is here
    
packet
    movlw OPC_ACON      ;is it a long event to teach/monitor?
    subwf Rx0d0,W
    bz    teach1
    movlw OPC_ACOF      ;is it a long event to teach/monitor?
    subwf Rx0d0,W
    bz    teach1
    movlw OPC_ASON      ;is it a short event to teach/monitor?
    subwf Rx0d0,W
    bz    teach0
    movlw OPC_ASOF    ;is it a short event to teach/monitor?
    subwf Rx0d0,W
    bz    teach0
    movlw OPC_BOOT      ;reboot
    subwf Rx0d0,W
    bz    reboot
    movlw OPC_RQNPN
    subwf Rx0d0,W
    bz    para1a      ;read individual parameters
    movlw OPC_QNN     ; QNN
    subwf Rx0d0,W
    bz    doQnn
    btfss Mode0,1     ; FLiM mode?
    bra   main2
    movlw OPC_SNN     ;set NN
    subwf Rx0d0,W
    bz    setNN
    movlw OPC_RQNP    ;read manufacturer
    subwf Rx0d0,W
    bz    params1     ;read node parameters
    movlw OPC_RQMN
    subwf Rx0d0,w
    bz    name      ;read module name
    movlw OPC_NNLRN   ;FLiM learn mode
    subwf Rx0d0,W
    bz    fl_lrn1
    movlw OPC_NNULN   ;FLiM out of learn mode
    subwf Rx0d0,W
    bz    fl_ulrn1      
    movlw OPC_NENRD   ;read back event by index
    subwf Rx0d0,W
    bz    readEV1
    movlw OPC_NVSET   ;set a NV
    subwf Rx0d0,W
    bz    setNV
    movlw OPC_NVRD    ;read a NV
    subwf Rx0d0,W
    bz    readNV
    movlw OPC_EVLRNI    ;FLiM teach event
    subwf Rx0d0,W
    bz    fl_tch1
    movlw OPC_NNCLR   ;clear events back to default
    subwf Rx0d0,W
    bz    undo1
    movlw OPC_ENUM    ;re-enumerate
    subwf Rx0d0,W
    bz    enum1
    movlw OPC_CANID   ;force new CAN_ID
    subwf Rx0d0,W
    bz    newID1
    bra   main2

newID1  goto  newID
enum1 goto  enum
fl_tch1 goto  fl_teach

notNN bra   main2     ;not here   

reboot
    call  thisNN
    sublw 0
    bnz   notNN
reboot1 movlw 0xFF
    movwf EEADR
    movlw 0xFF
    call  eewrite     ;set last EEPROM byte to 0xFF
    reset         ;software reset to bootloader 

para1a  call  thisNN      ;read parameter by index
    sublw 0
    bnz   notNN
    call  para1rd
    bra   main2
      
main2 bcf   Datmode,MD_NEWFRM
    goto  main0     ;loop

newID call  thisNN
    sublw 0
    bnz   notNN
    movff Rx0d3,IDcount

    call  here2       ;put in as if it was enumerated
    movlw OPC_NNACK
    call  nnrel       ;acknowledge new CAN_ID
    goto  main2
    
setNN btfss Datmode,MD_NNWAIT   ;in NN set mode?
    bra   main2     ;no
    call  putNN     ;put in NN
    bcf   Datmode,MD_NNWAIT
    bsf   Datmode,MD_FLRUN
    movlw 10
    movwf Keepcnt     ;for keep alive
    movlw OPC_NNACK
    call  nnrel     ;confirm NN set
    call  loadnv      ;reload NV's
    call  buf_init    ;and reinitialise scan
    bsf   PORTB,6     ;LED ON
    bra   main2
    
sendNN  btfss Datmode,MD_NNWAIT   ;in NN set mode?
    bra   main2     ;no
    movlw OPC_RQNN    ;send back NN
    movwf Tx1d0
    movlw 3
    movwf Dlc
    call  sendTX
    bra   main2

enum  call  thisNN
    sublw 0
    bnz   notNN1
    call  self_en
    movlw OPC_NNACK
    call  nnrel     ;send confirm frame
    movlw B'00001000'   ;back to normal running
    movwf Datmode
    goto  main2
notNN1  goto  notNN
    
setNV 
    call  thisNN      ;set a new NV
    sublw 0
    bnz   notNN
    movf  Rx0d3,W
    bz    notNX     ;index of 0 is invalid
    movf  Rx0d3,W
    sublw NV_NUM
    bn    notNX     ;too many

    movlw LOW NVstart     ;get pointer in EEPROM
    addwf Rx0d3,W
    movwf EEADR
    decf  EEADR,F     ;address offset is 0 to NV_NUM-1
    movf  Rx0d4,W
    call  eesave      ;write new value if changed
    ;NV's may have changed, reinitialise everything
    call  loadnv

#if 0
    ;As reinitialisation can take a while, only do it
    ;after the LAST NV to prevent loss of NV data
    movf  Rx0d3,W     ;Get index
    sublw NV_NUM
    bnz   setnv1
#endif
    call  buf_init    ;reinitialise buffer
    call  moni_build    ;rebuild monitor table
setnv1  call  wrack     ;acknowledge
    bra   main2

readNV
    call  thisNN      ;is it this node?
    sublw 0
    bnz   notNN
    movf  Rx0d3,W
    bz    notNX     ;index of 0 is invalid
    movf  Rx0d3,W
    sublw NV_NUM
    bn    notNX     ;too many
    movlw LOW NVstart   ;get pointer in EEPROM
    addwf Rx0d3,W
    movwf EEADR
    decf  EEADR,F     ;address is 0 to NV_NUM -1
    call  eeread
    movwf Tx1d4     ;NV value
    movff Rx0d3,Tx1d3   ;rewrite index value
rdNV1 movlw OPC_NVANS
    movwf Tx1d0
    movlw 5
    movwf Dlc
    call  sendTX
    bra   main2

notNX clrf  Tx1d3     ;invalid index
    clrf  Tx1d4
    bra   rdNV1

fl_lrn
    call  thisNN
    sublw 0
    bnz   notNN1
    bsf   Mode0,4     ;set FLiM learn mode
    bra   main2
fl_ulrn btfss Mode0,1     ;is it in FLiM?
    bra   main2
    call  thisNN
    sublw 0
    bnz   notNN1
    bcf   Mode0,4     ;clear FLiM learn mode
    bra   main2

fl_teach
    bra   teach_f       ;put new event into FLASH



undo
    call  thisNN
    sublw 0
    bnz   notNN1
    btfss Mode0,4     ;is it in FLiM learn?
    bra   main2
    call  clear_events
    call  wrack
    bra   main2



params  btfss Datmode,MD_NNWAIT   ;only in setup mode
    bra   main2
    call  parasend
    bra   main2

teach movlw B'00001100'   ;is it learn and button pressed?
    subwf Mode0,W
    bnz   l_out2      ;no so do nothing
    call  learn_event
    bcf   Mode0,3
    bcf   Mode0,2
    bsf   PORTB,7     ;green steady
    bra   l_out2

teach_f btfss Mode0,4     ;is it in learn mode?
    bra   l_out2
    movff Rx0d5,Index   ;Rx0d7 has index
    decf  Index,F     ;sent as base 1
  
    call  learn_event
    call  wrack   ;send write acknowledge
    bra   l_out2

readEV  
    call  thisNN
    sublw 0
    bnz   l_out2
    call  read
    bra   l_out2
    
l_out bcf   Datmode,MD_EMSUP
    bcf   PORTB,6
l_out1  bcf   Datmode,MD_EVRD
l_out2  bcf   Datmode,MD_NEWFRM
    

    clrf  PCLATH
    goto  main2

  
do    btfss Mode0,1     ;is it FLiM?
    bra   do2
    btfss Datmode,MD_FLRUN    ;ignore if not set up
    bra   do1
    btfsc Datmode,MD_NNWAIT   ;don't do if in setup   
    bra   do1
do2   btfsc Sflag,SFNSCAN ;Skip if not time for scan
    call  scansw      ;scan inputs for change
                    
do1   goto  main0
    
;***************************************************************************
;   main setup routine
;*************************************************************************

setup clrf  INTCON      ;no interrupts yet
    clrf  ADCON0      ;turn off A/D, all digital I/O
    movlw B'00001111'
    movwf ADCON1
    
    ;port settings will be hardware dependent. RB2 and RB3 are for CAN.
    ;set S_PORT and S_BIT to correspond to port used for setup.
    ;rest are hardware options
    
  
    movlw B'00110000'   ;Port A 0 to 3 are column select, 4 is setup input
    movwf TRISA     ;
    movlw B'00111000'   ;RB2 = CANTX, RB3 = CANRX, RB4,5 are logic 
                ;input - RB6,7 for debug and diagnostics LEDs
    movwf TRISB
    bsf   PORTB,2     ;CAN recessive
    bcf   PORTB,6
    bcf   PORTB,7
    movlw B'11111111'   ;Port C  row inputs.
    movwf TRISC
    
; next segment is essential.
    
    bsf   RCON,IPEN   ;enable interrupt priority levels
    clrf  BSR       ;set to bank 0
    clrf  EECON1      ;no accesses to program memory  
    clrf  Datmode
    clrf  Latcount
    clrf  ECANCON     ;CAN mode 0 for now
     
    bsf   CANCON,7    ;CAN to config mode
    movlw B'00000011'   ;set CAN bit rate at 125000 for now
    movwf BRGCON1
    movlw B'10011110'   ;set phase 1 etc
    movwf BRGCON2
    movlw B'00000011'   ;set phase 2 etc
    movwf BRGCON3
    movlw B'00100000'
    movwf CIOCON      ;CAN to high when off
    movlw B'00100100'
    movwf RXB0CON     ;enable double buffer of RX0
    
;new code for extended frames bug fix
    movlb 15
    movlw B'00100000'   ;reject extended frames
    movwf RXB1CON
    clrf  RXF0SIDL
    clrf  RXF1SIDL
    movlb 0       

mskload lfsr  0,RXM0SIDH    ;Clear masks, point to start
mskloop clrf  POSTINC0    
    movlw LOW RXM1EIDL+1    ;end of masks
    cpfseq  FSR0L
    bra   mskloop

  
    clrf  CANCON      ;out of CAN setup mode
    clrf  CCP1CON
    movlw B'10000100'
    movwf T0CON     ;set T0 for LED flash
    movlw B'10000001'   ;Timer 1 control.16 bit write
    movwf T1CON     ;Timer 1 is for output duration
    movlw HIGH TMR1CN     ;Set interrupt rate
    movwf TMR1H     ;set timer hi byte

    clrf  Tx1con
    movlw B'00100011'   ;High priority IRQ for CAN interrupts
    movwf IPR3      ;high priority CAN RX and Tx error interrupts(for now)
    clrf  IPR1      ;all peripheral interrupts are low priority
    clrf  IPR2
    clrf  PIE2
    movlw B'00000001'
    movwf PIE1      ;enable interrupt for timer 1


;next segment required
    
  
    movlw B'00000001'
    movwf IDcount     ;set at lowest value for starters
    clrf  INTCON2     ;
    clrf  INTCON3     ;
    

    movlw B'00100011'   ;B'00100011'Rx0 and RX1 interrupt and Tx error
              
    movwf PIE3

    clrf  PIR1
    clrf  PIR2
    clrf  Sflag
    
no_load clrf  Mode2
    btfsc M_PORT,M_BIT  ;initialise SLiM mode for scan
    bsf   Mode2,0
  
    ;test for setup mode
    clrf  Mode0
    movlw Modstat     ;get setup status
    movwf EEADR
    call  eeread
    movwf Datmode
    sublw 0       ;is SLiM mode
    bnz   setid
    bra   slimset     ;set up in SLiM mode
    
  
setid bsf   Mode0,1     ;flag FLiM
    call  newid_f     ;put ID into Tx1buf, TXB2 and ID number store
    
seten_f   
    movlb 15
    bcf RXB1CON,RXFUL
    movlb 0
    bcf RXB0CON,RXFUL   ;ready for next
    bcf COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf COMSTAT,RXB1OVFL
    clrf  PIR3      ;clear all flags
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bcf   PORTB,7
    bsf   PORTB,6     ;RUN LED on. (yellow for FLiM)
    bcf   Datmode,0
    call  loadnv      ;Get NV's from EEPROM
    call  buf_init    ;and initialise buffers
    goto  main0

slimset movlw B'00110000'   ;get DIP switch setting
    andwf PORTB,W
    movwf Temp
    swapf Temp,W
    
    andlw B'00000011'
    
    addlw 1       ;NN start at 1
    movwf Atemp     ;for any changes

    clrf  NN_temph
    movwf NN_templ
    movlw LOW NodeID+1
    movwf EEADR
    call  eeread
    subwf NN_templ,W
    bz    not_new
    movf  NN_templ,W
    call  eewrite
    call  clear_events

not_new bcf   Mode0,0
    
    bcf   Mode0,1     ;not FLiM
  
    movf  NN_templ,W
    call  newid1      ;put ID into Tx1buf, TXB2 and ID number store
    movlw LOW Node_st
    movwf EEADR
    call  eeread
    sublw 0
    bnz   seten     ;table already loaded
    call  clear_events
    call  wrack
    
    ;
seten   
    movlb 15
    bcf RXB1CON,RXFUL
    movlb 0
    bcf RXB0CON,RXFUL   ;ready for next
    bcf COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf COMSTAT,RXB1OVFL
    clrf  PIR3      ;clear all flags
    
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bcf   PORTB,6
    bsf   PORTB,7     ;RUN LED on. Green for SLiM
    call  buf_init  
    goto  main0
  
  
;****************************************************************************
;   start of subroutines  



;   Send contents of Tx1 buffer via CAN TXB1

sendTX1 lfsr  0,Tx1con
    lfsr  1,TXB1CON
    
    movlb 15       ;check for buffer access
ldTx2 btfsc TXB1CON,TXREQ ; Tx buffer available...?
    bra   ldTx2     ;...not yet
    movlb 0
    
ldTX1 movf  POSTINC0,W
    movwf POSTINC1    ;load TXB1
    movlw Tx1d7+1
    cpfseq  FSR0L
    bra   ldTX1
    movlb 15       ;bank 15
tx1test btfsc TXB1CON,TXREQ ;test if clear to send
    bra   tx1test
    bsf   TXB1CON,TXREQ ;OK so send
    
tx1done movlb 0       ;bank 0
    return          ;successful send


sendTXNN
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
sendTX
sendTXa movf  Dlc,W       ;get data length
    movwf Tx1dlc
    movlw B'00001111'   ;clear old priority
    andwf Tx1sidh,F
    movlw B'10110000'
    iorwf Tx1sidh     ;low priority
    movlw 10
    movwf Latcount
    call  sendTX1     ;send frame
    return          


;*********************************************************************
;   put in NN from command

putNN movff Rx0d1,NN_temph
    movff Rx0d2,NN_templ
    movlw LOW NodeID
    movwf EEADR
    call  eeread
    subwf NN_temph,W
    bnz   notsame
    incf  EEADR
    call  eeread
    subwf NN_templ,W
    bnz   notsame
    bra   same
notsame movlw LOW NodeID    ;only reload table if NN is changed
    movwf EEADR
    movf  Rx0d1,W
    call  eewrite
    incf  EEADR
    movf  Rx0d2,W
    call  eewrite
    call  clear_events  ;create new FLASH lookup table
    call  wrack
same  movlw Modstat
    movwf EEADR
    movlw B'00001000'   ;Module status has NN set
    call  eewrite
    return
  
;***********************************************************************

putNNs  movlw LOW NodeID    ;put SLiM NN into EEPROM
    movwf EEADR
    movff Atemp,NN_templ
    movf  NN_temph,W
    call  eewrite
    incf  EEADR
    movf  NN_templ,W
    call  eewrite
    movlw LOW CANid
    movwf EEADR
    movf  NN_templ,W
    call  eewrite
    return

;**************************************************************************
    


newid1  movwf CanID_tmp           
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh     ;leave priority bits alone
    movf  IDtempl,W
    movwf Tx1sidl     ;only top three bits used
    movlb 15       ;put ID into TXB2 for enumeration response to RTR
new_1 btfsc TXB2CON,TXREQ ;wait till sent
    bra   new_1
    clrf  TXB2SIDH
    movf  IDtemph,W
    movwf TXB2SIDH
    movf  IDtempl,W
    movwf TXB2SIDL
    movlw 0xB0
    iorwf TXB2SIDH    ;set priority
    clrf  TXB2DLC     ;no data, no RTR
    movlb 0

    return
    
nnack btfss Mode0,1     ;FLiM?
    return
    movlw OPC_RQNN    ;request frame for new NN or ack if not virgin
nnrel movwf Tx1d0
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    movlw 3
    movwf Dlc
    call  sendTX
    return


;**********************************************************************
    

newid_f   movlw LOW CANid     ;put in stored ID. FLiM mode
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf CanID_tmp     
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh     ;leave priority bits alone
    movf  IDtempl,W
    movwf Tx1sidl     ;only top three bits used
    movlw LOW NodeID
    movwf EEADR
    call  eeread
    movwf NN_temph      ;get stored NN
    incf  EEADR
    call  eeread
    movwf NN_templ  
    
    movlb 15       ;put ID into TXB2 for enumeration response to RTR
new_1f  btfsc TXB2CON,TXREQ
    bra   new_1f
    clrf  TXB2SIDH
    movf  IDtemph,W
    movwf TXB2SIDH
    movf  IDtempl,W
    movwf TXB2SIDL
    movlw 0xB0
    iorwf TXB2SIDH    ;set priority
    clrf  TXB2DLC     ;no data, no RTR
    movlb 0
  
    return


    
;*****************************************************************************
;
;   shuffle for standard ID. Puts 8 bit ID into IDtemph and IDtempl for CAN frame
shuffle movf  CanID_tmp,W
    movwf IDtempl   ;get 8 bit ID
    swapf IDtempl,F
    rlncf IDtempl,W
    andlw B'11100000'
    movwf IDtempl         ;has sidl
    movf  CanID_tmp,W
    movwf IDtemph
    rrncf IDtemph,F
    rrncf IDtemph,F
    rrncf IDtemph,W
    andlw B'00001111'
    movwf IDtemph         ;has sidh
    return

;*********************************************************************************

;   reverse shuffle for incoming ID. sidh and sidl into one byte.

shuffin movff Rx0sidl,IDtempl
    swapf IDtempl,F
    rrncf IDtempl,W
    andlw B'00000111'
    movwf IDtempl
    movff Rx0sidh,IDtemph
    rlncf IDtemph,F
    rlncf IDtemph,F
    rlncf IDtemph,W
    andlw B'01111000'
    iorwf IDtempl,W     ;returns with ID in W
    return

;************************************************************************************
;   
eeread  bcf   EECON1,EEPGD  ;read a EEPROM byte, EEADR must be set before this sub.
    bcf   EECON1,CFGS
    bsf   EECON1,RD
    movf  EEDATA,W
    return

;**************************************************************************
;Write a byte to eeprom IF it's changed
;Entry: EEADR=Eeprom Address, W=Byte to write
eesave  movwf EEtemp      ;Save data
    call  eeread      ;Read data
    cpfseq  EEtemp      ;Skip if same
    bra   eesav1      ;Write eeprom
    return

eesav1  movf  EEtemp,W    ;Get byte

;**************************************************************************
eewrite movwf EEDATA      ;write to EEPROM, EEADR must be set before this sub.
    bcf   EECON1,EEPGD
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
    movff INTCON,TempINTCON
    clrf  INTCON  ;disable interrupts
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR
eetest  btfsc EECON1,WR
    bra   eetest
    bcf   PIR2,EEIF
    bcf   EECON1,WREN
    
    movff TempINTCON,INTCON   ;reenable interrupts
    
    return  
    

;*********************************************************
;   a delay routine
      
dely  movlw 10
    movwf Dcount1
dely2 clrf  Dcount
dely1 decfsz  Dcount,F
    goto  dely1
    decfsz  Dcount1
    bra   dely2
    return    
    
;************************************************************************ 
;   longer delay

ldely movlw 100
    movwf Count2
ldely1  call  dely
    decfsz  Count2
    bra   ldely1
    return

;******************************************************************
; Initialise scan modes
scan_init
    clrf  Sflag     ;Clear flags
    clrf  Ccount      ;Initialise column count
    btfsc Mode0,1     ;Skip if SLiM
    return          ;Must be FLiM mode then
    movlw SCSWITCH    ;Set switch mode
    btfss Mode2,0     ;Skip if SLiM pushbutton mode
    bra   scini1      ;Set switch mode
    movlw (1 << SCPBPAIR) ;Pushbutton pair mode
    bsf   Sflag,SFSLIM1 ;Flag SLiM mode 1
scini1  movwf ScMode0
    movwf ScMode1
    movwf ScMode2
    movwf ScMode3
    movwf ScMode4
    movwf ScMode5
    movwf ScMode6
    movwf ScMode7
    return

;******************************************************************
; Low priority interrupt
lpint movwf W_tempL       ;Save registers
    movff STATUS,St_tempL
    movff BSR,Bsr_tempL
;   movff PCLATH,PCH_tempL  ;save PCLATH
;   clrf  PCLATH

    movlw LOW TMR1CN      ;Timer 1 lo byte. (adjust if needed)
    movwf TMR1L     ;reset timer 1
    clrf  PIR1      ;clear all timer flags

    bsf   Sflag,SFNSCAN ;Trigger a scan
    
    movff Bsr_tempL,BSR   ;End of low priority interrupt
    movf  W_tempL,W
    movff St_tempL,STATUS 
    retfie  


;******************************************************************
; Scans all 16 columns
; Switches are arranged in eight blocks as follows:
; Row   0 1 2 3   4 5 6 7
; Col   Block 0       Block 4     
; 0   0 1 2 3   64  65  66  67
; 1   4 5 6 7   68  69  70  71
; 2   8 9 10  11    72  73  74  75
; 3   12  13  14  15    76  77  78  79
;     Block 1       Block 5     
; 4   16  17  18  19    80  81  82  83
; 5   20  21  22  23    84  85  86  87
; 6   24  25  26  27    88  89  90  91
; 7   28  29  30  31    92  93  94  95
;     Block 2       Block 6     
; 8   32  33  34  35    96  97  98  99
; 9   36  37  38  39    100 101 102 103
; 10    40  41  42  43    104 105 106 107
; 11    44  45  46  47    108 109 110 111
;     Block 3       Block 7     
; 12    48  49  50  51    112 113 114 115
; 13    52  53  54  55    116 117 118 119
; 14    56  57  58  59    120 121 122 123
; 15    60  61  62  63    124 125 126 127

scansw  bcf   Sflag,SFNSCAN ;Clear scan flag
    movf  PORTC,W     ;Read row data
    movwf Row       ;Save row data, Each bit is 0 if switch closed
    lfsr  FSR0,Buffer0  ;point to buffer
    movf  Ccount,W    ;get column count
    andlw 0x0F      ;keep in range
    addwf FSR0L,F     ;FSR0 points to buffer entry for column
    movf  INDF0,W     ;get old row
    xorwf Row,W     ;compare with new
    bz    scan5     ;no change
    ;One or more switch inputs on this column has changed state
    movwf Bitcng      ;hold the bit change(s)
    movff Row,INDF0   ;Save this state
    clrf  Bitcnt      ;Count bits from 0..7
    movlw B'00000001'   
    movwf Bitmask     ;Initialise bit mask
scan2 rrcf  Bitcng,F    ;rotate to find which bit changed
    bnc   scan4     ;no change
    ;A bit has changed state, update OnOff flag
    bcf   Sflag,SFONOFF ;Clear OnOff flag
    movf  Row,W     ;Get this state
    andwf Bitmask,W   ;Mask
    bnz   scan3     ;Now OFF (high)
    bsf   Sflag,SFONOFF ;Set ON if switch low
    ;Calculate switch number (0..127)
scan3 movf  Ccount,W    ;Get column count (0..15)
    andlw 0x0F      ;keep in range
    addwf WREG,W      ;*2
    addwf WREG,W      ;*4
    addwf Bitcnt,W    ;Add bit (0..7)
    btfsc Bitcnt,2    ;Skip if left hand bank
    addlw 60       ;Add offset for right hand bank
    andlw 0x7F      ;Keep in range
    movwf Index     ;And save switch number (0..127)
    ;Calculate block number (0..7)
    swapf WREG,W      ;Swap nibbles
    andlw 0x07      ;Keep in range
    movwf Block     ;Save in block as well
    lfsr  FSR0,ScMode0  ;Point to scan modes
    movff PLUSW0,ScMode ;Save scan mode for block
    btfss Lockout,0   ;Skip if lockout enabled
    bcf   ScMode,SCLOCKOUT;Lockout not enabled, clear flag
    call  schange     ;Process changed switch
scan4 rlncf Bitmask,F   ;Move mask
    incf  Bitcnt,F    ;Increment count
    btfss Bitcnt,3    ;Skip if done eight bits
    bra   scan2     ;No, keep going
scan5 incf  Ccount,F    ;Next column
    movf  Ccount,W    ;Get column count
    andlw 0x0F      ;Mask crud
    movwf PORTA     ;Send it to the 1:16 decoder to select a column
    movf  Ccount,W    ;Get column count again
    bz    sodgen      ;Check for SOD generation
    return          ;finish scan    
     
;******************************************************************
;Process SOD generation. Called every 16*SCANTM mS
;******************************************************************
sodgen  bcf   Oflag,OFINIT  ;Clear init flag
    movf  SodMode,W   ;Get SOD mode
    bz    sodgenx     ;Nothing to do
    andlw SODTIME     ;Mask out time
#if SCANTM*.16 == 500000
    addwf WREG,W      ;*2 to get units of 500mS
#else
#if SCANTM*.16 != 1000000
    ERROR Need to modify sodgen to generate correct delay!
#endif
#endif
    incf  SodCnt,F    ;Increment counter
    cpfsgt  SodCnt      ;Skip if time expired
    return          ;Not expired, return
    clrf  SodCnt,F    ;Reset counter
    btfss SodMode,SODGN1  ;Skip if SOD ON
    bra   sodoff      ;Do SOD OFF
    bcf   SodMode,SODGN1  ;Clear flag so ON event only sent once
    bsf   Sflag,SFONOFF ;Set On/Off flag
    call  sodsnd      ;and send
    call  dely      ;wait a while
    bra   do_sod      ;send any of our SOD stuff

sodoff  btfss SodMode,SODGN0  ;Skip if Generate OFF event
    bra   sodclr      ;No, all done then
    bcf   Sflag,SFONOFF ;Clear On/Off flag
    call  sodsnd      ;send
sodclr  clrf  SodMode     ;All done
sodgenx return

sodsnd  movlw SODSW     ;Get 'switch' number for SOD event
    movwf Index     ;Save in Index
    goto  sendpkt     ;Send event

;******************************************************************
;Process changed switch
;Index = Switch Number (0..127)
;Block = Block Number (0..7)
;Ccount = Column Number (0..15)
;Sflag.SFONOFF = Switch State (1 = On)
;ScMode = Scan Mode

schange btfsc ScMode,SCPBPAIR ;Skip if NOT Pushbutton Pair mode?
    bra   chpair      ;Process Pushbutton Pair mode
    btfsc ScMode,SCPBTOGG ;Skip if NOT Pushbutton Toggle mode?
    bra   chtogg      ;Process Pushbutton Toggle mode
    btfsc ScMode,SCPBTOGN ;Skip if NOT Pushbutton Toggle with no monitor mode?
    bra   chtogg      ;Process Pushbutton Toggle mode
    ;Normal key; update state
    lfsr  FSR0,Switch_State;Point to switch states
    movf  Index,W     ;Get switch number
    addwf FSR0L,F     ;FSR0 points to toggle state
    bcf   INDF0,SS_STATE  ;Clear state
    btfsc Sflag,SFONOFF ;Skip if switch off
    bsf   INDF0,SS_STATE  ;Set state    
    ;Only send new state if block isn't locked out
    btfss ScMode,SCLOCKOUT;Skip if lockout enabled
    bra   sendpkt     ;and send packet out
    return

    ;Pushbutton Toggle mode - each push sends ON or OFF state
chtogg  btfsc ScMode,SCLOCKOUT;Skip if no lockout
    return          ;Locked out, ignore
    btfss Sflag,SFONOFF ;Skip if switch ON
    return          ;OFF: Nothing to do then
    ;Pushbutton toggle switch has been pushed ON
    lfsr  FSR0,Switch_State;Point to toggle states
    movf  Index,W     ;Get switch number
    addwf FSR0L,F     ;FSR0 points to toggle state
    btg   INDF0,SS_STATE  ;Change toggle state
    bcf   Sflag,SFONOFF ;Set "Switch OFF" command
    btfsc INDF0,SS_STATE  ;Skip if now off
    bsf   Sflag,SFONOFF ;Set "Switch ON" command
    bra   sendpkt

    ;Pairs have first row as ON, second row as OFF
chpair  btfsc ScMode,SCLOCKOUT;Skip if no lockout
    return          ;Locked out, ignore
    btfss Sflag,SFONOFF ;Skip if switch ON
    return          ;OFF: Nothing to do then
    bcf   Sflag,SFONOFF ;Set OFF
    btfss Index,0     ;Skip if off switch
    bsf   Sflag,SFONOFF ;Set ON
    bcf   Index,0     ;Clear LSB

    ;Send switch number 'Index', state in Sflag.SFONOFF
sendpkt btfsc Oflag,OFINIT  ;Skip if not first time through?
    return          ;Don't send changes
    btfss Mode0,2     ;is it learn mode?
    bra   sndpkt0
    bsf   Mode0,3     ;has got button push
    return          ;yes so don't send

    ;In SLiM Mode 1, we have to 'halve' the switch number
    ;to retain compatibility with existing systems
sndpkt0 btfsc Sflag,SFSLIM1 ;Skip if not SLIM mode 1
    rrncf Index,F
    call  get_event   ;gets event from table into Tx1
    btfsc Mode0,4     ;FLiM learn?
    goto  snd_inx     ;send event with index
    movf  Tx1d1,F
    bnz   long
    movf  Tx1d2,F
    bnz   long

    btfss Sflag,SFONOFF ;which direction?
    bra   short0
    ;Short ON event
    btfsc ScMode,SCOFFONLY;Skip if NOT off only
    return          ;Ignore
    movlw OPC_ASON    ;set command
    movwf Tx1d0
    bra   sndpkt4

    ;Short OFF event
short0  btfsc ScMode,SCONONLY ;Skip if NOT on only
    return          ;Ignore
    movlw OPC_ASOF    ;unset command
    movwf Tx1d0     ;put in CAN frame
sndpkt4 movlw 5
    movwf Dlc       ;5 byte command
    movff NN_temph,Tx1d1  ;put in node NN for traceability
    movff NN_templ,Tx1d2
    bra   sndpkt6

long  btfss Sflag,SFONOFF ;which direction?
    bra   long0
    ;Long ON event
    btfsc ScMode,SCOFFONLY;Skip if NOT off only
    return          ;Ignore
    movlw OPC_ACON    ;set command
    bra   sndpkt1

    ;Long OFF event
long0 btfsc ScMode,SCONONLY ;Skip if NOT on only
    return          ;Ignore
    movlw OPC_ACOF    ;unset command
sndpkt1 movwf Tx1d0     ;put in CAN frame
    bra   sndpkt4

    ;Send event with index
snd_inx movlw OPC_ACON1
    movwf Tx1d0     ;event with data byte
    movff Index,Tx1d5   ;get index
    incf  Tx1d5,F     ;pointno is index +1
    movlw 6
    movwf Dlc
    ;Send event
sndpkt6 movlw B'00001111'   ;clear last priority
    andwf Tx1sidh,F
    movlw B'10110000'   ;starting priority
    iorwf Tx1sidh,F
    movlw 10
    movwf Latcount
    goto  sendTX      ;send CAN frame and return

;***************************************************************
;Process a start of day event

do_sod  clrf  Index     ;Start at switch zero
do_sod1 movf  Index,W     ;Get switch number
    lfsr  FSR1,Switch_State;Point to toggle states
    addwf FSR1L,F     ;FSR1 now points to state
    swapf WREG,W      ;Swap nibbles
    andlw 0x07      ;Keep in range
    lfsr  FSR0,ScMode0  ;Point to scan modes
    addwf FSR0L,F     ;FSR0 now points to right mode
    btfss INDF0,SCLOCKOUT ;Skip if Lockout block
    bra   do_sodx     ;Ignore
    btfsc Lockout,0   ;Skip if lockout not enabled
    bra   do_sod3     ;Ignore
do_sodx btfsc INDF0,SCPBPAIR  ;Skip if not Pushbutton pair
    bra   do_sod3     ;Ignore
    btfsc INDF0,SCPBTOGG  ;Skip if not Pushbutton toggle
    bra   do_sod2     ;Process
    btfsc INDF0,SCPBTOGN  ;Skip if not Pushbutton toggle
    bra   do_sod2     ;Process
    btfss INDF0,SCSOD   ;Skip if not SOD
    bra   do_sod3     ;Ignore
    bcf   Sflag,SFONOFF ;Clear state
    btfsc INDF1,SS_STATE  ;Skip if switch off
    bsf   Sflag,SFONOFF ;Set state    
    call  sendpkt     ;Send state out
    call  dely      ;Wait a while
    bra   do_sod3     ;and do next switch

;Process pushbutton toggle switch
do_sod2 bcf   INDF1,SS_STATE  ;Set state off

;Select next switch
do_sod3 incf  Index,F     ;Next switch
    btfss Index,7     ;Got to 128?
    bra   do_sod1     ;No, do next
    return          ;All done

;********************************************************************
; initialise matrix buffer

buf_init clrf Ccount      ;column count
    lfsr  FSR0,Buffer0
inscan1 movlw B'00001111'
    andwf Ccount,F
    movf  Ccount,W
    movwf PORTA     ;set columns
    incf  Ccount      ;next column
    movf  PORTC,W     ;get row data
    xorlw 0xFF      ;Invert bits
    movwf POSTINC0    ;Save row data & increment
    btfss Ccount,4    ;more than 15?
    bra   inscan1     ;next column
                ;finish scan  
    ; Clear toggle memory
    movlw 128      ;128 bytes to clear
    lfsr  FSR0,Switch_State;Point to state
inscan3 clrf  POSTINC0    ;clear toggle state
    decfsz  WREG,W      ;drop count, skip if 0
    bra   inscan3     ;next column

    clrf  Ccount      ;column count
    bcf   Sflag,SFNSCAN ;Clear scan flag
    bsf   Oflag,OFINIT  ;Set init flag
    return

;********************************************************************
; Build monitor table
; This is used as a fast index between and incoming event number (0-255 only)
; and the associated switch number for that event
moni_build
    ; First, set all entries in table to 0xFF
    lfsr  FSR0,Moni_Table ;Point to table
    clrf  Index     ;Clear index
    movlw 0xFF      ;Initial value
monib1  movwf POSTINC0    ;Store 0xFF, increment FSR
    decfsz  Index,F     ;Decrement count, skip if zero
    bra   monib1      ;Loop
    ; Now see which switches to monitor
    clrf  Index     ;Clear switch number
    lfsr  FSR0,ScMode0  ;Point to scan modes
monib2  btfsc INDF0,SCPBTOGG  ;Skip if not toggle monitor mode
    bra   monib3      ;Do next
monibx  movlw 16       ;16 switches per block
    addwf Index,F     ;Advance index
    bra   monib4      ;and loop

monib3  call  get_event   ;Get event for this switch into Tx1d3/Tx1d4
    movf  Tx1d3,W     ;Get high event number
    bnz   monib4      ;Not zero, can't use this
    lfsr  FSR1,Moni_Table ;Point to table
    movf  Tx1d4,W     ;Get low event number into W
    addwf FSR1L,F     ;Add it to table pointer
    movff Index,INDF1   ;Save switch number in table
    incf  Index,F     ;Next switch number
    movf  Index,W     ;Get index
    andlw 0x0F      ;Done 16 yet?
    bnz   monib3      ;No, do next switch
monib4  incf  FSR0L,F     ;Point to next block
    btfss Index,7     ;Skip if done 128 switches
    bra   monib2      ;Do next
    return          ;finish scan    

;***************************************************************************
;Process incoming event
;Rx0d0 = Bit 0 clear for ON, set for OFF
;Rx0d1 = Node High (0 for short events)
;Rx0d2 = Node Low (0 for short events)
;Rx0d3 = Event High
;Rx0d4 = Event Low
; to update internal state of any "Pushbutton Toggle" switches

do_event
    btg   Rx0d0,0     ;Now 1=ON, 0=OFF

;***************************************************************
;Check for incoming lockout event

    movlw LOCKSW      ;Get lockout 'switch' number
    call  set_tblptr    ;Set TBLPTR registers from W
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Node number hi
    cpfseq  Rx0d1     ;Skip if match
    bra   dosev1      ;No match
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Node number lo
    cpfseq  Rx0d2     ;Skip if match
    bra   dosev1      ;No match
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Event number hi
    cpfseq  Rx0d3     ;Skip if match
    bra   dosev1      ;No match
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Event number lo
    cpfseq  Rx0d4     ;Skip if match
    bra   dosev1      ;No match
    ;This is the lockout event
    movff Rx0d0,Lockout ;Update lockout flag with ON/OFF state
    call  lock_save   ;Save state in eeproom
#if 1 ; I'm not convinced that one event should do more than one thing
    return          ;and finish here
#endif
dosev1

;***************************************************************
;Check for incoming Start of Day event

    btfss Rx0d0,0     ;Skip if ON event
    bra   dosev2      ;Ignore if OFF 
    movlw SODSW     ;Get Start of Day 'switch' number
    call  set_tblptr    ;Set TBLPTR registers from W
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Node number hi
    cpfseq  Rx0d1     ;Skip if match
    bra   dosev2      ;No match
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Node number lo
    cpfseq  Rx0d2     ;Skip if match
    bra   dosev2      ;No match
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Event number hi
    cpfseq  Rx0d3     ;Skip if match
    bra   dosev2      ;No match
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Event number lo
    cpfseq  Rx0d4     ;Skip if match
    bra   dosev2      ;No match
    ;This is the Start of Day event
    goto  do_sod      ;Do start of day
dosev2

;***************************************************************
;Check for monitor event
    movf  Rx0d3,F     ;Is high byte of event non-zero ?
    bnz   dosev3      ;No, exit
    lfsr  FSR0,Moni_Table ;Point to monitor table
    movf  Rx0d4,W     ;Get low event number
    addwf FSR0L,F     ;Add to FSR0
    btfsc INDF0,7     ;Skip if switch 0..127
    bnz   dosev3      ;No, exit
    movf  INDF0,W     ;Get switch number for event (0..127)
    ;Get event from flash table to check the node number matches
    call  set_tblptr    ;Set TBLPTR registers to W
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get node number hi
    cpfseq  Rx0d1     ;Skip if match
    bnz   dosev3      ;No, exit
    tblrd*+         ;read into TABLAT and increment
    movf  TABLAT,W    ;Get Node number lo
    cpfseq  Rx0d2     ;Skip if match
    bnz   dosev3      ;No, exit
    ; Only monitored pushbutton toggle switches will be in the Moni_Table
    ; so if we get this far, we know that this is to be changed
    ; The switch number (0..127) is in Index
    lfsr  FSR0,Switch_State;Point to states
    movf  Index,W     ;Get switch number
    addwf FSR0L,F     ;Add offset so FSR0 points to state
    btfsc Rx0d0,0     ;Skip if OFF command
    bra   monie3
    ;OFF Command
    bcf   INDF0,SS_STATE  ;Set toggle state
    return

    ;ON Command
monie3  bsf   INDF0,SS_STATE  ;Set toggle state
    return

dosev3
    return
    

;***************************************************************
; load NV's from EEPROM
loadnv  movlw LOW NVstart
    movwf EEADR
    call  eeread
    movwf ScMode0     

    incf  EEADR
    call  eeread
    movwf ScMode1

    incf  EEADR
    call  eeread
    movwf ScMode2   

    incf  EEADR
    call  eeread
    movwf ScMode3   

    incf  EEADR
    call  eeread
    movwf ScMode4   

    incf  EEADR
    call  eeread
    movwf ScMode5   

    incf  EEADR
    call  eeread
    movwf ScMode6   

    incf  EEADR
    call  eeread
    movwf ScMode7   

    incf  EEADR
    call  eeread
    movwf SodMode       ;Save SOD generation mode
    clrf  SodCnt        ;Clear counter as well

    call  lock_load     ;Load lockout state
    call  scan_init     ;do rest of scan initialisation
    goto  moni_build      ;build monitor table

;**************************************************************************

;   check if command is for this node

thisNN  movf  NN_temph,W
    subwf Rx0d1,W
    bnz   not_NN
    movf  NN_templ,W
    subwf Rx0d2,W
    bnz   not_NN
    retlw   0     ;returns 0 if match
not_NN  retlw 1

;************************************************************
;   send node parameter bytes (7 maximum)

parasend  
    movlw OPC_PARAMS
    movwf Tx1d0
    movlw LOW nodeprm
    movwf TBLPTRL
    movlw 8
    movwf TBLPTRH   ;relocated code
    lfsr  FSR0,Tx1d1
    movlw 7
    movwf Count
    bsf   EECON1,EEPGD
    
para1 tblrd*+
    movff TABLAT,POSTINC0
    decfsz  Count
    bra   para1
    bcf   EECON1,EEPGD  
    movlw 8
    movwf Dlc
    call  sendTXa
    return

;**************************************************************************
;   send module name - 7 bytes

namesend  
    movlw OPC_NAME
    movwf Tx1d0
    movlw LOW myName
    movwf TBLPTRL
    movlw HIGH myName
    movwf TBLPTRH   ;relocated code
    lfsr  FSR0,Tx1d1
    movlw 7
    movwf Count
    bsf   EECON1,EEPGD
    
name1 tblrd*+
    movff TABLAT,POSTINC0
    decfsz  Count
    bra   name1
    bcf   EECON1,EEPGD  
    movlw 8
    movwf Dlc
    call  sendTXa
    return
    

;**********************************************************

;   send individual parameter

;   Index 0 sends no of parameters

para1rd movf  Rx0d3,w
    sublw 0
    bz    numParams
    movlw PRMCOUNT
    movff Rx0d3, Temp
    decf  Temp
    cpfslt  Temp
    bra   pidxerr
    movlw OPC_PARAN
    movwf Tx1d0
    movlw 7   ;FLAGS index in nodeprm
    cpfseq  Temp
    bra   notFlags      
    call  getflags
    movwf Tx1d4
    bra   addflags
notFlags    
    movlw LOW nodeprm
    movwf TBLPTRL
    movlw HIGH nodeprm
    movwf TBLPTRH   ;relocated code
    clrf  TBLPTRU
    decf  Rx0d3,W
    addwf TBLPTRL
    bsf   EECON1,EEPGD
    tblrd*
    movff TABLAT,Tx1d4
addflags            
    movff Rx0d3,Tx1d3
    movlw 5
    movwf Dlc
    call  sendTXNN
    return  
    
numParams
    movlw OPC_PARAN
    movwf Tx1d0
    movlw PRMCOUNT
    movwf Tx1d4
    movff Rx0d3,Tx1d3
    movlw 5
    movwf Dlc
    call  sendTXNN
    return
    
pidxerr
    movlw 10
    call  errsub
    return
    
getflags    ; create flags byte
    movlw PF_PRODUCER
    btfsc Mode0,1
    iorlw 4   ; set bit 2
    movwf Temp
    bsf   Temp,3    ;set bit 3, we are bootable
    movf  Temp,w
    return
    
;**********************************************************
; returns Node Number, Manufacturer Id, Module Id and Flags

whoami
    call  ldely   ;wait for other nodes
    movlw OPC_PNN
    movwf Tx1d0
    movlw MAN_NO    ;Manufacturer Id
    movwf Tx1d3
    movlw MODULE_ID   ; Module Id
    movwf Tx1d4
    call  getflags
    movwf Tx1d5
    movlw 6
    movwf Dlc
    call  sendTXNN
    return
    

;*************************************************************************

errsub  movwf Tx1d3   ;main eror message send. Error no. in WREG
    movlw OPC_CMDERR
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    return

;**********************************************************************
;
;   self enumeration as separate subroutine
self_enA
    bcf   Datmode,MD_IDCONF     ;clear calling flag

self_en movff FSR1L,Fsr_tmp1Le  ;save FSR1 just in case
    movff FSR1H,Fsr_tmp1He 
    movlw B'11000000'
    movwf INTCON      ;start interrupts if not already started
    bsf   Datmode,MD_SETUP    ;set to 'setup' mode
    clrf  Tx1con      ;CAN ID enumeration. Send RTR frame, start timer
    movlw 14
    movwf Count
    lfsr  FSR0, Enum0
clr_en
    clrf  POSTINC0
    decfsz  Count
    bra   clr_en
    
    movlw B'10111111'   ;fixed node, default ID  
    movwf Tx1sidh
    movlw B'11100000'
    movwf Tx1sidl
    movlw B'01000000'   ;RTR frame
    movwf Dlc
    
    movlw 0x3C      ;set T3 to 100 mSec (may need more?)
    movwf TMR3H
    movlw 0xAF
    movwf TMR3L
    movlw B'10110001'
    movwf T3CON     ;enable timer 3

    movlw 10
    movwf Latcount
    
    call  sendTXa     ;send RTR frame
    clrf  Tx1dlc      ;prevent more RTR frames

self_en1
    btfss PIR2,TMR3IF   ;setup timer out?
    bra   self_en1      ;fast loop till timer out 
    bcf   T3CON,TMR3ON  ;timer off
    bcf   PIR2,TMR3IF   ;clear flag
    clrf  IDcount
    incf  IDcount,F     ;ID starts at 1
    clrf  Roll
    bsf   Roll,0
    lfsr  FSR1,Enum0      ;set FSR to start
here1 incf  INDF1,W       ;find a space
    bnz   here
    movlw 8
    addwf IDcount,F
    incf  FSR1L
    bra   here1
here  movf  Roll,W
    andwf INDF1,W
    bz    here2
    rlcf  Roll,F
    incf  IDcount,F
    bra   here
here2 movlw 100        ;limit to ID
    cpfslt  IDcount
    bra   segful        ;segment full
    
here3 movlw LOW CANid   ;put new ID in EEPROM
    movwf EEADR
    movf  IDcount,W
    call  eewrite
    call  newid_f     ;put new ID in various buffers

      
    movff Fsr_tmp1Le,FSR1L  ;
    movff Fsr_tmp1He,FSR1H 
    return

segful  movlw 7   ;segment full, no CAN_ID allocated
    call  errsub
    setf  IDcount
    bcf   IDcount,7
    bra   here3

;*************************************************************************
; Learn an event for 'Index' (0..255)
; Rx0d1 = Node Number High
; Rx0d2 = Node Number Low
; Rx0d3 = Event Number High
; Rx0d4 = Event Number Low
; Uses ENtempl,ENtemph,NN_templ,NN_temph

learn_event
    movff Index,ENtempl   ;Index for table is base 0
    clrf  ENtemph
    bcf   STATUS,C      ;Clear carry
    rlcf  ENtempl,F     ;double it for pointing to Table 
    rlcf  ENtemph,F     ;Shift carry up
    rlcf  ENtempl,F     ;double it
    rlcf  ENtemph,F     ;Shift carry up
    movlw B'11000000'
    andwf ENtempl,W
    movwf TBLPTRL       ;start of 64 byte block
    movf  ENtemph,W
    addlw high Event_Table
    movwf TBLPTRH
    
    movlw 64         ;read block 
    movwf Flcount
    lfsr  FSR2,Flash_buf    ;point to holding buffer
    clrf  TBLPTRU
    
learn1  tblrd*+       ;read into TABLAT and increment
    movf  TABLAT,W
    movwf POSTINC2
    decfsz  Flcount
    bra   learn1

    lfsr  FSR2,Flash_buf
    movf  ENtempl,W
    andlw B'00111111'     ;64 byte range
    
    addwf FSR2L,F       ;FSR2 points to EN to change
    movf  POSTINC2,W      ;get old NN
    subwf Rx0d1,W
    bnz   new
    movf  POSTINC2,W      ;get old NN
    subwf Rx0d2,W
    bnz   new
    movf  POSTINC2,W      ;get old NN
    subwf Rx0d3,W
    bnz   new
    movf  POSTINC2,W      ;get old NN
    subwf Rx0d4,W
    bnz   new
    btfsc Mode0,1       ;is it FLiM?
    bra   new
    lfsr  FSR2,Flash_buf    ;here if same so undo it
    movf  ENtempl,W
    andlw B'00111111'     ;64 byte range
    addwf FSR2L,F       ;FSR2 points to EN to change
    movff NN_temph,POSTINC2 ;restore original
    movff NN_templ,POSTINC2
    clrf  POSTINC2
    incf  Index,W
    movwf POSTINC2
    bra   new1

new   lfsr  FSR2,Flash_buf
    movf  ENtempl,W
    andlw B'00111111'     ;64 byte range
    
    addwf FSR2L,F       ;FSR2 points to EN to change

    movff Rx0d1,POSTINC2    ;NNhi
    movff Rx0d2,POSTINC2    ;NNlo
    movff Rx0d3,POSTINC2    ;ee hi
    movff Rx0d4,POSTINC2    ;ee lo

new1  call  event_erase     ;erase block
    movf  ENtempl,W     ;point to start of block rewrite
    movwf TBLPTRL
    movf  ENtemph,W
    addlw high Event_Table
    movwf TBLPTRH 
    call  f_write       ;write buffer back  
    goto  moni_build      ;rebuild monitor table

;***************************************************
;   Erase a page (64 bytes) of flash in the Event_Table
;   ENtemph/ENtempl contains index into Event_Table
;   TBLPTRU is assumed to be zero
    
event_erase
    movf  ENtempl,W     ;point to start of block to erase
    movwf TBLPTRL
    movf  ENtemph,W
    addlw high Event_Table
    movwf TBLPTRH
    bsf   EECON1,EEPGD    ;set up for erase
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
    bsf   EECON1,FREE
    clrf  INTCON
    
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR     ;erase
er_done btfsc EECON1,WR     ;check erase is done
    bra   er_done       ;Added v2g
    movlw B'11000000'
    movwf INTCON        ;reenable interrupts
    return
    
;*********************************************************************

;   write flash buffer to flash
;   set table pointer first   
    
f_write movlw B'11000000'
    andwf TBLPTRL,F     ;put to 64 byte boundary
    tblrd*-           ;back 1
f_writx lfsr  FSR2,Flash_buf    ;ready for rewrite
    movlw 8
    movwf Flcount
f_wr1 movlw 8
    movwf Flcount1
  
    
f_wr2 movf  POSTINC2,W
    movwf TABLAT
    tblwt+*
    decfsz  Flcount1
    bra   f_wr2
    bsf   EECON1,EEPGD    ;set up for write back
    bcf   EECON1,CFGS
    bcf   EECON1,FREE
    bsf   EECON1,WREN
  
    clrf  INTCON
    
    
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR     ;write back
wr_done btfsc EECON1,WR     ;write done?
    bra   wr_done
  
    decfsz  Flcount
    bra   f_wr1       ;next block of 8
    movlw B'11000000'
    movwf INTCON        ;reenable interrupts
    return

;**************************************************************
; fills flash buffer on first time only - or on full reset
; loses any stored non-default events

clear_events
    clrf  ENtempl     ;start at beginning
    clrf  ENtemph
    movlw 16       ;set for erase all. 16 blocks
    movwf Count
    clrf  TBLPTRU     ;set to flash start
    movlw high Event_Table
    movwf TBLPTRH
    clrf  TBLPTRL
    tblrd*-
c_buf call  event_erase   ;erase 64 bytes (16 entries)
    dcfsnz  Count
    bra   fb0       ;all done
    movlw 64       ;next block to erase
    bcf   STATUS,C
    addwf ENtempl
    bnc   c_buf
    incf  ENtemph
    bra   c_buf

;Flash buffer is now erased; fill all 256 entries with default events

fb0   clrf  Count1      ;button number
    incf  Count1      ;button numbers start at 1
    movlw 16       ;16 pages of 64 bytes (16 entries) to do
    movwf Count2
    clrf  TBLPTRU     ;set to flash start
    movlw high Event_Table
    movwf TBLPTRH
    clrf  TBLPTRL
    tblrd*-

fb1   lfsr  FSR2,Flash_buf
    movlw 16
    movwf Count
fb2
#if J5_SHORT          ;Default to short events if FLiM mode and J5 in upper posn
    btfss Mode0,1     ;Skip if FLiM?
    bra   fb2a      ;If SLiM
    btfss M_PORT,M_BIT  ;Check J5
    bra   fb2a      ;Use defaults
    clrf  POSTINC2    ;Zero; use short event
    clrf  POSTINC2
    bra   fb2b
fb2a
#endif
    movff NN_temph,POSTINC2 
    movff NN_templ,POSTINC2
fb2b  clrf  POSTINC2
    movff Count1,POSTINC2
    incf  Count1
    decfsz  Count
    bra   fb2 
    call  f_writx     ;write block of 64
    decfsz  Count2      ;last block?
    bra   fb1
    nop
    movlw LOW Node_st   ;reset loaded status
    movwf EEADR
    movlw 1
    call  eewrite
    clrf  Lockout     ;clear any lockout
    goto  lock_save


;****************************************************************
;   gets event from FLASH table
;   Uses index (0..255)
;   Result in Tx1 buffer

get_event
    movf  Index,W   ;Get index for table
    call  set_tblptr  ;Setup table pointers
    tblrd*+       ;read into TABLAT and increment
    movf  TABLAT,W
    movwf Tx1d1   ;NNhi
    tblrd*+       
    movf  TABLAT,W
    movwf Tx1d2   ;NNlo
    tblrd*+         ;high event byte
    movf  TABLAT,W
    movwf Tx1d3
    tblrd*+
    movf  TABLAT,W
    movwf Tx1d4
#if 0   ; why?
    nop
#endif
    return

;***************************************************************
;Set TBLPTR to Event_Table index in W (0..255)
;Assumes Event_Table is aligned on a 256 byte boundary
;TBLPTR = 00000000 001000hg fedcba00 where W = hgfedcba 

set_tblptr
    clrf  TBLPTRU   ;Clear upper byte
    clrf  TBLPTRH   ;Clear high byte
    movwf TBLPTRL   ;Save index in low byte
    bcf   STATUS,C  ;Clear carry
    rlcf  TBLPTRL,F ;low byte *2
    rlcf  TBLPTRH,F ;Shift carry into high byte
    rlcf  TBLPTRL,F ;low byte *4
    rlcf  TBLPTRH,F ;Shift carry into high byte
    movlw high Event_Table;Get start of table
    addwf TBLPTRH   ;Add to high pointer
    return

;***************************************************************
;     read an event back by index no.

read  movff Rx0d3,Index
    decf  Index,F     ;index sent as base 1
    call  get_event
    movlw OPC_ENRSP
    movwf Tx1d0     ;OPC for answer
    movff Rx0d3,Tx1d7   ;index
    movff Tx1d4,Tx1d6   ;shuffle
    movff Tx1d3,Tx1d5
    movff Tx1d2,Tx1d4
    movff Tx1d1,Tx1d3
    movff NN_templ,Tx1d2
    movff NN_temph,Tx1d1
    movlw 8
    movwf Dlc
    call  sendTXa
    return

;****************************************************************

; write acknowledge CAN frame for FLASH write timing

wrack movlw OPC_WRACK   ;set up WRACK frame
    movwf Tx1d0
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    movlw 3
    movwf Dlc
    call  sendTXa
    return

;************************************************************************
;Set/Clear lockout flag (stored in eeprom)

lock_save
    movlw LOW NVlock      ;get pointer in EEPROM
    movwf EEADR
    movf  Lockout,W
    goto  eesave        ;write new value if changed
    
;************************************************************************
;Update lockout flag in ram from eeprom

lock_load
    movlw LOW NVlock
    movwf EEADR
    call  eeread
    movwf Lockout
    return

;************************************************************************
;Event lookup table in FLASH
;Each Entry is 4 bytes, Node High, Node Low, Event High, Event Low
;Entries 0..127 are used for each of the 128 switches
;Entries 128..255 are used for any special events
;Total size is 256 x 4 = 1024 bytes

  ORG 0x002000
Event_Table
  ;Don't reserve any space here though as it'll overwrite
  ;the event table when programming the PIC!
  ;res    256*4

;************************************************************************
  ORG 0xF00000      ;EEPROM data. Defaults
  
CANid de  B'01111111',0 ;CAN id default and module status
NodeID  de  0,0     ;Node ID
NVold de  0xAA,0xAA   ;Node variable from older code (to keep them out of the way)
NVstart de  0,0     ;Node variables
    de  0,0
    de  0,0
    de  0,0
;More for later
    de  0,0
    de  0,0

NVlock  de  0,0     ;Set/Cleared by lockout event

Node_st de  0,0     ;Move this well out of the way to avoid corrupting eeprom
    
  ORG 0xF000FE
    de  0,0     ;For boot   
    end
