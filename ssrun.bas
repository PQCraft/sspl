$CONSOLE
$SCREENHIDE
_TITLE ""
_CONSOLETITLE ""
ON ERROR GOTO ERRHNDL
CHDIR _STARTDIR$
FILE$ = COMMAND$(1)
_TITLE FILE$
DIM SHARED EC%
NULLERR
DIM SHARED TRUE AS _BIT
DIM SHARED FALSE AS _BIT
TRUE = -1
FALSE = 0
DIM SHARED DEBUG AS _BIT
DEBUG = FALSE
DIM SHARED INDSP AS _BIT
DIM SHARED INCON AS _BIT
DSP FALSE
CON TRUE
IF DEBUG THEN PRINT _CWD$
DIM SHARED INWIN AS _BIT
INWIN = INSTR(_OS$, "WINDOWS") <> 0
DIM SHARED C$
C$ = ""
DIM SHARED A$
A$ = ""
_CONSOLETITLE FILE$
CMD$ = COMMAND$
IF (CMD$ = "--help" AND NOT INWIN) OR (CMD$ = "/?" AND INWIN) THEN
    P_HLP
END IF
IF FILE$ = "" THEN
    P_HLP
ELSE
    IF NOT _FILEEXISTS(FILE$) THEN
        P_ERR FILE$ + " does not exist."
    END IF
END IF
OPEN FILE$ FOR BINARY AS #1
RANDOMIZE USING TIMER
DIM SHARED PPOS AS LONG
DIM SHARED PDO AS LONG
DIM SHARED PCHR AS _UNSIGNED _BYTE
DIM SHARED PLIN AS LONG
DIM SHARED LAP AS _UNSIGNED _BYTE
DIM SHARED LA(0 TO 255) AS LONG
DIM SHARED INARG AS _BIT
DIM SHARED VAR$(0 TO 255, 0 TO 15)
DIM SHARED VAR%(0 TO 255, 0 TO 255)
DIM SHARED VAR&(0 TO 15, 0 TO 15)
DIM SHARED IMGHNDL&(0 TO 15)
DIM SHARED SNDHNDL&(0 TO 15)
DIM SHARED NETHNDL&(0 TO 15)
DIM SHARED NETURL$(0 TO 15)
'DIM SHARED CIMG
DIM SHARED DCHAR
DCHAR = 32
PPOS = 0
PLIN = 0
LAP = -1
_ALLOWFULLSCREEN _OFF , _OFF
DO
    C$ = ""
    A$ = ""
    PLIN = PLIN + 1
    INARG = FALSE
    DO
        PPOS = PPOS + 1
        GET #1, PPOS, PCHR
        IF (PCHR = 32 AND NOT INARG) OR PCHR = 13 OR PCHR = 10 THEN
            IF PCHR = 32 THEN
                INARG = TRUE
            ELSE
                GET #1, PPOS + 1, TCHR
                IF TCHR = 10 OR TCHR = 13 THEN PPOS = PPOS + 1
                EXIT DO
            END IF
        ELSE
            IF INARG THEN
                A$ = A$ + CHR$(PCHR)
            ELSE
                C$ = C$ + CHR$(PCHR)
            END IF
        END IF
        IF EOF(1) THEN EXIT DO
    LOOP
    C$ = UCASE$(_TRIM$(C$))
    A$ = _TRIM$(A$)
    IF DEBUG THEN PRINT "<"; C$; ">"; "["; A$; "]"
    IF C$ <> "" THEN
        IF ASC(C$, 1) <> 35 THEN
            EXECCMD
        END IF
    END IF
    GET #1, PPOS + 1, TCHR
    IF EOF(1) THEN SYSTEM
LOOP UNTIL C$ = "EXIT" OR EOF(1)
SYSTEM

'-------SUBS-------

ERRHNDL:
P_ERR "Internal Error: " + _TRIM$(STR$(ERR)) + ";" + _TRIM$(STR$(_ERRORLINE))
SUB P_ERR (ERRSTR$)
    CON TRUE
    PRINT CHR$(27); "[1m"; CHR$(27); "[31mE:";
    PRINT CHR$(27); "[0m "; ERRSTR$
    SYSTEM
END SUB
SUB P_WRN (ERRSTR$)
    CON TRUE
    PRINT CHR$(27); "[1m"; CHR$(27); "[33m!:";
    PRINT CHR$(27); "[0m "; ERRSTR$
END SUB
SUB P_HLP
    CON TRUE
    IF INWIN <> 0 THEN
        PRINT "ssrun <ssf_file> [program arguments]"
    ELSE
        PRINT "Usage: ssrun ssf_file [program arguments]"
    END IF
    SYSTEM
END SUB
SUB P_CON (CONSTR$)
    IF INCON THEN
        PRINT CONSTR$
    ELSE
        _DEST _CONSOLE
        PRINT CONSTR$
        _DEST 0
    END IF
END SUB
SUB CON (CONSTAT)
    INCON = CONSTAT
    INDSP = NOT CONSTAT
    IF INCON THEN
        _CONSOLE ON
        _DEST _CONSOLE
    ELSE
        _CONSOLE OFF
    END IF
END SUB
SUB DSP (DSPSTAT)
    INDSP = DSPSTAT
    INCON = NOT DSPSTAT
    IF INDSP THEN
        _SCREENSHOW
        _DEST 0
    ELSE
        _SCREENHIDE
    END IF
END SUB
SUB EXECCMD
    EC% = 255
    ES$ = _TRIM$(C$) + " is not a command."
    IF A$ = CHR$(0) THEN A$ = ""
    DO WHILE ASC(RIGHT$(C$, 1)) = 0
        C$ = LEFT$(C$, LEN(C$) - 1)
    LOOP
    SELECT CASE C$
        CASE CHR$(0): NULLERR
        CASE "SET"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC$ = GETARG$(3)
                VC = VAL(VC$)
                SETVAR VA, VB, VC, VC$
            END IF
        CASE "ADD"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                IF VA > 271 THEN
                    VC$ = GETARG$(3)
                    SETVAR VA, VB, 0, GETVAR(VA, VB) + VC$
                ELSE
                    VC = VAL(GETARG$(3))
                    SETVAR VA, VB, VAL(GETVAR(VA, VB)) + VC, ""
                END IF
            END IF
        CASE "SUBT"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
                SETVAR VA, VB, VAL(GETVAR(VA, VB)) - VC, ""
            END IF
        CASE "MULT"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
                SETVAR VA, VB, VAL(GETVAR(VA, VB)) * VC, ""
            END IF
        CASE "DV"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
                SETVAR VA, VB, VAL(GETVAR(VA, VB)) / VC, ""
            END IF
        CASE "EXP"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
                SETVAR VA, VB, VAL(GETVAR(VA, VB)) ^ VC, ""
            END IF
        CASE "LPIF"
            NULLERR
            IF TEST(A$) THEN
                PPOS = LA(LAP)
            ELSE
                LAP = LAP - 1
            END IF
        CASE "LOOP"
            NULLERR
            PPOS = LA(LAP)
        CASE "DO"
            NULLERR
            LAP = LAP + 1
            LA(LAP) = PPOS
        CASE "WAIT"
            NULLERR
            _DELAY VAL(A$) / 1000
        CASE "EXIT"
            NULLERR
            SYSTEM
        CASE "CLRS", "CLS", "CLEAR"
            NULLERR
            CLS
        CASE "ECHO"
            NULLERR
            PRINT A$;
        CASE "ECLN"
            NULLERR
            PRINT A$
        CASE "ECHA"
            NULLERR
            PRINT GETARG$(1);
        CASE "ECLA"
            NULLERR
            PRINT GETARG$(1)
        CASE "ECHV"
            NULLERR
            VA = VAL(GETARG$(1))
            VB = VAL(GETARG$(2))
            PRINT _TRIM$(GETVAR$(VA, VB));
        CASE "PCHR"
            NULLERR
            IF LEN(A$) > 1 OR LEN(A$) < 1 THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                PRINT A$;
            END IF
        CASE "PASC"
            NULLERR
            PRINT CHR$(VAL(GETARG$(1)) MOD 256);
        CASE "LOC", "LOCATE"
            NULLERR
            VC = VAL(GETARG$(1))
            VL = VAL(GETARG$(2))
            LOCATE VL, VC
        CASE "CONR"
            NULLERR
            PRINT CHR$(27); "[0m";
        CASE "CONB"
            NULLERR
            PRINT CHR$(27); "[1m";
        CASE "CONU"
            NULLERR
            PRINT CHR$(27); "[4m";
        CASE "CONI"
            NULLERR
            PRINT CHR$(27); "[7m";
        CASE "DCON"
            NULLERR
            _DEST _CONSOLE
            INCON = TRUE
            INDSP = FALSE
        CASE "DDSP"
            NULLERR
            _DEST 0
            INDSP = TRUE
            INCON = FALSE
        CASE "CSCR", "CON"
            NULLERR
            DSP FALSE
            CON TRUE
        CASE "VSCR", "DSP"
            NULLERR
            CON FALSE
            DSP TRUE
        CASE "STTL", "TITLE"
            NULLERR
            _CONSOLETITLE A$
            _TITLE A$
        CASE "CTTL", "CTITLE", "CONTITLE", "CONTTL"
            NULLERR
            _CONSOLETITLE A$
        CASE "DTTL", "DTITLE", "DSPTITLE", "DSPTTL"
            NULLERR
            _TITLE A$
        CASE "VBFR"
            NULLERR
            IF (VAL(GETARG$(1)) MOD 2) * -1 THEN
                _DISPLAY
            ELSE
                _AUTODISPLAY
            END IF
        CASE "UBFR"
            NULLERR
            _DISPLAY
        CASE "FSCR"
            NULLERR
            VA~%% = VAL(GETARG$(1))
            SELECT CASE (VA~%% MOD 5)
                CASE 0
                    _FULLSCREEN _OFF
                CASE 1
                    _FULLSCREEN _STRETCH
                CASE 2
                    _FULLSCREEN _STRETCH , _SMOOTH
                CASE 3
                    _FULLSCREEN _SQUAREPIXELS
                CASE 4
                    _FULLSCREEN _SQUAREPIXELS , _SMOOTH
            END SELECT
        CASE "VTXT"
            NULLERR
            SCREEN 0
        CASE "VMODE"
            NULLERR
            VH = VAL(GETARG$(1))
            VV = VAL(GETARG$(2))
            VC = 32 + (1 - (VAL(GETARG$(3)) MOD 2)) * 224
            VB = VAL(GETARG$(4))
            SCREEN _NEWIMAGE(VH, VV, VC)
            IF (VB MOD 2) * -1 THEN
                _DISPLAY
            ELSE
                _AUTODISPLAY
            END IF
        CASE "COLOR"
            NULLERR
            VFGC% = VAL(GETARG$(1))
            VBGC$ = GETARG$(2)
            IF VBGC$ = "" THEN
                COLOR VFGC%
            ELSE
                COLOR VFGC%, VAL(VBGC$)
            END IF
        CASE "IMGLD"
            NULLERR
            IMGHNDL&(VAL(GETARG$(1))) = _LOADIMAGE(GETARG$(2))
        CASE "IMGPUT"
            NULLERR
            CIMG = VAL(GETARG$(1))
            IPX1% = VAL(GETARG$(2))
            IPY1% = VAL(GETARG$(3))
            IPX2$ = GETARG$(4)
            IPY2$ = GETARG$(5)
            IF IPX2$ = "" AND IPY2$ = "" THEN
                _PUTIMAGE (IPX1%, IPX1%), IMGHNDL&(CIMG)
            ELSE
                _PUTIMAGE (IPX1%, IPX1%)-(VAL(IPX2$), VAL(IPY2$)), IMGHNDL&(CIMG)
            END IF
        CASE "IMGRM"
            NULLERR
            _FREEIMAGE VAL(A$)
        CASE "MHIDE"
            NULLERR
            _MOUSEHIDE
        CASE "MSHOW"
            NULLERR
            _MOUSESHOW
        CASE "SNDLD"
            NULLERR
            SNDHNDL&(VAL(GETARG$(1))) = _SNDOPEN(GETARG$(2))
        CASE "SNDRM"
            NULLERR
            _SNDCLOSE SNDHNDL&(VAL(GETARG$(1)))
        CASE "SNDPLAY"
            NULLERR
            _SNDPLAY SNDHNDL&(VAL(GETARG$(1)))
        CASE "SNDLP"
            NULLERR
            _SNDLOOP SNDHNDL&(VAL(GETARG$(1)))
        CASE "SNDPAUSE"
            NULLERR
            _SNDPAUSE SNDHNDL&(VAL(GETARG$(1)))
        CASE "SNDPOS"
            NULLERR
            _SNDSETPOS SNDHNDL&(VAL(GETARG$(1))), VAL(GETARG$(2)) / 1000
        CASE "SNDSTOP"
            NULLERR
            _SNDSTOP SNDHNDL&(VAL(GETARG$(1)))
        CASE "SNDVOL"
            NULLERR
            _SNDVOL SNDHNDL&(VAL(GETARG$(1))), VAL(GETARG$(2)) / 100
        CASE "NETOPEN"
            NULLERR
            NH = VAL(GETARG$(1))
            U$ = GETARG$(2)
            NETURL$(NH) = U$
            P$ = GETARG$(3)
            IF P$ = "" THEN P$ = "80"
            NETHNDL&(NH) = _OPENCLIENT("TCP/IP:" + P$ + ":" + U$)
            IF NETHNDL&(NH) = 0 THEN NETURL$(NH) = "": P_WRN "NETOPEN: Connection to " + U$ + " failed."
        CASE "NETHTTP"
            NULLERR
            HR$ = "GET" + CHR$(13) + CHR$(10)
            PUT #NETHNDL&(VAL(GETARG$(1))), , HR$
        CASE "NETSEND"
            NULLERR
            D$ = GETARG$(2)
            IF D$ = CHR$(13) THEN D$ = CHR$(13) + CHR$(10)
            'PRINT D$
            PUT #NETHNDL&(VAL(GETARG$(1))), , D$
        CASE "NETSCHR"
            NULLERR
            D~%% = VAL(GETARG$(2))
            D$ = CHR$(D~%%)
            PUT #NETHNDL&(VAL(GETARG$(1))), , D$
        CASE "NETRCV"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                NH = VAL(GETARG$(1))
                VA = VAL(GETARG$(2))
                VB = VAL(GETARG$(3))
                GET #NETHNDL&(NH), , VC~%%
                VC$ = CHR$(VC~%%)
                SETVAR VA, VB, VC~%%, VC$
            END IF
        CASE "NETCLOSE"
            NULLERR
    END SELECT
    IF EC% > 0 THEN P_ERR "Line " + _TRIM$(STR$(PLIN)) + ": " + _TRIM$(ES$) + " (" + _TRIM$(STR$(EC%)) + ")"
END SUB
FUNCTION EXECFN$ (IN$)
    FN$ = UCASE$(LEFT$(IN$, INSTR(IN$, "(") - 1))
    FES$ = _TRIM$(FN$) + " is not a function."
    T$ = A$: TC = DCHAR: TE% = EC%
    A$ = MID$(IN$, INSTR(IN$, "(") + 1, _INSTRREV(IN$, ")") - INSTR(IN$, "(") - 1): DCHAR = 44: EC% = 254
    IF DEBUG THEN P_CON IN$
    SELECT CASE FN$
        CASE ""
            NULLERR
            P_WRN "Received blank function"
        CASE "VAR"
            NULLERR
            EXECFN$ = GETVAR$(VAL(GETARG$(1)), VAL(GETARG$(2)))
        CASE "CHR"
            NULLERR
            EXECFN$ = CHR$(VAL(GETARG$(1)))
        CASE "ASC"
            NULLERR
            EXECFN$ = _TRIM$(STR$(ASC(GETARG$(1) + CHR$(0))))
        CASE "RND", "RAND", "RANDOM"
            NULLERR
            EXECFN$ = _TRIM$(STR$(CINT(RND * VAL(GETARG$(2)) + VAL(GETARG$(1)))))
        CASE "KBIN"
            NULLERR
            EXECFN$ = INKEY$
        CASE "NETRCV"
            NULLERR
            NH = VAL(GETARG$(1))
            GET #NETHNDL&(NH), , VC~%%
            EXECFN$ = CHR$(VC~%%)
    END SELECT
    IF EC% > 0 THEN P_ERR "Line " + _TRIM$(STR$(PLIN)) + ": " + _TRIM$(FES$) + " (" + _TRIM$(STR$(EC%)) + ")"
    A$ = T$: DCHAR = TC: EC% = TE%
END FUNCTION
FUNCTION TEST (ARG$)

END FUNCTION
FUNCTION GETVAL$ (ARG$)

END FUNCTION
FUNCTION GETVALN (ARG$)

END FUNCTION
FUNCTION GETVAR$ (VN1, VN2)
    IF VN1 > 255 THEN
        VN1 = VN1 - 256
        IF VN1 > 15 THEN
            VN1 = VN1 - 16
            GETVAR$ = VAR$(VN1, VN2)
        ELSE
            GETVAR$ = STR$(VAR&(VN1, VN2))
        END IF
    ELSE
        GETVAR$ = STR$(VAR%(VN1, VN2))
    END IF
END FUNCTION
SUB SETVAR (VN1, VN2, N&, T$)
    IF VN1 > 255 THEN
        VN1 = VN1 - 256
        IF VN1 > 15 THEN
            VN1 = VN1 - 16
            VAR$(VN1, VN2) = T$
        ELSE
            VAR&(VN1, VN2) = N&
        END IF
    ELSE
        VAR%(VN1, VN2) = N&
    END IF
END SUB
FUNCTION GETARG$ (ANUM)
    IF A$ = "" THEN GETARG$ = "": EXIT FUNCTION
    DIM IIS AS _BIT
    IIS = 0
    IAS = 0
    IIP = 0
    CAN = 1
    HASQ = 0
    FOR I = 1 TO LEN(A$)
        CHAR = ASC(A$, I)
        IF CHAR = 40 AND IIS = FALSE THEN
            IF CAN = ANUM THEN GETARG$ = GETARG$ + CHR$(CHAR)
            IIP = TRUE
        ELSE
            IF CHAR = 34 THEN
                IIS = NOT IIS
                IAS = TRUE
            ELSE
                IF CHAR = DCHAR THEN
                    IF IIS OR IIP THEN
                        IF CAN = ANUM THEN GETARG$ = GETARG$ + CHR$(CHAR)
                    ELSE
                        CAN = CAN + 1
                        IF CAN > ANUM THEN GETARG$ = _TRIM$(GETARG$): EXIT FOR
                    END IF
                ELSE
                    IF CAN = ANUM THEN GETARG$ = GETARG$ + CHR$(CHAR)
                END IF
            END IF
        END IF
        IF CHAR = 41 AND IIP THEN IIP = FALSE
    NEXT
    IF GETARG$ = "" THEN EXIT FUNCTION
    'P_CON "GETARG$:1: " + GETARG$
    IF NOT IAS THEN GETARG$ = _TRIM$(GETARG$)
    IF RIGHT$(GETARG$, 1) = ")" AND NOT IAS THEN
        GETARG$ = EXECFN$(GETARG$)
    END IF
    'P_CON "GETARG$:2: " + GETARG$
END FUNCTION
SUB NULLERR
    EC% = 0
END SUB
