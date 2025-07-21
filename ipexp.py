#=======================================================================
#  IPEXP, Subroutine
#  Determines experiment and treatment selection
#-----------------------------------------------------------------------
#  INPUT  : MODEL,RUN,DS,SLNO,LNIC,LNSA,NYRS,VARNO,CROP,PATHMO,WMODI
#           FROP,SLTX
#  OUTPUT :
#-----------------------------------------------------------------------
#  Called : INPUT
#  Calls  : ERROR IGNORE VERIFY CLEAR FIND IPCUL PATH IPPLNT IPFLD IPSIM
#           YR_DOY IPENV IPHAR IPIRR IPRES IPFERT
#-----------------------------------------------------------------------
import math
from ModuleDefs import *
from UTILS import *
from COMIBS import *


# MODEL, FILECTL, SLNO,NYRS,VARNO, CROP,WMODI,FROP,TRTN,EXPP,EXPN,TITLET,TRTALL,
# FTYPEN, NFORC,PLTFOR,NDOF,PMTYPE, LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
# LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES, PMWD
def IPEXP (RUN,RNMODE,FILEX,PATHEX,FILEX_P,TRTNUM,ROTNUM, CONTROL, UseSimCtr, MODELARG):
    import fortranformat as ff
    from OPHEAD import OPHEAD
    from MAKEFILEW import MAKEFILEW
    from IPMAN import IPCUL
    from ERROR import ERROR
    from PATH import PATH
    from DATES import YR_DOY
    from WARNING import WARNING
    from IPENV import IPENV
    from IPMAN import IPHAR
    from COMSWI import SWITCH as sw
    from COMIBS import IBS01 as ib1, IBS02 as ib2, IBS03 as ib3, IBS04 as ib4
    from ModuleDefs import RunConstants
    from IPSIM import IPSIM
    from IPMAN import IPIRR, IPFERT
    from READS import FIND, IGNORE2, IGNORE

    LINE = []
    ALN = create_str_array(13)
    CHEXTR = create_str_array(NAPPL)
    MSG = create_str_array(5)
    IIRV = create_int_array(NAPPL)

    FEXIST = False

    # CONTROL = ControlType()
    ISWITCH = SwitchType()

#-----------------------------------------------------------------------

    LUNEXP = FILEX_P #16
    LUNLST = 17
    ERRKEY = 'IPEXP '
    BLANK = ' '
    FINDCH = '*TREAT'

    IPLTI = ' '
    EXPP = 0

    WTHSTR = ''
    LINEXP = 0
    ISECT = 0

    f56 = ff.FortranRecordReader("2I2,2(1X,I1),1X,A25,13I3")
    f55 = ff.FortranRecordReader("I3,I1,2(1X,I1),1X,A25,13I3")
#-----------------------------------------------------------------------
    FILELS = 'EXP.LST'
    NLOOP = 0
    if RUN == 1:
        EXPN   = 1
        EXPP   = 0
        TRTN   = 1
        TRTALL = 999
    else:
        EXPN   = EXPP

#     IF (RNMODE .EQ. 'I') THEN
#          OPEN (LUNLST, FILE = FILELS,STATUS = 'OLD',IOSTAT=ERRNUM)
#          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,0)
#
#          WRITE (*,200)
#          I = 0
#   300    CONTINUE
#          I = I + 1
#          LINF = 0
#   350    CONTINUE
#          CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
#          IF (ISECT .EQ. 2) GO TO 350
#
#          IF (ISECT .EQ. 1) THEN
#             READ (CHARTEST,410,IOSTAT=ERRNUM) ib1.EXPER,CG,ENAME
#             IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,LINEXP)
#             IF (MOD(I,16) .EQ. 0) THEN
#                WRITE (*,600)
#                READ (5,'(A1)') ANS
#             ENDIF
#             READ(ib1.EXPER(5:6),'(I2)') YR
#             IF (YR .GE. 10) THEN
#               WRITE (*,500) I,CG,ENAME(1:45),ib1.EXPER(1:2),ib1.EXPER(3:4),
#      &                    ib1.EXPER(5:6),ib1.EXPER(7:8)
#             ELSE
#               WRITE (*,501) I,CG,ENAME(1:45),ib1.EXPER(1:2),ib1.EXPER(3:4),
#      &                      ib1.EXPER(5:6),ib1.EXPER(7:8)
#             ENDIF
#
#           ELSE
#             GO TO 800
#          ENDIF
#
#          GO TO 300
#   800    CONTINUE
#          REWIND (LUNLST)
#
# C-----------------------------------------------------------------------
# C
# C-----------------------------------------------------------------------
#
#   850    CONTINUE
#          LINE(1) = ' '
#          NLOOP = NLOOP + 1
#          IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,2,FILELS,0)
#          WRITE (*,1000) EXPN
#          READ  (5,1100) LINE
#          CALL VERIFY (LINE,EXP,FLAG)
#
#          IF (EXP .LE. 0.0) THEN
#             EXP = EXPN
#           ELSEIF ((FLAG .GT. 0) .OR. (EXP .GT. (I-1))) THEN
#             WRITE (*,1101) (I-1)
#             GO TO 850
#           ELSEIF (EXP .NE. NINT(EXP)) THEN
#             WRITE (*,1102)
#             GO TO 850
#           ELSEIF (EXP .GT. 0.0) THEN
#             EXPN = NINT(EXP)
#           ELSE
#             CALL ERROR (ERRKEY,2,FILELS,0)
#          ENDIF
# C
# C     Establish the name of the experiment input file
# C
#         I = 0
#  950    CONTINUE
#         I = I + 1
#  975    CONTINUE
#         CALL IGNORE (LUNLST,LINF,ISECT,CHARTEST)
#         IF (ISECT .EQ. 2) GO TO 975
#         READ (CHARTEST,410,IOSTAT=ERRNUM) ib1.EXPER,CG,ENAME
#         IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILELS,LINEXP)
#         IF (I .LT. EXPN) GO TO 950
#         CLOSE (LUNLST)
#         FILEX(1:12) = ib1.EXPER//'.'//CG//'X'
#         FILEX_P(1:12) = FILEX
#       ELSE
#        READ(FILEX(10:11),'(A2)') CG
    sw.CG = FILEX[9:11]
#        READ(FILEX(1:8),'(A8)') ib1.EXPER
    ib1.EXPER = FILEX[0:8]

#      ENDIF

# VSH
    EXPNAME = ib1.EXPER
#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------

#      FILEA(1:12) = ib1.EXPER//'.'//CG//'A'
#      FILET(1:12) = ib1.EXPER//'.'//CG//'T'

    FILEA = ib1.EXPER + '.' + sw.CG + 'A'
    FILET = ib1.EXPER + '.' + sw.CG + 'T'


#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------

    NLOOP = 0
    I     = 0

# opens X-file and reads 1st line starting from position 25
      # OPEN (LUNEXP,FILE = FILEX_P,STATUS = 'OLD',IOSTAT=ERRNUM)
      # IF (ERRNUM .NE. 0) THEN
      #   MSG(1) = "File not found:"
      #   MSG(2) = FILEX_P
      #   CALL WARNING(2,ERRKEY,MSG)
      #   CALL ERROR (ERRKEY,29,FILEX_P,0)
      # ENDIF
      # READ(LUNEXP,1500) ENAME
    sw.ENAME = 'STRAWBERRY VARIETY TRIAL'
    CONTROL.ENAME = sw.ENAME

# 1500 FORMAT(25X,A60)
#      IF (RNMODE .EQ. 'I') CALL CLEAR
    if EXPN != EXPP:
        TRTN = 1
#      ENDIF

#       IF (RNMODE .EQ. 'I' .OR. RNMODE .EQ. 'A' .AND.
#      &   TRTALL .EQ. 999) THEN
#          IF (RNMODE .EQ. 'I') WRITE (*,2300) ENAME(1:40)
#          CALL FIND (LUNEXP,FINDCH,LINEXP,IFIND)
#          IF(IFIND .EQ. 0) CALL ERROR (ERRKEY,1,FILEX,LINEXP)
#  2400    CONTINUE
#          I = I + 1
#          CALL IGNORE (LUNEXP,LINEXP,ISECT,CHARTEST)
#          IF (ISECT .EQ. 1) THEN
#             READ (CHARTEST,54,IOSTAT=ERRNUM) (ALN(L),L=1,13)
#    54       FORMAT (34X,13A3)
#             DO L=1,13
#               ALLN = ALN(L)
#               IF (ALLN(3:3) .EQ. '?') THEN
#                  ERRNUM = 99
#                  CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
#               ENDIF
#             ENDDO
#             IF (RNMODE .EQ. 'Q') THEN
#               READ (CHARTEST,56,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,
#      &              TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
#      &              LNCHE,LNTIL,LNENV,LNHAR,LNSIM
#             ELSE
#               READ (CHARTEST,55,IOSTAT=ERRNUM)TRTNO,ROTNO,ROTOPT,CRPNO,
#      &              TITLET,LNCU,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
#      &              LNCHE,LNTIL,LNENV,LNHAR,LNSIM
#             ENDIF
#             IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEX,LINEXP)
#             IF (MOD(I,16) .EQ. 0 .AND. RNMODE .EQ. 'I') THEN
#                WRITE (*,600)
#                READ (5,'(A1)') ANS
#             ENDIF
#             READ(ib1.EXPER(5:6),'(I2)') YR
#             IF (YR .GE. 10) THEN
#             IF (RNMODE .EQ. 'I') WRITE (*,2600) I,TITLET,
#      &          ib1.EXPER(1:2),ib1.EXPER(3:4),ib1.EXPER(5:6),ib1.EXPER(7:8),TRTNO
#           ELSE
#               IF (RNMODE .EQ. 'I') WRITE (*,2601) I,TITLET,
#      &            ib1.EXPER(1:2),ib1.EXPER(3:4),ib1.EXPER(5:6),ib1.EXPER(7:8),TRTNO
#             ENDIF
#           ELSE
#             GO TO 2700
#          ENDIF
#          GO TO 2400
#  2700    CONTINUE
#          TRTALL = I - 1
#          IF (RNMODE .EQ. 'A') TRTN = 1
# C-GH     IF (RNMODE .EQ. 'I')
# C-GH     &   WRITE (*,2650) I,ib1.EXPER(1:2),ib1.EXPER(3:4),ib1.EXPER(5:6),ib1.EXPER(7:8)
#
# C-----------------------------------------------------------------------
# C
# C-----------------------------------------------------------------------
#  2750    CONTINUE
#          NLOOP = NLOOP + 1
#          LINE(1) = ' '
#          IF (NLOOP .GT. 25) CALL ERROR(ERRKEY,4,FILEX,LINEXP)
#          IF (RNMODE .EQ. 'I') THEN
#            WRITE (*,2900) TRTN
# C
# C        Read the correct treatment number
# C
#            READ (5,1100) LINE
#            CALL VERIFY (LINE,TRT,FLAG)
#          ENDIF
#          IF (TRT .LE. 0.0) THEN
#             TRT = TRTN
# C-GH      ELSEIF (TRT .EQ. (TRTALL+1) .AND. RNMODE .EQ. 'I') THEN
# C-GH        RNMODE = 'A'
# C-GH        TRTN   = 1
#           ELSEIF ((FLAG .GT. 0) .OR. (TRT .GT. I)) THEN
#             WRITE (*,2751) (I-1)
#             GO TO 2750
#           ELSEIF (TRT .NE. NINT(TRT)) THEN
#             WRITE(*,2752)
#             GO TO 2750
#           ELSEIF (TRT .GT. 0.) THEN
#             TRTN = NINT(TRT)
#           ELSE
#             CALL ERROR (ERRKEY,4,FILEX,LINEXP)
#          ENDIF
#        ELSEIF (INDEX ('Q',RNMODE) .GT. 0) THEN
#          !READ (TRNARG(1:6),'(I6)') TRTN
#          !READ (ROTNARG(1:6),'(I6)') ROTN
#          TRTN = TRTNUM
#          ROTN = ROTNUM
#          I = 999
#        ELSEIF (INDEX ('NQGSFBECTY',RNMODE) .GT. 0) THEN
# !         READ (TRNARG(1:6),'(I6)') TRTN
    TRTN = TRTNUM
    I = 999
      #  ELSEIF (INDEX ('A',RNMODE) .GT. 0) THEN
      #    TRTN = TRTN + 1
      # ENDIF

#-----------------------------------------------------------------------
#     Find treatment number and appropriate levels
#-----------------------------------------------------------------------
    # REWIND (LUNEXP)
    LINEXP,IFIND = FIND(LUNEXP,FINDCH)
# Looks for *TREAT section
    if IFIND == 0: ERROR (ERRKEY,1,FILEX,LINEXP)

#    Read @Line, check that 13th column = "SM"
    LINEXP,ISECT,CHARTEST = IGNORE2(LUNEXP,LINEXP)
#    finds sect 3 TREAT
#    CHARTEST contains column headers
#    CHARTEST = '@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM'
    SimLevel = True
    if ISECT == 3 and CHARTEST[71:73] != 'SM': SimLevel = False

    I = 0
    while True:
        I = I + 1
        LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP,LINEXP)
        # finds column values in CHARTEST
        #     CHARTEST = '1 1 1 0 Radiance                   1  1  0  0  1  0  0  0  0  0  0  1  1'
        if RNMODE == 'Q':
            try:
                (ib2.TRTNO, ib2.ROTNO, ib2.ROTOPT, ib2.CRPNO,
                 TITLET, LNCU, LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,LNCHE,
                LNTIL,LNENV,LNHAR,LNSIM) = f56.read(CHARTEST)
            except Exception as ERRNUM:
                ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)
        else:
            try:
                (ib2.TRTNO, ib2.ROTNO, ib2.ROTOPT, ib2.CRPNO,
                 TITLET, LNCU, LNFLD, LNSA, LNIC, LNPLT, LNIR, LNFER, LNRES, LNCHE,
                 LNTIL, LNENV, LNHAR, LNSIM) = f55.read(CHARTEST)
            except Exception as ERRNUM:
                ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

# VSH This can make to go 50
        if not (('BEDNSGFCTY'.find(RNMODE) >= 0 and TRTN != ib2.TRTNO) or \
            ('Q'.find(RNMODE) >= 0 and (TRTN != ib2.TRTNO )) or \
            ('AI'.find(RNMODE) >= 0 and I < TRTN)):
            break

    #Generate header information for Warnings or Errors in input module
    #(Updates global Headers object declared in ModuleDefs)
    OPHEAD(RunConstants.RUNINIT,99,0.0,0.0,"                ",0.0,0.0,
        "      ", RUN,"        ", TITLET, WTHSTR, RNMODE,
        CONTROL, ISWITCH, UseSimCtr, PATHEX)
#-----------------------------------------------------------------------
#     Call MAKEFILEW to read FILEX and
#-----------------------------------------------------------------------
    MAKEFILEW(LUNEXP,sw.DSSATP,PATHEX,FILEX,SimLevel,LNSIM,LNPLT,LNFLD)
#-----------------------------------------------------------------------
#     Call input section for cultivar selection
#-----------------------------------------------------------------------
#     REWIND (LUNEXP) VSH address this

    CROP, VARNO = IPCUL (LUNEXP,FILEX,LNCU)
    if CROP  == '  ' : ERROR(ERRKEY,10,FILEX,LINEXP)
    if VARNO == '  ' : ERROR (ERRKEY,11,FILEX,LINEXP)
    CONTROL.CROP = CROP

    (ib1.PLME, ib1.PLDS, ib3.ROWSPC, ib3.AZIR, ib3.SDEPTH, ib3.SDWTPL, ib3.PLTPOP, ib3.PLANTS, ib3.SDAGE,
     ib3.ATEMP, ib3.PLPH, ib2.IEMRG, ib2.YRPLT, ib3.SPRLAP, NFORC, PLTFOR, NDOF, PMTYPE) = (
        IPPLNT_Inp(LUNEXP,FILEX,CROP,LNPLT,IPLTI))

    (ib1.TITSIM, NYRS, ib2.NREPSQ, ib1.ISIMI, ib2.PWDINF, ib2.PWDINL, ib3.SWPLTL, ib1.NCODE, ib3.SWPLTH,
     ib3.SWPLTD, ib2.YEAR, ib3.PTX, ib3.PTTN, ib3.DSOIL, ib3.THETAC, ib3.IEPT, ib1.IOFF, ib1.IAME, ib3.DSOILN,
     ib3.SOILNC, ib2.YRSIM, ib3.SOILNX, ib1.NEND, ib3.RIP, ib2.NRESDL, ib3.DRESMG, ib2.HDLAY, ib2.HLATE,
     ib3.HPP, ib3.HRP, FTYPEN, ib2.RSEED1, ib3.AIRAMT, ib3.EFFIRR, FROP, MODEL, CONTROL, ISWITCH) \
            = IPSIM(LUNEXP, LNSIM, SimLevel, RUN, LINEXP, CROP, RNMODE, FILEX, CONTROL, UseSimCtr, MODELARG, ib2.YRPLT)

#-----------------------------------------------------------------------
#        Select crop parameter input file
#-----------------------------------------------------------------------

    if CROP != 'FA':
        sw.FILEC = CROP + MODEL[2:8] + '.SPE'
        FEXIST = INQUIRE (sw.FILEC)
        if not FEXIST:
            sw.PATHCR, NAMEF = PATH('CRD',sw.DSSATP,1)
        else:
            sw.PATHCR = BLANK
#-----------------------------------------------------------------------
#       Select genetic parameter input file
#       READ GENCALC2.CUL if RNMODE = G & T
#       READ ???????0.CUL for other modes
#-----------------------------------------------------------------------
        sw.FILEG = CROP + MODEL[2:8] + '.CUL'
        FEXIST = INQUIRE (sw.FILEG)
        if not FEXIST :
            FILETMP = PATHEX.strip() + sw.FILEG
            FEXIST = INQUIRE (FILETMP)
            if not FEXIST:
                sw.PATHGE, NAMEF = PATH('CRD',sw.DSSATP,1)
            else:
                sw.PATHGE = PATHEX.strip()
        else:
            sw.PATHGE = BLANK
#-----------------------------------------------------------------------
#       Select ecotype parameter input file
#       READ GENCALC2.ECO if RNMODE = G & T
#       READ ???????0.ECO for other modes;
#-----------------------------------------------------------------------
        sw.FILEE = CROP + MODEL[2:8] + '.ECO'
        FEXIST = INQUIRE (sw.FILEE)
        if not FEXIST :
            FILETMP = PATHEX.strip() + sw.FILEE
            FEXIST = INQUIRE (FILETMP)
            if not FEXIST :
                sw.PATHEC, NAMEF = PATH ('CRD',sw.DSSATP,1)
            else:
                sw.PATHEC = PATHEX.strip()
        else:
            sw.PATHEC = BLANK
#-----------------------------------------------------------------------
#       Select pest parameter input file
#-----------------------------------------------------------------------
        sw.FILEP = CROP + MODEL[2:8] + '.PST'
        FEXIST = INQUIRE (sw.FILEP)
        if not FEXIST:
            sw.PATHPE, NAMEF = PATH('PSD',sw.DSSATP,1)
        else:
            sw.PATHPE = BLANK
#-----------------------------------------------------------------------
#        End of IF NOT fallow
#-----------------------------------------------------------------------

    ib1.CROPD = GET_CROPD(CROP)
    #LUNEXP.seek(0)

#     Regen short headers now that MODEL is known.
    OPHEAD (RunConstants.RUNINIT,99,0.0,0.0,"                ",0.0,0.0,
    "      ",RUN, MODEL,TITLET,WTHSTR, RNMODE,
    CONTROL, ISWITCH, UseSimCtr, PATHEX)

#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
#     Skip soils field and soils input for sequence mode
    if 'FQ'.find(RNMODE) <= 0 or RUN == 1 :
        (ib1.FLDNAM, ib1.WSTA, WSTA1, SLNO, ib1.SLTX, ib1.FLST, ib3.SLOPE, ib1.DFDRN, ib3.FLDD,
         ib3.SFDRN, ib3.FLOB, ib4.SLDP, PMWD, ib3.XCRD, ib3.YCRD, ib3.ELEV, ib3.AREA, ib3.SLEN,
         ib3.FLWR, ib3.SLAS, ib1.FldHist, ib2.FHDur, ib4.PMALB) = IPFLD(LUNEXP,FILEX,LNFLD)

#-----------------------------------------------------------------------
#     Select soil profile input file
#       1. SOIL.SOL
#       2. ??.SOL where ?? = Institute ID from Soil Profile Number
#       3. From C:\DSSAT45\DSSATPRO.V45  SOIL.SOL
#       4. From C:\DSSAT45\DSSATPRO.V45  ??.SOL
#-----------------------------------------------------------------------
        FILES_a = 'SOIL.SOL'
        FILES_b = SLNO[:2] + '.SOL  '

        FEXIST = INQUIRE (FILE = FILES_a)
        if FEXIST:
#           SOIL.SOL in current directory
            sw.FILES = FILES_a
            sw.PATHSL = BLANK
        else:
            FEXIST = INQUIRE (FILE = FILES_b)
            if FEXIST:
#               Alt soil name in current directory
                sw.FILES = FILES_b
                sw.PATHSL = BLANK
            else:
                FILETMP = PATHEX.strip() + FILES_a
                FEXIST = INQUIRE (FILE = FILETMP)
                if FEXIST:
#                   SOIL.SOL in experiment directory
                    sw.FILES = FILES_a
                    sw.PATHSL = PATHEX.strip()
                else:
                    FILETMP = PATHEX.strip() + FILES_b
                    FEXIST = INQUIRE (FILE = FILETMP)
                    if FEXIST:
#                   Alt soil name in experiment directory
                        sw.FILES = FILES_b
                        sw.PATHSL = PATHEX.strip()
                    else:
                        PROCOD = 'SLD'
                        sw.PATHSL,NAMEF = PATH(PROCOD,CONTROL.DSSATP,1)
                        PATHL  = sw.PATHSL.find(BLANK)
                        FILETMP = sw.PATHSL[:PATHL] + FILES_a
                        FEXIST = INQUIRE (FILE = FILETMP)
                        if FEXIST:
#                       SOIL.SOL in DSSAT soil directory
                            sw.FILES = FILES_a
                        else:
                            FILETMP = sw.PATHSL[:PATHL] + FILES_b
                            FEXIST = INQUIRE (FILE = FILETMP)
                            if FEXIST:
#                           Alt soil name in DSSAT soil directory
                                sw.FILES = FILES_b
                            else:
#                         No valid soils file found
                                MSG[1] = f"Soil files not found: {FILES_a}, {FILES_b}"
                                MSG[2] = "Searched current directory and the following:"
                                MSG[3] = PATHEX[0:76]
                                MSG[4] = sw.PATHSL[0:76]
                                WARNING(4,ERRKEY,MSG)
                                ERROR(ERRKEY,80,FILES_a,0)
#-----------------------------------------------------------------------
#
#-----------------------------------------------------------------------
    if ib1.ISIMI == 'S' :
        if ib2.YRSIM < 0 : ib2.YRSIM = ib1.YRPLT
    elif ib1.ISIMI == 'P' :
        ib2.YRSIM = ib1.YRPLT
    elif ib1.ISIMI == 'E' :
        ib2.YRSIM = ib2.IEMRG
        ib1.YRPLT = ib2.IEMRG
    ib2.YEAR, ISIM = YR_DOY (ib2.YRSIM)
    CONTROL.YRSIM = ib2.YRSIM

#-----------------------------------------------------------------------
#    RNMODE = 'Y' indicates yield forecast mode. May need multiple
#     weather files.
#     If RNMODE = 'Y' and MEWTH = 'G','W','S', then also need a WTH file for
#     forecast year weather data.
#-----------------------------------------------------------------------
    # Generated weather data files
    if sw.MEWTH == 'G' :
        if WSTA1[3] == BLANK :
            if ib2.YEAR < 2000 :
                YR = ib2.YEAR - 1900
            elif ib2.YEAR < 3000 :
                YR = ib2.YEAR - 2000
            sw.FILEWG = ib1.WSTA + str(YR) + '01.WTG'
        else:
            sw.FILEWG = ib1.WSTA + WSTA1 + '.WTG'
        PROCODG = 'WGD'
    # Interactively generated weather
    if sw.MEWTH == 'S' or sw.MEWTH == 'W' :
        sw.FILEWC = ib1.WSTA + '.CLI    '
        PROCODC = 'CLD'
    # Measured weather data
    if sw.MEWTH == 'M' or RNMODE == 'Y' :
        if WSTA1[3] == BLANK :
            if ib2.YEAR < 2000 :
                YR = ib2.YEAR - 1900
            elif ib2.YEAR < 3000 :
                YR = ib2.YEAR - 2000
            sw.FILEW = ib1.WSTA + str(YR) + '01.WTH'
        else:
            sw.FILEW = ib1.WSTA + WSTA1 + '.WTH'
        PROCODW = 'WED'
    # wrong condition in original VSH
    # if 'GSWM'.find(RNMODE) < 0: ERROR (ERRKEY,22,FILEX,LINEXP)

    # Check for existing FILEW, FILEWC, and FILEWG
    for I in range(1,4): #TODO why for loop here?
        if I == 1:
            if sw.MEWTH == 'M' or RNMODE == 'Y' :
                FILE_CHECK = sw.FILEW
                PROCOD = PROCODW
            else:
                continue
        if I == 2:
            if sw.MEWTH == 'G' :
              FILE_CHECK = sw.FILEWG
              PROCOD = PROCODG
            else:
              continue
        if I == 3:
            if sw.MEWTH == 'S' or sw.MEWTH == 'W' :
              FILE_CHECK = sw.FILEWC
              PROCOD = PROCODC
            else:
                continue

    # Check weather filename in current directory
        FEXIST = INQUIRE (FILE_CHECK)
        if FEXIST :
            PATHWT = BLANK
        # Check weather filename in data directory
        else:
            FILETMP = PATHEX.strip() + FILE_CHECK
            FEXIST = INQUIRE (FILETMP)
            if FEXIST :
                PATHWT = PATHEX.strip()
            # Check weather filename in default DSSAT directory
            else:
                PATHWT, NAMEF = PATH(PROCOD,sw.DSSATP,1)
                FILETMP = PATHWT.strip() +  FILE_CHECK
                FEXIST = INQUIRE (FILETMP)
                if FEXIST :
                    PATHWT = PATHWT
                # Check 4-character file name in data directory
                else:
                    FILEW4 = FILE_CHECK[0:4] + ".WTH"
                    FILETMP = PATHEX.strip() + FILEW4
                    FEXIST = INQUIRE (FILETMP)
                    if FEXIST :
                        PATHWT = PATHEX.strip()
                        FILE_CHECK = FILEW4
                # Check 4-character filename in default DSSAT directory
                    else:
                        FILETMP = PATHWT.strip() + FILE_CHECK
                        FEXIST = INQUIRE (FILETMP)
                        if FEXIST :
                            PATHWT = PATHWT
                            FILE_CHECK = FILEW4
                        else:
                            MSG[0] = "Weather file not found."
                            MSG[1] = "  Neither " + FILE_CHECK + " nor " + FILEW4
                            MSG[2] = "  were found in weather or experiment directories."
                            MSG[3] = "Simulation will end."
                            CONTROL.ErrCode = 29
                            Put_CONTROL(CONTROL)
                            WARNING(4,ERRKEY,MSG)

        if I == 1:
            sw.FILEW  = FILE_CHECK
            sw.PATHWTW = PATHWT
        if I == 2:
            sw.FILEWG = FILE_CHECK
            sw.PATHWTG = PATHWT
        if I == 3:
            sw.FILEWC = FILE_CHECK
            sw.PATHWTC = PATHWT
#-----------------------------------------------------------------------
#     Build output files.
#
#     Generic output file names with extension 'OUT' are overwritten
#     at the start of each simulation.
#
#     IOX = 'Y' creates experiment specific output file names
#-----------------------------------------------------------------------
    if sw.IOX == 'Y' :
        OUTO = ib1.EXPER + '.' + sw.CG + 'O'
    else:
        OUTO  = 'OVERVIEW.OUT'
#-----------------------------------------------------------------------
#     Call IPENV
#-----------------------------------------------------------------------
    ib3.CO2ADJ, ib1.CO2FAC, ib3.DAYADJ, ib1.DAYFAC, ib3.DPTADJ, ib1.DPTFAC, ib2.NEV, ib3.PRCADJ, ib1.PRCFAC, \
    ib3.RADADJ, ib1.RADFAC, ib3.TMADJ, ib1.TMFAC, ib3.TXADJ, ib1.TXFAC, ib2.WMDATE, WMODI, ib3.WNDADJ, \
    ib1.WNDFAC, ib4.WTHADJ  =  IPENV(FILEX,LNENV,LUNEXP)
#-----------------------------------------------------------------------
#     Call IPHAR
#-----------------------------------------------------------------------
    # HDATE, HSTG, HCOM, HSIZ, HPC, NHAR, IHARI, YRSIM, CROP, \
    # HBPC = IPHAR (LUNEXP,FILEX,LNHAR, sw.IHARI, ib2.YRSIM, CROP)
    ib2.HDATE, ib1.HSTG, ib1.HCOM, ib1.HSIZ, ib3.HPC, ib3.HBPC, ib2.NHAR = (
        IPHAR (LUNEXP,FILEX, LNHAR, sw.IHARI, ib2.YRSIM, CROP))
#-----------------------------------------------------------------------
#     Call IPIRR
#-----------------------------------------------------------------------
    sw.ISWWAT, ib2.NIRR, ib3.EFFIRX, ib3.DSOILX, ib3.THETCX, ib3.IEPTX, ib1.IOFFX, ib1.IAMEX,\
    ib2.NAPW, ib4.TOTAPW, ib3.AIRAMX, ib2.IDLAPL, ib1.IRRCOD, ib3.AMT, IIRV, sw.IIRRI =(
    IPIRR (LUNEXP,FILEX,LNIR,ib2.YRSIM,sw.ISWWAT,
        ib2.NIRR, ib3.EFFIRX, ib3.DSOILX, ib3.THETCX, ib3.IEPTX, ib1.IOFFX, ib1.IAMEX, LNSIM,
        ib2.NAPW,ib4.TOTAPW,ib3.AIRAMX,ib2.IDLAPL,ib1.IRRCOD,ib3.AMT,IIRV,sw.IIRRI))

#-----------------------------------------------------------------------
#     Call IPFERT
#-----------------------------------------------------------------------
    #sw.IFERI
    ib2.NFERT, ib2.FDAY, ib1.IFTYPE, ib1.FERCOD, ib3.DFERT, ib3.ANFER, ib3.APFER, ib3.AKFER, ib3.ACFER,\
    ib3.AOFER, ib1.FOCOD, ib4.TOTNAP  = (
    IPFERT (LUNEXP,FILEX,LNFER,ib2.YRSIM,sw.ISWNIT,sw.ISWPHO,sw.ISWPOT, sw.ISWWAT ,LNSIM)
    )

#-----------------------------------------------------------------------
#     Call IPRES
#-----------------------------------------------------------------------
    # IPRES (LUNEXP,FILEX,LNRES,RESDAY,RESCOD,RESIDUE,
    #     RINP,DEPRES,RESN,RESP,RESK,NARES,RESAMT,ISWNIT,YRSIM,
    #     ISWPHO,ISWPOT,IRESI,ISWWAT,RMET,LNSIM)

#-----------------------------------------------------------------------
#     Call IPCHEM - Chemical applications
#-----------------------------------------------------------------------
    # IPCHEM (LUNEXP,FILEX,LNCHE,YRSIM,NCHEM,CDATE,
    #     CHCOD,CHAMT,CHMET,CHDEP,CHT,ISWCHE,CHEXTR)

#-----------------------------------------------------------------------
#     Call IPTILL - Tillage operations
#-----------------------------------------------------------------------
    # IPTILL (LUNEXP,FILEX,LNTIL,YRSIM,ISWTIL,NTIL,TDATE,
    #     TIMPL,TDEP,LNSIM)
    #
    #   CLOSE(LUNEXP)
    #   RETURN

# C-----------------------------------------------------------------------
# C     FORMAT Strings
# C-----------------------------------------------------------------------
#
#    55 FORMAT (I3,I1,2(1X,I1),1X,A25,14I3)
#    56 FORMAT (2I2,2(1X,I1),1X,A25,14I3)
#
#    75 FORMAT (A4,I2.2,A6)
#    76 FORMAT (3A4)
#    77 FORMAT (A4,A8)
#    80 FORMAT (A8,A1,A2,A1)
#   200 FORMAT (T57,'INST.',T64,'SITE',T70,'YEAR',T75,'EXPT.',
#      &  /,T7,'CROP ib1.EXPERIMENTAL CASE STUDIES',T58,'ID',T65,'ID',
#      &  T76,'NO',/T7,4('-'),1X,31('-'),T57,'----',
#      &    T64,'----',T70,'----',T75,'----')
#   410 FORMAT (3X,A8,1X,A2,2X,A60)
#   500 FORMAT (1X,I3,'.',2X,A2,2X,A45,1X,A2,5X,A2,3X,'19',A2,2X,A2)
#   501 FORMAT (1X,I3,'.',2X,A2,2X,A45,1X,A2,5X,A2,3X,'20',A2,2X,A2)
#   600 FORMAT (/,'  More.... press < ENTER > key',$)
#  1000 FORMAT (/,6X,'ib1.EXPERIMENT SELECTED ===>',1X,I3,
#      &        /,6X,'NEW SELECTION ?     --->',2X,' ',$)
#  1100 FORMAT (80A1)
#  1101 FORMAT (10X,'ERROR! Experiment Selection must be between 1',
#      &            ' and ',I3,/)
#  1102 FORMAT (10X,'ERROR! Experiment Selection must be an',
#      &            ' INTEGER value',/)
#  2300 FORMAT (T47,'INST.',T54,'SITE',T60,'YEAR',T66,'EXPT.',
#      &  T72,'TRT.',/,T7,A40,T48,'ID',T55,'ID',T67,'NO',
#      &  T73,'NO', /,T7,37('-'),T47,'----',
#      &  T54,'----',T60,'----',T66,'----',T72,'----')
#  2600 FORMAT (1X,I3,'.',1X,A25,16X,A2,5X,A2,3X,'19',A2,3X,A2,3X,I3)
#  2601 FORMAT (1X,I3,'.',1X,A25,16X,A2,5X,A2,3X,'20',A2,3X,A2,3X,I3)
#  2650 FORMAT (1X,I3,'.',1X,'RUN ALL TREATMENTS',23X,
#      &        A2,5X,A2,3X,'19',A2,3X,A2,4X,I2)
#  2751 FORMAT (10X,'ERROR! Treatment Selection must be between 1',
#      &            ' and ',I3,/)
#  2752 FORMAT (10X,'ERROR! Treatment Selection must be an INTEGER',/)
#  2900 FORMAT (/,6X,'TREATMENT SELECTED ===>',1X,I3,
#      &        /,6X,'NEW SELECTION ?    --->',2X,' ',$)
#  3450 FORMAT (//////,15X,' Reading Data.  Please be patient.',/,
#      &               15X,' Do not touch the keyboard !',/,16X,33('='))
#  5000 FORMAT("Soil files not found: ",A,", ",A)
#  5010 FORMAT("Searched current directory and the following:")
#  5020 FORMAT(2X,A76)
#  5030 FORMAT(2X,A76)

#      END SUBROUTINE IPEXP

    return (MODEL, SLNO, NYRS,VARNO,CROP, WMODI, FROP,TRTN,EXPP,EXPN,TITLET,TRTALL,
            IIRV, FTYPEN,CHEXTR,NFORC,PLTFOR, NDOF, PMTYPE,LNSIM,LNCU,LNHAR,
            LNENV,LNTIL,LNCHE,LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,CONTROL,ISWITCH,PMWD)

#=======================================================================
#  IPPLNT, Subroutine
#  Reads parameters related to planting operation from FILEX file
#-----------------------------------------------------------------------
#  INPUT
#       LUNEXP-
#       FILEX -
#       LNPLT -
#       CROP -

#  OUTPUT : PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
#           YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE,
#=======================================================================
def IPPLNT_Inp(LUNEXP,FILEX,CROP,LNPLT,IPLTI):
    import fortranformat as ff
    from ERROR import ERROR
    from READS import FIND, IGNORE, strToType
    from DATES import Y4K_DOY, YR_DOY

    ERRKEY = 'IPPLNT'
    FINDCH = '*PLANT'
    f60 = ff.FortranRecordReader("I3,I5,1X,I5,2(F6.0),2(5X,A1),8(1X,F5.0),I6,F6.0,2I6")

    if LNPLT > 0:
        LINEXP, IFIND = FIND(LUNEXP,FINDCH)
        if not IFIND:
             ERROR(ERRKEY, 1, FILEX, LINEXP)

        #Read lines in Planting section, until target treatment # LNPLT is found
        while True:
            LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
            if ISECT == 1:
                try:
                    LN,YRPLT,IEMRG,PLANTS,PLTPOP,PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL, \
                    SDAGE,ATEMP,PLPH,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE = f60.read(CHARTEST)
                except Exception as ERRNUM: #Exception structure for python instead of error number
                    ERROR(ERRKEY, ERRNUM, FILEX, LINEXP)

                if LN == LNPLT:
                    YRPLT = Y4K_DOY(YRPLT,FILEX,LINEXP,ERRKEY,10)
                    IEMRG = Y4K_DOY(IEMRG,FILEX,LINEXP,ERRKEY,15)
                    YR, IPLT = YR_DOY(YRPLT) #Neither YR or IPLT are used in the subroutine
                    break
            else:
                ERROR(ERRKEY, 2, FILEX, LINEXP)
        if IPLTI == 'R':
            if (YRPLT < 1 or YRPLT > 9999999) and IEMRG < 1:
                ERROR(ERRKEY, 10, FILEX, LINEXP)

        if PLTPOP <= 0.0 and PLANTS > 0.0:
            PLTPOP = PLANTS

        if PLTPOP <= 0.0:
            if CROP != 'SC':
                ERROR(ERRKEY, 11, FILEX, LINEXP)

        if ROWSPC <= 0.0:
            # if MODEL[:5] == 'SCCAN':
            #     ROWSPC = 0.0
            # else:
            ROWSPC = 1.0 / (PLTPOP ** 0.5) * 100.0
            MSG = ["",""]
            MSG[1] = "Missing row spacing in experiment file."
            MSG[2] = f"Value set to {ROWSPC:.1f} cm"
            #err.WARNING(2, ERRKEY, MSG)

        if (-90 < AZIR < 0.0) or AZIR > 99999.0:
            ERROR(ERRKEY, 13, FILEX, LINEXP)

        if SDEPTH <= 0.0 or SDEPTH > 100.0:
            if CROP != 'SC':
                ERROR(ERRKEY, 14, FILEX, LINEXP)

        if 'PT' in CROP:
            if SPRLAP <= 0.0:
                ERROR(ERRKEY, 16, FILEX, LINEXP)
            if SDWTPL <= 0.0:
                ERROR(ERRKEY, 17, FILEX, LINEXP)

        if not any(c in PLME for c in 'TSPNRCBHIV'):
            ERROR(ERRKEY, 19, FILEX, LINEXP)

        if 'CS' in CROP:
            if not any(c in PLME for c in 'VHI'):
                ERROR(ERRKEY, 19, FILEX, LINEXP)

        if 'RI' not in CROP:
            if 'T' in PLME:
                if SDWTPL < 0.0:
                    ERROR(ERRKEY, 18, FILEX, LINEXP)

    return (PLME,PLDS,ROWSPC,AZIR,SDEPTH,SDWTPL,PLTPOP,PLANTS,SDAGE,ATEMP,PLPH,IEMRG,
            YRPLT,SPRLAP,NFORC,PLTFOR,NDOF,PMTYPE)

#=======================================================================
#  IPFLD, Subroutine
#  Reads parameters related to field operation from FILEX file
#-----------------------------------------------------------------------
#  INPUT  : LUNEXP,FILEX,LNFLD
#  LOCAL  : LN
#  OUTPUT :
#-----------------------------------------------------------------------
#  Called : IPEXP
#  Calls  : ERROR IGNORE FIND YR_DOY
#=======================================================================
def IPFLD (LUNEXP,FILEX,LNFLD):
    from ERROR import ERROR
    from READS import FIND, IGNORE, HFIND
    from WARNING import WARNING
    from OPSUM import SUMVALS
    from fortranformat import FortranRecordReader
    from ModuleDefs import PUT_Char

    MSG = []
    CKELEV = True
    LN = -99

    # Arrays which contain data for printing in SUMMARY.OUT file
    SUMNUM = 3
    LABEL = create_str_array(3)
    VALUE = create_float_array(SUMNUM)

    ERRKEY='IPFLD '
    FINDCH='*FIELD'

    LINEXP = 0
    # REWIND (LUNEXP)  VSH to be addressed
    LINEXP, IFIND = FIND(LUNEXP,FINDCH)
    if IFIND == 0 : ERROR(ERRKEY,1,FILEX,LINEXP)
    while LN != LNFLD :
        LINEXP, ISECT, CHARTEST = IGNORE(LUNEXP, LINEXP)
        if ISECT == 1:
            f60 = FortranRecordReader('I3,A8,1X,2A4,1X,F5.0,1X,F5.0,1X,A5,2(1X,F5.0),2(1X,A5),1X,F5.0,1X,A10')
            LN, FLDNAM, WSTA, WSTA1, SLOPE, FLOB, DFDRN, FLDD, SFDRN, FLST, SLTX, \
                SLDP, SLNO = f60.read(CHARTEST)
            #HZ: Removed FLNAME and added WSTA1 to match original call. Changed from line_to_var to using FortranFromat
        else:
            ERROR(ERRKEY,2,FILEX,LINEXP)

    WSTA = (WSTA.strip()).upper()

    if WSTA == '-99' and SLNO == '-99' :
        LUNEXP.close()
        exit()

    if WSTA == ' ' : ERROR(ERRKEY,10,FILEX,LINEXP)
    if SLNO == ' ' : ERROR(ERRKEY,11,FILEX,LINEXP)
    if SLOPE < 0.0 : SLOPE = 0.0
    if SFDRN  < 0.0 : SFDRN = 100.
    # New section
    # Find header and read second line of field information
    HFNDCH='SLAS'
    LINEXP,IFIND = HFIND(LUNEXP,HFNDCH,LINEXP)
    if IFIND == 1 :
        while True: #HZ: Changed to do while loop
            LINEXP,ISECT,CHARTEST = IGNORE (LUNEXP,LINEXP)
            if ISECT == 1 :
                f80 = FortranRecordReader('I3,2(A15,1X),A9,1X,F17.0,3(1X,F5.0),1X,A5,I6')
                LN,CXCRD,CYCRD,CELEV,AREA,SLEN,FLWR,SLAS, FldHist, \
                FHDUR = f80.read(CHARTEST)
            else:
                ERROR (ERRKEY,2,FILEX,LINEXP)
            if LN == LNFLD:
                break

    if AREA <= 0.0 : AREA = 1.0
    if FLWR <= 0.0 : FLWR = 1.0
    if SLEN <= 0.0 : SLEN = math.sqrt(AREA*FLWR*10000.0)

    # FO - Store Summary.out labels and values in arrays to send to
    #     OPSUM routines for printing.  Integers are temporarily
    #     saved as real numbers for placement in real array.

    try :
        XCRD = float(CXCRD)
    except ValueError as e:
        XCRD = -999.0
        MSG[1] = 'Error reading latitude from experimental file'
        MSG[2] = FILEX
        WARNING(2, ERRKEY, MSG)

    try :
        YCRD = float(CYCRD)
    except ValueError as e:
        YCRD = -99.0
        MSG[0] = 'Error reading longitude from experimental file'
        MSG[1] = FILEX
        WARNING(2, ERRKEY, MSG)

    try:
        ELEV = float(CELEV)
    except ValueError as e:
        ELEV = -99.0
        MSG[0] = 'Error reading elevation from experimental file'
        MSG[1] = FILEX
        WARNING(2, ERRKEY, MSG)

    if YCRD >= -90.0 and YCRD <= 90.0 and XCRD >= -180.0 and XCRD <= 180.0 and \
        len(CYCRD) > 0 and len(CXCRD) > 0 and (math.fabs(YCRD) > 1.E-15 or math.fabs(XCRD) > 1.E-15) :
        # Transfer data to the modules
        PUT_Char('FIELD','CYCRD',CYCRD)
        PUT_Char('FIELD','CXCRD',CXCRD)
        LABEL[0] = 'YCRD'; VALUE[0] = YCRD
        LABEL[1] = 'XCRD'; VALUE[1] = XCRD
        CKELEV = True
    else:
        # Transfer data to the modules
        PUT_Char('FIELD','CYCRD','            -99')
        PUT_Char('FIELD','CXCRD','            -99')
        LABEL[0] = 'YCRD'; VALUE[0] = -99.0
        LABEL[1] = 'XCRD'; VALUE[1] = -999.0
        CKELEV = False

    # Check elevation (CKELEV) based on latitude and longitude
    if CKELEV == True :
        if ELEV > -99.0 and len(CELEV) > 0.0 :
            PUT_Char('FIELD','CELEV',CELEV)
            LABEL[2] = 'ELEV'; VALUE[2] = ELEV
        else:
            PUT_Char('FIELD','CELEV','      -99')
            LABEL[2] = 'ELEV'; VALUE[2] = -99.0
    else:
        if ELEV > -99.0 and len(CELEV) > 0.0 and math.fabs(ELEV) > 1.E-15 :
            PUT_Char('FIELD','CELEV',CELEV)
            LABEL[2] = 'ELEV'; VALUE[2] = ELEV
        else:
            PUT_Char('FIELD','CELEV','      -99')
            LABEL[2] = 'ELEV'; VALUE[2] = -99.0

    # Send labels and values to OPSUM
    SUMVALS(SUMNUM, LABEL, VALUE)
    #
    # End New section

    # New section (3rd)
    # Find header and read second line of field information
    HFNDCH='PMALB'
    LINEXP,IFIND = HFIND(LUNEXP,HFNDCH,LINEXP)
    if IFIND == 1 :
        while LN != LNFLD :
            LINEXP,ISECT,CHARTEST = IGNORE (LUNEXP,LINEXP)
            if ISECT == 1 :
                f90 = FortranRecordReader("I3, 4F6.0")
                LN, PMALB, PMWD = f90.read(CHARTEST)

    else:
        PMWD = -99.0
        PMALB = -99.0
    if PMWD <= 0.0 : PMWD = -99.0
    if PMALB >= 0.0 : PMALB = -99.0

    return FLDNAM, WSTA, WSTA1,SLNO,SLTX,FLST,SLOPE,DFDRN,FLDD,SFDRN,FLOB,SLDP, \
            PMWD,XCRD,YCRD,ELEV,AREA,SLEN,FLWR,SLAS,FldHist,FHDUR,PMALB
#=======================================================================

# Test driver for IPFLD
# LUNEXP = "C://DSSAT48//Strawberry//UFBA1701.SRX" #16
# FILEX = 'UFBA1701.SRX'
# LNFLD = 1
#
# FLDNAM, WSTA, WSTA1, SLNO, SLTX, FLST, SLOPE, DFDRN, FLDD, SFDRN, FLOB, SLDP, PMWD, \
#         XCRD, YCRD, ELEV, AREA, SLEN, FLWR, SLAS, FldHist, FHDur, PMALB = IPFLD(LUNEXP,FILEX,LNFLD)
#
# print(IPFLD (LUNEXP,FILEX,LNFLD))


# Test driver for IPPLNT_Inp
# LUNEXP = 'C:/DSSAT48/Strawberry/UFBA1701.SRX'
# FILEX = 'UFBA1701.SRX'
# LNPLT = 1
# CROP = 'SR'
# IPLTI =' '
# print(IPPLNT_Inp(LUNEXP,FILEX,LNPLT,CROP,IPLTI))