#  Reads FileX, includes sensitivity analysis and writes a
#  temporary output file for input by the crop models
#  Calls  : ERROR CLEAR INTRO IPEXP IPSOIL IPVAR IPECO IPSLIN IPSLAN
#           SENS INSOIL WEATHR

def INPUT_SUB(FILEIO, FILEX, MODELARG, PATHEX,
              RNMODE, ROTNUM, RUN, TRTNUM, CONTROL):

#      EXTERNAL ERROR, YR_DOY, CLEAR, OPHEAD, PATHD, INTRO, JOIN_TRIM,
#     &  IPEXP, IPSOIL_INP, IPVAR, IPSLIN, IPSLAN, SENS, INSOIL,
#     &  WEATHR_INP, OPTEMPY2K, OPTEMPXY2K, OPGEN

    from COMIBS import IBS01 as ib1, IBS02 as ib2, IBS03 as ib3, IBS04 as ib4
    from COMSOI import SOI01 as S1, SOI02 as S2, SOI03 as S3
    from COMSWI import SWITCH as sw
    from ModuleDefs import NAPPL, NL, ISWITCH, RunConstants
    import os
    from UTILS import create_str_array, create_int_array, create_float_array
    from ipexp import IPEXP
    from IPSOIL_Inp import IPSOIL_Inp
    from IPVAR import IPVAR
    from IPSLIN import IPSLIN
    from OPHEAD import OPHEAD
    from INSOIL import INSOIL
    from WEATHR_Inp import WEATHR_Inp
    from OPTEMPY2K import OPTEMPY2K
    from OPTEMPXY2K import OPTEMPXY2K
    from OPGEN import OPGEN

    WMODI = ' '
    CROP,PRCROP = ' ' * 2
    ECONAM = ' '
    TITLET = ' '
    CHEXTR = create_str_array(NAPPL)
    WTHSTR, FILECTL = ' ' * 2

    NLOOP,FROP,FTYPEN = [1] * 3
    IIRV = create_int_array(NAPPL)
    LUNIO,NYRS,ERRNUM,NSENS,YRIC = [1] * 5
    IVRGRP,IPLT,ISIM,EXPP,EXPN,TRTN,TRTALL = [1] * 7
    NFORC,NDOF,PMTYPE,ISENS = [1] * 4
    LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE = [1] * 6
    LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES = [1] * 7

    FEXIST,INITIAL, UseSimCtr = False, False, False

    WRESR,WRESND,TSOC,CO2 = [0.0] * 4
    SWINIT = create_float_array(NL)
    INO3 = create_float_array(NL),
    INH4 = create_float_array(NL)
    EFINOC,EFNFIX = [0.0] * 2
    AINO3,AINH4,TNMIN,ANO3,ANH4,TSWINI = [0.0] * 6
    ESW = create_float_array(NL)
    SW = create_float_array(NL)
    TLL,TSW,TDUL,TSAT,TPESW,CUMDEP,PESW = [0.0] * 7
    PLTFOR, PMBD = [0.0] * 2


    ERRKEY = 'INPUT '

#-----------------------------------------------------------------------
#     Get argument from runtime module to determine path and run mode
#-----------------------------------------------------------------------
#   Fortran Compaq Visual Fortran
#-----------------------------------------------------------------------
#     CALL GETARG (0,INPUTX) returns
    INPUTX = 'C:\\DSSAT48\\DSCSM048.EXE'

#      call path_adj(inputx)
#      IPX = LEN_TRIM(INPUTX)
    IPX = len(INPUTX.strip())

#D     INPUTX = STDPATH // 'DSCSM048.EXE'
#      CALL PATHD(DSSATP,INPUTX,IPX) returns C:\DSSAT48\DSSATPRO.V48
    sw.DSSATP = 'C:\\DSSAT48\\DSSATPRO.V48'
    CONTROL.DSSATP = sw.DSSATP

#-----------------------------------------------------------------------
#    Initialize and delete previous copy of FILEIO
#-----------------------------------------------------------------------
# if exist reads INP file and then closes, delets
#      INQUIRE (FILE = FILEIO,EXIST = FEXIST)
#      IF (FEXIST) THEN
#          OPEN (LUNIO, FILE = FILEIO,STATUS = 'UNKNOWN',IOSTAT=ERRNUM)
#          READ (LUNIO,40) EXPP,TRTN,TRTALL
#          READ (LUNIO,70,IOSTAT=ERRNUM) IOX,IDETO,IDETS,FROP,IDETG,
#     &            IDETC,IDETW,IDETN,IDETP,IDETD,IDETL,IDETH,IDETR
#          CLOSE (LUNIO,STATUS = 'DELETE')
#      ENDIF

#-----------------------------------------------------------------------
#     BEGINNING of READING INPUT files
#-----------------------------------------------------------------------
#     IF (RNMODE .EQ. 'I' .AND. RUN .EQ. 1) THEN
#       CALL CLEAR
#       CALL INTRO
#     ENDIF
    NSENS  = 0
    ISENS  = 0
    TITLER = ' ' * 5

    FILEX_P = os.path.join(PATHEX,FILEX)
#   CALL Join_Trim(PATHEX, FILEX, FILEX_P) VSH:not clear what this does
#-----------------------------------------------------------------------
#     Call IPEXP
#-----------------------------------------------------------------------
    # IPEXP (MODEL, RUN, RNMODE, FILEX,PATHEX,FILEX_P, FILECTL,
    #     SOI01.SLNO,NYRS,VARNO,CROP,WMODI,
    #     FROP,TRTN,EXPP,EXPN,TITLET,TRTALL,TRTNUM,ROTNUM,
    #     IIRV,FTYPEN,CHEXTR,NFORC,PLTFOR,NDOF,PMTYPE,
    #     LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
    #     LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
    #     CONTROL, ISWITCH, UseSimCtr, MODELARG, PMBD)
    (MODEL, S1.SLNO, NYRS, VARNO, CROP, WMODI, FROP, TRTN, EXPP, EXPN, TITLET, TRTALL,IIRV,
    FTYPEN, CHEXTR, NFORC, PLTFOR, NDOF, PMTYPE, LNSIM, LNCU, LNHAR, LNENV, LNTIL, LNCHE,
    LNFLD, LNSA, LNIC, LNPLT, LNIR, LNFER, LNRES, CONTROL, ISWITCH, PMBD) = (
        IPEXP(RUN,RNMODE,FILEX,PATHEX,FILEX_P,TRTNUM,ROTNUM,CONTROL,UseSimCtr,MODELARG))

#-----------------------------------------------------------------------
#     Call IPSOIL
#-----------------------------------------------------------------------
# ** DEFAULT MESOL = 2 **
#  MESOL = '1' Original soil layer distribution. Calls LYRSET.
#  MESOL = '2' New soil layer distribution. Calls LYRSET2.
#  MESOL = '3' User specified soil layer distribution. Calls LYRSET3.
#     Skip soils field and soils input for sequence mode
    if 'FQ'.find(RNMODE) == 0 or RUN == 1 :
        IPSOIL_Inp (RNMODE,sw.FILES,sw.PATHSL,NSENS,ISWITCH)
#-----------------------------------------------------------------------
#     Call IPVAR
#-----------------------------------------------------------------------
    if CROP != 'FA':
        VARTY, VRNAME, ECONO, ATLINE = IPVAR (sw.FILEG,NSENS,RNMODE,VARNO,sw.PATHGE,MODEL)
#-----------------------------------------------------------------------
#     Call IPSLIN to read initial soil conditions
#-----------------------------------------------------------------------
    if 'FQ'.find(RNMODE) <= 0 or RUN == 1:
        YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID = (
            IPSLIN (FILEX, FILEX_P, LNIC,S2.NLAYR,S3.DUL,S1.PEDON,S1.SLNO,S3.DS,ISWITCH))
        if ib1.ISIMI == 'I':
            pass
         #   IF (YRIC .LT. YRSIM .AND. YRIC .GT. 0) THEN
         #     YRSIM = YRIC
         #     CALL YR_DOY (YRSIM,YEAR,ISIM)
         #     IF (MEWTH .EQ. 'M' .OR. RNMODE .EQ. 'Y') THEN
         #        WRITE (FILEW(5:6),'(I2)') YEAR
         #     ENDIF
         #     IF (MEWTH .EQ. 'G') THEN
         #        WRITE (FILEWG(5:6),'(I2)') YEAR
         #     ENDIF
         #   ENDIF
# C-----------------------------------------------------------------------
# C        Call IPSLAN to read soil analysis information
# C-----------------------------------------------------------------------
        if sw.ISWNIT == 'Y':
            pass
#             CALL IPSLAN (FILEX, FILEX_P,LNSA, BD, DS, EXK, EXTP, OC,
#      &            PEDON, PH, PHKCL, SLNO, SMHB, SMKE, SMPX, TOTN,
#      &            SASC, SAEA, NLAYR)    !, YRSIM)
# C-----------------------------------------------------------------------
# C        Sensitivity Analysis Section
# C-----------------------------------------------------------------------
    if 'IE'.find(RNMODE) > 0 and NYRS == 1:
        pass
#          IF (INDEX('I',RNMODE) .GT. 0) THEN
#            NLOOP = 0
#   300      CONTINUE
#            NLOOP = NLOOP + 1
#            IF (NLOOP .GT. 25) CALL ERROR (ERRKEY,1,' ',0)
#            CALL CLEAR
#            WRITE (*,400)
#            READ (5,'(I2)',ERR = 300) NSENS
#          ELSE
#            NSENS = 1
#          ENDIF
#          IF (NSENS .EQ. 1) THEN
#             INITIAL = (ISWWAT .EQ.'N')
#             CALL SENS (NSENS,VARNO,VARTY,VRNAME,FTYPEN,LNIC,LNSA,
#      &        WRESR,WRESND,ISIM,NYRS,IPLT,WMODI,ECONO,ECONAM,ECOTYP,
#      &        PRCROP,SWINIT,INO3,INH4,RUN,FROP,YRIC,EFINOC,EFNFIX,
#      &        CROP,IVRGRP,ISENS,MODEL, RNMODE, FILEX,FILEX_P,
#      &        ISWITCH,CONTROL)
#             IF (INITIAL) THEN
#                IF ((ISWNIT .EQ. 'Y') .OR. (ISWWAT .NE.'N')) THEN
#                   NSENS = 0
#                   CALL IPSOIL_Inp(RNMODE,FILES,PATHSL,NSENS,ISWITCH)
#                   CALL IPSLIN (FILEX,FILEX_P,LNIC,NLAYR,DUL,YRIC,
#      &                 PRCROP,WRESR,WRESND,EFINOC,EFNFIX,PEDON,SLNO,DS,
#      &                 SWINIT,INH4,INO3,ISWITCH,
#      &                 ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID)    !,YRSIM)
#                   CALL IPSLAN (FILEX, FILEX_P,LNSA, BD, DS, EXK, EXTP,
#      &            OC, PEDON, PH, PHKCL, SLNO, SMHB, SMKE, SMPX, TOTN,
#      &            SASC, SAEA, NLAYR)    !, YRSIM)
#                   NSENS = 1
#                ENDIF
#             ENDIF
#          ENDIF
#          WRITE (*,1000) RUN
#          READ (5,'(A25)') TITLER
#          IF (TITLER .EQ. '                         ') THEN
#             TITLER = TITLET
#          ENDIF
    else:
        TITLER = TITLET
#
#      Regenarate short headers now that Run Title is known.
    OPHEAD (RunConstants.RUNINIT,99,0.0,0.0,"                ",0.0,0.0,
            "      ",RUN,MODEL,TITLER,WTHSTR, RNMODE,CONTROL, ISWITCH, UseSimCtr, PATHEX)
#-----------------------------------------------------------------------
#     Call INSOIL to calculate initial conditions for each soil layer
#-----------------------------------------------------------------------
#     Skip soils field and soils input for sequence mode
    if 'FQ'.find(RNMODE) <= 0 or RUN == 1:
        (PESW, CUMDEP, TSW, TSWINI, TPESW, TDUL, TLL, TSAT, AINO3, AINH4, ANO3, ANH4, TNMIN, TSOC, SWINIT,
        INH4, INO3, ESW, SW, S3.BD, S3.PH, S3.OC, S3.TOTN, ib1.SLTX, S1.SLTXS) = (
        INSOIL(sw.ISWWAT, sw.ISWNIT, SWINIT, S2.NLAYR, S3.DUL, S3.LL, ESW, S3.DLAYR, S3.SAT, SW, S3.BD,
               INO3, INH4, S3.OC, S3.PH, ib1.SLTX, S1.SLTXS, S3.TOTN))
#-----------------------------------------------------------------------
#     Call WEATHR to set CO2 conditions and weather parameter modifications
#-----------------------------------------------------------------------
        CO2, WTHSTR = WEATHR_Inp(ib3.CO2ADJ, ib1.CO2FAC, ib3.DAYADJ, ib1.DAYFAC, ib3.DPTADJ, ib1.DPTFAC, ib3.PRCADJ,
                                 ib1.PRCFAC, ib3.RADADJ, ib1.RADFAC, ib3.TMADJ, ib1.TMFAC, ib3.TXADJ, ib1.TXFAC, WMODI,
                                 ib3.WNDADJ, ib1.WNDFAC, ib4.WTHADJ, CO2, WTHSTR, ib2.NEV)
#-----------------------------------------------------------------------
#     Write temporary output files for runtime modules
#-----------------------------------------------------------------------
#     Write DSSAT Format Version 4 Output file for input by Version 4
#
#-----------------------------------------------------------------------
    OPTEMPY2K(RNMODE, FILEX, PATHEX, YRIC, PRCROP, WRESR, WRESND, EFINOC, EFNFIX, SWINIT, INH4, INO3, NYRS, VARNO,
          VRNAME, CROP, MODEL, RUN, FILEIO, EXPN, ECONO, FROP, TRTALL, TRTN, CHEXTR, NFORC, PLTFOR, NDOF, PMTYPE, ISENS,
          PMBD)

    OPTEMPXY2K (YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,NYRS,VARNO,VRNAME,CROP,
                FILEIO,FROP,ECONO,ATLINE,LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,LNFLD,LNSA,LNIC,LNPLT,
                LNIR,LNFER,LNRES,NFORC,PLTFOR,PMTYPE,NDOF,CHEXTR,MODEL,PATHEX,PMBD)

#-----------------------------------------------------------------------
#     Write DSSAT Format Version 4 Output files
#-----------------------------------------------------------------------
    OPGEN (CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,TSWINI,RUN,MODEL,CROP,ib1.CROPD,TITLET,ECONO,VARTY,
           ESW,SWINIT,INO3,INH4,TSOC,WTHSTR,NYRS, RNMODE,CONTROL, ISWITCH, UseSimCtr, ATLINE, PATHEX)
#
#-----------------------------------------------------------------------
#     FORMAT Strings
#-----------------------------------------------------------------------
#   40  FORMAT (36X,3(1X,I5))
#   70  FORMAT (17(/),14X,3(5X,A1),4X,I2,9(5X,A1))
#  400  FORMAT (/////,5X,'What Would You Like To Do ?',
#      &            //,1X,' 0. Run Simulation.',
#      &             /,1X,' 1. Select Sensitivity Analysis Options.',
#      &            //,1X,'    CHOICE ?   [ Default = 0 ] ===> ',$)
#  1000 FORMAT (/,5X,'Please enter Run',I3,' name : ===> ',$)
#
#       END SUBROUTINE INPUT_SUB

    return ISWITCH, CONTROL


# Test Driver
# FILEIO = 'DSSAT48.inp'
# FILEX = 'UFBA1701.SRX'
# MODELARG = 'CRGRO048'
# PATHEX = 'C:/DSSAT48/Strawberry/'
# RNMODE = 'B'
# ROTNUM = 0
# RUN = 1
# TRTNUM = 1
