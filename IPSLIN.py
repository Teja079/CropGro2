# =======================================================================
#   IPSLIN, Subroutine
#   Reads soil initial conditions from FILEX file
#   Read initial values of rapidly changing soil variables,
#   and convert values to standard soil depths
# -----------------------------------------------------------------------
#   INPUT  : FILEX, FILE_P,LNIC,NLAYR,DUL,PEDON,SLNO, ISWITCH
#
#   LOCAL  : LN,NLAYRI,NLAYR,DS,DLAYRI,LINEXP,ISECT,CHARTEST,LUNEXP, SWINIT
#
#   OUTPUT : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,INO3,INH4,SWINIT
# -----------------------------------------------------------------------
#   Called : SESOIL INPUT
#
#   Calls  : FIND IGNORE ERROR
# =======================================================================
def IPSLIN(FILEX, FILEX_P, LNIC,NLAYR,DUL,PEDON,SLNO,DS,ISWITCH):
    from ModuleDefs import NL
    from ERROR import ERROR
    from READS import find, ignore
    from DATES import Y4K_DOY
    from LMATCH import LMATCH
    import fortranformat as ff

    f55 = ff.FortranRecordReader("I3,3X,A2,1X,I5,10(1X,F5.0)")
    f60 = ff.FortranRecordReader("I3,F5.0,3(1X,F5.0)")
    f65 = ff.FortranRecordReader("26X,3F6.0")

    DLAYRI, SWINIT, INO3, INH4 = ([-99.0] * NL for _ in range(4))
    SOM1I, SOM2I, SOM3I = ([-99.0] * NL for _ in range(3))
    LUNEXP = 16
    ERRKEY = 'IPSLIN'
    FINDCH = '*INITI'

#   Set default initial conditions in case they are missing
    YRIC = 0
    PRCROP = '  '
    WRESR  = 0.0
    WRESND = 0.0
    EFINOC = 1.0
    EFNFIX = 1.0
    ICWD   = -99
    ICRES  = 0.0
    ICREN  = 0.0
    ICREP  = 0.0
    ICRIP  = 100.0
    ICRID  = 0.0

    if PEDON != SLNO or LNIC <= 0:
        return YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID

    try:
        with (open(FILEX_P, 'r') as f):
            lines = f.readlines()
    except IOError as e:
        ERROR(ERRKEY, e.errno, FILEX, 0)
        return

    LINEXP, IFIND = find(lines, FINDCH)
    if not IFIND: ERROR (ERRKEY,1,FILEX,LINEXP)

    while True:
        LINEXP, ISECT = ignore(lines, LINEXP+1)
        if ISECT == 1:
            CHARTEST = lines[LINEXP]
            try:
                LN = int(CHARTEST[0:3])
            except IOError as e:
                ERROR(ERRKEY, -99, FILEX, LINEXP)
            if LN == LNIC: #Correct line found
                try:
                    LN, PRCROP, YRIC, WRESR, WRESND, EFINOC, EFNFIX, ICWD, ICRES, ICREN, ICREP, ICRIP, ICRID = (
                        f55.read(CHARTEST))
                except Exception as e:
                    ERROR(ERRKEY, -99, FILEX, LINEXP)
                break
        else:
            ERROR(ERRKEY, 2, FILEX, LINEXP)
            break

    YRIC = Y4K_DOY (YRIC,FILEX,LINEXP,ERRKEY,3)

    if ISWITCH.ISWNIT == 'Y':
        WRESR = max(WRESR, 0.0)
        if WRESND < 0.0: WRESND = 0.0
        if EFINOC < 0.0: EFINOC = 1.0
        if EFNFIX < 0.0: EFNFIX = 1.0
        ICRES = max(ICRES, 0.0)
        ICREN = max(ICREN, 0.0)
        ICREP = max(ICREP, 0.0)
        ICRID = max(ICRID, 0.0)
        if ICRIP < 0.0: ICRIP = 100.0

#   Read layer information for the correct IC level number
    NLAYRI = 0
    while True:
        LINEXP, ISECT = ignore(lines, LINEXP + 1)
        if ISECT == 1:
            CHARTEST = lines[LINEXP]
            try:
                LN = int(CHARTEST[0:3])
            except IOError as e:
                ERROR(ERRKEY, -99, FILEX, LINEXP)

            if LN == LNIC: #Correct line found
                try:
                    LN, DLAYRI[NLAYRI], SWINIT[NLAYRI], INH4[NLAYRI], INO3[NLAYRI] = f60.read(CHARTEST)
                    NLAYRI += 1
                except Exception as e:
                    ERROR(ERRKEY, -99, FILEX, LINEXP)
        else:
            #ERROR(ERRKEY, 2, FILEX, LINEXP)
            break
    SWINIT = LMATCH (NLAYRI,DLAYRI,SWINIT,NLAYR,DS)
    INH4 =  LMATCH (NLAYRI,DLAYRI,INH4,  NLAYR,DS)
    INO3 =  LMATCH (NLAYRI,DLAYRI,INO3,  NLAYR,DS)

    for L in range(1, NLAYR + 1):
        if ISWITCH.ISWWAT != 'N':
            if SWINIT[L - 1] > 0.75:
                ERROR(ERRKEY, 10, FILEX, LINEXP)
            if SWINIT[L - 1] < 0.00:
                SWINIT[L - 1] = DUL[L - 1]

        if ISWITCH.ISWNIT == 'Y':
            if INH4[L - 1] < 0.0:
                INH4[L - 1] = 0.01
            if INH4[L - 1] > 100.0:
                ERROR(ERRKEY, 11, FILEX, LINEXP)
            if INO3[L - 1] < 0.0:
                INO3[L - 1] = 0.01
            if INO3[L - 1] > 100.0:
                ERROR(ERRKEY, 12, FILEX, LINEXP)

    return YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,ICWD,ICRES,ICREN,ICREP,ICRIP,ICRID

#TEST
# from ModuleDefs import SwitchType
# FILEX = 'UFBA1701.SRX'
# FILEX_P = 'C://DSSAT48//Strawberry//UFBA1701.SRX'
# LNIC = 1
# NLAYR = 9
# DUL = [9.1e-2,9.1e-2,7.6e-2,7.1e-2,7.1e-2,8.1e-2,7.8e-2,8.4e-2, 8.4e-2]
# DS = [5,15,30,45,60,90,120,150,180]
# PEDON = 'UFGA010700'
# SLNO= 'UFGA010700'
# ISWITCH = SwitchType()
# ISWITCH.ISWNIT = 'N'
# print(IPSLIN(FILEX, FILEX_P, LNIC,NLAYR,DUL,PEDON,SLNO,DS,ISWITCH))