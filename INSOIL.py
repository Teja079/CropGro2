# =======================================================================
#   INSOIL, Subroutine
#
#   Determines soil initialization
# -----------------------------------------------------------------------
#   INPUT  : ISWWAT,ISWNIT,AINO3,ANO3,AINH4,ANH4,TNMIN,SWINIT,TSWINI,NLAYR,
#            DUL,LL,ESW,DLAYR,SAT,SW,TLL,TDUL,TSAT,TPESW,CUMDEP,PESW,TSW,BD,
#            INO3,INH4,TSOC,OC,PH
#
#   LOCAL  :
#
#   OUTPUT :
# -----------------------------------------------------------------------
#   Called : INPUT
#
#   Calls  : None
# -----------------------------------------------------------------------
#                          DEFINITIONS
#
#   HDLAY  :
# =======================================================================
def INSOIL(ISWWAT, ISWNIT, SWINIT, NLAYR, DUL, LL, ESW, DLAYR, SAT, SW, BD, INO3, INH4, OC, PH,
    SLTX, SLTXS, TOTN):
    from ModuleDefs import NL
    PESW = 0.0
    TSW = 0.0
    TSWINI = 0.0
    TPESW = 0.0
    TDUL = 0.0
    TLL = 0.0
    TSAT = 0.0
    AINO3 = 0.0
    AINH4 = 0.0
    ANO3 = 0.0
    ANH4 = 0.0
    CUMDEP = 0.0
    TNMIN = 0.0
    TSOC = 0.0

    if ISWWAT != 'Y':
        SWINIT = DUL
        INH4 = [0.0] * NL
        INO3 = [0.0] * NL
        return (PESW, CUMDEP, TSW, TSWINI, TPESW, TDUL, TLL, TSAT, AINO3, AINH4, ANO3, ANH4, TNMIN, TSOC, SWINIT,
        INH4, INO3, ESW, SW, BD, PH, OC, TOTN, SLTX, SLTXS)

    HUM = [0.0] * NL
    KG2PPM = [0.0] * NL
    SNO3 = [0.0] * NL
    SNH4 = [0.0] * NL

    for L in range(NLAYR):
        if SWINIT[L] <= 0:
            SWINIT[L] = DUL[L]
        ESW[L] = DUL[L] - LL[L]
        SW[L] = SWINIT[L]
        CUMDEP += DLAYR[L]
        TSWINI += SWINIT[L] * DLAYR[L]
        TPESW += ESW[L] * DLAYR[L]
        TLL += LL[L] * DLAYR[L]
        TDUL += DUL[L] * DLAYR[L]
        TSAT += SAT[L] * DLAYR[L]

        if BD[L] <= 0.0:
            BD[L] = 0.0
        if PH[L] <= 0.0:
            PH[L] = 0.0
        if OC[L] <= 0.0:
            OC[L] = -99.0
        if TOTN[L] < -9.0:
            TOTN[L] = -9.0
        if INO3[L] <= 0.0:
            INO3[L] = 0.0
        if INH4[L] <= 0.0:
            INH4[L] = 0.0

        if ISWNIT == 'Y':
            if BD[L] <= 0.0:
                BD[L] = 1.2
            if PH[L] <= 0.0:
                PH[L] = 7.0
            KG2PPM[L] = 1.0 / (BD[L] * 0.1 * DLAYR[L])
            SNO3[L] = INO3[L] / KG2PPM[L]
            SNH4[L] = INH4[L] / KG2PPM[L]
            AINO3 += SNO3[L]
            AINH4 += SNH4[L]
            if OC[L] > 1e-6:
                HUM[L] = OC[L] * 1000.0 * BD[L] * DLAYR[L]
            else:
                HUM[L] = 0.0
            TSOC += HUM[L]

    TSW = TSWINI
    PESW = max(0.0, TSW - TLL)


    SLTX = SLTX.lstrip()
    SLTXS = SLTXS.lstrip()
    if ((SLTX[0:2] == '-9' or SLTX == '') and SLTXS != ''):
        SLTX = SLTXS
    elif (SLTXS == '' and (SLTX != '' and SLTX[0:2] != '-9')):
        SLTXS = SLTX

    return (PESW, CUMDEP, TSW, TSWINI, TPESW, TDUL, TLL, TSAT, AINO3, AINH4, ANO3, ANH4, TNMIN, TSOC, SWINIT,
            INH4, INO3, ESW, SW, BD, PH, OC, TOTN, SLTX, SLTXS)

# #TEST:
# from ModuleDefs import NL
# ISWWAT = 'Y'
# ISWNIT = 'N'
# SWINIT = [-99.0] * NL
# NLAYR = 9
# DUL = [9.1e-2,9.1e-2,7.6e-2,7.1e-2,7.1e-2,8.1e-2,7.8e-2,8.4e-2, 8.4e-2]
# LL = [4.5e-2, 4.5e-2, 3.7e-2, 3.4e-2, 3.4e-2, 4.3e-2, 4.3e-2, 4.7e-2, 4.7e-2]
# ESW = [0.0] * NL
# DLAYR =  [5.0, 10.0, 15.0, 15.0, 15.0, 30.0, 30.0, 30.0, 30.0]
# SAT = [0.412, 0.412, 0.402, 0.415, 0.415, 0.412, 0.412, 0.416, 0.416]
# SW = [0.0] * NL
# BD = [1.49, 1.49, 1.52, 1.49, 1.49, 1.5, 1.5, 1.49, 1.49]
# INO3 = [-99.0] * NL
# INH4 = [-99.0] * NL
# OC = [0.48, 0.48, 0.18, 0.09, 0.09, 0.09, 0.02, 0.02, 0.02]
# PH = [5.5, 5.5, 5.3, 5.1, 5.1, 5.2, 5.3, 5.2, 5.2]
# SLTX = '-99 '
# SLTXS = 'S  '
# TOTN = [-99.0] * NL
#
#
# (PESW, CUMDEP, TSW, TSWINI, TPESW, TDUL, TLL, TSAT, AINO3, AINH4, ANO3, ANH4, TNMIN, TSOC, SWINIT,
# INH4, INO3, ESW, SW, BD, PH, OC, TOTN, SLTX, SLTXS) = INSOIL(ISWWAT, ISWNIT, SWINIT, NLAYR, DUL, LL, ESW, DLAYR, SAT, SW, BD, INO3, INH4, OC, PH,
#     SLTX, SLTXS, TOTN)
# print(PESW, CUMDEP, TSW, TSWINI, TPESW, TDUL, TLL, TSAT, AINO3, AINH4, ANO3, ANH4, TNMIN, TSOC, SWINIT,
# INH4, INO3, ESW, SW, BD, PH, OC, TOTN, SLTX, SLTXS)