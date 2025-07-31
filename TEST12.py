#=======================================================================
#  VEGGR, Subroutine, J. W. Jones, K. J. Boote, G. Hoogenboom
#  Calculates vegetative partitioning as a function of V stage,
#  and limits growth prior to VSINK.
#
#  Called by: PLANT
#  Calls:     CANOPY
#             ERROR, FIND, IGNORE
#========================================================================
#
import numpy as np
from ModuleDefs import RunConstants as RC, TS, GET_CONTROL
from ERROR import ERROR
from READS import FIND, IGNORE
def VEGGR(DYNAMIC, AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,FNINS, KCAN, NAVL, NDMNEW, NDMOLD,NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,PStres2, ROWSPC, RVSTGE, STMWT, TGRO,
    TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,
    WCRST, WTLF, XLAI, YRDOY, YREMRG,
    AGRVG, FRLF, FRRT, FRSTM):

#
# !-----------------------------------------------------------------------
#       USE ModuleDefs
#       USE ModuleData
#       IMPLICIT NONE
#       EXTERNAL GETLUN, FIND, ERROR, IGNORE, CANOPY
#       SAVE
    ERRKEY = 'VEGGR'
#
#       CHARACTER*6  ECONO, SECTION
#       CHARACTER*80 C80
#       CHARACTER*92 FILECC, FILEGC
#
#       INTEGER DYNAMIC
#       INTEGER YRDOY, YREMRG, NR1, DAS
#       INTEGER I, LUNCRP, ERR, LINC, LNUM, ISECT, FOUND
#
#       REAL AGRLF, AGRRT, AGRSTM, CMINEP, CMOBMX
#       REAL DTX, DXR57, FNINL, FNINR, FNINS, KCAN
#       REAL NAVL, NDMNEW, NDMOLD, PAR, PCH2O, PG
#       REAL PROLFI, PRORTI, PROSTI, ROWSPC
#       REAL RVSTGE, STMWT, TURFAC, WCRLF, WCRRT, WCRSH, WCRST
#       REAL WTLF, XLAI
#
#       REAL AGRVG, CADLF, CADST, CANHT, CANWH, CMINEA
#       REAL CRUSLF, CRUSRT, CRUSST, CRUSSH, CUMTUR
#       REAL EXCESS, FRLF, FRRT, FRSTM, NADLF, NADRT, NADST
#       REAL NGRLF, NGRRT, NGRST, NSTRES, PGAVL
#       REAL TNLEAK, VSTAGE, WLDOTN, WRDOTN, WSDOTN
#
#       REAL ATOP, CADSTF, FNINLG, FNINRG, FNINSG
#       REAL PROLFG, PRORTG, PROSTG
#       REAL NRATIO,NGRVEG,NADRAT,NLEFT
#       REAL NGRVGG, NGRLFG,NGRSTG,NGRRTG
#       REAL PROLFT,PROSTT,PRORTT
#       REAL VGRDEM, SUPPN, PGLEFT, LSTR, CSAVEV
#       REAL NLEAK
#       REAL NMINEA, NFIXN, TRNU
#
    TGRO = np.zeros(TS, dtype=float)
#
# !     FO - Cotton-Nitrogen
#       REAL NSTFAC, PNSTRES, XNSTRES
#
# !     P module
#       REAL PStres2
#
#       TYPE (ControlType) CONTROL
    GET_CONTROL(CONTROL)
    DAS = CONTROL.DAS
#
# !***********************************************************************
# !***********************************************************************
# !     Run Initialization - Called once per simulation
# !***********************************************************************
    if DYNAMIC == RC.RUNINIT:
# !-----------------------------------------------------------------------
# !     Read in values from input file, which were previously input
# !       in Subroutine IPCROP.
# !-----------------------------------------------------------------------
        LUNCRP = GETLUN('FILEC')
        try:
            file_obj = open(FILECC, 'r')
            ERR = 0
        except OSError as e:
            ERR = e.errno
        if ERR != 0:
            ERROR(ERRKEY, ERR, FILECC, 0)
        LNUM = 0
# !-----------------------------------------------------------------------
# !    Find and Read Plant Composition Section
# !-----------------------------------------------------------------------
# !     Subroutine FIND finds appropriate SECTION in a file by
# !     searching for the specified 6-character string at beginning
# !     of each line.
# !-----------------------------------------------------------------------
        SECTION = '!*PLAN'
        LINC, FOUND = FIND(LUNCRP, SECTION)
        LNUM += LINC
        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            ISECT, C80=IGNORE(LUNCRP, LNUM)
            PROLFI, PROLFG, PROSTI, PROSTG, ERR = READ(C80, '(2F6.0,6X,2F6.0)')
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)
        #
            ISECT, C80=IGNORE(LUNCRP, LNUM)
            PRORTI, PRORTG, ERR = READ(C80, '(2F6.0)')
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)
# !-----------------------------------------------------------------------
# !    Find and Read Plant Composition Section
# !-----------------------------------------------------------------------
        SECTION = '!*CARB'
        LINC, FOUND = FIND(LUNCRP, SECTION)
        LNUM += LINC
        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            ISECT, C80=IGNORE(LUNCRP, LNUM)
            CMOBMX, CADSTF, ERR = READ(C80, '(2F6.0)')
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)
            ISECT, C80=IGNORE(LUNCRP, LNUM)
            NSTFAC, ERR = READ(C80, '(6X,F6.0)')
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

# !-----------------------------------------------------------------------
# !    Find and Read Partitioning Section
# !-----------------------------------------------------------------------
        SECTION = '!*VEGE'
        LINC, FOUND = FIND(LUNCRP, SECTION)
        LNUM += LINC
        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            for i in range(1, 5):
                ISECT = 2
                while ISECT != 1:
                    ISECT, C80=IGNORE(LUNCRP, LNUM)
            ATOP, ERR = READ(C80, '(24X,F6.0)')
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)
        LUNCRP.close()

#
# !-----------------------------------------------------------------------
# !    Call CANOPY for input
# !-----------------------------------------------------------------------
        CANHT, CANWH = CANOPY(RC.RUNINIT,ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES)

#
# !***********************************************************************
# !***********************************************************************
# !     Seasonal initialization - run once per season
# !***********************************************************************
    elif DYNAMIC == RC.SEASINIT:
# !-----------------------------------------------------------------------
        CADLF  = 0.0
        CADST  = 0.0
        CMINEA = 0.0
        CRUSLF = 0.0
        CRUSRT = 0.0
        CRUSST = 0.0
        CUMTUR = 1.0
        EXCESS = 1.0
        FNINLG = 0.0
        FNINRG = 0.0
        FNINSG = 0.0
        NADLF  = 0.0
        NADRT  = 0.0
        NADST  = 0.0
        NGRLF  = 0.0
        NGRRT  = 0.0
        NGRST  = 0.0
        NSTRES = 1.0
        PGLEFT = 0.0
        SUPPN  = 0.0
        TNLEAK = 0.0
        VGRDEM = 0.0
        WLDOTN = 0.0
        WRDOTN = 0.0
        WSDOTN = 0.0
# !     FO/KJB - Running average
        PNSTRES= 1.0
        XNSTRES= 1.0
#
        CANHT, CANWH =  CANOPY(RC.SEASINIT,ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES)
#
# !***********************************************************************
# !***********************************************************************
# !     EMERGENCE CALCULATIONS - Performed once per season upon emergence
# !         or transplanting of plants
# !***********************************************************************
    elif DYNAMIC == EMERG:
# !-----------------------------------------------------------------------
        FNINLG = PROLFG * 0.16
        FNINRG = PRORTG * 0.16
        FNINSG = PROSTG * 0.16
        CUMTUR = 1.0
        CANHT, CANWH = CANOPY(EMERG,ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES)
#
# !***********************************************************************
# !***********************************************************************
# !     DAILY RATE/INTEGRATION
# !***********************************************************************
    elif DYNAMIC == RC.INTEGR:
# !-----------------------------------------------------------------------
# !-----------------------------------------------------------------------
# C     Partitioning is modified by water stress and nitrogen stress
# C-----------------------------------------------------------------------
        SUPPN = NFIXN + TRNU + NMINEA
# ! KJB - Replacing a hardwire 0.70 for N-Stress
        NSTFAC = min(NSTFAC,1.0)
        NSTFAC = max(NSTFAC,0.1)
# !    chp added check for YRDOY = YREMRG, but on the next day, it still
# !     shows N stress because there is little supply.  Force a lag time?
# !      IF (SUPPN .LT. 0.70 * NDMNEW .AND. NDMNEW .GT. 0.) THEN
# !     FO/KJB - Running average
        PNSTRES = XNSTRES
#
        if SUPPN < NSTFAC * NDMNEW and NDMNEW > 0 and YRDOY != YREMRG:
            XNSTRES = min(1.0, SUPPN / (NDMNEW * NSTFAC))
        else:
            XNSTRES = 1.0

#
# !     FO/KJB - Running average
        NSTRES = XNSTRES * 0.5 + PNSTRES * 0.5
#
# !      FRRT  = ATOP * (1.0 - (MIN(TURFAC,NSTRES)))*(1.0-FRRT) + FRRT
        FRRT = ATOP * (1.0 - min(TURFAC, NSTRES, PStres2)) * (1.0 - FRRT) + FRRT
# C-----------------------------------------------------------------------
# C     Cumulative turgor factor that remembers veg drought stress
# C     to shift partitioning between leaf and stem toward leaf,
# C     especially after drought is released.
# C     Sort of 20-day rolling average
# C-----------------------------------------------------------------------
        CUMTUR = 0.95 * CUMTUR + 0.05 * TURFAC
        if CUMTUR < 1e-7:
            CUMTUR = 0.0   #prevent underflow
# C-----------------------------------------------------------------------
# C     0.6 IS A SCALAR, COULD BE LESS, was once 0.8 and 0.7
# C     0.7 appears to be too much for peanut, but not for soybean.
# C-----------------------------------------------------------------------
        FRLF = (1.0 + 0.6 * (1.0 - CUMTUR)) * (1.0 - FRRT) * FRLF / (FRLF + FRSTM)
        FRLF = min(FRLF, 0.90 * (1.0 - FRRT))
        FRSTM = 1.0 - FRRT - FRLF
# C-----------------------------------------------------------------------
# C     To prevent negative partitioning to root and limit leaf plus
# C     stem to a maximum of 98 % of the vegetative partitioning
# C-----------------------------------------------------------------------
        FRLF = min(FRLF, FRLF * 0.98 / max(0.001, FRLF + FRSTM))
        FRSTM = min(FRSTM, FRSTM * 0.98 / max(0.001, FRLF + FRSTM))
        FRRT = 1.0 - FRLF - FRSTM
# C-----------------------------------------------------------------------
# C     Calculate weighted PHI + GR = 1/E = AGRVG for veg. growth
# C-----------------------------------------------------------------------
        AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
# C-----------------------------------------------------------------------
# C     Calculate New Growth Rate of Leaves, Stems, and Roots
# C-----------------------------------------------------------------------
        VGRDEM = PGAVL / AGRVG
        WLDOTN = FRLF * VGRDEM
        WSDOTN = FRSTM * VGRDEM
        WRDOTN = FRRT * VGRDEM
# C-----------------------------------------------------------------------
# C     Compute maximum N required for tissue growth
# C-----------------------------------------------------------------------
        NGRLF  = WLDOTN * FNINL
        NGRST  = WSDOTN * FNINS
        NGRRT  = WRDOTN * FNINR
        NGRVEG = NGRLF + NGRST + NGRRT
# C-----------------------------------------------------------------------
# C     Compute minimum N required for tissue growth
# C-----------------------------------------------------------------------
        NGRLFG = WLDOTN * FNINLG
        NGRSTG = WSDOTN * FNINSG
        NGRRTG = WRDOTN * FNINRG
        NGRVGG = NGRLFG + NGRSTG + NGRRTG
        NRATIO = 1.0
        if NAVL < NGRVGG:

# C-----------------------------------------------------------------------
# C     Compute ratio for reducing leaf growth to prevent N conc of
# C       new tissue from being below the minimum for growth
# C-----------------------------------------------------------------------
            if NGRVGG > 0.0:
                NRATIO = NAVL / NGRVGG
                WLDOTN = WLDOTN * NRATIO
                WSDOTN = WSDOTN * NRATIO
                WRDOTN = WRDOTN * NRATIO
                NGRLF  = NGRLFG * NRATIO
                NGRST  = NGRSTG * NRATIO
                NGRRT  = NGRRTG * NRATIO
# C-----------------------------------------------------------------------
# C     Adjust conversion costs to account for composition of tissue at
# C       lower N concentration
# C-----------------------------------------------------------------------
                AGRVG = (AGRLF * FRLF * (1.0 - (PROLFG - PROLFI) / (1.0 - PROLFI))+ AGRRT * FRRT * (1.0 - (PRORTG - PRORTI) / (1.0 - PRORTI))+ AGRSTM * FRSTM * (1.0 - (PROSTG - PROSTI) / (1.0 - PROSTI)))
        else:
# C-----------------------------------------------------------------------
# C     NAVL IS between lower and maximum N limit in this case,
# C       leaf expansion occurs as normal, but N concentration is reduced
# C-----------------------------------------------------------------------
            if NGRVEG > 0.0 and NAVL < NGRVEG:
                NGRLF = min(NAVL * NGRLF / NGRVEG, NGRLF)
                NGRST = min(NAVL * NGRST / NGRVEG, NGRST)
                NGRRT = min(NAVL * NGRRT / NGRVEG, NGRRT)

# C-----------------------------------------------------------------------
# C     Compute protein fraction of new vegetative tissue growth
# C-----------------------------------------------------------------------
            if WLDOTN > 0.0:
                PROLFT = NGRLF * (100.0/16.0) / WLDOTN
            else:
                PROLFT = 0.0

            if WSDOTN > 0.0:
                PROSTT = NGRST * (100.0/16.0) / WSDOTN
            else:
                PROSTT = 0.0

            if WRDOTN > 0.0:
                PRORTT = NGRRT * (100.0/16.0) / WRDOTN
            else:
                PRORTT = 0.0

#-----------------------------------------------------------------------
#     Recompute respiration costs if expansion occurs at low N-conc.,
#       allow N dilution during growth of leaves, stems, and roots
#-----------------------------------------------------------------------
            AGRVG = AGRLF * FRLF * (1.0 - (PROLFT - PROLFI) / (1.0 - PROLFI)) \
                + AGRRT * FRRT * (1.0 - (PRORTT - PRORTI) / (1.0 - PRORTI)) \
                + AGRSTM * FRSTM * (1.0 - (PROSTT - PROSTI) / (1.0 - PROSTI))
#       ENDIF
#-----------------------------------------------------------------------
#     Compute C and N remaining to add to reserves
#-----------------------------------------------------------------------
        PGLEFT = max(0.0, PGAVL - ((WLDOTN + WSDOTN + WRDOTN) * AGRVG))
        if PGLEFT < 1e-5:
            PGLEFT = 0.0
#-----------------------------------------------------------------------
#     Scales to 1.0 if PGLEFT is small fraction, and to 0.2 if large
#     fraction.  Used 0.04, so minor PGLEFT has no effect.  Used square
#     root.  Creates almost no effect if PGLEFT/PG is small, but goes to
#     0.2 as PGLEFT/PG  approaches 1.0.  0.04 could be parameterized as
#     kickoff point.  Upper cutoff is the value 1.04.  Limit of 1.04 -
#     1.00 forces relationship to stop at 0.04, gives 0.2 of normal PG.
#     value 1.04 -0.04 also can not be greater than 1.0 or we get
#     stimulation of photosynthesis and the sq root works differently.
#-----------------------------------------------------------------------
        if PG > 0.0001 and PGLEFT > 0.00001:
            EXCESS = (1.20 - min(1.0, max(PGLEFT/PG, 0.20))) ** 0.5
        else:
            EXCESS = 1.0
#
        CADST = 0.0
        CADLF = 0.0
        CMINEA = 0.0
        CRUSLF = 0.0
        CRUSST = 0.0
        CRUSRT = 0.0
        CRUSSH = 0.0
#-----------------------------------------------------------------------
#    Calculate Increase in Remobilizable C due to N shortage and
#      add to Carbon Pool.  Distribute to Leaves and Stems.
#-----------------------------------------------------------------------
#    Want half as much accumulation in stem in veg phae
#-----------------------------------------------------------------------
        if DAS < NR1:
            LSTR = (1.0 - 0.6 * CADSTF) / (0.6 * CADSTF)
        else:
            LSTR = (1.0 - CADSTF) / CADSTF

        if STMWT + WTLF > 0.0:
            LSTR = LSTR * WTLF / (STMWT + WTLF * LSTR)

        if PGLEFT >= CMINEP:
            CADLF = (PGLEFT - CMINEP) / PCH2O * LSTR
            CADST = (PGLEFT - CMINEP) * (1.0 - LSTR) / PCH2O
        else:
#
#-----------------------------------------------------------------------
#    Calculate actual C used (CMINEA) , compute how much is taken
#    from LF, ST, RT, and SH, which may be less than orig calc of CMINEP
#
#    8/26/97 KJB  DTX IN PLACE OF 1 TO SLOW IT DOWN A BIT AT ALL TIMES
#    AND TO BE SENSITIVE TO TEMPERATURE PRIOR TO R5 STAGE, BUT
#    STILL WANT THE SPEED-UP CAUSED BY THE "+ DXR57" FEATURE AFTER R5.
#
#-----------------------------------------------------------------------
            if CMINEP > 0:
                CMINEA = CMINEP - PGLEFT
                CRUSLF = CMINEA / CMINEP * CMOBMX * WCRLF * (DTX + DXR57)
                CRUSST = CMINEA / CMINEP * CMOBMX * WCRST * (DTX + DXR57)
                CRUSRT = CMINEA / CMINEP * CMOBMX * WCRRT * (DTX + DXR57)
                CRUSSH = CMINEA / CMINEP * CMOBMX * WCRSH * (DTX + DXR57)

        CADLF = CADLF + CSAVEV / PCH2O * LSTR
        CADST = CADST + CSAVEV * (1.0 - LSTR) / PCH2O

#
#-----------------------------------------------------------------------
#    Calculate Increase in Remobilizable N Due to a C shortage,
#      add to Nitrogen pool
#-----------------------------------------------------------------------
        NLEFT = max(0.0, NAVL - (NGRLF + NGRST + NGRRT))
        if NLEFT > 0.0:
            if NLEFT > NDMOLD:
                NLEAK = NLEFT - NDMOLD
                TNLEAK += NLEAK
                NLEFT -= NLEAK
            else:
                NLEAK = 0.0
            NADRAT = NLEFT / (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR)
            NADLF = NADRAT * FRLF * FNINL
            NADST = NADRAT * FRSTM * FNINS
            NADRT = NADRAT * FRRT * FNINR
        else:
            NADRAT = NADLF = NADST = NADRT = 0.0
#
#-----------------------------------------------------------------------
#     Subroutine CANOPY calculates height and width of the canopy as a
#     function of VSTAGE, air temperature, drought stress (TURFAC),
#     daylenght and radiation (PAR).
#-----------------------------------------------------------------------
        CANHT, CANWH = CANOPY(RC.INTEGR,ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC,RVSTGE, TGRO, TURFAC, VSTAGE, XLAI, NSTRES)
#
# !***********************************************************************
# !***********************************************************************
# !     END OF DYNAMIC IF CONSTRUCT
# !***********************************************************************
#       ENDIF
# !***********************************************************************
    return (AGRVG, FRLF, FRRT, FRSTM,CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,NADST, NGRLF, NGRRT, NGRST, NSTRES, TNLEAK,WLDOTN, WRDOTN, WSDOTN)
#-----------------------------------------------------------------------
#       END ! SUBROUTINE VEGGR