#=======================================================================
#  DEMAND, Subroutine, J.W. Jones and G. Hoogenboom.
#-----------------------------------------------------------------------
#  Calculates potential demand for C and N based upon new growth and
#  existing N deficiency in old tissue.
#-----------------------------------------------------------------------
#  Called by:  PLANT
#  Calls:      SDCOMP, IPDMND
#=======================================================================
from CropGro2.WARNING import WARNING


def DEMAND(DYNAMIC, CONTROL,
    AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57, FILECC, FILEGC,
    FILEIO, FNINSH, FRACDN, LAGSD, LNGPEG, NDLEAF, NSTRES, PAR, PCNL,
    PCNRT, PCNST, PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT, SDDES,
    SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC, TAVG, TDUMX, TDUMX2, TGRO,
    TURFAC, VSTAGE, WCRLF, WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,
    WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7):

    from ModuleDefs import RunConstants as RC, NCOHORTS, TS, GET_CONTROL
    from WARNING import WARNING
    import numpy as np
    from DSSATUtils import curv

    TYPSDT = ' '*3
    ERRKEY = 'DEMAND' #parameter
    MSG =[' '*78, ' '*78]

    NPP, I, NAGE, DAS, CropStatus = [0]*5
    YREND = [0]*7

    FRLFM, FRSTMM, YY, XX, TMPFAC = [0.0]*5
    REDPUN,TMPFCS,PAGE,REDSHL,SDMAX,CDMSH,GDMSH,ADDSHL = [0.0]*8
    TEMXFR,CAVTOT,GDMSDO,CNOLD = [0.0]*4
    NVSTL,NVSTS,NVSTR,FRNLFT = [0.0]*4
    TABEX,CURV = [0.0]*2
    POTLIP, POTCAR = [0.0]*2

    TPHFAC,PARSLA,FFVEG = [0.0]*3
    GROYES,GAINNW,GAINWT = [0.0]*3
    SLAVAR, SLAREF, FINREF = [0.0]*3
    SLAMAX, SLAMIN, THRESH = [0.0]*3
    FRLFF, FRSTMF = [0.0]*5
    CARMIN, LIPOPT, LIPTB, SLOSUM = [0.0]*4

    AGRSD1, AGRSD2, AGRVG, AGRVG2 = [0.0]*5
    CDMREP, CDMSD, CDMSDR, CDMTOT = [0.0]*4
    CDMVEG, DUMFAC, F = [0.0]*5
    FNINL, FNINR, FNINS, FNINSD = [0.0]*5
    FRLF, FRLFMX = [0.0]*3
    FRRT, FRSTM, FVEG = [0.0]*3
    GDMSD, GDMSDR = [0.0]*2
    GROMAX, GRRAT1, LNGSH = [0.0]*5
    NDMNEW, NDMOLD, NDMREP = [0.0]*3
    NDMSD, NDMSDR, NDMSH, NDMTOT, NDMVEG = [0.0]*5
    NMINEP, NMOBMX, NMOBR, NRCVR = [0.0]*5
    NVSMOB = 0.0
    PLIGSD, PMINSD, POASD = [0.0]*5
    PROLFF, PROLFI = [0.0]*2
    PRORTF, PRORTI, PROSTF, PROSTI, RCH2O = [0.0]*5
    RLIG, RLIP, RMIN, RNO3C = [0.0]*4
    ROA, RPRO, SDGR = [0.0]*5
    SDLIP, SDPRO, SHLAG = [0.0]*5
    SIZELF, SIZREF, SLAMN, SLAMX, SLAPAR = [0.0]*5
    SRMAX = [0.0]*5
    SIZRAT = [0.0]*2
    TURADD, TURSLA, TURXFR = [0.0]*4
    VSSINK = [0.0]*6
    XFRMAX = [0.0]*5
    XFRT, XFRUIT = [0.0]*3

    FNSDT = np.zeros(4)
    XVGROW, YVGROW, YVREF = (np.zeros(6) for _ in range(3))
    XSLATM, YSLATM, XTRFAC, YTRFAC = (np.zeros(10) for _ in range(4))
    XXFTEM, YXFTEM = (np.zeros(10) for _ in range(2))
    XLEAF, YLEAF, YSTEM = (np.zeros(25) for _ in range(3))
    PHTIM, PNTIM = (np.zeros(NCOHORTS) for _ in range(2))
    TURFSL = 0.0

    #CHP - puncture variables, not functional
    RPRPUN = [0.0]*3

    #***********************************************************************
    #     Run Initialization - Called once per simulation
    #***********************************************************************
    if DYNAMIC == RC.RUNINIT:
        (CARMIN, FINREF, FNSDT, FRLFF, FRLFMX, FRSTMF, LIPOPT, LIPTB, LNGSH,
         NMOBMX, NRCVR, NVSMOB, PLIGSD, PMINSD, POASD, PROLFF, PROLFI, PRORTF,
         PRORTI, PROSTF, PROSTI, RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA, RPRO,
         SDLIP, SDPRO, SHLAG, SLAMAX, SLAMIN, SLAPAR, SLAREF, SLAVAR, SLOSUM,
         SIZELF, SIZREF, SRMAX, THRESH, TURSLA, TYPSDT, VSSINK, XFRMAX, XFRUIT,
         XLEAF, XSLATM, XTRFAC, XVGROW, XXFTEM, YLEAF, YSLATM, YSTEM, YTRFAC,
         YVREF, YXFTEM) = (IPDMND(FILECC, FILEGC, FILEIO))

        #***********************************************************************
        #     Seasonal initialization - run once per season
        #***********************************************************************
    elif DYNAMIC == RC.SEASINIT:
        CDMSDR = 0.0
        GDMSDR = 0.0
        FNINSD = 0.0
        NDMNEW = 0.0
        NDMREP = 0.0
        NDMSD = 0.0
        NDMSH = 0.0
        NDMSDR = 0.0
        NDMVEG = 0.0
        NMOBR = 0.0
        SDGR = 0.0
        FNINL = 0.0
        FNINS = 0.0
        FNINR = 0.0
        NMINEP = 0.0

        RPRPUN = 1.0
        TMPFAC = 1.0

        if CROP != 'FA':
            DUMFAC = SLAVAR / SLAREF
            F = DUMFAC * FINREF
            FVEG = DUMFAC * SLAMAX
            SLAMN = DUMFAC * SLAMIN
            SLAMX = DUMFAC * SLAMAX
            GROMAX = 0.0
            SIZRAT = SIZELF / SIZREF

            for i in range(1,7):
                YVGROW[i] = SIZRAT * YVREF[i]

            FRLF = TABEX(YLEAF, XLEAF, 0.0, 8)
            FRSTM = TABEX(YSTEM, XLEAF, 0.0, 8)
            FRRT = 1.0 - FRLF - FRSTM

        #***********************************************************************
        #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
        #         or transplanting of plants
        #***********************************************************************
    elif DYNAMIC == RC.EMERG:
        XFRT = XFRUIT
        ADDSHL = 0.0
        TURXFR = 0.0
        GDMSD = 0.0
        CDMSD = 0.0
        NDMSD = 0.0
        GDMSDR = 0.0
        CDMSDR = 0.0
        NDMSDR = 0.0
        CDMREP = 0.0
        NAGE = 0

        for NPP in range(1,NCOHORTS + 1):
            PHTIM[NPP] = 0.0
            PNTIM[NPP] = 0.0

        FNINSD = SDPRO * 0.16
        FNINL = PROLFI * 0.16
        FNINS = PROSTI * 0.16
        FNINR = PRORTI * 0.16

        #***********************************************************************
        #     DAILY RATE/INTEGRATION
        #***********************************************************************
    elif DYNAMIC == RC.INTEGR:
        # -----------------------------------------------------------------------
        # DAS = MAX(0,TIMDIF(YRSIM,YRDOY))
        GET_CONTROL(CONTROL)
        DAS = CONTROL.DAS
        # -----------------------------------------------------------------------
        # Compute max N mining, NMINEP, based on stage-dependent mining
        # rate, NMOBR
        # -----------------------------------------------------------------------
        # Assume that a Maximum Fraction (NMOBMX) of N can be Mobilized per Day
        # NVSMOB is the relative N mobil rate in veg stage, rel to reprod. stage
        # -----------------------------------------------------------------------

        NMOBR = NVSMOB * NMOBMX * TDUMX
        if DAS > NR5:
            NMOBR = NMOBMX * TDUMX2 * (1.0 + 0.5 * (1.0 - SWFAC)
                    * (1.0 + 0.3 * (1.0 - NSTRES)) * (NVSMOB + (1. - NVSMOB)
                    * max(XPOD, DXR57 ** 2.))

        NMINEP = NMOBR * (WNRLF + WNRST + WNRRT + WNRSH)

        # -----------------------------------------------------------------------
        if DAS >= NR1:
            # -----------------------------------------------------------------------
            # Accumulate physiological age of flower (PNTIM) and pod (PHTIM) cohorts
            # -----------------------------------------------------------------------
            if (DAS - NR1 + 1) > NCOHORTS:
                MSG[1] = f'Number of flower cohorts exceeds maximum limit of {NCOHORTS}'
                WARNING(1, ERRKEY, MSG)
                ErrorCode(CONTROL, 100, ERRKEY, YREND)
                CropStatus = 100
                return

            if DAS == NR1:
                PNTIM[0] = 0.0
            else:
                PNTIM[DAS - NR1+ 1] = PNTIM[DAS - NR1] + TDUMX

            if DAS <= NR2:
                PHTIM[0] = 0.0
            else:
                PHTIM[DAS - NR2+ 1] = PHTIM[DAS - NR2] + TDUMX

            # -----------------------------------------------------------------------
            # Calculate function for modifying seed growth rate with temperature
            # -----------------------------------------------------------------------
            TMPFAC = 0.0
            TMPFCS = 0.0

            for i in range(1, TS + 1):
                TMPFAC = curv(TYPSDT, FNSDT[0], FNSDT[1], FNSDT[2], FNSDT[3], TGRO[i])
                TMPFCS += TMPFAC

            TMPFAC = TMPFCS / float(TS)

            # -----------------------------------------------------------------------
            # Calculate reduction in seed growth due to insect punctures
            # -----------------------------------------------------------------------
            if PUNCSD > 0.0:
                REDPUN = 1.0 - (PUNCTR / PUNCSD) * RPRPUN
                REDPUN = max(0.0, REDPUN)
            else:
                REDPUN = 1.0

            # -----------------------------------------------------------------------
                # Water stress factor (TURADD) effect on reproductive growth and
                # pod addition. Stress is defined to INCREASE growth and addition.
                # -----------------------------------------------------------------------
            TURADD = TABEX(YTRFAC, XTRFAC, TURFAC, 4)

            # -----------------------------------------------------------------------
            # Calculate maximum growth per seed based on temp and seed punctures
            # -----------------------------------------------------------------------
            SDGR = SDVAR * TMPFAC * REDPUN * (1. - (1. - DRPP) * SRMAX) * (1. + TURADD)

            # -----------------------------------------------------------------------
            # Initialize Seed Growth Demands and CH2O and N required for seed growth
            # -----------------------------------------------------------------------
            GDMSD = CDMSD = NDMSD = GDMSDR = CDMSDR = NDMSDR = 0.0

            # -----------------------------------------------------------------------
            if DAS > NR2:
                for NPP in range(1, DAS - NR2 + 1):
                    # -----------------------------------------------------------------------
                    # Calculate physiol age of seed cohort. Do not allow seed to grow
                    # until shells are greater than LAGSD physiol age.
                    # -----------------------------------------------------------------------
                    PAGE = PHTIM[DAS - NR2] - PHTIM[NPP]

                    if PAGE >= LAGSD:
                        # -----------------------------------------------------------------------
                        # Allow cohort growth until threshing limit (seed wt./pod wt) occurs
                        # taking into account damage by pests to seed and shells
                        # -----------------------------------------------------------------------
                        REDSHL = 0
                        if SDDES[NPP] > 0:
                            REDSHL = WTSHE[NPP] * SDDES[NPP] / (SDDES[NPP] + SDNO[NPP])

                        SDMAX = (WTSHE[NPP] - REDSHL) * THRESH / (100. - THRESH) - WTSD[NPP]
                        SDMAX = max(0.0, SDMAX)

                        # -----------------------------------------------------------------------
                        # Compute Seed Growth Demand, GDMSD, and N required for seed, NDMSD
                        # -----------------------------------------------------------------------
                        GDMSD += min(SDGR * SDNO[NPP] * REDPUN, SDMAX)

                # -----------------------------------------------------------------------
                # Call seed composition routine
                # -----------------------------------------------------------------------
                AGRSD1, AGRSD2,FNINSD, POTCAR, POTLIP = SDCOMP(
                    CARMIN, LIPOPT, LIPTB, PLIGSD, PMINSD, POASD,
                    RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA, SDLIP,
                    SDPRO, SLOSUM, TAVG
                )

                NDMSD = FNINSD * GDMSD

                # -----------------------------------------------------------------------
                # Calculate Amount of Mobilized N Which Can be Used for Seed Growth
                # NDMSDR, potential seed growth from this source of N, GDMSDR,
                #     and CH2O required for this seed growth from mobilized N, CDMSDR
                # -----------------------------------------------------------------------
                NDMSDR = min(NDMSD, NMINEP)
                GDMSDR = NDMSDR / FNINSD
                CDMSDR = GDMSDR * (AGRSD1 + FNINSD * 6.25 * RPRO)

                # -----------------------------------------------------------------------
                # Compute Total CH2O Demand to Grow GDMSD g Tissue
                # -----------------------------------------------------------------------
                CDMSD = max(0.0, (GDMSD - GDMSDR)) * AGRSD2 + CDMSDR

            # Compute max growth per shell, depending on temp, daylength
            GRRAT1 = SHVAR * TMPFAC * (1.0 - (1.0 - DRPP) * SRMAX) * (1.0 + TURADD)

            # Initialize Shell Growth Demand, N (NDMSH) and C (CDMSH) needed for growth
            GDMSH = 0.0
            NDMSH = 0.0
            CDMSH = 0.0

            # Compute growth demand for shells, GDMSH, allowing slow growth
            # until LNGPEG age, then potential growth until LNGSH
            if DAS > NR2:
                for NPP in range(1, DAS - NR2+1):
                    NAGE = DAS - NR2 - NPP  # NAGE not used - chp
                    PAGE = PHTIM[DAS - NR2] - PHTIM[NPP]

                    # Correction for small seeded crops like chia
                    if PAGE <= LNGSH and SHELN[NPP] > 0.0 and GRRAT1 > 0.0:
                        if PAGE >= LNGPEG:
                            # Shells between LNGPEG and LNGSH
                            ADDSHL = GRRAT1 * SHELN[NPP]
                        else:
                                # Shells < LNGPEG
                            ADDSHL = GRRAT1 * SHELN[NPP] * SHLAG

                        GDMSH += ADDSHL

                    # Compute CH2O required for the potential shell growth
                CDMSH = GDMSH * AGRSH2

                # Compute TEMXFR, the temp effect on partitioning to pods
                # High temp would increase fraction growth to vegetative tissue
            TEMXFR = 0.
            TEMXFR = sum(TABEX(YXFTEM, XXFTEM, TGRO[i], 6) for i in range(TS)) / float(TS)

            # Partitioning to pods is increased under drought stress conditions
            # depending on XFRMAX, an input parameter
            TURXFR = XFRMAX * (1.0 - TURFAC)
            TURXFR = min(TURXFR, 1.0)
            TURXFR = max(TURXFR, 0.0)

            if XFPHT > 0.0 and XFINT >= 0.0 and NPP <= NCOHORTS:
                if PHTIM[NPP] <= XFPHT:
                    XFINT = min(XFINT, 1.0)
                    AXFINT = max(0.0, 1.0 - XFINT)
                    XFRUIT = (XFRUIT2 / XFPHT * PHTIM[NPP] * AXFINT) \
                    + (XFINT * XFRUIT2)

            # Night length and temperature are multiplicative
            # but turgor effect adds to the partitioning
            XFRT = XFRUIT * TEMXFR + XFRUIT * TURXFR
            XFRT = min(XFRT, 1.0)
            XFRT = max(XFRT, 0.0)

            # Total Potential Available CH2O for Reproductive Growth (CAVTOT)
            # and total CH2O needed for potential reproductive growth (CDMREP)
            CAVTOT = PGAVL * XFRT
            CDMREP = CDMSH + CDMSD

            # Adjust C-Demand for New Growth if C-Available is Less than C Demand
            # Also adjust tissue growth demand for seeds and shells
            GDMSDO = GDMSD
            if CDMREP > CAVTOT:
                if CDMSD > CAVTOT:
                    CDMSH = 0.0
                    GDMSH = 0.0
                    CDMSD = CAVTOT
                    if CDMSDR > CAVTOT:
                            CDMSDR = CAVTOT

                    GDMSD = (max(0.0, (CDMSD - CDMSDR)) / AGRSD2 +
                             CDMSDR / (AGRSD1 + FNINSD * 6.25 * RPRO))
                    NDMSDR = GDMSDR * FNINSD
                else:
                    CDMSH = CAVTOT - CDMSD
                    GDMSH = CDMSH / AGRSH2

                CDMREP = CDMSD + CDMSH

            # Compute N demand for seed, shell, and total reproductive growth
            NDMSD = GDMSD * FNINSD
            NDMSH = GDMSH * FNINSH
            NDMREP = NDMSD + NDMSH

            # Vegetative partitioning factors and demand for C and N for new
            # growth before VSSINK, assume leaf expansion is fixed, compute
            # SLA based on function of light, temp, etc, then compute
            # FRLF (leaf partitioning), then FRRT, FRSTM

            # Check to See if New Vegetative Tissue Can Be Grown, Using PGAVL
            CDMVEG = max(0.0, (1.0 - XFRT) * PGAVL)
            NDMVEG = 0.0
            CDMVEG = (PGAVL * XFRT - CDMREP) + CDMVEG

            if DAS == NR1:
                # Fraction of growth going to leaves and roots decreases
                # linearly between R1 and NDLEAF.
                FRLFM = TABEX(YLEAF, XLEAF, VSTAGE, 8)
                FRSTMM = TABEX(YSTEM, XLEAF, VSTAGE, 8)
                YY = FRLFM - FRLFF
                XX = FRSTMM - FRSTMF

            if DAS < NR1:
                # Calculate Pattern of Vegetative Partitioning, a function of V-STAGE
                FRLF = TABEX(YLEAF, XLEAF, VSTAGE, 8)
                FRSTM = TABEX(YSTEM, XLEAF, VSTAGE, 8)
            else:
                # Partitioning between vegetative tissues depends on development
                # as expressed by FRACDN, the relative development between R1 and NDLEAF
                FRLF = FRLFM - YY * FRACDN
                FRSTM = FRSTMM - XX * FRACDN
                if DAS >= NDLEAF:
                    FRLF = FRLFF
                    FRSTM = FRSTMF

            FRRT = 1.0 - FRLF - FRSTM

            # Compute F, specific leaf area for new leaf weight
            TPHFAC = 0.
            TPHFAC = sum(TABEX(YSLATM, XSLATM, TGRO[i], 5) for i in range(TS)) / float(TS)

            PARSLA = (SLAMN + (SLAMX - SLAMN) * np.exp(SLAPAR * PAR)) / SLAMX
            TURFSL = max(0.1, (1.0 - (1.0 - TURFAC) * TURSLA))

#-----------------------------------------------------------------------
#     Nitrogen effect by KJB
#-----------------------------------------------------------------------
            if NSLA > 1.2:                      #To limit NSLA to 1.2
                NSLA=1.2
            NFSL   = MAX(0.1, (1.0 - (1.0 - NSTRES)*NSLA))
            CUMNSF = 0.75*CUMNSF + 0.25*NFSL

            # Compute overall effect of TMP, PAR, water stress on SLA (F), first
            # for veg stages, then transition to rep stage from R1 to end leaf
            FFVEG = FVEG * TPHFAC * PARSLA * TURFSL

            F = FFVEG
            if XFRT * FRACDN >= 0.05:
                F = FFVEG * (1.0 - XFRT * FRACDN)

            if XFRUIT > 0.9999 and DAS >= NDLEAF:
                F = 0.0

            if VSTAGE < VSSINK:
                GROYES = GROMAX
                GROMAX = TABEX(YVGROW, XVGROW, VSTAGE, 6) * SIZELF / SIZREF
                GAINNW = (GROMAX - GROYES) * PLTPOP

                # Calculate minimum weight needed to add GAINNW leaf area/m²
                # and the amount of leaf weight which can be grown with PG available
                if F > 1.0e-5:
                    GAINWT = GAINNW / F
                else:
                    GAINWT = 0.0

                # Compute fraction of C partitioned to leaves, based on F, VSSINK
                # Limit leaf partitioning to FRLFMX (i.e., FRLFMX = 0.7)
                FRLF = (AGRLF * GAINWT) / (CDMVEG + 0.0001)
                if FRLF > FRLFMX:
                    GAINWT = (CDMVEG / AGRLF) * FRLFMX
                    GAINNW = GAINWT * F
                    FRLF = FRLFMX

                # Recompute FRSTM and FRRT based on FRLF
                FRSTM = (1.0 - FRLF) * FRSTM / (FRSTM + FRRT)
                FRRT = 1.0 - FRLF - FRSTM

                # Compute CH2O cost per g of tissue, excluding cost for protein (AGRVG)
                # and total CH2O cost per g of veg tissue (AGRVG2)
            AGRVG = AGRLF * FRLF + AGRRT * FRRT + AGRSTM * FRSTM
            AGRVG2 = AGRVG + (FRLF * PROLFI + FRRT * PRORTI + FRSTM * PROSTI) * RPROAV

            # Compute N Demand for New Tissue, including reproductive and vegetative
            NDMVEG = (CDMVEG / AGRVG2) * (FRLF * FNINL + FRSTM * FNINS + FRRT * FNINR)
            NDMNEW = NDMREP + NDMVEG

            # Check to See if Any C is Left After Reproductive Growth for
            # Reducing N to Re-Fill Old Tissue, if N Can Be Taken up by Roots
            CNOLD = max(0.0, PGAVL - CDMREP)
            NDMOLD = 0.0

            #Nitrogen Demand for Old Tissue
            if NVEG0 < DAS < NR7 and CNOLD > 0.0:
                NVSTL = FNINL
                NVSTS = FNINS
                NVSTR = FNINR
                if DXR57 > 0.0:
                    FRNLFT = (NRCVR + (1. - NRCVR) * (1. - DXR57**2))
                    NVSTL = PROLFF*0.16 + (FNINL-PROLFF*0.16) * FRNLFT
                    NVSTS = PROSTF*0.16 + (FNINS-PROSTF*0.16) * FRNLFT
                    NVSTR = PRORTF*0.16 + (FNINR-PRORTF*0.16) * FRNLFT
                NDMOLD = (
                    (WTLF  - WCRLF) * max(0.0,(NVSTL - PCNL /100.))
                    + (STMWT - WCRST) * max(0.0,(NVSTS - PCNST/100.))
                    + (RTWT  - WCRRT) * max(0.0,(NVSTR - PCNRT/100.))
                    )
                if NDMOLD > (CNOLD/RNO3C*0.16):
                        NDMOLD = CNOLD/RNO3C*0.16

                # Total N Demand
            NDMTOT = NDMREP + NDMVEG + NDMOLD

            # Compute Total Demand for C, and Max. C that Could be Mined
            CDMTOT = CDMREP + CDMVEG + NDMOLD * RNO3C / 0.16
            GDMSD = GDMSDO

    return  (AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, CropStatus, F, FNINL,
             FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD, GRRAT1,
             NDMNEW,  NDMOLD, NDMREP, NDMSDR, NDMTOT, NDMVEG, NMINEP,
             NMOBR, PHTIM, PNTIM, POTCAR, POTLIP, SDGR, TURADD, XFRT, YREND)
#=======================================================================

#=======================================================================
#  IPDMND, Subroutine
#-----------------------------------------------------------------------
#  Reads input data for DEMAND subroutine
#-----------------------------------------------------------------------
#  Called by:  DEMAND
#  Calls:      FIND, ERROR, IGNORE
#=======================================================================
def IPDMND(FILECC, FILEGC, FILEIO):
    from ERROR import ERROR
    from READS import FIND, IGNORE
    import fortranformat as ff
    import numpy as np
#       IMPLICIT NONE
#       EXTERNAL GETLUN, ERROR, FIND, IGNORE, WARNING
# !-----------------------------------------------------------------------
#       CHARACTER*3   TYPSDT
#       CHARACTER*6   ERRKEY
    ERRKEY = 'IPDMND'
#       CHARACTER*6   SECTION
#       CHARACTER*6   ECOTYP, ECONO
#       CHARACTER*30  FILEIO
#       CHARACTER*78  MSG(4)
    MSG = np.empty(4, dtype=str)
#       CHARACTER*80  C80
#       CHARACTER*92  FILECC, FILEGC
#       CHARACTER*255 C255
#
#       INTEGER LUNCRP, LUNIO, LUNECO, ERR, LINC, LNUM, FOUND, ISECT
#       INTEGER I, II
#
#       REAL CARMIN, FINREF, FRLFF, FRLFMX, FRSTMF,
#      &  LIPOPT, LIPTB, NMOBMX, NRCVR, NVSMOB,
#      &  PLIGSD, PMINSD, POASD, PROLFF,
#      &  PROLFI, PRORTF, PRORTI, PROSTF, PROSTI,
#      &  RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,
#      &  RPRO, SHLAG, SLAMAX, SLAMIN, SLAPAR,
#      &  SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF,
#      &  SRMAX, TURSLA, VSSINK, XFRMAX, XFRUIT
#         REAL LNGSH, THRESH, SDPRO, SDLIP, XFPHT, XFINT
#         REAL NSLA
#
#         REAL FNSDT(4)
    FNSDT = np.full(5, '-99.0', dtype=float)
#         REAL XVGROW(6), YVREF(6)
    XVGROW = np.full(7, '-99.0', dtype=float)
    YVREF = np.full(7, '-99.0', dtype=float)


#         REAL XSLATM(10), YSLATM(10), XTRFAC(10), YTRFAC(10),
#      &                  XXFTEM(10), YXFTEM(10)
    YTRFAC = np.full(11, '-99.0', dtype=float)
    XTRFAC = np.full(11, '-99.0', dtype=float)
    YXFTEM = np.full(11, '-99.0', dtype=float)
    XXFTEM = np.full(11, '-99.0', dtype=float)
    XSLATM = np.full(11, '-99.0', dtype=float)
    YSLATM = np.full(11, '-99.0', dtype=float)
#         REAL XLEAF(25), YLEAF(25), YSTEM(25)

    XLEAF = np.full(25,'-99.0', dtype=float)
    YLEAF = np.full(25, '-99.0', dtype=float)
    YSTEM = np.full(25, '-99.0', dtype=float)
#-----------------------------------------------------------------------
    # CALL GETLUN('FILEIO', LUNIO)
    # OPEN (LUNIO, FILE = FILEIO,STATUS = 'OLD',IOSTAT=ERR)
    try:
        with open(FILEIO, 'r') as f:
            LUNIO = f.readlines()

    # if ERR != 0 : ERROR(ERRKEY,ERR,FILEIO,0)
            LNUM = 0
#-----------------------------------------------------------------------
#    Find and Read Field Section from FILEIO - previously read in IPIBS
#       Look for the second section header beginning with '*CULTI'
#-----------------------------------------------------------------------
            SECTION = '*CULTI'
            LINC, FOUND = FIND(LUNIO, SECTION )
            LNUM = LNUM + LINC
            if FOUND == 0:
                ERROR(SECTION, 42, FILEIO, LNUM)

            LINC, FOUND = FIND(LUNIO, SECTION)
            LNUM = LNUM + LINC
            if FOUND == 0:
                ERROR(SECTION, 42, FILEIO, LNUM)
            else:
                #f10
                # READ(LUNIO,'(24X,A6,48X,3F6.0,24X,3F6.0)',IOSTAT=ERR) ECONO, SLAVAR, SIZELF, XFRUIT, THRESH, SDPRO, SDLIP
                f10 = ff.FortranRecordReader("24X,A6,48X,3F6.0,24X,3F6.0")
                ECONO, SLAVAR, SIZELF, XFRUIT, THRESH, SDPRO, SDLIP = f10.read(LUNIO[LNUM])
                LNUM = LNUM + 1
                # if ERR != 0: ERROR(ERRKEY,ERR,FILEIO,LNUM)

    # CLOSE (LUNIO)
    except OSError:
        ERROR(ERRKEY, -99, FILEIO, 0)
        return
#-----------------------------------------------------------------------
#     Read in values from species file
#-----------------------------------------------------------------------
    # CALL GETLUN('FILEC', LUNCRP)
    # OPEN (LUNCRP,FILE = FILECC, STATUS = 'OLD',IOSTAT=ERR)
    try:
        with open(FILECC, 'r') as f:
            LUNCRP = f.readlines()

            LNUM = 0
            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)
#-----------------------------------------------------------------------
#    Find and Read Respiration Section
#-----------------------------------------------------------------------
            SECTION = '!*RESP'
            LINC, FOUND = FIND(LUNCRP, SECTION)
            LNUM = LNUM + LINC
            if FOUND == 0:
                ERROR(SECTION, 42, FILECC, LNUM)
            else:
                LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                # READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) RNO3C, RPRO
                f20 = ff.FortranRecordReader("F6.0,6X,F6.0")
                RNO3C, RPRO = f20.read(C80)
                # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                # READ(C80,'(5F6.0)',IOSTAT=ERR)RCH2O,RLIP,RLIG,ROA,RMIN
                f30 = ff.FortranRecordReader("5F6.0")
                RCH2O, RLIP, RLIG, ROA, RMIN = f30.read(C80)
                # ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

#-----------------------------------------------------------------------
#    Find and Read Plant Composition Section
#-----------------------------------------------------------------------
                SECTION = '!*PLAN'
                LINC, FOUND = FIND(LUNCRP, SECTION)
                LNUM = LNUM + LINC
                if FOUND == 0 :
                    ERROR(SECTION, 42, FILECC, LNUM)
                else:
                    LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                    # READ(C80,'(F6.0,6X,2F6.0,6X,F6.0)',IOSTAT=ERR) PROLFI, PROLFF, PROSTI, PROSTF
                    f40 = ff.FortranRecordReader("F6.0,6X,2F6.0,6X,F6.0")
                    PROLFI, PROLFF, PROSTI, PROSTF = f40.read(C80)

                    if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                    LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                    # READ(C80,'(F6.0,6X,F6.0)',IOSTAT=ERR) PRORTI, PRORTF
                    f50 = ff.FortranRecordReader("F6.0,6X,F6.0")
                    PRORTI, PRORTF = f50.read(C80)

                    if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                    LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                    LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                    LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                    LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                    # READ(C80,'(24X,F6.0)',IOSTAT=ERR) PLIGSD
                    f60 = ff.FortranRecordReader("24X,F6.0")
                    PLIGSD = f60.read(C80)

                    if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                    LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                    # READ(C80,'(24X,F6.0)',IOSTAT=ERR) POASD
                    f70 = ff.FortranRecordReader("24X,F6.0")
                    POASD = f70.read(C80)

                    if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                    LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                    # READ(C80,'(24X,F6.0)',IOSTAT=ERR) PMINSD
                    f80 = ff.FortranRecordReader("24X,F6.0")
                    PMINSD = f80.read(C80)

                    # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

#-----------------------------------------------------------------------
#    Find and Read Seed Composition Section
#-----------------------------------------------------------------------
                    SECTION = '!*SEED'
                    LINC, FOUND = FIND(LUNCRP, SECTION)
                    LNUM = LNUM + LINC
                    if FOUND == 0:
                        ERROR(SECTION, 42, FILECC, LNUM)
                    else:
                        LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                        # READ(C80,'(4F6.0)',IOSTAT=ERR) LIPTB, LIPOPT, SLOSUM, CARMIN
                        f90 = ff.FortranRecordReader("4F6.0")
                        LIPTB, LIPOPT, SLOSUM, CARMIN = f90.read(C80)

                        if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)
                        SLOSUM = SLOSUM / 100.0

#-----------------------------------------------------------------------
#    Find and Read Carbon and Nitrogen Mining Section
#-----------------------------------------------------------------------
                        SECTION = '!*CARB'
                        LINC, FOUND = FIND(LUNCRP, SECTION)
                        LNUM = LNUM + LINC
                        if FOUND == 0:
                            ERROR(SECTION, 42, FILECC, LNUM)
                        else:
                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(18X,3F6.0)',IOSTAT=ERR) NMOBMX, NVSMOB, NRCVR
                            f100 = ff.FortranRecordReader("18X,3F6.0")
                            NMOBMX, NVSMOB, NRCVR = f100.read(C80)

                            if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

#-----------------------------------------------------------------------
#    Find and Read Vegetative Partitioning Section
#-----------------------------------------------------------------------
                        SECTION = '!*VEGE'
                        LINC, FOUND = FIND(LUNCRP, SECTION)
                        LNUM = LNUM + LINC
                        if FOUND == 0:
                            ERROR(SECTION, 42, FILECC, LNUM)
                        else:
                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(8F6.0)',IOSTAT=ERR)(XLEAF(II),II=1,8)
                            f110 = ff.FortranRecordReader("8F6.0")
                            XLEAF[1:9] = f110.read(C80)

                            if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(8F6.0)',IOSTAT=ERR)(YLEAF(II),II=1,8)
                            YLEAF[1:9] = f110.read(C80)

                            if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(8F6.0)',IOSTAT=ERR)(YSTEM(II),II=1,8)
                            YSTEM[1:9] = f110.read(C80)

                            if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(12X,2F6.0)',IOSTAT=ERR) FRSTMF, FRLFF
                            f120 = ff.FortranRecordReader("12X,2F6.0")
                            FRSTMF, FRLFF = f120.read(C80)

                            if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(F6.0)',IOSTAT=ERR) FRLFMX
                            f130 = ff.FortranRecordReader("F6.0")
                            FRLFMX = f130.read(C80)

                            if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

#-----------------------------------------------------------------------
#    Find and Read Leaf Growth Section
#-----------------------------------------------------------------------
                        SECTION = '!*LEAF'
                        LINC, FOUND = FIND(LUNCRP, SECTION)
                        LNUM = LNUM + LINC
                        if FOUND == 0:
                            ERROR(SECTION, 42, FILECC, LNUM)
                        else:
                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(4F6.0)',IOSTAT=ERR) FINREF, SLAREF, SIZREF, VSSINK
                            f140 = ff.FortranRecordReader("4F6.0")
                            FINREF, SLAREF, SIZREF, VSSINK = f140.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(6F6.0)',IOSTAT=ERR) SLAMAX, SLAMIN, SLAPAR, TURSLA, NSLA
                            f150 = ff.FortranRecordReader("6F6.0")
                            SLAMAX, SLAMIN, SLAPAR, TURSLA, NSLA = f150.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(6F6.0)',IOSTAT=ERR)(XVGROW(II),II=1,6)
                            f160 = ff.FortranRecordReader("6F6.0")
                            XVGROW[1:7] = f160.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(6F6.0)',IOSTAT=ERR)(YVREF(II),II=1,6)
                            f170 = ff.FortranRecordReader("6F6.0")
                            YVREF[1:7] = f170.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(5F6.0)',IOSTAT=ERR)(XSLATM(II),II = 1,5)
                            f180 = ff.FortranRecordReader("5F6.0")
                            XSLATM[1:6] = f180.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(5F6.0)',IOSTAT=ERR)(YSLATM(II),II = 1,5)
                            YSLATM[1:6] = f180.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

#-----------------------------------------------------------------------
#    Find and Read Seed and Shell Growth Section
#-----------------------------------------------------------------------
                        SECTION = '!*SEED'
                        LINC, FOUND = FIND(LUNCRP, SECTION)
                        LNUM = LNUM + LINC
                        if FOUND == 0:
                            ERROR(SECTION, 42, FILECC, LNUM)
                        else:
                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(6X,F6.0)',IOSTAT=ERR) SRMAX
                            f190 = ff.FortranRecordReader("6X,F6.0")
                            SRMAX = f190.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(6X,2F6.0)',IOSTAT=ERR) XFRMAX, SHLAG
                            f200 = ff.FortranRecordReader("6X,2F6.0")
                            XFRMAX, SHLAG = f200.read(C80)

                            # ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(4(1X,F5.2),3X,A3)',IOSTAT=ERR) (FNSDT(II),II=1,4), TYPSDT
                            f210 = ff.FortranRecordReader("4(1X,F5.2),3X,A3")
                            FNSDT[1:5], TYPSDT = f210.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(6F6.0)',IOSTAT=ERR)(XXFTEM(II),II = 1,6)
                            f220 = ff.FortranRecordReader("6F6.0")
                            XXFTEM[1:7] = f220.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(6F6.0)',IOSTAT=ERR)(YXFTEM(II),II = 1,6)
                            YXFTEM[1:7] = f220.read(C80)
                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            for I in range(1,6):
                                LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)

                            # READ(C80,'(4F6.0)',IOSTAT=ERR)(XTRFAC(II),II = 1,4)
                            f230 = ff.FortranRecordReader("4F6.0")
                            XTRFAC[1:5] = f230.read(C80)

                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)

                            LNUM,ISECT,C80 = IGNORE(LUNCRP,LNUM)
                            # READ(C80,'(4F6.0)',IOSTAT=ERR)(YTRFAC(II),II = 1,4)
                            YTRFAC[1:5] = f230.read(C80)
                            # if ERR != 0: ERROR(ERRKEY,ERR,FILECC,LNUM)
    except OSError:
        ERROR(ERRKEY, -99, FILEIO, 0)
        return
#-----------------------------------------------------------------------
#     CLOSE(LUNCRP)

#-----------------------------------------------------------------------
#    Read Ecotype Parameter File
#-----------------------------------------------------------------------
        # CALL GETLUN('FILEE', LUNECO)
        # OPEN (LUNECO,FILE = FILEGC,STATUS = 'OLD',IOSTAT=ERR)

    try:
        with open(FILEGC, 'r') as f:
            LUNECO = f.readlines()

            # if ERR != 0: ERROR(ERRKEY,ERR,FILEGC,0)
            ECOTYP = '      '
            LNUM = 0
            while True:
                LNUM, ISECT, C255 = IGNORE(LUNECO, LNUM)
                if ((ISECT == 1) and (C255(1:1) != ' ') and (C255(1:1) != '*')):
    #               READ (C255,'(A6,66X,F6.0,30X,3F6.0)',IOSTAT=ERR) ECOTYP, LNGSH, THRESH, SDPRO, SDLIP
    #                 READ (C255,'(A6,66X,F6.0,54X,2(F6.0))',IOSTAT=ERR) ECOTYP,LNGSH, XFPHT, XFINT
                    f240 = ff.FortranRecordReader("A6,66X,F6.0,54X,2(F6.0)")
                    ECOTYP,LNGSH, XFPHT, XFINT = f240.read(C80)

                    # if ERR != 0: ERROR(ERRKEY,ERR,FILEGC,LNUM)
                    if ECOTYP == ECONO:
                        return

                elif ISECT == 0:
                    if ECONO == 'DFAULT': ERROR(ERRKEY,35,FILEGC,LNUM)
                    ECONO = 'DFAULT'
                    # REWIND(LUNECO)
                    LNUM = 0
                if ECOTYP == ECONO:
                    break

            if XFPHT < 0.0:
                MSG[1] = 'Ecotype coefficient is not properly defined.'
                MSG[2] = 'Time required to reach maximum partitioning to '
                MSG[3] = 'pod/fruit. (photothermal days)'
                MSG[4] = 'XFPHT must be greater then 0.0.'
                WARNING (4, ERRKEY, MSG)
                ERROR(ERRKEY,1,FILEGC,0)
            elif (XFINT < 0.0 or XFINT > 1.0):
                MSG[1] = 'Ecotype Coefficients is not properly defined.'
                MSG[2] = 'Initial partitioning to pod/fruit during early '
                MSG[3] = 'pod/fruit growth.'
                MSG[4] = 'XFINT must be between/included 0.0 and 1.0.'
                WARNING (4, ERRKEY, MSG)
                ERROR(ERRKEY,2,FILEGC,0)
    except OSError:
        ERROR(ERRKEY, -99, FILEIO, 0)
        return

    return (CARMIN, FINREF, FNSDT, FRLFF, FRLFMX,FRSTMF, LIPOPT, LIPTB, LNGSH, NMOBMX,
        NRCVR, NVSMOB, PLIGSD, PMINSD, POASD,PROLFF, PROLFI, PRORTF, PRORTI, PROSTF,
        PROSTI, RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA,RPRO, SDLIP, SDPRO, SHLAG, SLAMAX,
        SLAMIN,SLAPAR, SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF, SRMAX, THRESH, TURSLA,
        TYPSDT, VSSINK, XFRMAX, XFRUIT, XLEAF, XSLATM, XTRFAC, XVGROW, XXFTEM, YLEAF,
        YSLATM, YSTEM, YTRFAC, YVREF, YXFTEM, XFPHT, XFINT, NSLA)
#=======================================================================



# !=======================================================================
#
# !=======================================================================
# !  IPDMND, Subroutine,
# !-----------------------------------------------------------------------
# !  Reads input data for DEMAND subroutine
# !-----------------------------------------------------------------------
# !  Called by:  DEMAND
# !  Calls:      FIND, ERROR, IGNORE
# !=======================================================================
# def IPDMND(FILECC, FILEGC, FILEIO):
#     #Input e.g. \TMGRO048.SPE, \TMGRO048.ECO, \DSSAT48.INP
#
#     ERRKEY = 'IPDMND'
#
#     SECTION = '*CULTI'
#     with open(FILEIO, 'r') as file:
#         lines = file.readlines()
#     found = 0
#     for i, line in enumerate(lines):
#         if SECTION in line:
#             found += 1
#             if found == 2:
#                 try:
#                     data = lines[i + 1].split()
#                     ECONO = data[0]
#                     SLAVAR, SIZELF, XFRUIT, THRESH, SDPRO, SDLIP = map(float, data[1:])
#                 except:
#                     raise ValueError(f"Error reading {FILEIO} at line {i + 1}")
#                 break
#
#         if found < 2:
#             raise ValueError(f"{SECTION} not found twice in {FILEIO}")
#
#         with open(FILECC, 'r') as file:
#             lines = file.readlines()
#
#         sections = {'!*RESP': [], '!*PLAN': [], '!*SEED': [], '!*CARB': [], '!*VEGE': [], '!*LEAF': [], '!*SEED': []}
#         current_section = None
#
#         for line in lines:
#             if line.strip() in sections:
#                 current_section = line.strip()
#             elif current_section:
#                 sections[current_section].append(line.strip())
#
#         if '!*RESP' in sections:
#             try:
#                 RNO3C, RPRO = map(float, sections['!*RESP'][0].split()[:2])
#                 RCH2O, RLIP, RLIG, ROA, RMIN = map(float, sections['!*RESP'][4].split()[:5])
#             except:
#                 raise ValueError(f"Error reading RESP section in {FILECC}")
#
#         if '!*PLAN' in sections:
#             try:
#                 PROLFI, PROLFF, PROSTI, PROSTF = map(float, sections['!*PLAN'][2].split()[:4])
#                 PRORTI, PRORTF = map(float, sections['!*PLAN'][4].split()[:2])
#                 PLIGSD = float(sections['!*PLAN'][10].split()[0])
#                 POASD = float(sections['!*PLAN'][12].split()[0])
#                 PMINSD = float(sections['!*PLAN'][14].split()[0])
#             except:
#                 raise ValueError(f"Error reading PLAN section in {FILECC}")
#
#         if '!*SEED' in sections:
#             try:
#                 LIPTB, LIPOPT, SLOSUM, CARMIN = map(float, sections['!*SEED'][2].split()[:4])
#                 SLOSUM /= 100.0
#             except:
#                 raise ValueError(f"Error reading SEED section in {FILECC}")
#
#         if '!*CARB' in sections:
#             try:
#                 NMOBMX, NVSMOB, NRCVR = map(float, sections['!*CARB'][2].split()[2:])
#             except:
#                 raise ValueError(f"Error reading CARB section in {FILECC}")
#
#         with open(FILEGC, 'r') as file:
#             lines = file.readlines()
#
#         ECONO = ECONO.strip()
#         ECOTYP = ''
#
#         # for i, line in enumerate(lines):
#         #     if line.strip() and not line.startswith('*'):
#         #         parts = line.split()
#         #         if len(parts) >= 2:
#         #             ECOTYP = parts[0]
#         #             LNGSH = float(parts[1])
#         #             if ECOTYP == ECONO:
#         #                     break
#
#     return (CARMIN, FINREF, FNSDT, FRLFF, FRLFMX, FRSTMF, LIPOPT, LIPTB,
#             LNGSH, NMOBMX, NRCVR, NVSMOB, PLIGSD, PMINSD, POASD, PROLFF,
#             PROLFI, PRORTF, PRORTI, PROSTF, PROSTI, RCH2O, RLIG, RLIP,
#             RMIN, RNO3C, ROA, RPRO, SDLIP, SDPRO, SHLAG, SLAMAX, SLAMIN,
#             SLAPAR, SLAREF, SLAVAR, SLOSUM, SIZELF, SIZREF, SRMAX, THRESH,
#             TURSLA, TYPSDT, VSSINK, XFRMAX, XFRUIT, XLEAF, XSLATM, XTRFAC,
#             XVGROW, XXFTEM, YLEAF, YSLATM, YSTEM, YTRFAC, YVREF, YXFTEM)
#======================================================================
#      Variable definitions for DEMAND and IPDMND
#----------------------------------------------------------------------
# ADDSHL    Today's growth demand for shells of age NPP (g[shell] / m2 / d)
# AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
# AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
# AGRSD1    CH2O requirement for seed growth, excluding cost for protein
#             content (g[CH2O] / g[seed])
# AGRSD2    CH2O requirement for seed growth, including cost for protein
#             content (g[CH2O] / g[seed])
# AGRSH2    CH2O requirement for shell growth, including cost for protein
#             content (g[CH2O] / g[shell])
# AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
# AGRVG     Mass of CH2O required for vegetative tissue growth including
#             stoichiometry and respiration (g[CH2O] / g[tissue])
# AGRVG2    Total mass of CH2O required for vegetative tissue growth
#            (g[CH2O] / g[tissue])
# C255      255-character record read from file
# C80       80-character record read from file
# CARMIN    Minimum carbohydrate fraction
# CAVTOT    Total potential available CH2O for reproductive growth
#            (g[CH2O] / m2)
# CDMREP    Total CH2O needed for potential reproductive growth
#            (g[CH2O] / m2 / d)
# CDMSD     Total CH2O demand to grow seed demand (GDMSD)
#            (g[CH2O] / m2 / d)
# CDMSDR    CH2O required for seed growth from mobilized N
#            (g[CH2O] / m2 / d)
# CDMSH     Total CH2O demand to grow shell demand (GDMSH)
#            (g[CH2O] / m2 / d)
# CDMTOT    Total CH2O demand (g[CH2O] / m2 / d)
# CDMVEG    Carbon demand for vegetative growth (g[CH2O] / m2 / d)
# CNOLD     Available CH2O after reproductive growth (g[CH2O] / m2 / d)
# CROP      Crop identification code
# DAS       Days after start of simulation (d)
# DRPP      Photoperiod days which occur in a real day
#            (photoperiod days / day)
# DXR57     Relative time between first seed (NR5) and physiological
#             maturity (NR7) (fraction)
# ECONO     Ecotype code - used to match ECOTYP in .ECO file
# ECOTYP    Ecotype code for this simulation
# ERR       Error code for file operation
# F         Specific leaf area of new leaf tissue growth, including N
#            (cm2[leaf] / g[leaf])
# FFVEG     Specific leaf area of new leaf tissue growth (interim value)
#            (cm2[leaf] / g[leaf])
# FILECC    Path plus filename for species file (*.spe)
# FILEGC    Pathname plus filename for ECO file
# FINREF    Specific leaf area (SLA) of leaves of standard crop cultivar
#             when plants emerge (cm2[leaf] / g[leaf])
# FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
# FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
# FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
# FNINSD    Maximum fraction of N for growing seed tissue based on
#             temperature (g[N] / g[seed])
# FNINSH    Maximum fraction of N for growing shell tissue
#           (g[N] / g[shell])
# FNSDT(I)  Temperature values which describe function for modifying seed
#             growth rate with temperature (°C)
# FOUND     Indicator that good data was read from file by subroutine FIND
#             (0 - End-of-file encountered, 1 - NAME was found)
# FRACDN    Relative time between flowering (NR1) and last leaf appearance
#             (NDLEAF)
# FRLF      Fraction of vegetative tissue growth that goes to leaves on a
#             day (g[leaf] / g[veg])
# FRLFF     Fraction of daily increase in vegetative weight which goes to
#             leaves after the day on which the maximum number of V-stages
#             occurs (NDVSTG). (g[leaf] / g[veg])
# FRLFM     Fraction of growth going to leaves, decreases linearly between
#             R1 and NDLEAF (g[leaf] / g[veg])
# FRLFMX    Maximum leaf partitioning (g[leaf] / g[veg])
# FRNLFT    A quadratic function of the progress from NR5 to NR7 (DXR57),
#             used to compute the change in maximum tissue N content
#             between its maximum value and a fractional value (NRCVR)
#             between the minimum and maximum tissue  N concentrations
# FRRT      Fraction of vegetative tissue growth that goes to roots on a
#             day (g[root] / g[veg])
# FRSTM     Fraction of vegetative tissue growth that goes to stems on a
#             day (g[stem] / g[veg])
# FRSTMF    Fraction of daily dry weight increase in vegetative plant parts
#             which goes to stems after the day on which the maximum number
#             of V-stages occurs (NDVSTG). (g[stem] / g[veg])
# FRSTMM    Fraction of growth going to stems, decreases linearly between
#             R1 and NDLEAF (g[stem] / g[veg])
# FVEG      Specific leaf area prior to computing effects of temperature,
#             PAR, water stress (cm2[leaf] / g[leaf])
# GAINNW    Leaf area added (prior to VSSINK) (cm2[leaf] / m2[ground])
# GAINWT    Leaf weight added (prior to VSSINK and after NDLEAF)
#            (g[leaf] / m2[ground])
# GDMSD     Seed growth demand based on temperature and photoperiod
#            (g[seed] / m2 / d)
# GDMSDO    Seed growth demand (temporary value) (g[seed] / m2 / d)
# GDMSDR    Potential seed growth from NDMSDR (amount of Mobilized N which
#             can be used for seed growth) (g[seed] / m2 / d)
# GDMSH     Growth demand for shells (g[shell] / m2 / d)
# GROMAX    Maximum leaf area which can be added per plant between
#             emergence and day of simulation as a function of V-stage on
#             day of simulation (cm2[leaf] / plant)
# GROYES    Maximum leaf area which could have been added per plant between
#             emergence and yesterday as a function of V-stage
#             (cm2[leaf] / plant)
# GRRAT1    Maximum growth per individual shell (g / shell / d)
# ISECT     Indicator of completion of IGNORE routine: 0 - End of file
#             encountered, 1 - Found a good line to read, 2 - End of
#             Section in file encountered denoted by * in column 1.
# LAGSD     Time required between shell growth and seed growth, per cohort
#            (Photo-thermal days)
# LINC      Line number of input file
# LIPOPT    Temperature above which lipid composition is at a maximum (°C)
# LIPTB     Temperature below which lipid composition is zero (°C)
# LNGPEG    Time between start of peg (full flower) and shell formation
#             (for peanuts only).  Defines slow growth period.
#             (Photo-thermal days)
# LNGSH     Time required for shell growth (Photo-thermal days)
# LNUM      Current line number of input file
# LUNCRP    Logical unit number for FILEC (*.spe file)
# LUNECO    Logical unit number for FILEE (*.eco file)
# NAGE      Age of cohort (d)
# NDLEAF    Day when leaf expansion ceased (d)
# NDMNEW    Total N demand for new growth (g[N] / m2 / d)
# NDMOLD    N demand for old tissue (g[N] / m2 / d)
# NDMREP    Total N needed for potential reproductive growth
#            (g[N] / m2 / d)
# NDMSD     Total N demand to grow seed demand (GDMSD) (g[N] / m2 / d)
# NDMSDR    Amount of Mobilized N which can be used for seed growth
#            (g[N] / m2 / d)
# NDMSH     Total N demand to grow shell demand (GDMSH) (g[N] / m2 / d)
# NDMTOT    Total N demand (g[N] / m2 / d)
# NDMVEG    N required for vegetative growth if all PGAVL is used as
#             computed (g[N] / m2 / d)
# NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
# NMOBMX    Maximum fraction of N which can be mobilized in a day
# NMOBR     Stage-dependent potential N mining rate expressed as a fraction
#             of the maximum rate (NMOBMX)
# NPP       Cohort number used as index in loops
# NR1       Day when 50% of plants have at least one flower (d)
# NR2       Day when 50% of plants have one fruit (pod or peg) (d)
# NR5       Day when 50% of plants have pods with beginning seeds (d)
# NR7       Day when 50% of plants first have yellowing or maturing pods
#            (d)
# NRCVR     Fractional value between minimum and maximum tissue N values
#             (0-1)
# NSTRES    Nitrogen stress factor (1=no stress, 0=max stress)
# NVEG0     Day of emergence (d)
# NVSMOB    Relative rate of N mining during vegetative stage to that in
#            reproductive stage
# NVSTL     N content in leaves (fraction)
# NVSTR     N content in roots (fraction)
# NVSTS     N content in stems (fraction)
# PAGE      Photothermal age of each cohort (Photo-thermal days)
# PAR       Daily photosynthetically active radiation or photon flux
#             density (moles[quanta]/m2-d)
# PARSLA    Effect of PAR on specific leaf area
# PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
# PCNRT     Percent N in root tissue (100 g[N] / g[root])
# PCNST     Percent N in stem tissue (100 g[N] / g[stem])
# PGAVL     Total available CH2O available for growth & respiration
#            (g[CH2O] / m2)
# PHTIM     Cumulative photothermal time ages of seeds and shells
# PLIGSD    Proportion of seed tissue that is lignin (fraction)
# PLTPOP    Plant population (# plants / m2)
# PMINSD    Proportion of seed tissue that is mineral (fraction)
# PNTIM(I)  Photothermal days from first flower when flowers in age group I
#             formed (p-t-d)
# POASD     Proportion of seed tissue that is organic acid (fraction)
# POTCAR    Potential carbohydrate composition of seed based on temperature
#            (fraction)
# POTLIP    Potential lipid composition of seed based on temperature
#            (fraction)
# PROLFF    Minimum leaf protein composition after N mining
#            (g[protein] / g[leaf])
# PROLFI    Maximum protein composition in leaves during growth with
#             luxurious supply of N (g[protein] / g[leaf tissue])
# PRORTF    Minimum root protein composition after N mining
#            (g[protein] / g[root])
# PRORTI    Maximum protein composition in roots during growth with
#             luxurious supply of N (g[protein] / g[root])
# PROSTF    Minimum stem protein composition after N mining
#           (g[protein] / g[stem])
# PROSTI    Maximum protein composition in stems during growth with
#             luxurious supply of N (g[protein] / g[stem])
# PUNCSD    Cumulative puncture damage to seed (not yet implemented)
# PUNCTR    Cumulative puncture damage (not yet implemented)
# RCH2O     Respiration required for synthesizing CH2O structure
#            (g[CH2O] / g[tissue])
# REDPUN    Reduces growth of seed in an age group due to pest-caused
#             punctures in seed (0 to 1) (not yet implemented)
# REDSHL    Reduces growth of shell in an age group due to pest-caused
#             punctures in seed (0 to 1)
# RLIG      Respiration required for synthesizing lignin structure
#            (g[CH2O] / g[lignin])
# RLIP      Respiration required for synthesizing lipid structure
#            (g[CH2O] / g[lipid])
# RMIN      Respiration required for synthesizing mineral structure
#            (g[CH2O] / g[mineral])
# RNO3C     Respiration required for reducing NO3 to protein
#            (g[CH2O] / g[protein])
# ROA       Respiration required for synthesizing organic acids
#            (g[CH2O] / g[product])
# RPRO      Respiration required for re-synthesizing protein from mobilized
#             N (g[CH2O] / g[protein])
# RPROAV    Respiration required for protein synthesis, average based on
#             sources of N (g[CH2O] / g[protein])
# RPRPUN    Puncture damage reduction variable (not yet implemented) (0-1)
# RTWT      Dry mass of root tissue, including C and N
#            (g[root] / m2[ground])
# SDDES(J)  Number of seeds destroyed today in cohort J when shells are not
#             destroyed (#/m2/d)
# SDGR      Potential growth rate per seed (g / seed / d)
# SDLIP     Maximum lipid composition in seed (fraction)
# SDMAX     A maximum amount of remaining growth for each cohort (g/m2)
# SDNO(J)   Number of seeds for cohort J (#/m2)
# SDPRO     Seed protein fraction at 25ºC (g[protein] / g[seed])
# SDVAR     Maximum cultivar-dependent seed growth rate, per seed
#            (g / seed / d)
# SECTION   Section name in input file
# SHELN(J)  Number of shells for cohort J (#/m2)
# SHLAG     Shell (peg) growth rate during its initial slow growth phase
#             after beginning pegging (R2) as a fraction of shell growth
#             rate (SHVAR) during its rapid growth phase.
# SHVAR     Shell growth rate during its rapid growth phase, per shell
#            (g / shell / d)
# SIZELF    The size of a normal upper node leaf (nodes 8 - 10) used to
#             adjust leaf area expansion during sink-limited phase of
#             vegetative growth, i.e., prior to VSSINK nodes on the main stem
#             (cm2/leaf)
# SIZRAT    Ratio of upper node normal leaf size for given variety to that
#             for standard cultivar, used to adjust table of maximum leaf
#             area vs. V-stage
# SIZREF    The size of a normal upper node  leaf (nodes 8 - 10) of
#             standard cultivar. (cm2 / leaf)
# SLAMAX    The maximum specific leaf area (SLA) for new leaves when grown
#             under low (nearly zero) radiation but optimum water and
#             temperature for the standard cultivar. (cm2 / g)
# SLAMIN    The minimum specific leaf area (SLA) for new leaves when grown
#             under infinitely high radiation, optimum water and
#             temperature for the standard cultivar. (cm2 / g)
# SLAMN     Minimum specific leaf area for new leaves when grown under high
#             radiation and optimum water and temperature conditions (cm2 / g)
# SLAMX     Maximum specific leaf area for new leaves when grown under low
#             radiation, but optimum water and temperature conditions
#             (cm2 / g)
# SLAPAR    Coefficient in exponential equation to reduce SLA as PAR
#            increases (leaf curvature)
# SLAREF    Specific leaf area (SLA) for new leaves during peak vegetative
#             growth for the standard cultivar. (cm2/g)
# SLAVAR    Specific leaf area (SLA) for new leaves during peak vegetative
#             growth for cultivar I, modified by environmental factor (cm2/g)
# SLOSUM    Slope of temperature vs. SUMTEM line (1/ºC)
# SRMAX     Maximum fraction change in seed growth rate for long day
#             lengths
# STMWT     Dry mass of stem tissue, including C and N
#            (g[stem] / m2[ground)
# SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress,
#             0.0=max stress
# TAVG      Average daily temperature (°C)
# TDUMX     Photo-thermal time that occurs in a real day based on early
#             reproductive development temperature function
#             (photo-thermal days / day)
# TDUMX2    Photo-thermal time that occurs in a real day based on late
#             reproductive development temperature function
#             (photo-thermal days / day)
# TEMXFR    Temperature effect on partitioning to pods, high temp.
#             increases fraction of growth to vegetative tissue (0-1)
# TGRO(I)   Hourly canopy temperature (°C)
# THRESH    The maximum ratio mass of seed to mass of seed plus shell at
#             maturity.  Causes seed to stop growing as their dry weights
#             increase until shells are filled in a cohort.
# TMPFAC    Modifies maximum growth rate for seed and shells depending on
#             temperature
# TMPFCS    Interim value of TMPFAC
# TPHFAC    Reduction in specific leaf area due to daytime temperature
#             being less than optimal (0-1)
# TURADD    Water stress factor (TURFAC) effect on reproductive growth and
#             pod addition.  Stress is defined to INCREASE growth and
#             addition.
# TURFAC    Water stress factor for expansion (0 - 1)
# TURFSL    Factor which applies water stress to specific leaf area of new
#             leaf tissue growth
# TURSLA    Water stress effects on leaf area expansion
# TURXFR    Turgor water stress factor used to modify partitioning to
#             reproductive growth
# TYPSDT    Curve type for temperature factor calculations (for use in
#             function subroutine CURV)
# VSSINK    Vegetative stage beyond which sink-limited leaf area expansion
#             can no longer limit photosynthesis or leaf area growth.
# VSTAGE    Number of nodes on main stem of plant (nodes)
# WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
# WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
# WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
# WNRLF     N available for mobilization from leaves above lower limit of
#             mining (g[N] / m2)
# WNRRT     N available for mobilization from roots above lower limit of
#             mining (g[N] / m2)
# WNRSH     N available for mobilization from shells above lower limit of
#             mining (g[N] / m2)
# WNRST     N available for mobilization from stems above lower limit of
#             mining (g[N] / m2)
# WTLF      Dry mass of leaf tissue including C and N
#            (g[leaf] / m2[ground])
# WTSD(J)   Seed mass  for cohort J (g/m2)
# WTSHE(J)  Shell mass  for cohort J (g/m2)
# XFRMAX    Maximum increase in partitioning to fruits induced under water
#             stress, assuming no problem in pod setting
# XFRT      Current day's partitioning to reproductive growth (0-1)
#            (g[fruit] / g[plant])
# XFRUIT    Maximum fraction of daily available gross photosynthate (PG)
#             which is allowed to go to seeds plus shells, varies from 0 to
#             1.0.
# XLEAF(I)  V-stage at which partitioning to leaves is YLEAF(I).
#            (leaf nodes)
# XPOD      Growth partitioning to pods which slows node appearance
#            (fraction)
# XSLATM(I) Temperature values for function that reduces specific leaf area
#             (SLA) (°C)
# XTRFAC(I) Values of TURFAC for function which reduces reproductive growth
#             based on water stress
# XVGROW(I) V-stage at which maximum leaf area growth per plant since
#             emergence is YVGROW(I). (# leaf nodes)
# XX        Difference between partitioning fraction to stems at beginning
#             bloom (R1) and at the day on which the maximum number of
#             V-stages occurs (NDLEAF)
# XXFTEM(I) Array of temperature values in table lookup describing effect
#             of temperature on partitioning to pods (YXFTEM = 0 TO 1). (°C)
# YLEAF(I)  Partitioning fraction to leaves at V-stage XLEAF(I)
#            (g[leaf] / g[veg. plant])
# YSLATM(I) Array which describes the effect of temperature on specific
#             leaf area
# YSTEM(I)  Partitioning factor for stem growth at V-stage XSTEM(I)
#            (g[stem] / g[veg. plant])
# YTRFAC(I) Factor which affects reproductive growth based on water stress
# YVGROW(I) Maximum leaf area grown per plant at V-stage XVGROW(I)
#            (cm2 / plant)
# YVREF(I)  Maximum leaf area grown per plant at V-stage XVGROW(I), for
#             reference cultivar. (cm2 / plant)
# YXFTEM(I) Array describing the relative partitioning to pods (0 to 1
#             effect on XFRUIT) as temperature increases.
# YY        Used to linearly interpolate the difference in partitioning to
#             leaves between stages R1 and NDLEAF
# ----------------------------------------------------------------------
#       END SUBROUTINE DEMAND
# ======================================================================