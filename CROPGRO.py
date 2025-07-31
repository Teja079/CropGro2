#=======================================================================
#                     DSSAT Foundation
#                     University of Florida, Gainesville, Florida
#                     International Fertilizer Development Center
#
# ALL RIGHTS RESERVED
#=======================================================================
#=======================================================================
#  CROPGRO
#-----------------------------------------------------------------------
#  CROPGRO template plant growth subroutine.
#  Computes plant development and growth.
#-----------------------------------------------------------------------
import numpy as np


def CROPGRO(CONTROL, ISWITCH,  EOP, HARVFRAC, NH4, NO3, SOILPROP, SPi_AVAIL,
            ST, SW, TRWUP, WEATHER, YREND, YRPLT):
    from ModuleDefs import RunConstants as RC, GET_Char, PUT_Char, NCOHORTS, NL, ResidueType
    from IPPLNT import IPPLNT
    from PHOTO import PHOTO
    from Phenol import PHENOL
    from DEMAND import DEMAND
    from INCOMP import INCOMP
    from NUPTAKE import NUPTAK
    from NFIX import NFIX
    from POD import PODS
    from SENES import SENES
    from ROOTS import ROOTS
    from GROW import GROW
    from Opgrow import OPGROW
    from OPHARV import OPHARV
    from MOBIL import MOBIL
    from VEGGR import VEGGR
    from RESPIR import RESPIR
    from PlantNBal import PlantNBal

    BETN    : float = -99.0
    DXR57   : float = -99.0
    EXCESS  : float = -99.0
    PSTRES1 : float = -99.0
    RNITP : float = -99.0
    SLAAD : float = -99.0
    SWFAC : float = -99.0
    XHLAI : float = -99.0
    XPOD  : float = -99.0
    NSTRES  : float = -99.0
    PSTRES2 : float = -99.0
    TURFAC : float = -99.0
    AGRLF  : float = -99.0
    AGRRT  : float = -99.0
    AGRSH2 : float = -99.0
    AGRSTM : float = -99.0
    FNINSH : float = -99.0
    LAGSD  : float = -99.0
    LNGPEG : float = -99.0
    PCNL  : float = -99.0
    PCNRT : float = -99.0
    PCNST : float = -99.0
    PGAVL : float = -99.0
    PUNCSD : float = -99.0
    PUNCTR : float = -99.0
    PLTPOP : float = -99.0
    RPROAV : float = -99.0
    RTWT   : float = -99.0
    SDVAR : float = -99.0
    SHVAR : float = -99.0
    STMWT : float = -99.0
    WCRLF : float = -99.0
    WCRRT : float = -99.0
    WCRST : float = -99.0
    WNRLF : float = -99.0
    WNRRT : float = -99.0
    WNRSH : float = -99.0
    WNRST : float = -99.0
    WTLF  : float = -99.0
    CNODMN : float = -99.0
    CTONOD : float = -99.0
    NAVL : float = -99.0
    NRUSSH : float = -99.0
    SHELWT : float = -99.0
    CMINEP : float = -99.0
    CSAVEV : float = -99.0
    NMINEA : float = -99.0
    ROWSPC : float = -99.0
    WCRSH  : float = -99.0
    XLAI   : float = -99.0
    CLW    : float = -99.0
    NRUSLF : float = -99.0
    RHOL : float = -99.0
    RO   : float = -99.0
    RP   : float = -99.0
    WTNEW : float = -99.0
    DISLA : float = -99.0
    NPLTD : float = -99.0
    NRUSRT : float = -99.0
    NRUSST : float = -99.0
    PPLTD  : float = -99.0
    SDIDOT : float = -99.0
    WLIDOT : float = -99.0
    WSIDOT : float = -99.0
    WRIDOT : float = -99.0
    KCAN   : float = -99.0
    KC_SLOPE : float = -99.0
    CADLF  : float = -99.0
    CADST  : float = -99.0
    CRUSLF : float = -99.0
    CRUSRT : float = -99.0
    CRUSSH : float = -99.0
    CRUSST : float = -99.0
    NADLF : float = -99.0
    NADRT : float = -99.0
    NADST : float = -99.0
    NDTH  : float = -99.0
    NFIXN : float = -99.0
    NGRLF : float = -99.0
    NGRRT : float = -99.0
    NGRSD : float = -99.0
    NGRSH : float = -99.0
    NGRST : float = -99.0
    NODGR : float = -99.0
    SLDOT : float = -99.0
    SLNDOT : float = -99.0
    SRDOT  : float = -99.0
    SSDOT  : float = -99.0
    SSNDOT : float = -99.0
    TRNH4U : float = -99.0
    TRNO3U : float = -99.0
    TRNU   : float = -99.0
    WLDOTN : float = -99.0
    WRDOTN : float = -99.0
    WSDDTN : float = -99.0
    WSDOTN : float = -99.0
    WTABRT : float = -99.0
    WSHDTN : float = -99.0
    WTSHMT : float = -99.0
    SWIDOT : float = -99.0
    WLFDOT : float = -99.0
    WSHIDT : float = -99.0
    WTNFX  : float = -99.0
    RPRO   : float = -99.0
    AGRSD3 : float = -99.0
    PCTMAT : float = -99.0
    CANHT  : float = -99.0
    CANWH  : float = -99.0
    CMINEA : float = -99.0
    PODNO  : float = -99.0
    PODWTD : float = -99.0
    RWUEP1 : float = -99.0

    SDDES = np.full(NCOHORTS, -99.0, dtype=float)
    SDNO = np.full(NCOHORTS, -99.0, dtype=float)
    SHELN = np.full(NCOHORTS, -99.0, dtype=float)
    WTSD = np.full(NCOHORTS, -99.0, dtype=float)
    WTSHE = np.full(NCOHORTS, -99.0, dtype=float)

    RLV = np.full(NL, -99.0, dtype=float)
    SENNOD = np.full(NL, -99.0, dtype=float)
    SENRT = np.full(NL, -99.0, dtype=float)

    NR5 : int = -99
    NOUTDO : int = -99
    YREMRG : int = -99

    FILECC : str = ''
    FILEGC : str = ''


    # CONTROL = ControlType()
    # SOILPROP = SoilType()
    # ISWITCH = SwitchType()
    # HARVRES = ResidueType()
    # SENESCE = ResidueType()
    # WEATHER = WeatherType()

    CROP = CONTROL.CROP
    DYNAMIC = CONTROL.DYNAMIC
    FILEIO = CONTROL.FILEIO
    YRDOY = CONTROL.YRDOY
    DAS = CONTROL.DAS
    RUN = CONTROL.RUN
    RNMODE = CONTROL.RNMODE

    DLAYR = SOILPROP.DLAYR
    DS = SOILPROP.DS
    DUL = SOILPROP.DUL
    KG2PPM = SOILPROP.KG2PPM
    LL = SOILPROP.LL
    NLAYR = SOILPROP.NLAYR
    SAT = SOILPROP.SAT
    SLPF = SOILPROP.SLPF
    WR = SOILPROP.WR

    IDETO = ISWITCH.IDETO
    ISWDIS = ISWITCH.ISWDIS
    ISWNIT = ISWITCH.ISWNIT
    ISWSYM = ISWITCH.ISWSYM
    ISWPHO = ISWITCH.ISWPHO
    ISWWAT = ISWITCH.ISWWAT
    MEPHO = ISWITCH.MEPHO

    CO2 = WEATHER.CO2
    DAYL = WEATHER.DAYL
    PAR = WEATHER.PAR
    TAVG = WEATHER.TAVG
    TDAY = WEATHER.TDAY
    TGRO = WEATHER.TGRO
    TGROAV = WEATHER.TGROAV
    TMIN = WEATHER.TMIN

    HARVRES = ResidueType()

    # ***********************************************************************
    #     Run Initialization - Called once per simulation
    # ***********************************************************************
    if DYNAMIC == RC.RUNINIT:
        # -----------------------------------------------------------------------
        #     Call input routine for CROPGRO module parameters
        # -----------------------------------------------------------------------
        (
            CADPR1, CMOBMX, CROP, DETACH, ECONO, EORATIO, FILECC, FILEGC, FRCNOD,
            FREEZ1, FREEZ2, KCAN, KC_SLOPE, KEP, NOUTDO, PCARSH, PCH2O, PLIPSH,
            PLIGSD, PLIGSH, PMINSD, PMINSH, POASD, POASH, PORMIN, PROLFI, PRORTI,
            PROSHI, PROSTI, R30C2, RCH2O, RES30C, RFIXN, RLIG, RLIP, RMIN, RNH4C,
            RNO3C, ROA, RPRO, RWUEP1, RWUMX, TTFIX
        ) = IPPLNT(CONTROL, ISWITCH)

        KTRANS, KSEVAP = KEP, -99.0  # Default to old light extinction calculation for soil evap.

        if CROP != 'FA' and MEPHO == 'C':
            (
                AGEFAC, PG
            ) = PHOTO(
                CONTROL, BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE, NR5, PAR,
                PStres1, SLPF, RNITP, SLAAD, SWFAC, TDAY, XHLAI, XPOD
            )
        # -----------------------------------------------------------------------
        (
            CropStatus, DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF, NDSET, NR1, NR2,
            NR5, NR7, NVEG0, PHTHRS, RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,
            TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1, YRNR2, YRNR3, YRNR5, YRNR7
        ) = PHENOL(
            CONTROL, ISWITCH, DAYL, NSTRES, PStres2, SOILPROP, ST, SW, SWFAC,
            TGRO, TMIN, TURFAC, XPOD, YRPLT
        )
        # -----------------------------------------------------------------------
        # if ISWDIS == 'Y':
        #     (
        #         RLV, SDNO, SHELN, SWIDOT, VSTAGE, WSHIDT, WTSD, WTSHE, ASMDOT,
        #         DISLA, NPLTD, PPLTD, SDDES, WLIDOT, WRIDOT, WSIDOT, SDWT
        #     ) = PEST(
        #         CONTROL, ISWITCH, AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,
        #         PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP, SSDOT, STMWT, TOPWT,
        #         WLFDOT, WTLF, YRPLT
        #     )
        # -----------------------------------------------------------------------
        if CROP != 'FA':
            (
                AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, CropStatus, F, FNINL, FNINR,
                FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD, GRRAT1, NDMNEW, NDMOLD,
                NDMREP, NDMSDR, NDMTOT, NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM,
                POTCAR, POTLIP, SDGR, TURADD, XFRT, YREND
            ) = DEMAND(
                RC.RUNINIT, CONTROL, AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP,
                DXR57, FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD, LNGPEG,
                NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST, PGAVL, PUNCSD, PUNCTR,
                PLTPOP, RPROAV, RTWT, SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT,
                SWFAC, TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, WCRRT,
                WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF, WTSD, WTSHE, XPOD,
                NVEG0, NR1, NR2, NR5, NR7
            )
            # -----------------------------------------------------------------------
            (
                AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2, AGRSH1, AGRSH2, AGRSTM,
                AGRVG, AGRVG2, SDPROR
            ) = INCOMP(
                RC.RUNINIT, FILECC, FILEIO, FRLF, FRRT, FRSTM
            )
            # -----------------------------------------------------------------------
            (
                TRNH4U, TRNO3U, TRNU, UNH4, UNO3
            ) = NUPTAK(
                RC.RUNINIT, DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT, NH4,
                NO3, NLAYR, RLV, SAT, SW
            )
            # -----------------------------------------------------------------------
            if ISWSYM == 'Y':
                (
                    CNOD, DWNOD, DWNODA, NDTH, NFIXN, NODGR, WTNFX, SENNOD
                ) = NFIX(
                    RC.RUNINIT, AGRNOD, CNODMN, CTONOD, DLAYR, DXR57, FILECC, FILEIO,
                    NLAYR, NR7, PLTPOP, SAT, ST, SW, TURFAC
                )
            # -----------------------------------------------------------------------
            (
                AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT, PODNO, POTCAR,
                POTLIP, SDNO, SDVAR, SEEDNO, SHELN, SHVAR, WSDDTN, WSHDTN,
                WTABRT, WTSD, WTSHE, WTSHMT, FLWN
            ) = PODS(
                RC.RUNINIT, AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC, FILEGC, FILEIO,
                FNINL, FNINSD, FNINSH, GDMSD, GRRAT1, ISWWAT, LL, NAVL, NDSET,
                NLAYR, NRUSSH, NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,
                PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC, TDUMX, TGRO,
                TURADD, XFRT, YRDOY, YRNR1, YRNR2, PSTRES2, YRPLT
            )
            # -----------------------------------------------------------------------
            if DETACH == 'Y':
                PODWTD, SDNO, SHELN, SWIDOT, WSHIDT, WTSD, WTSHE = PODDET(
                    FILECC, TGRO, WTLF, YRDOY, YRNR2,  # Input
                    RC.RUNINIT  # Control
                )

            # -----------------------------------------------------------------------
            (
                AGRVG, FRLF, FRRT, FRSTM, CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,
                CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT, NADST, NGRLF, NGRRT, NGRST,
                NSTRES, TNLEAK, WLDOTN, WRDOTN, WSDOTN
            ) = VEGGR(
                RC.RUNINIT, AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX, DXR57, ECONO, FILECC, FILEGC,
                FNINL, FNINR, FNINS, KCAN, NAVL, NDMNEW, NDMOLD, NFIXN, NMINEA, NR1, PAR, PCH2O,
                PG, PGAVL, PSTRES2, ROWSPC, RVSTGE, STMWT, TGRO, TRNU, TURFAC, VSTAGE, WCRLF,
                WCRRT, WCRSH, WCRST, WTLF, XLAI, YRDOY, YREMRG
            )

            # -----------------------------------------------------------------------
            SLDOT, SLNDOT, SSDOT, SSNDOT = SENES(
                RC.RUNINIT, FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR, RHOL, SLAAD, STMWT, SWFAC,
                VSTAGE, WTLF, XLAI
            )

            # -----------------------------------------------------------------------
            RLV, RTDEP, SATFAC, SENRT, SRDOT = ROOTS(
                RC.RUNINIT, AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, ISWWAT, LL, NLAYR, PG,
                PLTPOP, RO, RP, RTWT, SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW
            )

            # -----------------------------------------------------------------------
            (
                AREALF, BETN, CANNAA, CANWAA, CLW, CropStatus, CSW, DWNOD, DWNODA, GROWTH,
                GRWRES, LAIMX, PCCSD, PCLSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,
                PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST, PODWT, PUNCSD, PUNCTR, RHOL,
                RHOS, RNITP, ROWSPC, RTWT, SDNPL, SDRATE, SDWT, SEEDNI, SEEDNO, SENESCE,
                SHELWT, SLA, SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH, WCRST, WNRLF,
                WNRRT, WNRSH, WNRST, WTCO, WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,
                WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO, WTNRA, WTNRO, WTNRT, WTNSA, WTNSD,
                WTNSDA, WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST, WTNUP, WTRO, WTSDO, WTSHO,
                WTSO, XLAI, XPOD, ShutMob, RootMob, ShelMob
            ) = GROW(
                CONTROL, ISWITCH, RC.RUNINIT, SOILPROP, AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,
                CRUSST, DISLA, F, FILECC, FRLF, FRSTM, NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF,
                NGRRT, NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO, NPLTD, NRUSLF, NRUSRT, NRUSSH,
                NRUSST, POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR, SENNOD, SENRT, SLDOT, SLNDOT,
                SRDOT, SSDOT, SSNDOT, TRNH4U, TRNO3U, TRNU, TURFAC, WLDOTN, WLIDOT, WRDOTN,
                WRIDOT, WSDDTN, WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1, MDATE, YRPLT
            )

            # -----------------------------------------------------------------------
            (
                CANHT, CANWH, CMINEA, DWNOD, GROWTH, GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR,
                NSTRES, PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, PODNO, PODWT, PODWTD,
                PSTRES1, PSTRES2, RHOL, RHOS, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO,
                SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, TOTWT, TURFAC, VSTAGE, WTLF,
                WTNCAN, WTNLF, WTNST, WTNSD, WTNUP, WTNFX, XLAI, YRPLT
            ) = OPGROW(
                CONTROL, ISWITCH, SOILPROP, CADLF, CADST
            )

            # -----------------------------------------------------------------------
            SDWTAH = OPHARV(
                CONTROL, ISWITCH, AGEFAC, CANHT, CANNAA, CANWAA, CROP, HARVFRAC, LAIMX, MDATE,
                NSTRES, PCLSD, PCNSD, PODNO, PODWT, PSTRES1, PSTRES2, SDRATE, SDWT, SEEDNO, STGDOY,
                SWFAC, TOPWT, TURFAC, VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP, XLAI, RSTAGE,
                YREMRG, YRNR1, YRNR3, YRNR5, YRNR7, YRPLT
            )

            # -----------------------------------------------------------------------
            if RUN == 1 or 'QF' not in RNMODE:
                HARVRES = {
                    'RESWT': 0.0,
                    'RESLIG': 0.0,
                    'RESE': 0.0
                }
    # ***********************************************************************
    # ***********************************************************************
    # Seasonal initialization - run once per season
    # ***********************************************************************
    elif DYNAMIC == RC.SEASINIT:
        # ***********************************************************************
        # The following statements came from INPLNT
        # -----------------------------------------------------------------------
        CMINEP = 0.0
        CNOD = 0.0
        CNODMN = 0.0
        CTONOD = 0.0
        MAINR = 0.0
        NAVL = 0.0
        PGAVL = 0.0
        RO = 0.0
        RP = 0.0
        RPROAV = RFIXN

        TURFAC = 1.0
        SWFAC = 1.0

        RSPNO3 = 0.0
        RSPNH4 = 0.0

        KSTRES = 1.0

        # -----------------------------------------------------------------------
        if CROP != 'FA':
            if MEPHO == 'L':
                # Retrieve AGEFAC and PG from ETPHOT routine.
                AGEFAC = GET_Char('SPAM', 'AGEFAC')
                PG = GET_Char('SPAM', 'PG')
            elif MEPHO == 'C':
                # Call PHOTO function and unpack output parameters
                AGEFAC, PG = PHOTO(
                    CONTROL, BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,
                    NR5, PAR, PSTRES1, SLPF, RNITP, SLAAD,
                    SWFAC, TDAY, XHLAI, XPOD
                )

        # -----------------------------------------------------------------------
        (
            DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF, NDSET, NR1, NR2, NR5, NR7,
            NVEG0, PHTHRS, RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX, TDUMX2,
            VegFrac, VSTAGE, YREMRG, YRNR1, YRNR2, YRNR3, YRNR5, YRNR7, CropStatus
        ) = PHENOL(
            CONTROL, ISWITCH, DAYL, NSTRES, PSTRES2, SOILPROP, ST, SW, SWFAC,
            TGRO, TMIN, TURFAC, XPOD, YRPLT
        )

        # -----------------------------------------------------------------------
        # Initialize pest coupling point and damage variables
        # Need to initialize even if pests are not being modeled this
        # season - zero out values from last simulation.
        # -----------------------------------------------------------------------
        # (
        #     RLV, SDNO, SHELN, SWIDOT, VSTAGE, WSHIDT, WTSD, WTSHE,
        #     ASMDOT, DISLA, NPLTD, PPLTD, SDDES, WLIDOT, WRIDOT, WSIDOT, SDWT
        # ) = PEST(
        #     CONTROL, ISWITCH, AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,
        #     PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP, SSDOT, STMWT, TOPWT,
        #     WLFDOT, WTLF, YRPLT
        # )

        # -----------------------------------------------------------------------
        # Initialization call to DEMAND must precede initialization calls
        # to INCOMP and GROW (need to initialize values of F, FRLF,
        # FRRT, and FRSTM for use in those routines)
        # -----------------------------------------------------------------------
        (
            AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, CropStatus, F, FNINL, FNINR,
            FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD, GRRAT1, NDMNEW, NDMOLD,
            NDMREP, NDMSDR, NDMTOT, NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM,
            POTCAR, POTLIP, SDGR, TURADD, XFRT, YREND
        ) = DEMAND(
            RC.SEASINIT, CONTROL, AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,
            FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD, LNGPEG, NDLEAF,
            NSTRES, PAR, PCNL, PCNRT, PCNST, PGAVL, PUNCSD, PUNCTR, PLTPOP,
            RPROAV, RTWT, SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,
            TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, WCRRT, WCRST,
            WNRLF, WNRRT, WNRSH, WNRST, WTLF, WTSD, WTSHE, XPOD, NVEG0, NR1,
            NR2, NR5, NR7
        )

        # -----------------------------------------------------------------------
        #     Call plant COMPosition INitialization
        #     This call must precede initialization call to GROW (need to
        #         initialize value of SDPROR for use in that routine) chp 9/22/98
        # -----------------------------------------------------------------------
        if CROP != 'FA':
            AGRLF, AGRNOD, AGRRT, AGRSD1, AGRSD2, AGRSH1, AGRSH2, AGRSTM, AGRVG, AGRVG2, SDPROR = INCOMP(
                RC.SEASINIT, FILECC, FILEIO, FRLF, FRRT, FRSTM
            )

        # -----------------------------------------------------------------------
        (AREALF, BETN, CANNAA, CANWAA, CLW, CropStatus, CSW, DWNOD, DWNODA, GROWTH, GRWRES, LAIMX,
        PCCSD, PCLSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP, PLIGLF, PLIGNO, PLIGRT, PLIGSD,
        PLIGSH, PLIGST, PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP, ROWSPC, RTWT, SDNPL, SDRATE,
        SDWT, SEEDNI, SEEDNO, SENESCE, SHELWT, SLA, SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT,
        WCRSH, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO, WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA,
        WTNLF, WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO, WTNRA, WTNRO, WTNRT, WTNSA, WTNSD,
        WTNSDA, WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST, WTNUP, WTRO, WTSDO, WTSHO, WTSO,
        XLAI, XPOD, ShutMob, RootMob, ShelMob)= (
        GROW(
            CONTROL, ISWITCH, RC.SEASINIT, SOILPROP, AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,
            CRUSST, DISLA, F, FILECC, FRLF, FRSTM, NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,
            NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO, NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,
            POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR, SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,
            SSNDOT, TRNH4U, TRNO3U, TRNU, TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,
            WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1, MDATE, YRPLT, SWIDOT, WLFDOT,
            WSHIDT, WTNFX, XHLAI
        ))

        # -----------------------------------------------------------------------
        TRNH4U, TRNO3U, TRNU, UNH4, UNO3 = NUPTAK(
            RC.SEASINIT, DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT,
            NH4, NO3, NLAYR, RLV, SAT, SW
        )

        # # Plant phosphorus module initialization
        # PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed, PStres1, PStres2, PUptake, FracRts = P_CGRO(
        #     DYNAMIC, ISWITCH, CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV, RootMob, RTDEP,
        #     RTWT, SDWT, SeedFrac, ShelMob, SHELWT, ShutMob, SOILPROP, SPi_AVAIL, STMWT,
        #     SWIDOT, VegFrac, WLIDOT, WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT, SENESCE
        # )

        # -----------------------------------------------------------------------
        NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST = MOBIL(
            RC.SEASINIT, NDMNEW, NMINEP, NMOBR, RPRO, TRNU,
            WNRLF, WNRRT, WNRSH, WNRST
        )

        # -----------------------------------------------------------------------
        CNOD, DWNOD, DWNODA, NDTH, NFIXN, NODGR, WTNFX, SENNOD = NFIX(
            RC.SEASINIT, AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,
            FILECC, FILEIO, NLAYR, NR7, PLTPOP, SAT, ST, SW, TURFAC
        )

        # -----------------------------------------------------------------------
        AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT, PODNO, POTCAR, POTLIP, SDNO, SDVAR,
        SEEDNO, SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD, WTSHE, WTSHMT, FLWN = PODS(
            RC.SEASINIT, AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC, FILEGC, FILEIO, FNINL, FNINSD, FNINSH,
            GDMSD, GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, NSTRES, PGAVL, PHTHRS,
            PHTIM, PNTIM, PUNCSD, PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC, TDUMX,
            TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, PSTRES2, YRPLT
        )

        # -----------------------------------------------------------------------
        SLDOT, SLNDOT, SSDOT, SSNDOT = SENES(
            RC.SEASINIT, FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,
            RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI
        )

        # -----------------------------------------------------------------------
        RLV, RTDEP, SATFAC, SENRT, SRDOT = ROOTS(
            RC.SEASINIT, AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, ISWWAT,
            LL, NLAYR, PG, PLTPOP, RO, RP, RTWT, SAT, SW, SWFAC, VSTAGE,
            WR, WRDOTN, WTNEW
        )

        # -----------------------------------------------------------------------
        #     Write headings to output file GROWTH.OUT
        # -----------------------------------------------------------------------
        OPGROW(
            CONTROL, ISWITCH, SOILPROP, CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD,
            GROWTH, GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES, PCLSD,
            PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG, PODNO, PODWT, PODWTD,
            PSTRES1, PSTRES2, RHOL, RHOS, RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT,
            SEEDNO, SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT, TOTWT, TURFAC,
            VSTAGE, WTLF, WTNCAN, WTNLF, WTNST, WTNSD, WTNUP, WTNFX, XLAI, YRPLT
        )

        # Zero the value of HARVRES composite variable here
        HARVRES.RESWT = 0.0
        HARVRES.RESLIG = 0.0
        HARVRES.RESE = 0.0

    # ***********************************************************************
    # ***********************************************************************
    #     DAILY RATE CALCULATIONS
    # ***********************************************************************
    elif DYNAMIC == RC.RATE:
        # ***********************************************************************
        if YRDOY > YREMRG > 0 and ISWWAT == 'Y':
            # Calculate daily water stress factors (from SWFACS)
            # EOP in mm/d
            # TRWUP and EP1 in cm/d
            SWFAC = 1.0
            TURFAC = 1.0
            if EOP > 0.001:
                EP1 = EOP * 0.1
                if (TRWUP / EP1) < RWUEP1:
                    TURFAC = (1.0 / RWUEP1) * (TRWUP / EP1)
                if EP1 >= TRWUP:
                    SWFAC = TRWUP / EP1

        # -----------------------------------------------------------------------
        #     CALL vegetative and reproductive development subroutine
        # -----------------------------------------------------------------------
        if CROP != 'FA':
            (
                DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF, NDSET, NR1, NR2, NR5, NR7,
                NVEG0, PHTHRS, RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX, TDUMX2,
                VegFrac, VSTAGE, YREMRG, YRNR1, YRNR2, YRNR3, YRNR5, YRNR7, CropStatus
            ) = PHENOL(
                CONTROL, ISWITCH, DAYL, NSTRES, PSTRES2, SOILPROP, ST, SW, SWFAC,
                TGRO, TMIN, TURFAC, XPOD, YRPLT
            )

        # ----------------------------------------------------------------------
        # if ISWDIS == 'Y':
        #     (
        #         ASMDOT, DISLA, NPLTD, PPLTD, SDDES, WLIDOT, WRIDOT, WSIDOT, SDWT
        #     ) = PEST(
        #         CONTROL, ISWITCH, AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL, PHTIM,
        #         PLTPOP, RTWT, SLA, SLDOT, SOILPROP, SSDOT, STMWT, TOPWT, WLFDOT, WTLF,
        #         YRPLT, RLV, SDNO, SHELN, SWIDOT, VSTAGE, WSHIDT, WTSD, WTSHE
        #     )

        if CROP != 'FA' and DAS > NVEG0:
            if MEPHO == 'L':
                # Retrieve AGEFAC and PG from ETPHOT routine.
                AGEFAC = GET_Char('SPAM', 'AGEFAC')
                PG = GET_Char('SPAM', 'PG')
            elif MEPHO == 'C':
                AGEFAC, PG = PHOTO(
                    CONTROL, BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE, NR5, PAR,
                    PSTRES1, SLPF, RNITP, SLAAD, SWFAC, TDAY, XHLAI, XPOD
                )

    # ***********************************************************************
    # ***********************************************************************
    #     DAILY INTEGRATION
    # ***********************************************************************
    elif DYNAMIC == RC.INTEGR and CROP != 'FA':
        # ***********************************************************************
        # Move PHENOL integration up here.
        # Need to set NVEG0 before testing for DAS = NVEG0, otherwise,
        # initialization on the day of emergence will never occur.
        # ***********************************************************************
        (
            CropStatus, DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF, NDSET, NR1, NR2,
            NR5, NR7, NVEG0, PHTHRS, RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,
            TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1, YRNR2, YRNR3, YRNR5, YRNR7
        ) = PHENOL(
            CONTROL, ISWITCH, DAYL, NSTRES, PSTRES2, SOILPROP, ST, SW, SWFAC,
            TGRO, TMIN, TURFAC, XPOD, YRPLT
        )

        if DAS == NVEG0:
            # ----------------------------------------------------------------------
            # On the day of emergence, initialize:
            # ----------------------------------------------------------------------
            (
                AREALF, BETN, CANNAA, CANWAA, CLW, CropStatus, CSW, DWNOD, DWNODA,
                GROWTH, GRWRES, LAIMX, PCCSD, PCLSD, PCNL, PCNRT, PCNSD, PCNSH,
                PCNST, PLTPOP, PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,
                PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP, ROWSPC, RTWT, SDNPL,
                SDRATE, SDWT, SEEDNI, SEEDNO, SENESCE, SHELWT, SLA, SLAAD, STMWT,
                TOPWT, TOTWT, WCRLF, WCRRT, WCRSH, WCRST, WNRLF, WNRRT, WNRSH,
                WNRST, WTCO, WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF, WTNOO,
                WTNNAG, WTNNO, WTNNOD, WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA,
                WTNSHO, WTNSO, WTNST, WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,
                ShutMob, RootMob, ShelMob
            ) = GROW(
                CONTROL, ISWITCH, RC.EMERG, SOILPROP, AGEFAC, CADLF, CADST, CRUSLF,
                CRUSRT, CRUSSH, CRUSST, DISLA, F, FILECC, FRLF, FRSTM, NADLF, NADRT,
                NADST, NDTH, NFIXN, NGRLF, NGRRT, NGRSD, NGRSH, NGRST, NMINEA, NODGR,
                NOUTDO, NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST, POTCAR, POTLIP, PPLTD,
                SDIDOT, SDPROR, SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT, SSNDOT,
                TRNH4U, TRNO3U, TRNU, TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT,
                WSDDTN, WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1, MDATE, YRPLT
            )

            # Call root growth and rooting depth routine
            (
                RLV, RTDEP, SATFAC, SENRT, SRDOT
            ) = ROOTS(
                RC.EMERG, AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, ISWWAT, LL,
                NLAYR, PG, PLTPOP, RO, RP, RTWT, SAT, SW, SWFAC, VSTAGE, WR, WRDOTN,
                WTNEW
            )

            # Dynamic emergence calculations
            (
                AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, CropStatus, F, FNINL, FNINR,
                FNINS, FNINSD, FRLF, FRRT, FRSTM, GDMSD, GRRAT1, NDMNEW, NDMOLD,
                NDMREP, NDMSDR, NDMTOT, NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM,
                POTCAR, POTLIP, SDGR, TURADD, XFRT, YREND
            ) = DEMAND(
                RC.EMERG, CONTROL, AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,
                FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD, LNGPEG, NDLEAF,
                NSTRES, PAR, PCNL, PCNRT, PCNST, PGAVL, PUNCSD, PUNCTR, PLTPOP,
                RPROAV, RTWT, SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,
                TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF, WCRRT, WCRST,
                WNRLF, WNRRT, WNRSH, WNRST, WTLF, WTSD, WTSHE, XPOD, NVEG0, NR1, NR2,
                NR5, NR7
            )
            # ----------------------------------------------------------------------
            (
                AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,
                PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,
                SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,
                WTSHE, WTSHMT, FLWN
            ) = PODS(
                RC.EMERG,AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,
                FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,
                GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH,
                NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,
                PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,
                TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2,
                PSTRES2, YRPLT
            )
            # ----------------------------------------------------------------------
            # -----------------------------------------------------------------------
            # Call VEGGR function
            (AGRVG, FRLF, FRRT, FRSTM,
             CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,
             CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT,
             NADST, NGRLF, NGRRT, NGRST, NSTRES,
             TNLEAK, WLDOTN, WRDOTN, WSDOTN) = VEGGR(
                RC.EMERG, AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,  # Input
                DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,  # Input
                FNINS, KCAN, NAVL, NDMNEW, NDMOLD,  # Input
                NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,  # Input
                PSTRES2, ROWSPC, RVSTGE, STMWT, TGRO,  # Input
                TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,  # Input
                WCRST, WTLF, XLAI, YRDOY, YREMRG,  # Input
                AGRVG, FRLF, FRRT, FRSTM  # I/O
            )

            # if ISWPHO == 'Y' or ISWPHO == 'H':
            #     # Plant phosphorus module initialization at plant emergence
            #     (PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed,
            #      PStres1, PStres2, PUptake, FracRts) = P_CGRO(
            #         RC.EMERG, ISWITCH,
            #         CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,  # Input
            #         RootMob, RTDEP, RTWT, SDWT, SeedFrac,  # Input
            #         ShelMob, SHELWT, ShutMob, SOILPROP,  # Input
            #         SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,  # Input
            #         WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,  # Input
            #         SENESCE  # I/O
            #     )

        # -----------------------------------------------------------------------
        if DETACH == 'Y' and DAS <= NVEG0 + 1:
            # Call PODDET function
            (PODWTD, SDNO, SHELN, SWIDOT,
             WSHIDT, WTSD, WTSHE) = PODDET(
                FILECC, TGRO, WTLF, YRDOY, YRNR2,  # Input
                RC.EMERG  # Control
            )

        if DAS >= NVEG0:
            # -----------------------------------------------------------------------
            # Initialize available N and C for beginning of daily calculations.
            # -----------------------------------------------------------------------
            NAVL = 0.0
            PGAVL = 0.0

            # -----------------------------------------------------------------------
            # Initialize variables that represent N and C availability during a day
            # Assume that Fraction CMOBMX of CH2O can be Mobilized per Day
            # PGAVL is the total available CH2O available for growth & respiration
            # -----------------------------------------------------------------------
            CMINEP = CMOBMX * (DTX + DXR57) * (WCRST + WCRRT + WCRSH + WCRLF)
            PGAVL = PG + CMINEP

            # -----------------------------------------------------------------------
            # Compute maintenance respiration and subtract from available CH2O
            # -----------------------------------------------------------------------
            MAINR = RESPIR(PG, R30C2, RES30C, TGRO, WTMAIN, RO, RP)  # Output MAINR

            if MAINR > PGAVL:
                PGAVL = 0.0
            else:
                PGAVL -= MAINR

            # -----------------------------------------------------------------------
            # Reduce PGAVL if pest damage occurs to C assimilation
            # Moved to PEST module - chp
            # -----------------------------------------------------------------------
            # if ISWDIS == 'Y':
            #     (ASMDOT, DISLA, NPLTD, PPLTD,
            #      SDDES, WLIDOT, WRIDOT, WSIDOT, SDWT) = PEST(
            #         CONTROL, ISWITCH,
            #         AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,  # Input
            #         PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,  # Input
            #         SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,  # Input
            #         RLV, SDNO, SHELN, SWIDOT,  # Input/Output
            #         VSTAGE, WSHIDT, WTSD, WTSHE  # Input/Output
            #     )
            #
            #     if ASMDOT > 1.0E-4:
            #         PGAVL = max(0.0, PGAVL - ASMDOT)

            # -----------------------------------------------------------------------
            # Call Subroutine to calculate Nitrogen and Carbon Demand for new growth
            # -----------------------------------------------------------------------
            (AGRSD1, AGRSD2, AGRVG, AGRVG2, CDMREP, CropStatus,
             F, FNINL, FNINR, FNINS, FNINSD, FRLF, FRRT, FRSTM,
             GDMSD, GRRAT1, NDMNEW, NDMOLD, NDMREP, NDMSDR,
             NDMTOT, NDMVEG, NMINEP, NMOBR, PHTIM, PNTIM,
             POTCAR, POTLIP, SDGR, TURADD, XFRT, YREND) = DEMAND(
                RC.INTEGR, CONTROL,
                AGRLF, AGRRT, AGRSH2, AGRSTM, CROP, DRPP, DXR57,  # Input
                FILECC, FILEGC, FILEIO, FNINSH, FRACDN, LAGSD,  # Input
                LNGPEG, NDLEAF, NSTRES, PAR, PCNL, PCNRT, PCNST,  # Input
                PGAVL, PUNCSD, PUNCTR, PLTPOP, RPROAV, RTWT,  # Input
                SDDES, SDNO, SDVAR, SHELN, SHVAR, STMWT, SWFAC,  # Input
                TAVG, TDUMX, TDUMX2, TGRO, TURFAC, VSTAGE, WCRLF,  # Input
                WCRRT, WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTLF,  # Input
                WTSD, WTSHE, XPOD, NVEG0, NR1, NR2, NR5, NR7  # Input
            )

            if YRDOY == YREND:
                return

            # -----------------------------------------------------------------------
            # Compute N Available From Seed, During Early Growth
            # chp - this takes much longer than 7 ptd to deplete seed N.
            # -----------------------------------------------------------------------
            if SDNPL > 0.0001:
                NAVL = max(SDNPL * DTX / 7.0, 0.0)
                SDNPL -= NAVL
            else:
                SDNPL = 0.0
                NAVL = 0.0

            # -----------------------------------------------------------------------
            # If ISWNIT = Y - Call soil N routines. Balance Available C and N
            # If ISWNIT = N - Do not call soil N routines, N assumed to be limited by C
            # -----------------------------------------------------------------------
            if ISWNIT == 'Y':
                (TRNH4U, TRNO3U, TRNU, UNH4, UNO3) = NUPTAK(
                    RC.INTEGR,
                    DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT,  # Input
                    NH4, NO3, NLAYR, RLV, SAT, SW  # Input
                )

                # -----------------------------------------------------------------------
                # Account for C Used to reduce N Uptake to protein
                # -----------------------------------------------------------------------
                RSPNO3 = TRNO3U / 0.16 * RNO3C
                RSPNH4 = TRNH4U / 0.16 * RNH4C
                if PGAVL < (RSPNO3 + RSPNH4):
                    PGAVL = 0.0
                else:
                    PGAVL -= (RSPNO3 + RSPNH4)

                # -----------------------------------------------------------------------
                # Accumulate nitrogen for today's growth, NAVL
                # -----------------------------------------------------------------------
                NAVL += TRNU
            # -----------------------------------------------------------------------
            # CALL Nitrogen mobilization subroutine
            # to compute availability of N from other tissue (NMINEA)
            # -----------------------------------------------------------------------
            NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST = MOBIL(
                RC.INTEGR,
                NDMNEW, NMINEP, NMOBR, RPRO, TRNU,  # Input
                WNRLF, WNRRT, WNRSH, WNRST  # Input
            )

            # -----------------------------------------------------------------------
            # Plant phosphorus module
            # -----------------------------------------------------------------------
            if ISWPHO in ('Y', 'H'):
                (PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed,
                 PStres1, PStres2, PUptake, FracRts) = P_CGRO(
                    DYNAMIC, ISWITCH,
                    CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,  # Input
                    RootMob, RTDEP, RTWT, SDWT, SeedFrac,  # Input
                    ShelMob, SHELWT, ShutMob, SOILPROP,  # Input
                    SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,  # Input
                    WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,  # Input
                    SENESCE  # I/O
                )

            # -----------------------------------------------------------------------
            # Accumulate NAVL for growth, reduce PGAVL by protein re-synthesis cost
            # -----------------------------------------------------------------------
            if PGAVL > (NMINEA / 0.16 * RPRO):
                PGAVL -= NMINEA / 0.16 * RPRO
            else:
                PGAVL = 0.0

            NAVL += NMINEA
            #-----------------------------------------------------------------------
            #     Allow some of today's PG to be used for N fixation, depending
            #     on N uptake and mining, and on demand for N.
            #     NAVLV = N available for veg growth from uptake and mining
            #     CAVVEG = C available for veg growth
            #     NDMVEG = N required for veg growth if all PGAVL is used as computed
            #     CNDFX = carbon needed to fix N needed but not supplied by uptake or mining
            #     PROVEG = average protein composition of growing tissue today
            #     CTONOD = C to allocate to nodules to fix N needed for Rep and Veg growth
            #-----------------------------------------------------------------------
            CTONODR = max(0.0, (NDMREP - NAVL) * RFIXN / 0.16)
            CTONODR = min(CTONODR, PGAVL)
            CTONOD = 0.0
            CAVVEG = max(0.0, (PGAVL - CDMREP))
            NAVLV = max(0.0, (NAVL - NDMREP))
            CNDFX = max(0.0, (RFIXN / 0.16) * (NDMVEG - NAVLV))

            if CAVVEG > 1.0e-4 and CNDFX > 1.0e-4:
                PROVEG = PROLFI * FRLF + PRORTI * FRRT + PROSTI * FRSTM
                CTONOD = CAVVEG - (CAVVEG + (NAVLV * RFIXN / 0.16)) * AGRVG / (AGRVG + PROVEG * RFIXN)

            # Reserve carbon for nodule growth
            if DAS < NR2:
                CNODMN = CAVVEG * FRRT * FRCNOD
            else:
                CNODMN = 0.0

            CTONOD = min(CNODMN + max(0.0, CTONOD), CAVVEG) + CTONODR

            # Call nitrogen fixation routine if ISWSYM is 'Y' and thermal time exceeds lag phase
            if ISWNIT == 'Y' and ISWSYM == 'Y':
                if VSTAGE > TTFIX:
                    CNOD, DWNOD, DWNODA, NDTH, NFIXN, NODGR, WTNFX, SENNOD = NFIX(
                        RC.INTEGR, AGRNOD, CNODMN, CTONOD, DLAYR, DXR57,  # Input
                        FILECC, FILEIO, NLAYR, NR7, PLTPOP,  # Input
                        SAT, ST, SW, TURFAC  # Input
                    )

            # If ISWSYM = 'U', assume N-fixation occurs as carbon allows, without explicit nodules
            if (ISWNIT == 'Y' and ISWSYM == 'U') or (ISWNIT != 'Y'):
                NFIXN = max(0.0, NDMREP + NDMVEG - NAVL)
                CNOD = RFIXN * NFIXN / 0.16

            # Accumulate NAVL for growth, reduce PGAVL by cost to fix N
            if PGAVL > CNOD:
                PGAVL -= CNOD
            else:
                PGAVL = 0.0

            NAVL += NFIXN

            # -----------------------------------------------------------------------
            # Call routine to compute actual seed and shell growth
            # -----------------------------------------------------------------------
            (
                AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT, PODNO, POTCAR, POTLIP,
                SDNO, SDVAR, SEEDNO, SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,
                WTSHE, WTSHMT, FLWN
            ) = PODS(
                RC.INTEGR, AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,  # Input
                FILEGC, FILEIO, FNINL, FNINSD, FNINSH, GDMSD,  # Input
                GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH,  # Input
                NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,  # Input
                PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  # Input
                TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2,  # Input
                PStres2, YRPLT  # Input
            )

            # -----------------------------------------------------------------------
            # Call specific routines for peanut to determine:
            #     Seed size
            #     Pod color
            #     Pod Detachment
            # -----------------------------------------------------------------------
            if DETACH == 'Y' and DAS >= NR1:
                (WSHIDT, WTSD, WTSHE) = PODDET(
                    FILECC, TGRO, WTLF, YRDOY, YRNR2,  # Input
                    PODWTD, SDNO, SHELN, SWIDOT,  # Output
                    RC.INTEGR  # Control
                )

            # -----------------------------------------------------------------------
            # Compute carbon required for seed (CGRSD) and shell (CGRSH) growth
            # -----------------------------------------------------------------------
            CGRSD = WSDDTN * AGRSD3
            CGRSH = WSHDTN * AGRSH1

            # -----------------------------------------------------------------------
            # Reduce PGAVL by C used for seed growth
            # Also reduce NAVL by N used for seed growth
            # -----------------------------------------------------------------------
            if PGAVL > (CGRSD + CGRSH):
                PGAVL = PGAVL - CGRSD - CGRSH
            else:
                PGAVL = 0.0

            NAVL = NAVL - (NGRSD + NGRSH)
            PGAVL = max(0.0, PGAVL)
            NAVL = max(0.0, NAVL)

            # -----------------------------------------------------------------------
            # CSAVEV is a fraction of PG for vegetative growth that is stored
            # as CH2O. Increase this as plant moves from R1 into seed fill.
            # These two statements came from the VEGGR subroutine - chp
            # -----------------------------------------------------------------------
            CSAVEV = CADPR1 * PGAVL * FRACDN
            PGAVL = PGAVL - CSAVEV

            # -----------------------------------------------------------------------
            # Call routine to compute actual vegetative growth, C to mine or add
            # -----------------------------------------------------------------------
            (
                AGRVG, FRLF, FRRT, FRSTM, CADLF, CADST, CANHT, CANWH, CMINEA, CRUSLF,
                CRUSRT, CRUSSH, CRUSST, EXCESS, NADLF, NADRT, NADST, NGRLF, NGRRT,
                NGRST, NSTRES, TNLEAK, WLDOTN, WRDOTN, WSDOTN
            ) = VEGGR(
                RC.INTEGR, AGRLF, AGRRT, AGRSTM, CMINEP, CSAVEV, DTX,  # Input
                DXR57, ECONO, FILECC, FILEGC, FNINL, FNINR,  # Input
                FNINS, KCAN, NAVL, NDMNEW, NDMOLD,  # Input
                NFIXN, NMINEA, NR1, PAR, PCH2O, PG, PGAVL,  # Input
                PStres2, ROWSPC, RVSTGE, STMWT, TGRO,  # Input
                TRNU, TURFAC, VSTAGE, WCRLF, WCRRT, WCRSH,  # Input
                WCRST, WTLF, XLAI, YRDOY, YREMRG  # Input
            )

            # -----------------------------------------------------------------------
            # Compute C required for LF, ST, and RT growth, and remaining C and N
            # -----------------------------------------------------------------------
            PGAVL = PGAVL - AGRVG * (WLDOTN + WSDOTN + WRDOTN)
            NAVL = NAVL - (NGRLF + NGRST + NGRRT)
            NAVL = NAVL - (NADLF + NADST + NADRT)
            PGAVL = PGAVL - (CADST + CADLF) * PCH2O

            # -----------------------------------------------------------------------
            # Call leaf senescence routine to compute leaf loss variables
            # -----------------------------------------------------------------------
            (SLDOT, SLNDOT, SSDOT, SSNDOT) = SENES(
                RC.INTEGR, FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,  # Input
                RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI  # Input
            )
            # -----------------------------------------------------------------------
            #     Call freeze damage routine if TMIN is less than FREEZ1 deg C
            # -----------------------------------------------------------------------
            if TMIN < FREEZ1:
                (MDATE, WLFDOT, CropStatus) = FREEZE(
                    FREEZ2, IDETO, NOUTDO, NRUSLF, SLDOT,  # Input
                    TMIN, WTLF, YRDOY, YRPLT,  # Input
                    MDATE  # Input/Output
                )
            else:
                WLFDOT = 0.0

            # -----------------------------------------------------------------------
            #     Call to root growth and rooting depth routine
            # -----------------------------------------------------------------------
            (RLV, RTDEP, SATFAC, SENRT, SRDOT) = ROOTS(
                RC.INTEGR,
                AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT,  # Input
                ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,  # Input
                SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW  # Input
            )

            # -----------------------------------------------------------------------
            #     Compute total C cost for growing seed, shell, and vegetative tissue
            #     for tomorrow's potential growth calculations
            # -----------------------------------------------------------------------
            #       Calculate the respiration required for seed, shell, and veg tissue
            #       depending on the source of N uptake
            # -----------------------------------------------------------------------
            if (TRNU + NFIXN + NMINEA) > 1.0E-4:
                RPROAV = ((RSPNO3 + RSPNH4) * 0.16 + NFIXN * RFIXN + NMINEA * RPRO) / (TRNU + NFIXN + NMINEA)
            else:
                RPROAV = (RNO3C + RNH4C) / 2.0

            # -----------------------------------------------------------------------
            #     AGRSD2 = SDPRO*RPROAV + PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA
            #    &         + (SDLIP*RLIP + PCARSD*RCH2O)*(1. - SDPROR)
            # -----------------------------------------------------------------------
            AGRSD2 = FNINSD * 6.25 * RPROAV + PMINSD * RMIN + PLIGSD * RLIG + POASD * ROA + POTLIP * RLIP + POTCAR * RCH2O
            AGRSH2 = PROSHI * RPROAV + PLIPSH * RLIP + PLIGSH * RLIG + POASH * ROA + PMINSH * RMIN + PCARSH * RCH2O
            AGRVG2 = AGRVG + (FRLF * PROLFI + FRRT * PRORTI + FRSTM * PROSTI) * RPROAV

            # -----------------------------------------------------------------------
            #     Call routine to integrate growth and damage
            # -----------------------------------------------------------------------
            (
                AREALF, BETN, CANNAA, CANWAA, CLW, CropStatus,
                CSW, DWNOD, DWNODA, GROWTH, GRWRES, LAIMX, PCCSD,
                PCLSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PLTPOP,
                PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,
                PODWT, PUNCSD, PUNCTR, RHOL, RHOS, RNITP,
                ROWSPC, RTWT, SDNPL, SDRATE, SDWT,
                SEEDNI, SEEDNO, SENESCE, SHELWT, SLA,
                SLAAD, STMWT, TOPWT, TOTWT, WCRLF, WCRRT, WCRSH,
                WCRST, WNRLF, WNRRT, WNRSH, WNRST, WTCO,
                WTLF, WTLO, WTMAIN, WTNCAN, WTNEW, WTNLA, WTNLF,
                WTNLO, WTNNA, WTNNAG, WTNNO, WTNNOD, WTNOO,
                WTNRA, WTNRO, WTNRT, WTNSA, WTNSD, WTNSDA,
                WTNSDO, WTNSH, WTNSHA, WTNSHO, WTNSO, WTNST,
                WTNUP, WTRO, WTSDO, WTSHO, WTSO, XLAI, XPOD,
                ShutMob, RootMob, ShelMob, XHLAI
            ) = GROW(
                CONTROL, ISWITCH, RC.INTEGR, SOILPROP,
                AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,  # Input
                CRUSST, DISLA, F, FILECC, FRLF, FRSTM,  # Input
                NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,  # Input
                NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,  # Input
                NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,  # Input
                POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,  # Input
                SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,  # Input
                SSNDOT, TRNH4U, TRNO3U, TRNU,  # Input
                TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,  # Input
                WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,  # Input
                MDATE, YRPLT,  # Input
                SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI  # Input/Output
            )

            # -----------------------------------------------------------------------
            #     Compute percentage nitrogen in vegetative tissue
            # -----------------------------------------------------------------------
            if (WTLF + STMWT) > 0.0001:
                PCNVEG = (WTNLF + WTNST) / (WTLF + STMWT) * 100.0
            else:
                # PCNVEG = -99.    # Wait for GBuild fix for -99's
                PCNVEG = 0.0
        #-----------------------------------------------------------------------
        #     End of DAS > NVEG0 if construct
        #-----------------------------------------------------------------------
        # -----------------------------------------------------------------------
        # OUTPUT / SEASEND section
        # -----------------------------------------------------------------------
        if DYNAMIC == RC.OUTPUT or DYNAMIC == RC.SEASEND:
            # -----------------------------------------------------------------------
            # Handle crop-specific operations
            # -----------------------------------------------------------------------
            if CROP != 'FA':
                if YRDOY == YREND and DYNAMIC == RC.OUTPUT:
                    STGDOY[16] = YREND

                # -----------------------------------------------------------------------
                # Call PEST function if disease simulation is enabled
                # -----------------------------------------------------------------------
                # if ISWDIS == 'Y':
                #     PEST(
                #         CONTROL, ISWITCH,
                #         AREALF, CLW, CSW, LAGSD, LNGPEG, NR2, PGAVL,  # Input
                #         PHTIM, PLTPOP, RTWT, SLA, SLDOT, SOILPROP,  # Input
                #         SSDOT, STMWT, TOPWT, WLFDOT, WTLF, YRPLT,  # Input
                #         RLV, SDNO, SHELN, SWIDOT,  # I/O
                #         VSTAGE, WSHIDT, WTSD, WTSHE,  # I/O
                #         ASMDOT, DISLA, NPLTD, PPLTD,  # Output
                #         SDDES, WLIDOT, WRIDOT, WSIDOT, SDWT  # Output
                #     )

                # -----------------------------------------------------------------------
                # Call PODS function to compute pod growth and related processes
                # -----------------------------------------------------------------------
                PODS(
                    DYNAMIC,
                    AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,  # Input
                    FILEGC, FILEIO, FNINL, FNINSD, FNINSH, GDMSD,  # Input
                    GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH,  # Input
                    NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,  # Input
                    PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  # Input
                    TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2,  # Input
                    PStres2, YRPLT,  # Input
                    AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,  # Output
                    PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,  # Output
                    SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,  # Output
                    WTSHE, WTSHMT, FLWN  # Output
                )

                # -----------------------------------------------------------------------
                # Call OPGROW function for growth tracking and related computations
                # -----------------------------------------------------------------------
                OPGROW(
                    CONTROL, ISWITCH, SOILPROP,
                    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,
                    GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES,
                    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG,
                    PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS,
                    RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO,
                    SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT,
                    TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST,
                    WTNSD, WTNUP, WTNFX, XLAI, YRPLT
                )

                # -----------------------------------------------------------------------
                # Call P_CGRO function for phosphorus uptake modeling
                # -----------------------------------------------------------------------
                if ISWPHO in ('Y', 'H'):
                    P_CGRO(
                        DYNAMIC, ISWITCH,
                        CROP, FILECC, MDATE, PCNVEG, PLTPOP, RLV,  # Input
                        RootMob, RTDEP, RTWT, SDWT, SeedFrac,  # Input
                        ShelMob, SHELWT, ShutMob, SOILPROP,  # Input
                        SPi_AVAIL, STMWT, SWIDOT, VegFrac, WLIDOT,  # Input
                        WRIDOT, WSHIDT, WSIDOT, WTLF, YRPLT,  # Input
                        SENESCE,  # I/O
                        PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed,  # Output
                        PStres1, PStres2, PUptake, FracRts  # Output
                    )

            # -----------------------------------------------------------------------
            # Write to Overview.out and summary.out files
            # -----------------------------------------------------------------------
            OPHARV(
                CONTROL, ISWITCH,
                AGEFAC, CANHT, CANNAA, CANWAA, CROP,  # Input
                HARVFRAC, LAIMX, MDATE, NSTRES, PCLSD, PCNSD,  # Input
                PODNO, PODWT, PStres1, PStres2, SDRATE, SDWT,  # Input
                SEEDNO, STGDOY, SWFAC, TOPWT, TURFAC,  # Input
                VSTAGE, WTNCAN, WTNFX, WTNSD, WTNST, WTNUP,  # Input
                XLAI, RSTAGE, YREMRG, YRNR1, YRNR3, YRNR5,  # Input
                YRNR7, YRPLT,  # Input
                SDWTAH  # Output
            )

            # -----------------------------------------------------------------------
            # Call PlantNBal function for seasonal nitrogen balance calculations
            # -----------------------------------------------------------------------
            if DYNAMIC == RC.SEASEND:
                if CROP != 'FA':
                    PlantNBal(
                        CONTROL, ISWITCH,
                        SEEDNI, TNLEAK, WTNFX, WTNLA, WTNLF, WTNLO,  # Input
                        WTNNA, WTNNO, WTNNOD, WTNRA, WTNRO, WTNRT,  # Input
                        WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA,  # Input
                        WTNSHO, WTNSO, WTNST, WTNUP  # Input
                    )

                # -----------------------------------------------------------------------
                # Calculate harvest residue left in field
                # -----------------------------------------------------------------------
                HRes_CGRO(
                    CONTROL,
                    CROP, DLAYR, DWNOD, HARVFRAC, NLAYR, PConc_Shut,  # Input
                    PConc_Root, PConc_Shel, PConc_Seed, PLIGLF,  # Input
                    PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST, RLV,  # Input
                    RTWT, SDWT, SENESCE, SHELWT, STMWT, WTLF,  # Input
                    WTNLF, WTNNOD, WTNRT, WTNSD, WTNSH, WTNST,  # Input
                    HARVRES  # Output
                )

                # Reset senescence-related attributes
                SENESCE.ResWt = 0.0
                SENESCE.ResLig = 0.0
                SENESCE.ResE = 0.0
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************
    #***********************************************************************
    #     Store plant module data for use in ETPHOT.
    PUT_Char('PLANT', 'CANHT',  CANHT)
    PUT_Char('PLANT', 'CANWH',  CANWH)
    PUT_Char('PLANT', 'DXR57',  DXR57)
    PUT_Char('PLANT', 'EXCESS', EXCESS)
    PUT_Char('PLANT', 'NR5',    NR5)
    PUT_Char('PLANT', 'PLTPOP', PLTPOP)
    PUT_Char('PLANT', 'RNITP',  RNITP)
    PUT_Char('PLANT', 'SLAAD',  SLAAD)
    PUT_Char('PLANT', 'XPOD',   XPOD)


    return (
        CANHT, CropStatus, EORATIO, HARVRES, KSEVAP, KTRANS, MDATE, NSTRES,
        PSTRES1, PUptake, PORMIN, RLV, RWUMX, SENESCE, STGDOY, FracRts, UNH4,
        UNO3, XHLAI, XLAI
    )

#***********************************************************************
# AGEFAC    Relative effect of current leaf N on canopy photosynthesis
#             (0-1) (fraction)
# AGRLF     Mass of CH2O required for new leaf growth (g[CH2O] / g[leaf])
# AGRNOD    CH2O requirement for nodule growth (g[CH2O] / g[nodule])
# AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
# AGRSD1    CH2O requirement for seed growth, excluding cost for protein
#             content (g[CH2O] / g[seed])
# AGRSD2    CH2O requirement for seed growth, including cost for protein
#             content (g[CH2O] / g[seed])
# AGRSD3    CH2O requirement for seed growth, with reduced N content
#            (g[CH2O] / g[seed])
# AGRSH1    CH2O required for shell growth, excluding cost for protein
#             content (g[CH2O] / g[shell])
# AGRSH2    CH2O requirement for shell growth, including cost for protein
#             content (g[CH2O] / g[shell])
# AGRSTM    Mass of CH2O required for new stem growth (g[CH2O] / g[stem])
# AGRVG     Mass of CH2O required for vegetative tissue growth including
#             stoichiometry and respiration (g[CH2O] / g[tissue])
# AGRVG2    Total mass of CH2O required for vegetative tissue growth
#            (g[CH2O] / g[tissue])
# AREALF    Area of leaves (one side) per unit ground area
#            (cm2[leaf] / m2[ground])
# ASMDOT    Daily assimilative damage (g[CH2O] /m2 / d)
# BETN      Spacing between plants along a row (m / plant)
# BWAH      Weight of by-product not harvested (top weight minus seed
#             weight) (g/m2)
# CADLF     Mass of CH2O added to leaf reserves after growth
#            (g[CH2O] / m2 / d)
# CADPR1    Maximum fraction of stem growth after flowering that can be
#             allocated to carbohydrate storage just before a full seed
#             load is set. (fraction)
# CADST     Mass of CH2O added to stems (g[CH2O] / m2 / d)
# CANHT     Canopy height (m)
# CANNAA    Weight of N in total plant at flowering (g[N] / m2)
# CANWAA    Canopy weight at flowering stage (g[plant] / m2)
# CANWH     Canopy width normal to row (m)
# CAVVEG    C available for vegetative tissue growth (g[CH2O] / m2 / d)
# CDMREP    Total CH2O needed for potential reproductive growth
#            (g[CH2O] / m2 / d)
# CGRSD     Carbon required for seed growth (g[CH2O] / m2 / d)
# CGRSH     Carbon required for shell growth (g[CH2O] / m2 / d)
# CLW       Cumulative leaf growth (g[leaf]/m2)
# CMINEA    Actual carbon mined from vegetative tissue (g[CH2O] / m2 / d)
# CMINEP    Potential CH2O mobilization from storage (g[CH2O] / m2 / d)
# CMOBMX    Maximum C pool mobilization rate (g[CH2O] / m2 / d)
# CNDFX     Carbon needed to fix N needed but not supplied by uptake or
#             mining (g[CH2O] / m2 / d)
# CNOD      C used in N-Fixation and nodule growth (including respiration
#             costs) today (g[CH2O] / m2 / d)
# CNODMN    Minimum C reserved for nodule growth (g[CH2O] / m2 / d)
# CO2       Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
# CONTROL   Composite variable containing variables related to control
#             and/or timing of simulation.    See Appendix A.
# CROP      Crop identification code
# CRUSLF    C mobilized from leaf tissue in a day (g[CH2O] / m2 / d)
# CRUSRT    C mobilized from root tissue in a day (g[CH2O] / m2 / d)
# CRUSSH    C mobilized from shell tissue in a day (g[CH2O] / m2 / d)
# CRUSST    C mobilized from stem tissue in a day (g[CH2O] / m2 / d)
# CSAVEV    Fraction of PG for VEG that is stored as CH2O
# CSW       Cumulative stem growth (g[stem]/m2)
# CTONOD    C to allocate to nodules to fix N needed for reproductive and
#             vegetative growth (g[CH2O] / m2 / d)
# CTONODR   CH2O allocated to nodules for fixing N needed for reproductive
#             growth (g[CH2O] / m2 / d)
# DAS       Days after start of simulation (d)
# DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
# DETACH    Switch to determine if pod detachment will be simulated (Y or
#             N)
# DISLA     Diseased leaf area (cm2[leaf]/m2[ground]/d)
# DLAYR(L)  Thickness of soil layer L (cm)
# DRPP      Photoperiod days which occur in a real day
#            (photoperiod days / day)
# DS(L)     Cumulative depth in soil layer L (cm)
# DTX       Thermal time that occurs in a real day based on vegetative
#             development temperature function (thermal days / day)
# DUL(L)    Volumetric soil water content at Drained Upper Limit in soil
#             layer L (cm3[water]/cm3[soil])
# DWNOD     Current nodule mass (g[nodule] / m2)
# DWNODA    Cumulative nodule growth (g[nodule] / m2)
# DXR57     Relative time between first seed (NR5) and physiological
#             maturity (NR7) (fraction)
# ECONO     Ecotype code - used to match ECOTYP in .ECO file
# EOP       Potential plant transpiration rate (mm/d)
# EORATIO   Ratio of increase in potential evapotranspiration with increase
#             in LAI (up to LAI=6.0) for use with FAO-56 Penman reference
#             potential evapotranspiration.
# EP1       Actual plant transpiration rate (cm/d)
# EXCESS    Factor based on excess PG used to affect tomorrow's PG
#             calculation
# F         Specific leaf area of new leaf tissue growth, including N
#            (cm2[leaf] / g[leaf])
# FILECC    Path plus filename for species file (*.spe)
# FILEGC    Pathname plus filename for ECO file
# FILEIO    Filename for input file (e.g., IBSNAT35.INP)
# FLWN(J)   Number of flowers added for cohort J (# / m2)
# FNINL     Maximum fraction of N for growing leaf tissue (g[N] / g[leaf])
# FNINR     Maximum fraction of N for growing root tissue (g[N] / g[root])
# FNINS     Maximum fraction of N for growing stem tissue (g[N] / g[stem])
# FNINSD    Maximum fraction of N for growing seed tissue based on
#             temperature (g[N] / g[seed])
# FNINSH    Maximum fraction of N for growing shell tissue
#            (g[N] / g[shell])
# FRACDN    Relative time between flowering (NR1) and last leaf appearance
#             (NDLEAF)
# FRCNOD    Fraction of new root dry matter allocation that is diverted to
#             nodule growth
# FREEZ1    Temperature below which plant loses all leaves, but development
#             continues (°C)
# FREEZ2    Temperature below which plant growth stops completely. (°C)
# FRLF      Fraction of vegetative tissue growth that goes to leaves on a
#             day (g[leaf] / g[veg])
# FRRT      Fraction of vegetative tissue growth that goes to roots on a
#             day (g[root] / g[veg])
# FRSTM     Fraction of vegetative tissue growth that goes to stems on a
#             day (g[stem] / g[veg])
# GDMSD     Seed growth demand based on temperature and photoperiod
#            (g[seed] / m2 / d)
# GROWTH    Total new growth of plant tissue on a day (g[tissue] / m2 / d)
# GRRAT1    Maximum growth per individual shell (g / shell / d)
# GRWRES    Growth respiration (g[CH2O]/m2-d)
# HARVFRAC  Two-element array containing fractions of (1) yield harvested
#             and (2) by-product harvested (fraction)
# HARVRES   Composite variable containing harvest residue amounts for total
#           dry matter, lignin, and N amounts.  Structure of variable is
#           defined in ModuleDefs.for.
# IDETO     Switch for printing OVERVIEW.OUT file
# ISWDIS    Pest damage simulation switch (Y or N)
# ISWITCH   Composite variable containing switches which control flow of
#             execution for model.  The structure of the variable
#             (SwitchType) is defined in ModuleDefs.for.
# ISWNIT    Nitrogen simulation switch (Y or N)
# ISWSYM    Nitrogen fixation simulation switch (Y = simulate nodule
#             growth, N = no nodule growth, U = N-fixation occurs at a rate
#             that carbon will allow, and nodules are not grown explicitly)
# ISWWAT    Water simulation control switch (Y or N)
# KCAN      Canopy light extinction coefficient for daily PAR, for
#             equidistant plant spacing, modified when in-row and between
#             row spacing are not equal
# KEP       Energy extinction coefficient for partitioning EO to EP
# KSEVAP    Light extinction coefficient used for computation of soil
#             evaporation
# KTRANS    Light extinction coefficient used for computation of plant
#             transpiration
# LAGSD     Time required between shell growth and seed growth, per cohort
#          (Photo-thermal days)
# LAIMX     Maximum leaf area index this season (m2[leaf] / m2[ground])
# LL(L)     Volumetric soil water content in soil layer L at lower limit
#            (cm3 [water] / cm3 [soil])
# LNGPEG    Time between start of peg (full flower) and shell formation
#             (for peanuts only).  Defines slow growth period.
#             (Photo-thermal days)
# MAINR     Maintenance respiration (g[CH2O] / m2 / d)
# MDATE     Harvest maturity date (YYYYDDD)
# MEPHO     Method for photosynthesis computation ('C'=Canopy or daily,
#             'L'=hedgerow or hourly)
# NADLF     N added to leaf N reserves (g[N] / m2 / d)
# NADRT     N added to root N reserves (g[N] / m2 / d)
# NADST     N added to stem N reserves (g[N] / m2 / d)
# NAVL      Total mass of nitrogen available for growth (g[N] / m2 / d)
# NAVLV     N available for vegetative growth (g[N] / m2 / d)
# NDLEAF    Day when leaf expansion ceased (d)
# NDMNEW    Total N demand for new growth (g[N] / m2 / d)
# NDMOLD    N demand for old tissue (g[N] / m2 / d)
# NDMREP    Total N needed for potential reproductive growth
#            (g[N] / m2 / d)
# NDMSDR    Amount of Mobilized N which can be used for seed growth
#            (g[N] / m2 / d)
# NDMTOT    Total N demand (g[N] / m2 / d)
# NDMVEG    N required for vegetative growth if all PGAVL is used as
#             computed (g[N] / m2 / d)
# NDSET     Normal time by which a pod load (full number) should be
#             achieved with no water stress (d)
# NDTH      Nodule death rate (g[nodule] / m2 / d)
# NFIXN     Amount of N fixed during the day (g[N] / m2 / d)
# NGRLF     Maximum N demand for leaf growth (g[leaf N] / m2[ground] / d)
# NGRRT     Maximum N demand for root growth (g[root N] / m2[ground] / d)
# NGRSD     Rate of N accumulation in new seeds (g[N] / m2 / d)
# NGRSH     Rate of N accumulation in new shells (g[N] / m2 / d)
# NGRST     Maximum N demand for stem growth (g[stem N] / m2[ground] / d)
# NH4(L)    Ammonium N in soil layer L (µg[N] / g[soil])
# NLAYR     Actual number of soil layers
# NMINEA    Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
# NMINEP    Potential N mobilization from storage (g[N] / m2 / d)
# NMOBR     Stage-dependent potential N mining rate expressed as a fraction
#             of the maximum rate (NMOBMX)
# NO3(L)    Nitrate in soil layer L (µg[N] / g[soil])
# NODGR     New nodule growth (g[nod] / m2 / d)
# NOUTDO    Logical unit for OVERVIEW.OUT file
# NPLTD     Number of plants destroyed (#/m2/d)
# NR1       Day when 50% of plants have at least one flower (d)
# NR2       Day when 50% of plants have one fruit (pod or peg) (d)
# NR5       Day when 50% of plants have pods with beginning seeds (d)
# NR7       Day when 50% of plants first have yellowing or maturing pods
#            (d)
# NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
# NRUSRT    N actually mobilized from roots in a day (g[N]/m2-d)
# NRUSSH    N actually mobilized from shells in a day (g[N]/m2-d)
# NRUSST    N actually mobilized from stems in a day (g[N]/m2-d)
# NSTRES    Nitrogen stress factor (1=no stress, 0=max stress)
# NVEG0     Day of emergence (d)
# PAR       Daily photosynthetically active radiation or photon flux
#             density (moles[quanta]/m2-d)
# PCARSH    Proportion of shell tissue that is carbohydrate (fraction)
# PCCSD     Percentage of carbohydrate in seed tissue (100 g[C] / g[seed])
# PCH2O     Respiration loss due to storage/mobilization of CH2O
#            (g[CH2O] / g[CH2O])
# PCLSD     Percentage of lipid in seed tissue (100 g[lipid] / g[seed])
# PCNL      Percentage of N in leaf tissue (100 g[N] / g[leaf])
# PCNRT     Percent N in root tissue (100 g[N] / g[root])
# PCNSD     Percentage of N in seed tissue (100 g[N] / g[seed])
# PCNSH     Percentage of N in shell tissue (100 g[N] / g[shell])
# PCNST     Percent N in stem tissue (100 g[N] / g[stem])
# PCTMAT    Fraction of pods that are mature (seed are 90% of final size)
# PG        Daily gross photosynthesis (g[CH2O] / m2 - d)
# PGAVL     Total available CH2O available for growth & respiration
#            (g[CH2O] / m2)
# PHTHRS(I) Threshold time that must accumulate in phase I for the next
#             stage to occur (thermal or photothermal days)
# PHTIM(I)  Cumulative photothermal time ages of seeds and shells
# PLIGLF    Proportion of leaf tissue that is lignin (fraction)
# PLIGNO    Proportion of nodule tissue that is lignin (fraction)
# PLIGRT    Proportion of root tissue that is lignin (fraction)
# PLIGSD    Proportion of seed tissue that is lignin (fraction)
# PLIGSH    Proportion of shell tissue that is lignin (fraction)
# PLIGST    Proportion of stem tissue that is lignin (fraction)
# PLIPSH    Proportion of shell tissue that is lipid (fraction)
# PLTPOP    Plant population (# plants / m2)
# PMINSD    Proportion of seed tissue that is mineral (fraction)
# PMINSH    Proportion of shell tissue that is mineral (fraction)
# PNTIM(I)  Photothermal days from first flower when flowers in age group I
#             formed (p-t-d)
# POASD     Proportion of seed tissue that is organic acid (fraction)
# POASH     Proportion of shell tissue that is organic acid (fraction)
# PODNO     Total number of pods (#/m2)
# PODWT     Dry mass of seeds plus shells, including C and N
#            (g[pods] / m2[ground])
# PODWTD    Mass of detached pods (g[pods] / m2[ground])
# PORMIN    Minimum pore space required for supplying oxygen to roots for
#             optimal growth and function (cm3/cm3)
# POTCAR    Potential carbohydrate composition of seed based on temperature
#            (fraction)
# POTLIP    Potential lipid composition of seed based on temperature
#            (fraction)
# PPLTD     Percent plants destroyed (%/m2/d)
# PROLFI    Maximum protein composition in leaves during growth with
#             luxurious supply of N (g[protein] / g[leaf tissue])
# PRORTI    Maximum protein composition in roots during growth with
#             luxurious supply of N (g[protein] / g[root])
# PROSHI    Maximum protein composition in shells during growth with
#             luxurious supply of N (g[protein] / g[shell tissue])
# PROSTI    Maximum protein composition in stems during growth with
#             luxurious supply of N (g[protein] / g[stem])
# PROVEG    Average protein composition of growing tissue today
#            (g[protein] / g[veget. tissue])
# PUNCSD    Cumulative puncture damage to seed (not yet implemented)
# PUNCTR    Cumulative puncture damage (not yet implemented)
# R30C2     Respiration coefficient that depends on total plant mass, value
#             at 30C (g[CH2O] used / g[CH2O] fixed / hr)
# RCH2O     Respiration required for synthesizing CH2O structure
#            (g[CH2O] / g[tissue])
# RES30C    Respiration coefficient that depends on gross photosynthesis,
#             value at 30C (g CH2O/g DW/hr)
# RFIXN     CH2O required for biological N fixation (g[CH2O] / g[protein])
# RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
# RHOS      Fraction of stem which is carbohydrate (g [CH2O] / g[stem])
# RLIG      Respiration required for synthesizing lignin structure
#            (g[CH2O] / g[lignin])
# RLIP      Respiration required for synthesizing lipid structure
#            (g[CH2O] / g[lipid])
# RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
# RMIN      Respiration required for synthesizing mineral structure
#            (g[CH2O] / g[mineral])
# RNH4C     CH2O required for protein synthesis when source of N is
#             ammonium uptake (g[CH2O] / g[protein])
# RNITP     True nitrogen concentration in leaf tissue for photosynthesis
#             reduction. (%)
# RNMODE    Simulation run mode (I=Interactive, A=All treatments, B=Batch
#             mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
# RNO3C     Respiration required for reducing NO3 to protein
#            (g[CH2O] / g[protein])
# RO        Respiration coefficient that depends on total plant mass
#            (g[CH2O] / g[tissue])
# ROA       Respiration required for synthesizing organic acids
#            (g[CH2O] / g[product])
# ROWSPC    Row spacing (m)
# RP        Proportion of the day's photosynthesis which is respired in the
#             maintenance process
# RPRO      Respiration required for re-synthesizing protein from mobilized
#           N (g[CH2O] / g[protein])
# RPROAV    Respiration required for protein synthesis, average based on
#             sources of N (g[CH2O] / g[protein])
# RSPNH4    Respiration required for reducing NH4 to protein
#            (g[CH2O] / m2 / d)
# RSPNO3    Respiration required for reducing NO3 to protein
#            (g[CH2O] / m2 / d)
# RSTAGE    Number of RSTAGES which have occurred.
# RTDEP     Root depth (cm)
# RTWT      Dry mass of root tissue, including C and N
#            (g[root] / m2[ground])
# RUN       Change in date between two observations for linear
#             interpolation
# RVSTGE    Rate of VSTAGE change (nodes/day)
# RWUEP1    Threshold for reducing leaf expansion compared w/ ratio of
#             TRWU/EP1 (total potential daily root water uptake/ actual
#             transpiration)
# RWUMX     Maximum water uptake per unit root length, constrained by soil
#             water (cm3[water] / cm [root])
# SAT(L)    Volumetric soil water content in layer L at saturation
#            (cm3 [water] / cm3 [soil])
# SATFAC    Root length weighted soil water excess stress factor ( 0 = no
#             stress; 1 = saturated stress )
# SDDES(J)  Number of seeds destroyed today in cohort J when shells are not
#             destroyed (#/m2/d)
# SDGR      Potential growth rate per seed (g / seed / d)
# SDIDOT    Number of seeds destroyed on the current day (#/m2/d)
# SDNO(J)   Number of seeds for cohort J (#/m2)
# SDNPL     Seed N (g[N] / m2)
# SDPROR    Ratio to adjust lipid and carbohydrate proportions when seed
#             protein differs from protein composition of standard cultivar
#             (SDPROS)
# SDRATE    Seeding rate, mass of seed sown (g[seed] / m2[ground])
# SDVAR     Maximum cultivar-dependent seed growth rate, per seed
#            (g / seed / d)
# SDWT      Dry mass of seed tissue, including C and N
#            (g[seed] / m2[ground])
# SDWTAH    Actual seed weight harvested (g[seed] / m2[ground])
# SEEDNI    Seed or transplant N at planting (g[N] / m2)
# SEEDNO    Total number of seeds (#/m2)
# SENESCE   Composite variable containing data about daily senesced plant
#             matter. Structure of variable is defined in ModuleDefs.for
# SENNOD(L) Daily senesced matter from nodules in soil layer L
#            (kg[dry matter]/ha)
# SENRT(L)  Daily senesced matter from roots in soil layer L
#            (kg[dry matter]/ha)
# SHELN(J)  Number of shells for cohort J (#/m2)
# SHELWT    Total mass of all shells (g / m2)
# SHVAR     Shell growth rate during its rapid growth phase, per shell
#            (g / shell / d)
# SLA       Specific leaf area (cm2[leaf] / m2[ground])
# SLAAD     Specific leaf area, excluding weight of C stored in leaves
#            (cm2[leaf] / g[leaf])
# SLDOT     Defoliation due to daily leaf senescence (g/m2/day)
# SLNDOT    Leaf senescence due to water stress (g/m2/day)
# SLPF      Soil photosynthesis factor, 0 to 1 scale
# SOILPROP  Composite variable containing soil properties including bulk
#             density, drained upper limit, lower limit, pH, saturation
#             water content.  Structure defined in ModuleDefs.
# SRDOT     Daily root senescence (g / m2 / d)
# SSDOT     Daily senescence of petioles (g / m2 / d)
# SSNDOT    Petiole senescence due to water stress (g/m2/day)
# ST(L)     Soil temperature in soil layer L (°C)
# STGDOY(I) Day when plant stage I occurred (YYYYDDD)
# STMWT     Dry mass of stem tissue, including C and N
#            (g[stem] / m2[ground)
# SW(L)     Volumetric soil water content in layer L
#            (cm3 [water] / cm3 [soil])
# SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress,
#             0.0=max stress
# SWIDOT    Daily seed mass damage (g/m2/day)
# TAVG      Average daily temperature (°C)
# TDAY      Average temperature during daylight hours (°C)
# TDUMX     Photo-thermal time that occurs in a real day based on early
#             reproductive development temperature function
#             (photo-thermal days / day)
# TDUMX2    Photo-thermal time that occurs in a real day based on late
#             reproductive development temperature function
#             (photo-thermal days / day)
# TGRO(I)   Hourly canopy temperature (°C)
# TGROAV    Average daily canopy temperature (°C)
# TMIN      Minimum daily temperature (°C)
# TNLEAK    Total nitrogen leak (g[N] / m2 / d)
# TOPWT     Total weight of above-ground portion of crop, including pods
#            (g[tissue] / m2)
# TOTWT     Total weight of crop (g[tissue] / m2)
# TRNH4U    Total N uptake in ammonium form in a day (kg[N] / ha / d)
# TRNO3U    Total N uptake in nitrate form in a day (kg[N] / ha / d)
# TRNU      Total N uptake in a day (kg[N] / ha / d)
# TRWUP     Potential daily root water uptake over soil profile (cm/d)
# TTFIX     Physiological days delay in nodule initiation
#            (photo-thermal days / day)
# TURADD    Water stress factor (TURFAC) effect on reproductive growth and
#             pod addition.  Stress is defined to INCREASE growth and
#             addition.
# TURFAC    Water stress factor for expansion (0 - 1)
# UNH4(L)   Rate of root uptake of NH4, computed in NUPTAK
#            (kg [N] / ha - d)
# UNO3(L)   Rate of root uptake of NO3, computed in NUPTAK (kg [N] / ha -d)
# VSTAGE    Number of nodes on main stem of plant (nodes)
# WCRLF     Mass of CH2O reserves in leaves (g[leaf CH2O] / m2[ground])
# WCRRT     Mass of CH2O reserves in roots (g[root CH2O] / m2[ground])
# WCRSH     Mass of CH2O reserves in shells (g[shell CH2O] / m2[ground])
# WCRST     Mass of CH2O reserves in stems (g[stem CH2O] / m2[ground])
# WLDOTN    Dry weight growth rate of new leaf tissue including N but not C
#             reserves (g[leaf] / m2[ground]-d)
# WLFDOT    Leaf weight losses due to freezing (g[leaf]/m2-d)
# WLIDOT    Daily pest or freeze damage to leaf mass (g/m2/day)
# WNRLF     N available for mobilization from leaves above lower limit of
#             mining (g[N] / m2)
# WNRRT     N available for mobilization from roots above lower limit of
#             mining (g[N] / m2)
# WNRSH     N available for mobilization from shells above lower limit of
#             mining (g[N] / m2)
# WNRST     N available for mobilization from stems above lower limit of
#           mining (g[N] / m2)
# WR(L)     Root hospitality factor, used to compute root distribution
# WRDOTN    Dry weight growth rate of new root tissue including N but not C
#             reserves (g[root] / m2[ground]-d)
# WRIDOT    Daily pest damage to root mass (g/m2/day)
# WSDDTN    New seed growth today (g[seed] / m2 / d)
# WSDOTN    Dry weight growth rate of new stem tissue including N but not C
#             reserves (g[stem] / m2[ground]-d)
# WSHDTN    New shell growth today (g[shell] / m2 / d)
# WSHIDT    Weight of shell tissue consumed by pests today (g[shell]/m2-d)
# WSIDOT    Daily pest damage to stem mass (g/m2/day)
# WTABRT    Weight of shells aborted on a day (g[shell] / m2 / d)
# WTCO      Cumulative losses of plant tissue (g[tissue] / m2)
# WTLF      Dry mass of leaf tissue including C and N
#            (g[leaf] / m2[ground])
# WTLO      Cumulative leaf losses (g[leaf] / m2)
# WTMAIN    Mass of tissue assumed to require maintenance (g[tissue] / m2)
# WTNCAN    Mass of N in canopy (g[N] / m2[ground])
# WTNEW     Initial mass of seedling or seed (g / plant)
# WTNFX     Cumulative weight of N fixed (g[N] / m2)
# WTNLA     Cumulative N added to leaves (g[N]/m2-d)
# WTNLF     Mass of N in leaves (g[leaf N] / m2[ground])
# WTNLO     Cumulative N loss from leaves (g[N] / m2)
# WTNNA     Cumulative N added to nodules (g[N]/m2-d)
# WTNNAG    Total accumulated N used in nodule growth (g[N] / m2)
# WTNNO     Cumulative N loss from nodules (g[N] / m2)
# WTNNOD    Mass of N in nodules (g[N] / m2[ground])
# WTNOO     Cumulative nodule losses (g[nodule] / m2)
# WTNRA     Cumulative N added to roots (g[N]/m2-d)
# WTNRO     Cumulative N loss from roots (g[N] / m2)
# WTNRT     Mass of N in roots (g[root N] / m2[ground])
# WTNSA     Cumulative N added to stems (g[N]/m2-d)
# WTNSD     Mass of N in seeds (g[N] / m2[ground])
# WTNSDA    Cumulative N added to seeds (g[N]/m2-d)
# WTNSDO    Cumulative N loss from seeds (g[N] / m2)
# WTNSH     Mass of N in shells (g[N] / m2[ground])
# WTNSHA    Cumulative N added to shells (g[N]/m2-d)
# WTNSHO    Cumulative N loss from shells (g[N] / m2)
# WTNSO     Cumulative N loss from stems (g[N] / m2)
# WTNST     Mass of N in stems (g[stem N] / m2[ground])
# WTNUP     Cumulative N uptake (g[N] / m2)
# WTRO      Cumulative root losses (g[root] / m2)
# WTSD(J)   Seed mass  for cohort J (g/m2)
# WTSDO     Cumulative seed losses (g[seed] / m2)
# WTSHE(J)  Shell mass  for cohort J (g/m2)
# WTSHMT    Cohorts that reach THRESH today (g/m2)
# WTSHO     Cumulative shell losses (g[shell] / m2)
# WTSO      Cumulative stem losses (g[stem] / m2)
# XFRT      Current day's partitioning to reproductive growth (0-1)
#            (g[fruit] / g[plant])
# XHLAI     Healthy leaf area index (m2[leaf] / m2[ground])
# XLAI      Leaf area (one side) per unit of ground area
#            (m2[leaf] / m2[ground])
# XPOD      Growth partitioning to pods which slows node appearance
#            (fraction)
# YRDOY     Current day of simulation (YYYYDDD)
# YREMRG    Day of emergence (YYYYDDD)
# YREND     Date for end of season (usually harvest date) (YYYYDDD)
# YRNR1     Day when 50% of plants have at least one flower (YYYYDDD)
# YRNR2     Day when 50% of plants have one fruit (pod or peg) (YYYYDDD)
# YRNR3     Day when 50% of plants have at least one beginning pod
#            (YYYYDDD)
# YRNR5     Day when 50% of plants have pods with beginning seeds (YYYYDDD)
# YRNR7     Day when 50% of plants first have yellowing or maturing pods
#            (YYYYDDD)
# YRPLT     Planting date (YYYYDDD)
# **********************************************************************