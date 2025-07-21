# C=======================================================================
# C  GROW, Subroutine, G. Hoogenboom, J.W. Jones, and K.J.Boote
# C-----------------------------------------------------------------------
# C  Governing Model Equations
# C  Integrates all growth related variables
# C-----------------------------------------------------------------------
# C  Called by:  PLANT
# C  Calls:      IPGROW, STRESS
# C              ERROR
# C=======================================================================
from ERROR import ERROR
from ModuleDefs import *
def GROW(CONTROL, ISWITCH, DYNAMIC, SOILPROP,
    AGEFAC, CADLF, CADST, CRUSLF, CRUSRT, CRUSSH,     #Input
    CRUSST, DISLA, F, FILECC, FRLF, FRSTM,            #Input
    NADLF, NADRT, NADST, NDTH, NFIXN, NGRLF, NGRRT,   #Input
    NGRSD, NGRSH, NGRST, NMINEA, NODGR, NOUTDO,       #Input
    NPLTD, NRUSLF, NRUSRT, NRUSSH, NRUSST,            #Input
    POTCAR, POTLIP, PPLTD, SDIDOT, SDPROR,            #Input
    SENNOD, SENRT, SLDOT, SLNDOT, SRDOT, SSDOT,       #Input
    SSNDOT, TRNH4U, TRNO3U, TRNU,                     #Input
    TURFAC, WLDOTN, WLIDOT, WRDOTN, WRIDOT, WSDDTN,   #Input
    WSDOTN, WSHDTN, WSIDOT, WTABRT, WTSHMT, YRNR1,    #Input
    MDATE, YRPLT,                                     #Input
    SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI):     #Input/output

    SENESCE = ResidueType()
    ERRKEY = 'GROW  '
    FILEIO  = CONTROL.FILEIO
    YRDOY   = CONTROL.YRDOY

    IDETO  = ISWITCH.IDETO
    IHARI  = ISWITCH.IHARI
    ISWNIT = ISWITCH.ISWNIT
    ISWSYM = ISWITCH.ISWSYM

    NLAYR  = SOILPROP.NLAYR

    
    # !***********************************************************************
    # !     Run Initialization - Called once per simulation
    # !***********************************************************************
    if DYNAMIC == RUNINIT:
        (CROP,ALPHL,  ALPHR,  ALPHS,  ALPHSH,
        PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,
        PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,
        PLIPLF, PLIPST, PLIPRT, PLIPSH,                 PLIPNO,
        PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO,
        POALF,  POAST,  POART,  POASH,  POASD,  POANO,
        PROLFF, PROSTF, PRORTF, PROSHF,                 PRONOD,
        PROLFI, PROSTI, PRORTI,
        PLTPOP, ROWSPC, RMIN,   PLME,   SDWTPL,
        SDLIP,  SDPRO,  WTFSD,  WTPSD, XPODF) = IPGROW(FILEIO, FILECC)

        if CROP != 'FA':
            ROWSPC = ROWSPC / 100.
            if ROWSPC * PLTPOP > 1.E-4:
                BETN = 1. / (ROWSPC * PLTPOP)
            else:
                BETN = 0.

        CPFLF = 30. / 44. * (PLIPLF * 1.720 + PLIGLF * 0.659 + POALF * (-0.011)
                             + PMINLF * RMIN + PCARLF * 0.170)
        CPFSTM = 30. / 44. * (PLIPST * 1.720 + PLIGST * 0.659 + POAST * (-0.011)
                              + PMINST * RMIN + PCARST * 0.170)
        CPFRT = 30. / 44. * (PLIPRT * 1.720 + PLIGRT * 0.659 + POART * (-0.011)
                             + PMINRT * RMIN + PCARRT * 0.170)
        CPFNOD = 30. / 44. * (PLIPNO * 1.720 + PLIGNO * 0.659 + POANO * (-0.011)
                              + PMINNO * RMIN + PCARNO * 0.170)
        CPFSH1 = 30. / 44. * (PLIPSH * 1.720 + PLIGSH * 0.659 + POASH * (-0.011)
                              + PMINSH * 0.073 + PCARSH * 0.170)
        CPFSD1 = 30. / 44. * (PMINSD * 0.073 + PLIGSD * 0.659 + POASD * (-0.011)
                              + (SDLIP * 1.720 + PCARSD * 0.170) * (1. - SDPROR))

    #C-----------------------------------------------------------------------
    #  CALCULATE PERCENT NITROGEN IN LEAVES AT END OF SEASON
        PCNMIN = PROLFF * 16.0
    elif(DYNAMIC == SEASINIT):
        ALFDOT = 0.0
        AREALF = 0.0
        AREAH  = 0.0
        CANWAA = 0.0
        CANNAA = 0.0
        GROWTH = 0.0
        GRWRES = 0.0
        LAIMX  = 0.0
        NLDOT  = 0.0
        NSDOT  = 0.0
        NRDOT  = 0.0
        NSDDOT = 0.0
        NSHDOT = 0.0
        NTOVR  = 0.0
        PCCSD  = 0.0
        PCLSD  = 0.0
        PCNL   = 0.0
        PCNRT  = 0.0
        PCNSD  = 0.0
        PCNSH  = 0.0
        PCNST  = 0.0
        PODWT  = 0.0
        RHOL   = 0.0
        RHOR   = 0.0
        RHOS   = 0.0
        RHOSH  = 0.0
        RTWT   = 0.0
        SDRATE = 0.0
        SDWT   = 0.0
        SDWTAM = 0.0
        SEEDNI = 0.0
        SEEDNO = 0.0

        SenWt  = 0.0
        SenLig = 0.0
        SenE   = 0.0
        SENESCE.ResWt  = 0.0
        SENESCE.ResLig = 0.0
        SENESCE.ResE   = 0.0
        SENESCE.CumResE = [0.0]*N #N is global?

        SHELWT = 0.0
        SLAAD  = 0.0
        STMWT  = 0.0
        TGROW  = 0.0
        TOPWT  = 0.0
        TOTWT  = 0.0
        WCRLF  = 0.0
        WCRRT  = 0.0
        WCRSH  = 0.0
        WCRST  = 0.0
        WNRLF  = 0.0
        WNRRT  = 0.0
        WNRSH  = 0.0
        WNRST  = 0.0
        WSDDOT = 0.0
        WSHDOT = 0.0
        WTCO   = 0.0
        WTCSD  = 0.0
        WTLF   = 0.0
        WTLO   = 0.0
        WTLSD  = 0.0
        WTMAIN = 0.0
        WTNCAN = 0.0
        WTNLA  = 0.0
        WTNLF  = 0.0
        WTNLO  = 0.0
        WTNMOB = 0.0
        WTNNA  = 0.0
        WTNNAG = 0.0
        WTNNO  = 0.0
        WTNNOD = 0.0
        WTNOO  = 0.0
        WTNRA  = 0.0
        WTNRO  = 0.0
        WTNRT  = 0.0
        WTNSA  = 0.0
        WTNSD  = 0.0
        WTNSDA = 0.0
        WTNSDO = 0.0
        WTNSH  = 0.0
        WTNSHA = 0.0
        WTNSHO = 0.0
        WTNSO  = 0.0
        WTNST  = 0.0
        WTNTOT = 0.0
        WTNUP  = 0.0
        WTRO   = 0.0
        WTSDO  = 0.0
        WTSHO  = 0.0
        WTSO   = 0.0
        XLAI   = 0.0
        XHLAI  = 0.0
        XPOD   = 0.0

        CLW = 0.0   #Cumulative leaf growth
        CSW = 0.0   #Cumulative stem growth

        ShutMob = 0.0
        RootMob = 0.0
        ShelMob = 0.0

        SDPDOT = 0.0    #CHP - not used
        PUNDOT = 0.0    #CHP - not used

        NLPEST = 0.0    #CHP - N loss due to pest damage
        SLA = 0.0

# !***********************************************************************
# !     EMERGENCE CALCULATIONS - Performed once per season upon emergence
# !         or transplanting of plants
# !***********************************************************************
    elif DYNAMIC == EMERG:
        WLDOT  = 0.0
        WSDOT  = 0.0
        WRDOT  = 0.0
        WNDOT  = 0.0
        WPDOT  = 0.0
        WLFDOT = 0.0

        # Initial seedling or transplant weight
        SDRATE  = WTPSD * PLTPOP / 0.8 * 10

        if PLME != 'T':
            WTNEW  = WTFSD * WTPSD
            TOTWT  = WTNEW * PLTPOP
        else:
            TOTWT = SDWTPL / 10.
            WTNEW = TOTWT / PLTPOP

        #Initial mass of plant components
        WTLF  = FRLF * TOTWT
        STMWT = FRSTM * TOTWT
        TOPWT = WTLF + STMWT
        RTWT  = TOTWT - TOPWT

        WLFI   = WTLF
        WSTI   = STMWT
        WRTI   = RTWT

        # Initialize cumulative variables
        CLW   = WTLF
        CSW   = STMWT

        # CH2O reserves
        WCRLF = ALPHL * WTLF
        WCRST = ALPHS * STMWT
        WCRRT = ALPHR * RTWT

        # Carbohydrate composition of plant components (fraction)
        RHOL   = ALPHL
        RHOR   = ALPHR
        RHOS   = ALPHS
        RHOSH  = ALPHSH

        # Net carbon addition variables
        WRCLDT = 0.0
        WRCSDT = 0.0
        WRCRDT = 0.0
        WRCSHD = 0.0

        # Compute N in plant components
        WTNLF  = WLFI * PROLFI * 0.16
        WTNST  = WSTI * PROSTI * 0.16
        WTNRT  = WRTI * PRORTI * 0.16
        WTNSH  = 0.0
        WTNSD  = 0.0
        WTNTOT = WTNLF + WTNST + WTNRT + WTNSH + WTNSD

        # Seed or transplant N at planting
        SDNPL  = WTPSD * SDPRO * 0.16 * 0.75 * PLTPOP - (WTNLF + WTNST + WTNRT)
        SDNPL  = max(SDNPL,0.0)
        SEEDNI = WTNLF + WTNST + WTNRT + SDNPL

        # Initialize cumulative N variables
        WTNLA  = WTNLF
        WTNSA  = WTNST
        WTNRA  = WTNRT

        # Percent N in plant components
        PCNL   = WTNLF / WLFI * 100.
        RNITP  = PCNL
        PCNST  = WTNST / WSTI * 100.
        PCNRT  = WTNRT / WRTI * 100.

        # Intialize leaf area.  Value of F initialized in DEMAND.
        AREALF = WTLF * F
        AREAH  = AREALF
        SLA    = AREALF / WTLF
        SLAAD  = AREALF / (WTLF - WCRLF)
        XLAI   = AREALF / 10000.
        XHLAI  = XLAI

    #***********************************************************************
    #     Daily integration
    #***********************************************************************
    elif DYNAMIC == INTEGR:
        NLPEST = 0.0    #CHP - N loss due to pest damage
        GROWTH = WLDOTN + WSDOTN + WRDOTN + WSHDTN + WSDDTN + NODGR
        GRWRES = (WLDOTN*CPFLF + WSDOTN*CPFSTM + WRDOTN * CPFRT + NODGR*CPFNOD +
                  WSHDTN*CPFSH1 + WSDDTN*CPFSD1 + 30./44 *
                  (TRNO3U/0.16 * 1.798 + TRNH4U/0.16 * 0.462 + NFIXN/0.16 * 1.798))
        # C-----------------------------------------------------------------------
        # C    Use 1982 Penning de Vries book, p. 125, composition of tissue,
        # C    fraction C for six categories, convert by 30/12 to CH20.
        # C-----------------------------------------------------------------------
        # C     Account for N added to existing plant tissue, e.g., NADLF, that
        # C     is damaged by insects, freezing, or senesced.  Otherwise, could
        # C     get increase in tissue N composition when tissue is aborted.  Need
        # C     to account for mass, N and C lost this way in sections below
        WLDOT = WLDOTN - SLDOT - WLIDOT - WLFDOT - NRUSLF / 0.16 - CRUSLF

        #ShutMob is amount of leaf mass lost due to N and C mobilization
        #A positive value represents leaf mass lost. (kg/ha)
        ShutMob = (NRUSLF/0.16 + CRUSLF) * 10.      #kg/ha

        if WTLF > 1.E-4:
            WLDOT = WLDOT + (CADLF+NADLF/0.16) * (1. - min(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
            ADD = (CADLF+NADLF/0.16) * (1. - MIN(1.0,(SLDOT+WLIDOT+WLFDOT)/WTLF))
            ShutMob = ShutMob - ADD * 10.             #kg/ha
        else:
            ADD = 0.0

         if WLDOT < 0.0:
           WLDOT = min(WLDOT, -WTLF)

        # -----------------------------------------------------------------------
        #       WSDOT = Net stem growth rate
        # -----------------------------------------------------------------------
        WSDOT = WSDOTN - SSDOT - WSIDOT - NRUSST / 0.16 - CRUSST
        ShutMob = ShutMob + (NRUSST / 0.16 + CRUSST) * 10.  # kg/ha

        if STMWT > 1.0e-4:
            WSDOT = WSDOT + (CADST + NADST / 0.16) * (1.0 - min(1.0, (SSDOT + WSIDOT) / STMWT))
            ADD = (CADST + NADST / 0.16) * (1.0 - min(1.0, (SSDOT + WSIDOT) / STMWT))
            ShutMob = ShutMob - ADD * 10.  # kg/ha
        else:
            ADD = 0.0

        if WSDOT < 0.0:
            WSDOT = max(WSDOT, -TMWT)

        # -----------------------------------------------------------------------
        #     Net root growth rate
        # -----------------------------------------------------------------------
        WRDOT = WRDOTN - SRDOT - NRUSRT / 0.16 - CRUSRT - WRIDOT
        RootMob = (NRUSRT / 0.16 + CRUSRT) * 10.  # kg/ha

        if RTWT > 1.0e-4:
            WRDOT = WRDOT + (NADRT / 0.16) * (1.0 - min(1.0, (SRDOT + WRIDOT) / RTWT))
            ADD = (NADRT / 0.16) * (1.0 - min(1.0, (SRDOT + WRIDOT) / RTWT))
            RootMob = RootMob - ADD * 10.  # kg/ha
        else:
            ADD = 0.0

        if WRDOT < 0.0:
            WRDOT = max(WRDOT, -RTWT)

        # -----------------------------------------------------------------------
        #     Net shell growth rate
        # -----------------------------------------------------------------------
        WSHIDT = min(WSHIDT, SHELWT)  # pest damage to shells
        WSHDOT = WSHDTN - WSHIDT - WTABRT - NRUSSH / 0.16 - CRUSSH
        ShelMob = (NRUSSH / 0.16 + CRUSSH) * 10.  # kg/ha

        # -----------------------------------------------------------------------
        #     Net seed growth rate
        # -----------------------------------------------------------------------
        SWIDOT = min(SWIDOT, SDWT)  # pest damage to seeds
        WSDDOT = WSDDTN - SWIDOT
        WTLSD = WTLSD + WSDDOT * POTLIP  # lipids in seed
        WTCSD = WTCSD + WSDDOT * POTCAR  # carbohydrates in seed
        # -----------------------------------------------------------------------
        #     Net nodule growth rate
        # -----------------------------------------------------------------------
        WNDOT = NODGR - NDTH

        # -----------------------------------------------------------------------
        #     Net pod growth rate
        # -----------------------------------------------------------------------
        WPDOT = WSHDOT + WSDDOT

        # -----------------------------------------------------------------------
        #     Total Net plant growth rate
        # -----------------------------------------------------------------------
        WDOT = WLDOT + WSDOT + WRDOT + WPDOT + WNDOT

        # -----------------------------------------------------------------------
        if GROWTH > 1.0e-4:
            if XPODF == 'PD':
                XPOD = 0.17 * (WSDDTN + WSHDTN) / GROWTH + 0.83 * XPOD
            elif XPODF == 'SD':
                XPOD = 0.17 * (WSDDTN) / GROWTH + 0.83 * XPOD
            else:
                ERROR(ERRKEY, 1, '      ', 0)

        # -----------------------------------------------------------------------
        #    Integration, Add Today's Net Growth to Existing Weights
        # -----------------------------------------------------------------------
        TOTWT = TOTWT + WDOT
        TOPWT = TOPWT + WSDOT + WLDOT + WPDOT
        WTLF = WTLF + WLDOT
        STMWT = STMWT + WSDOT
        SDWT = SDWT + WSDDOT
        SHELWT = SHELWT + WSHDOT
        RTWT = RTWT + WRDOT
        PODWT = PODWT + WPDOT
        DWNOD = DWNOD + WNDOT
        TGROW = TGROW + GROWTH

        # -----------------------------------------------------------------------
        #     Cumulative leaf and stem growth
        # -----------------------------------------------------------------------
        CLW = CLW + WLDOTN
        CSW = CSW + WSDOTN

        # -----------------------------------------------------------------------
        #     Store values of TOPWT at flowering and SDWT at maturity
        # -----------------------------------------------------------------------
        if YRDOY == YRNR1:
            CANWAA = TOPWT
        elif YRDOY == MDATE:
            SDWTAM = SDWT

        # ---------------------------------------------------------------------
        #     Compute Seed Weight for Which there is Maintenance Costs.
        # -----------------------------------------------------------------------
        WSDMAN = min(SDWT, SHELWT)
        WTMAIN = TOTWT - SDWT + WSDMAN

        # -----------------------------------------------------------------------
        #      Carbon Reserves:  Net Growth Rates for Mobile Carbohydrates
        # -----------------------------------------------------------------------
        #      Account for N added to existing plant tissue, e.g., NADLF, that
        #      is damaged by insects, freezing, or senesced.  Otherwise, could
        #      get increase in tissue N composition when tissue is aborted.  Need
        #      to account for mass, N and C lost this way in sections below
        # -----------------------------------------------------------------------
        WRCLDT = ALPHL * WLDOTN - CRUSLF - RHOL * (SLNDOT + WLIDOT + WLFDOT)
        if WTLF > 1.0e-4:
            WRCLDT = WRCLDT + CADLF * (1.0 - min(1.0, (SLDOT + WLIDOT + WLFDOT) / WTLF))

        WRCSDT = ALPHS * WSDOT - CRUSST - RHOS * SSNDOT
        if STMWT > 1.0e-4:
            WRCSDT = WRCSDT + CADST * (1.0 - min(1.0, (SSDOT + WSIDOT) / STMWT))

        WRCRDT = ALPHR * WRDOT - CRUSRT - RHOR * SRDOT
        WRCSHD = ALPHSH * WSHDOT - CRUSSH - RHOSH * (WTABRT + WTSHMT + WSHIDT)

        # -----------------------------------------------------------------------
        #     Update C Storage, Concentrations in Leaves, Stems, Roots, and Shells
        # -----------------------------------------------------------------------
        WCRLF = WCRLF + WRCLDT
        WCRST = WCRST + WRCSDT
        WCRRT = WCRRT + WRCRDT
        WCRSH = WCRSH + WRCSHD

        if WCRLF <= 1.0e-30:
            WCRLF = 0.0
        if WCRST <= 1.0e-30:
            WCRST = 0.0
        if WCRRT <= 1.0e-30:
            WCRRT = 0.0
        if WCRSH <= 1.0e-30:
            WCRSH = 0.0

        # -----------------------------------------------------------------------
        #     Compute Cumulative C loss During Season
        # -----------------------------------------------------------------------
        WTLO = WTLO + SLDOT + WLIDOT + WLFDOT
        WTSO = WTSO + SSDOT + WSIDOT
        WTRO = WTRO + SRDOT + WRIDOT
        WTSHO = WTSHO + WTABRT + WSHIDT
        WTSDO = WTSDO + SWIDOT
        WTNOO = WTNOO + NDTH
        WTCO = WTLO + WTSO + WTSHO + WTSDO

        # -----------------------------------------------------------------------
        #     Compute CH20 fractions
        # -----------------------------------------------------------------------
        if WTLF > 0.0001:
            RHOL = WCRLF / WTLF
        else:
            RHOL = 0.0

        if STMWT > 0.0001:
            RHOS = WCRST / STMWT
        else:
            RHOS = 0.0

        if RTWT > 0.0001:
            RHOR = WCRRT / RTWT
        else:
            RHOR = 0.0

        if SHELWT > 0.0001:
            RHOSH = WCRSH / SHELWT
        else:
            RHOSH = 0.0

        # =======================================================================
        #     Nitrogen Balance:  Net Growth Rates for Nitrogen Components
        # =======================================================================
        #      Account for N added to existing plant tissue, e.g., NADLF, that
        #      is damaged by insects, freezing, or senesced.  Otherwise, could
        #      get increase in tissue N composition when tissue is aborted.  Need
        #      to account for mass, N and C lost this way in sections below
        # -----------------------------------------------------------------------
        #
        # -----------------------------------------------------------------------
        #      Leaf nitrogen senescence and pest damage loss
        # -----------------------------------------------------------------------
        NLOFF = (SLNDOT + WLIDOT + WLFDOT) * (PCNL / 100.0) + (SLDOT - SLNDOT) * PROLFF * 0.16
        NLPEST = NLPEST + WLIDOT * PCNL / 100.0
        if NLOFF < 0.0:
            NLOFF = 0.0

        # -----------------------------------------------------------------------
        #     Net growth rate of nitrogen in the leaves
        # -----------------------------------------------------------------------
        NLDOT = NGRLF - NLOFF - NRUSLF
        if WTLF > 1.0e-4:
            NLDOT = NLDOT + NADLF * (1.0 - min(1.0, (SLDOT + WLIDOT + WLFDOT) / WTLF))

        # -----------------------------------------------------------------------
        #     Stem nitrogen senescence and pest damage loss
        # -----------------------------------------------------------------------
        NSOFF = (SSNDOT + WSIDOT) * (PCNST / 100.0) + (SSDOT - SSNDOT) * PROSTF * 0.16
        NLPEST = NLPEST + WSIDOT * PCNST / 100.0
        if NSOFF < 0.0:
            NSOFF = 0.0

        # -----------------------------------------------------------------------
        #     Net growth rate of nitrogen in the stems
        # -----------------------------------------------------------------------
        NSDOT = NGRST - NSOFF - NRUSST
        if STMWT > 1.0e-4:
            NSDOT = NSDOT + NADST * (1.0 - min(1.0, (SSDOT + WSIDOT) / STMWT))

        # -----------------------------------------------------------------------
        #     Root nitrogen senescence and pest damage loss
        # -----------------------------------------------------------------------
        NROFF = (SRDOT + WRIDOT) * (PCNRT / 100.0)
        if NROFF < 0.0:
            NROFF = 0.0

        # -----------------------------------------------------------------------
        #     Net growth rate of nitrogen in the roots
        # -----------------------------------------------------------------------
        NRDOT = NGRRT - NROFF - NRUSRT
        if RTWT > 1.0e-4:
            NRDOT = NRDOT + NADRT * (1.0 - min(1.0, (SRDOT + WRIDOT) / RTWT))

        # -----------------------------------------------------------------------
        #     Shell nitrogen senescence, abortion, and pest damage loss
        # -----------------------------------------------------------------------
        NSHOFF = (WTABRT + WSHIDT) * (PCNSH / 100.0)
        NLPEST = NLPEST + WSHIDT * PCNSH / 100.0
        if NSHOFF < 0.0:
            NSHOFF = 0.0

        # -----------------------------------------------------------------------
        #     Net growth rate of nitrogen in shells
        # -----------------------------------------------------------------------
        NSHDOT = NGRSH - NSHOFF - NRUSSH

        # -----------------------------------------------------------------------
        #     Seed nitrogen senescence, abortion, and pest damage loss
        # -----------------------------------------------------------------------
        NSDOFF = SWIDOT * PCNSD / 100.0
        if NSDOFF < 0.0:
            NSDOFF = 0.0

        # -----------------------------------------------------------------------
        #     Net growth rate of nitrogen in seeds
        # -----------------------------------------------------------------------
        NSDDOT = NGRSD - NSDOFF

        # -----------------------------------------------------------------------
        #     Integration, Update N in Each Component by Adding Today's Growth
        # -----------------------------------------------------------------------

        # -----------------------------------------------------------------------
        #     Total nitrogen in the leaves
        # -----------------------------------------------------------------------
        if (NLDOT < 0.0) and (abs(NLDOT) > WTNLF):
            NLDOT = -WTNLF
        WTNLF = WTNLF + NLDOT

        # -----------------------------------------------------------------------
        #     Total nitrogen in the stems
        # -----------------------------------------------------------------------
        if (NSDOT < 0.0) and (abs(NSDOT) > WTNST):
            NSDOT = -WTNST
        WTNST = WTNST + NSDOT

        # -----------------------------------------------------------------------
        #     Total nitrogen in the roots
        # -----------------------------------------------------------------------
        if (NRDOT < 0.0) and (abs(NRDOT) > WTNRT):
            NRDOT = -WTNRT
        WTNRT = WTNRT + NRDOT

        # -----------------------------------------------------------------------
        #     Total nitrogen in the nodules
        # -----------------------------------------------------------------------
        WTNNOD = DWNOD * 0.16 * PRONOD

        # -----------------------------------------------------------------------
        #     Total nitrogen in the shells
        # -----------------------------------------------------------------------
        if (NSHDOT < 0.0) and (abs(NSHDOT) > WTNSH):
            NSHDOT = -WTNSH
        WTNSH = WTNSH + NSHDOT

        if (NSDDOT < 0.0) and (abs(NSDDOT) > WTNSD):
            NSDDOT = -WTNSD

        # -----------------------------------------------------------------------
        #     Total nitrogen in the seed
        # -----------------------------------------------------------------------
        WTNSD = WTNSD + NSDDOT

        # -----------------------------------------------------------------------
        #     Total nitrogen in the plant
        # -----------------------------------------------------------------------
        WTNTOT = WTNLF + WTNST + WTNRT + WTNSH + WTNSD + WTNNOD

        # -----------------------------------------------------------------------
        #     Total nitrogen in the canopy (all above-ground components)
        # -----------------------------------------------------------------------
        WTNCAN = WTNLF + WTNST + WTNSH + WTNSD

        # -----------------------------------------------------------------------
        #     Save total nitrogen in the canopy at the start of flowering
        # -----------------------------------------------------------------------
        if YRDOY == YRNR1:
            CANNAA = WTNCAN
        # -----------------------------------------------------------------------
        #     Compute Cumulative N added during season
        # -----------------------------------------------------------------------
        # !   CHP - working on Plant N balance:
        # !   Add adjustment factors for N addition.
        # !     These factors are used in computation of net addition to
        # !     tissue, but previously not here.  Adding these statements
        # !     forces cumulative N additions to equal 'current N' plus
        # !     'senesced N'.
        # !   However . . .  by making this change, cumulative tissue N no longer
        # !     matches 'Seed N' plus 'N2 fixed' plus 'N uptake'.  Why???
        # Leaf
        NLALL = NGRLF - NRUSLF
        if WTLF > 1.0e-4:
            NLALL += NADLF * (1.0 - min(1.0, (SLDOT + WLIDOT + WLFDOT) / WTLF))

        # Stem
        NSALL = NGRST - NRUSST
        if STMWT > 1.0e-4:
            NSALL += NADST * (1.0 - min(1.0, (SSDOT + WSIDOT) / STMWT))

        # Root
        NRALL = NGRRT - NRUSRT
        if RTWT > 1.0e-4:
            NRALL += NADRT * (1.0 - min(1.0, (SRDOT + WRIDOT) / RTWT))

        # Shell and seed
        NSHALL = NGRSH - NRUSSH
        NSDALL = NGRSD

        WTNLA += NLALL
        WTNSA += NSALL
        WTNRA += NRALL
        WTNSHA += NSHALL
        WTNSDA += NSDALL

        # -----------------------------------------------------------------------
        #    Nodules
        # -----------------------------------------------------------------------
        if ISWNIT == 'Y' and ISWSYM == 'Y':
            DWNODA += NODGR
            WTNFX += NFIXN + (NODGR * 0.16 * PRONOD) - NTOVR
            WTNNAG = DWNODA * 0.16 * PRONOD

        # -----------------------------------------------------------------------
        #     Compute Cumulative N loss During Season
        # -----------------------------------------------------------------------
        WTNLO += NLOFF
        WTNSO += NSOFF
        WTNRO += NROFF
        WTNSHO += NSHOFF
        WTNSDO += NSDOFF
        if ISWNIT == 'Y' and ISWSYM == 'Y':
            NNOFF = NDTH * 0.16 * PRONOD
            WTNNO += NNOFF

        # -----------------------------------------------------------------------
        #     N Balance Components for Mobilized, Uptake, and Fixed N
        # -----------------------------------------------------------------------
        WTNMOB += NMINEA
        WTNUP += TRNU
        WTNNA = WTNNOD + WTNNO

        # -----------------------------------------------------------------------
        #     Compute Percentage N in each Plant Component
        # -----------------------------------------------------------------------
        PCNL = (WTNLF / WTLF * 100.0) if (WTLF > 1.0e-4 and WTLF > WTNLF and WTNLF > 1.0e-5) else 0.0
        PCNST = (WTNST / STMWT * 100.0) if (STMWT > 1.0e-4 and WTNST > 1.0e-5) else 0.0
        PCNRT = (WTNRT / RTWT * 100.0) if (RTWT > 1.0e-4 and WTNRT > 1.0e-5) else 0.0
        PCNSH = (WTNSH / SHELWT * 100.0) if (SHELWT > 1.0e-4 and WTNSH > 1.0e-5) else 0.0

        # -----------------------------------------------------------------------
        #     Compute Percentage Seed Composition
        # -----------------------------------------------------------------------
        if SDWT > 0.01:
            PCNSD = WTNSD / SDWT * 100.0
            PCLSD = WTLSD / SDWT * 100.0
            PCCSD = WTCSD / SDWT * 100.0
        else:
            PCNSD = PCLSD = PCCSD = 0.0

        # -----------------------------------------------------------------------
        #    Calculate true nitrogen concentration in leaf tissue for photosynthesis reduction.
        # -----------------------------------------------------------------------
        RNITP = (100.0 * WTNLF / (WTLF - WCRLF)) if (WTLF - WCRLF > 1.0e-4) else PCNMIN

        # -----------------------------------------------------------------------
        #     Calculate Remaining N in Shells, Leaves, Stems, and Roots That can be Mined (Plant N-Balance).
        # -----------------------------------------------------------------------
        WNRLF = max(WTNLF - PROLFF * 0.16 * (WTLF - WCRLF), 0.0) if (WTLF - WCRLF > 1.0e-4) else 0.0
        WNRST = max(WTNST - PROSTF * 0.16 * (STMWT - WCRST), 0.0) if (STMWT - WCRST > 1.0e-4) else 0.0
        WNRRT = max(WTNRT - PRORTF * 0.16 * RTWT, 0.0) if (RTWT > 1.0e-4) else 0.0
        WNRSH = max(WTNSH - PROSHF * 0.16 * SHELWT, 0.0) if (SHELWT > 1.0e-4) else 0.0

        # -----------------------------------------------------------------------
        #      This section added to provide senescence parameters to the
        #      Soil N routines (senescence and freeze values are added to soil
        #      residue; pest damage components are lost from the system)
        # -----------------------------------------------------------------------
        #      Surface carbon includes all senescence and freeze variables for
        #       leaf (SLDOT, WLFDOT), stem (SSDOT), and shell (WTABRT)
        #        At this time, the model does not senesce seed.
        #      Convert from biomass to C with a factor 0.40.
        # -----------------------------------------------------------------------
        # Convert senesced plant material to kg/ha
        SenWt[0] = max((SLDOT + WLFDOT + SSDOT + WTABRT), 0.0) * 10.0  # kg dry matter/ha

        # -----------------------------------------------------------------------
        # Surface nitrogen loss (excluding pest damage)
        # -----------------------------------------------------------------------
        SenE[0, 1] = max((NLOFF + NSOFF + NSHOFF - NLPEST), 0.0) * 10.0  # kg N/ha

        # -----------------------------------------------------------------------
        # Surface phosphorus senescence (handled in a separate routine)
        # -----------------------------------------------------------------------
        # SenE[0,2] = SenWt[0] * PConc_Shut  # kg P/ha

        # -----------------------------------------------------------------------
        # Contribution of lignin to surface litter from senesced and frozen plant matter
        # -----------------------------------------------------------------------
        SenLig[0] = max((SLDOT + WLFDOT) * PLIGLF + SSDOT * PLIGST + WTABRT * PLIGSH, 0.0) * 10.0  # kg lignin/ha

        # -----------------------------------------------------------------------
        # Senescence of roots and nodules (kg/ha)
        # -----------------------------------------------------------------------
        for L in range(1, NLAYR + 1):
            SenWt[L] = SENRT[L] + SENNOD[L]  # kg dry matter/ha
            SenLig[L] = SENRT[L] * PLIGRT + SENNOD[L] * PLIGNO  # kg lignin/ha
            # 01/19/2006 CHP: Root senescence at current N% (per KJB and JWJ)
            SenE[L, 1] = SENRT[L] * PCNRT / 100.0 + SENNOD[L] * PRONOD * 0.16

        # -----------------------------------------------------------------------
        # Leaf Area, Specific Leaf Area Calculations
        # -----------------------------------------------------------------------
        ALFDOT = WLDOTN * F - (SLDOT + WLIDOT + WLFDOT + NRUSLF / 0.16) * SLA
        if WTLF > 1.0e-4:
            ALFDOT += SLA * (NADLF / 0.16) * (1.0 - min(1.0, (SLDOT + WLIDOT + WLFDOT) / WTLF))

        AREALF += ALFDOT
        XLAI = AREALF / 10000.0

        if AREALF > 1.0e-5 and WTLF > 1.0e-4:
            SLA = AREALF / WTLF
            SLAAD = AREALF / (WTLF - WCRLF)
            if SLA > 999.0:
                SLA = 0.0
            if SLAAD > 999.0:
                SLAAD = -99.0

        # -----------------------------------------------------------------------
        # Remember XLAI
        # -----------------------------------------------------------------------
        LAIMX = max(XLAI, LAIMX)

        # -----------------------------------------------------------------------
        # Calculate "Healthy" or Non-Diseased Leaf Area Index
        # -----------------------------------------------------------------------
        AREAH = max(0.0, AREALF - DISLA)
        XHLAI = AREAH / 10000.0

        # -----------------------------------------------------------------------
        # Integrate Pest Damage to Seeds
        # -----------------------------------------------------------------------
        PUNCSD += SDPDOT  # CHP not used
        SEEDNO -= SDIDOT
        PUNCTR += PUNDOT  # CHP not used

        # -----------------------------------------------------------------------
        # Loss in Plants due to Pests, Adjust Plant Spacing Also
        # -----------------------------------------------------------------------
        if NPLTD > 1.0e-5:
            PLTPOP = max(PLTPOP - NPLTD, 0.0)
            if PLTPOP * ROWSPC > 1.0e-4:
                BETN = 1.0 / (ROWSPC * PLTPOP)

        if PPLTD > 1.0e-4:
            PLTPOP = max(PLTPOP - PPLTD * PLTPOP / 100.0, 0.0)
            if PLTPOP * ROWSPC > 1.0e-4:
                BETN = 1.0 / (ROWSPC * PLTPOP)

        # -----------------------------------------------------------------------
        # Terminate growth if stress causes extremely low plant weights
        # -----------------------------------------------------------------------
        if TOPWT < 1.0e-5 or STMWT < 1.0e-5:
            MDATE, CropStatus = STRESS(
                                    AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,  # Input
                                    RTWT, SDWT, SHELWT, STMWT, TOPWT,  # Input
                                    TOTWT, TURFAC, WTLF, YRDOY, YRPLT,  # Input
                                    MDATE  # Input/Output
                                )
            return

        if IHARI not in ('R', 'D'):
            if RTWT < 1.0e-5 or WTLF < 1.0e-5:
                MDATE, CropStatus = STRESS(
                                        AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,  # Input
                                        RTWT, SDWT, SHELWT, STMWT, TOPWT,  # Input
                                        TOTWT, TURFAC, WTLF, YRDOY, YRPLT,  # Input
                                        MDATE  # Input/Output
                                    )
                return

        # -----------------------------------------------------------------------
        # Store Senescence Data
        # -----------------------------------------------------------------------
        SENESCE["ResWt"] = SenWt
        SENESCE["ResLig"] = SenLig
        for L in range(NLAYR + 1):
            SENESCE["ResE"][L, N] = SenE[L, N]

    return (AREALF, BETN, CANNAA, CANWAA, CLW, CropStatus,
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
                ShutMob, RootMob, ShelMob,
                SWIDOT, WLFDOT, WSHIDT, WTNFX, XHLAI)

# C=======================================================================
# C  STRESS, from code in GROW subroutine
# C-----------------------------------------------------------------------
# C  Plant death due to stress
# !-----------------------------------------------------------------------
# !  Called by:  GROW
# !  Calls:      None
# C=======================================================================
def STRESS(
    AGEFAC, DWNOD, IDETO, IHARI, NOUTDO, PODWT,  # Input
    RTWT, SDWT, SHELWT, STMWT, TOPWT,            # Input
    TOTWT, TURFAC, WTLF, YRDOY, YRPLT,           # Input
    MDATE,                                       # Input/Output
):
    """
    Handles plant stress effects, setting plant weights and termination conditions.
    """

    # Ensure weights are non-negative
    TOTWT = max(0.0, TOTWT)
    TOPWT = max(0.0, TOPWT)
    WTLF = max(0.0, WTLF)
    STMWT = max(0.0, STMWT)
    SDWT = max(0.0, SDWT)
    SHELWT = max(0.0, SHELWT)
    RTWT = max(0.0, RTWT)
    PODWT = max(0.0, PODWT)
    DWNOD = max(0.0, DWNOD)

    # If the plant dies, set the maturity date and update crop status
    if MDATE < 0:
        MDATE = YRDOY
        CropStatus = 39

    # If harvest condition is met due to stress
    if IHARI == 'M':
        DAP = max(0, TIMDIF(YRPLT, YRDOY))  # Time difference function needed

        # Construct warning message
        YR, DOY = YR_DOY(YRDOY)  # Convert YRDOY to year and day-of-year
        MESSAGE_1 = f"Plant died due to extreme stress at {DAP} days after planting."
        MESSAGE_2 = f"(DAY : {YR} {DOY}; TURFAC : {TURFAC:.2f}; AGEFAC : {AGEFAC:.2f})"

        # Log warning message
        WARNING(2, 'CRPGRO', [MESSAGE_1, MESSAGE_2])

        # Output to detailed log if requested
        if IDETO == 'Y':
            print("\n  ", MESSAGE_1, "\n  ", MESSAGE_2)

    return MDATE, CropStatus  # Return updated values


# C=======================================================================
# C  IPGROW, Subroutine
# C-----------------------------------------------------------------------
# C  Reads input data for GROW routines.
# C-----------------------------------------------------------------------
# C  Called : GROW
# C  Calls  : FIND, ERROR, IGNORE
# C=======================================================================
def IPGROW(FILEIO, FILECC):
    from READS import FIND, IGNORE
    ERRKEY = 'IPGROW'
    try:
        with (open(FILEIO, 'r') as f):
            lines = f.readlines()
    except IOError as e:
        ERROR(1, e.errno, f, 0)
        return
    # -----------------------------------------------------------------------
    #     Find and read Cultivar Section
    # -----------------------------------------------------------------------
    SECTION = '*CULTI'
    LNUM, FOUND = FIND(lines, SECTION)
    if not FOUND:
        ERROR(SECTION, 42, FILEIO, LNUM)
    else:
        LNUM +=1
        CROP = lines[3:5]   #READ(LUNIO, '(3X,A2)', IOSTAT=ERR)
        #IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)
    SECTION = '*CULTI'
    LNUM, FOUND = FIND(lines, SECTION)

    if not FOUND:
        ERROR(SECTION, 42, FILEIO, LNUM)
    else:
        LNUM += 1
        CROP = lines[LNUM][3:5]  # READ(LUNIO, '(3X,A2)', IOSTAT=ERR)
        # IF (ERR .NE. 0) CALL ERROR(ERRKEY,ERR,FILEIO,LNUM)

    if CROP != 'FA':
        # -----------------------------------------------------------------------
        #    Read Planting Details Section
        # -----------------------------------------------------------------------
        SECTION = '*PLANT'
        LNUM, FOUND = FIND(lines, SECTION)

        if not FOUND:
            ERROR(SECTION, 42, FILEIO, LNUM)
        else:
            LNUM += 1
            row = lines[LNUM]  # Simulating reading the row
            try:
                #'(24X,F6.0,5X,A1,6X,F6.0,12X,F6.0)'
                PLTPOP = float(row[24:30].strip())  # F6.0
                PLME = row[35:36].strip()  # A1
                ROWSPC = float(row[42:48].strip())  # F6.0
                SDWTPL = float(row[60:66].strip())  # F6.0
                LNUM += 1
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILEIO, LNUM)

        # -----------------------------------------------------------------------
        #    Read Cultivar Section
        # -----------------------------------------------------------------------
        SECTION = '*CULTI'
        LNUM, FOUND = FIND(lines, SECTION)

        if not FOUND:
            ERROR(SECTION, 42, FILEIO, LNUM)
        else:
            LNUM += 1
            row = lines[LNUM]  # Simulating reading the row
            try:
                # '(24X,A6,66X,F6.0,24X,2F6.0)'
                ECONO = row[24:30].strip()  # A6
                WTPSD = float(row[96:102].strip())  # F6.0
                SDPRO = float(row[126:132].strip())  # F6.0
                SDLIP = float(row[132:138].strip())  # F6.0
                LNUM += 1
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILEIO, LNUM)

    if CROP != 'FA':
        # ----------------------------------------------------------------------
        #     Read in values from input file, which were previously input
        #       in Subroutine IPCROP.
        #-----------------------------------------------------------------------
        try:
            with (open(FILECC, 'r') as f):
                lines = f.readlines()
        except IOError as e:
            ERROR(1, e.errno, f, 0)
            return
        # -----------------------------------------------------------------------
        # Read RESP Section
        # -----------------------------------------------------------------------
        SECTION = '!*RESP'
        LNUM, FOUND = FIND(FILECC, SECTION)

        if not FOUND:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            LNUM += 1
            ISECT, C80 = IGNORE(FILECC, LNUM)
            ISECT, C80 = IGNORE(FILECC, LNUM)
            ISECT, C80 = IGNORE(FILECC, LNUM)

            try:
                RMIN = float(C80[24:30].strip())  # F6.0
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

        # -----------------------------------------------------------------------
        # Read PLAN Section
        # -----------------------------------------------------------------------
        SECTION = '!*PLAN'
        LNUM, FOUND = FIND(FILECC, SECTION)

        if not FOUND:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            LNUM += 1
            ISECT, C80 = IGNORE(FILECC, LNUM)

            try:
                PROLFI = float(C80[0:6].strip())
                PROLFF = float(C80[12:18].strip())
                PROSTI = float(C80[18:24].strip())
                PROSTF = float(C80[30:36].strip())
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            ISECT, C80 = IGNORE(FILECC, LNUM)
            try:
                PRORTI = float(C80[0:6].strip())
                PRORTF = float(C80[12:18].strip())
                PROSHF = float(C80[30:36].strip())
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            ISECT, C80 = IGNORE(FILECC, LNUM)
            try:
                PRONOD = float(C80[12:18].strip())
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            ISECT, C80 = IGNORE(FILECC, LNUM)
            try:
                PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO = [
                    float(C80[i:i + 6].strip()) for i in range(0, 36, 6)
                ]
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            ISECT, C80 = IGNORE(FILECC, LNUM)
            try:
                PLIPLF, PLIPST, PLIPRT, PLIPSH, PLIPNO = [
                    float(C80[i:i + 6].strip()) for i in range(0, 30, 6)
                ]
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            ISECT, C80 = IGNORE(FILECC, LNUM)
            try:
                PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO = [
                    float(C80[i:i + 6].strip()) for i in range(0, 36, 6)
                ]
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            ISECT, C80 = IGNORE(FILECC, LNUM)
            try:
                POALF, POAST, POART, POASH, POASD, POANO = [
                    float(C80[i:i + 6].strip()) for i in range(0, 36, 6)
                ]
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            ISECT, C80 = IGNORE(FILECC, LNUM)
            try:
                PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO = [
                    float(C80[i:i + 6].strip()) for i in range(0, 36, 6)
                ]
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

        # -----------------------------------------------------------------------
        # Read CARB Section
        # -----------------------------------------------------------------------
        SECTION = '!*CARB'
        LNUM, FOUND = FIND(FILECC, SECTION)

        if not FOUND:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            LNUM += 1
            ISECT, C80 = IGNORE(FILECC, LNUM)
            ISECT, C80 = IGNORE(FILECC, LNUM)

            try:
                XPODF = C80[4:6].strip()
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            ISECT, C80 = IGNORE(FILECC, LNUM)
            try:
                ALPHL, ALPHS, ALPHR, ALPHSH = [
                    float(C80[i:i + 6].strip()) for i in range(0, 24, 6)
                ]
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

        XPODF = XPODF.upper()

        # -----------------------------------------------------------------------
        # Read VEGE Section
        # -----------------------------------------------------------------------
        SECTION = '!*VEGE'
        LNUM, FOUND = FIND(FILECC, SECTION)

        if not FOUND:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            LNUM += 1
            ISECT, C80 = IGNORE(FILECC, LNUM)
            ISECT, C80 = IGNORE(FILECC, LNUM)
            ISECT, C80 = IGNORE(FILECC, LNUM)
            ISECT, C80 = IGNORE(FILECC, LNUM)

            try:
                WTFSD = float(C80[0:6].strip())
            except ValueError as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

    return (CROP, ALPHL,  ALPHR,  ALPHS,  ALPHSH,
           PCARLF, PCARST, PCARRT, PCARSH, PCARSD, PCARNO,
           PLIGLF, PLIGST, PLIGRT, PLIGSH, PLIGSD, PLIGNO,
           PLIPLF, PLIPST, PLIPRT, PLIPSH,         PLIPNO,
           PMINLF, PMINST, PMINRT, PMINSH, PMINSD, PMINNO,
           POALF,  POAST,  POART,  POASH,  POASD,  POANO,
           PROLFF, PROSTF, PRORTF, PROSHF,         PRONOD,
           PROLFI, PROSTI, PRORTI,
           PLTPOP, ROWSPC, RMIN,   PLME,   SDWTPL,
           SDLIP,  SDPRO,  WTFSD,  WTPSD,   XPODF)