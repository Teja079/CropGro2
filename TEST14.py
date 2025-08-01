#=======================================================================
#  HRes_CGRO, Subroutine
#-----------------------------------------------------------------------
#  Determines harvest residue at end of season for CROPGRO crops.
#-----------------------------------------------------------------------
#=======================================================================
#


def HRES_CGRO(CONTROL, CROP, DLAYR, DWNOD, HARVFRAC, NLAYR,
              PCONC_SHUT, PCONC_ROOT, PCONC_SHEL, PCONC_SEED,
              PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH, PLIGST,
              RLV, RTWT, SDWT, SENESCE, SHELWT, STMWT, WTLF,
              WTNLF, WTNNOD, WTNRT, WTNSD, WTNSH, WTNST):
    import numpy as np
    from ModuleDefs import ResidueType
    from fortranformat._edit_descriptors import P
    from ModuleDefs import N, NL, NELEM
#
# !-------------------------------------------------------------------------
#       USE ModuleDefs
#
#       IMPLICIT NONE
#       SAVE
#
# !     Input variables
#       CHARACTER*2 CROP
#       INTEGER NLAYR, N_ELEMS
#       REAL DWNOD, PLIGLF, PLIGNO, PLIGRT, PLIGSD, PLIGSH,
#      &     PLIGST, RTWT, SDWT, SHELWT, WTLF, STMWT,
#      &     WTNLF, WTNNOD, WTNRT, WTNSD, WTNSH, WTNST
#       REAL LFRES, STMRES, SDRES, SHLRES
#       REAL HARVFRAC(2)
#       REAL, DIMENSION(NL) :: DLAYR, RLV
#       REAL PConc_Shut, PConc_Root, PConc_Shel, PConc_Seed
#
# !     Output variables
    HARVRES = ResidueType()
#       Type (ResidueType) SENESCE
#       TYPE (ControlType) CONTROL
#
# !     Local Variables
#       INTEGER IEL, L
#       REAL TRTRES, TRLV
    TRTRESE = np.zeros(3)
    SRFC = 0
#
# !     Harvest residue variables 0 = surface
    HResWt = np.zeros(NL+1)       #Residue mass (kg[dry matter]/ha)
    HResLig = np.zeros(NL+1)      #Residue lignin (kg[lignin]/ha)
    HResE   = np.zeros((NL+1, NELEM))  #Residue element components (kg[E]/ha)
#
    N_ELEMS = CONTROL.N_ELEMS
#
# !-------------------------------------------------------------------------
    HResWt  = 0.0
    HResLig = 0.0
    HResE   = 0.0
#
# !     No residue left in fallow field.
# !     Residue only computed for sequenced runs.
    if (CROP != 'FA') and (CONTROL.RNMODE.find('QF') > 0):
#
# !-------------------------------------------------------------------------
# !       Shoot residues.
# !       ---------------
# !       Residue weight equals top weight minus seed and by-product.
# !        HResWt(SRFC) = TOPWT - SDWT - BWAH
# !                      g/m2    g/m2   g/m2
#
# !       By-product not harvested:
# !       TOPRES = (TOPWT - SDWT) * 10. * (1. - HARVFRAC(2))
# !       08/11/2005 GH/CHP
        LFRES  = max(0.0, WTLF  * 10. * (1. - HARVFRAC[1]))
        STMRES = max(0.0, STMWT * 10. * (1. - HARVFRAC[1]))

#
# !       Add weight of seeds not harvested
        SDRES = max(0.0, SDWT * 10. * (1. - HARVFRAC[1]))
#
# !       Add weight of shells (100% assumed to remain in field)
        SHLRES = max(0.0, SHELWT * 10.0)
#
        HResWt[SRFC] = LFRES + STMRES + SDRES + SHLRES
#
# !       N in residue
        HResE[SRFC, N] = ((WTNLF + WTNST) * 10.0 * (1.0 - HARVFRAC[2])
                       + WTNSD * 10.0 * (1.0 - HARVFRAC[1])
                       + WTNSH * 10.0)

        if N_ELEMS > 1:
# !         P in residue
            HResE[SRFC, P] = PCONC_SHUT * (LFRES + STMRES) \
                            + PCONC_SEED * SDRES \
                            + PCONC_SHEL * SHLRES
#         ENDIF
#
# !       Senescence has been added daily (subroutine SENESADD), so no need
# !       to add it here as WTLO, WTSO, WTSHO, WTSDO, as in the CERES-based
# !       module).
#
        if HResWt[SRFC] > 1.0e-4:
# !         The lignin concentration of the various plant parts varies
# !         widely: usually stem and leaf are similar, and so are seed and
# !         shell. So separate those here. Correct VEGRES for non-harvested
# !         seeds and shells, and add those separately with their own lignin
# !         concentration.
#
# !         chp 2024-07-01 HResLig should be in units of kg/ha! Don't divide by HResWt(SRFC)!
            HResLig[SRFC] = (LFRES * PLIGLF + STMRES * PLIGST +
                             SDRES * PLIGSD + SHLRES * PLIGSH)
#         ENDIF
#
# !-------------------------------------------------------------------------
# !       Distribute root+nodule residues by layer, according to the root
# !       length distribution (NB: this may give very wrong results if the
# !       roots vary in thickness by layer, but root weight is not available
# !       by layer).
        TRLV = 0.0
        for L in range(NLAYR):
            TRLV += RLV[L] * DLAYR[L]

#
# !       Root + nodule residues.
# !       -----------------------
# !       Total root residues (whole profile) equal root weight plus nodule
# !       weight.
        TRTRES = RTWT + DWNOD   #kg/ha
#
# !       N in root residues is N in roots plus N in nodules.
        TRTRESE[N] = WTNRT + WTNNOD
        TRTRESE[P] = PCONC_ROOT * RTWT
#
# !       Senescence has been added daily (subroutine SENESADD), so no need
# !       to add it here as WTRO and WTNOO, as in the CERES-based module.
        if TRLV > 1.0e-6 and TRTRES > 1.0e-6:
            for L in range(1, NLAYR+1):
                HResWt[L] = 10.0 * TRTRES * RLV[L] * DLAYR[L] / TRLV
                HResLig[L] = (RTWT * PLIGRT + DWNOD * PLIGNO) \
                             * RLV[L] * DLAYR[L] / TRLV  # !/ TRTRES
                for IEL in range(1, N_ELEMS+1):
                    HResE[L, IEL] = 10.0 * TRTRESE[IEL] * RLV[L] * DLAYR[L] / TRLV
            # !End of soil layer loop.
        else:
            for L in range(1, NLAYR+1):
                HResWt[L] = 0.0
                HResLig[L] = 0.0
                HResE[L, N] = 0.0
                HResE[L, P] = 0.0

#
# C-------------------------------------------------------------------------
#         !Add in last day of senesced plant material (not added in soil
#         !  module because it is computed after soil integration.
        HResWt[SRFC]  += SENESCE.ResWt[SRFC]
        HResLig[SRFC] += SENESCE.ResLig[SRFC]
        HResE[SRFC, N] += SENESCE.ResE[SRFC, N]
        HResE[SRFC, P] += SENESCE.ResE[SRFC, P]

        for L in range(1, NLAYR+1):
            HResWt[L]  += SENESCE.ResWt[L]
            HResLig[L] += SENESCE.ResLig[L]
            HResE[L, N] += SENESCE.ResE[L, N]
            HResE[L, P] += SENESCE.ResE[L, P]

#
#       ENDIF   !Crop .NE. 'FA'
# C-----------------------------------------------------------------------
# !     Transfer results to constructed variable
    HARVRES.ResWt  = HResWt
    HARVRES.ResLig = HResLig
    HARVRES.ResE   = HResE
#
    return HARVRES
#       END SUBROUTINE HRes_CGRO
#
#-----------------------------------------------------------------------
# ! Variable Definitions (11 March 2004)
#-----------------------------------------------------------------------
# ! BWAH       Weight of by-product not harvested (top weight minus seed
# !              weight) (g/m2)
# ! CONTROL    Composite variable containing variables related to control
# !              and/or timing of simulation.    See Appendix A.
# ! CROP       Crop identification code
# ! DWNOD      Current nodule mass (g[nodule] / m2)
# ! HARVRES    Composite variable containing harvest residue amounts for
# !              total dry matter, lignin, and N amounts.  Structure of
# !              variable is defined in ModuleDefs.for.
# ! HRESE(L,E) Amount of element E in plant residue left in field after
# !              harvest in soil/surface layer L (E=1 for nitrogen; E=2 for
# !              phosphorus; . . .) (kg[E]/ha (E=N, P, S,...))
# ! HRESLIG(L) Amount of lignin in plant residue left in field after harvest
# !              in soil/surface layer L (kg[lignin]/ha)
# ! HRESWT(L)  Amount of plant residue left in field after harvest in
# !              soil/surface layer L (kg[dry matter]/ha)
# ! NLAYR      Actual number of soil layers
# ! PLIGLF     Proportion of leaf tissue that is lignin (fraction)
# ! PLIGNO     Proportion of nodule tissue that is lignin (fraction)
# ! PLIGRT     Proportion of root tissue that is lignin (fraction)
# ! PLIGSD     Proportion of seed tissue that is lignin (fraction)
# ! PLIGSH     Proportion of shell tissue that is lignin (fraction)
# ! PLIGST     Proportion of stem tissue that is lignin (fraction)
# ! PRCEL      Cellulose fraction of the residue (fraction)
# ! PRCHO      Carbohydrate fraction of the residue (fraction)
# ! RLV(L)     Root length density for soil layer L (cm[root] / cm3[soil])
# ! RTWT       Dry mass of root tissue, including C and N
# !             (g[root] / m2[ground])
# ! SDWT       Dry mass of seed tissue, including C and N
# !             (g[seed] / m2[ground])
# ! SDWTAH     Actual seed weight harvested (g[seed] / m2[ground])
# ! SENESCE    Composite variable containing data about daily senesced plant
# !              matter. Structure of variable is defined in ModuleDefs.for
# ! SHELWT     Total mass of all shells (g / m2)
# ! TRLV       Total root length per square cm soil today
# !             (cm[root] / cm2[ground)
# ! TRTRES     Weight of root residues from previous crop
# !             (kg [dry matter] / ha)
# ! TRTRESE(E) Amount of element E in root/nodule residue left in field after
# !              harvest for soil profile (E=1 for nitrogen; E=2 for
# !              phosphorus; . . .) (g[E]/m2)
# ! WTNLF      Mass of N in leaves (g[leaf N] / m2[ground])
# ! WTNNOD     Mass of N in nodules (g[N] / m2[ground])
# ! WTNRT      Mass of N in roots (g[root N] / m2[ground])
# ! WTNSD      Mass of N in seeds (g[N] / m2[ground])
# ! WTNSH      Mass of N in shells (g[N] / m2[ground])
# ! WTNST      Mass of N in stems (g[stem N] / m2[ground])
# C=======================================================================
