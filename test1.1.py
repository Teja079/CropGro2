# !=======================================================================
#       SUBROUTINE SoilLayerClass(ISWITCH,
#      &    MULTI, DS, NLAYR, SLDESC, TAXON,                !Input
#      &    CaCO3, PH, CEC, CLAY, SOILLAYERTYPE)            !Output
import numpy as np
import WARNING from WARNING
def SoilLayerClass(ISWITCH,MULTI, DS, NLAYR, SLDESC, TAXON):
#       USE ModuleDefs
#       IMPLICIT NONE
#       EXTERNAL UPCASE, LENSTRING, INFO
#
#       TYPE (SwitchType) , INTENT(IN) :: ISWITCH  !Simulation options
#       INTEGER NLAYR, L, LENGTH, LEN1, LEN2, MULTI, LenString, I
#       REAL, DIMENSION(NL) :: DS, CaCO3, PH, CEC, CLAY
#       CHARACTER*1  UPCASE
#       CHARACTER*17 SOILLAYERTYPE(NL)
#       CHARACTER*50 SLDESC, TAXON
#       CHARACTER*7, PARAMETER :: ERRKEY = 'SOILLayerClass'
#       CHARACTER*78 MSG(NL+4)
#       LOGICAL VOLCANIC
    ERRKEY = 'SOILLayerClass'
    MSG = [''] * (NLAYR + 4)
    SOILLAYERTYPE = [' ' * 17 for _ in range(NLAYR)]
    VOLCANIC = False
#
# !     Define the type of soil layer.
# !     First check soil name for occurrence of 'ANDOSOL', 'ANDISOL',
# !     'VOLCAN' or 'ANDEPT'.  These indicate volcanic soils.
    VOLCANIC = False
    LENGTH = max(LENSTRING(SLDESC), LENSTRING(TAXON))
    SLDESC = ''.join([UPCASE(c) for c in SLDESC.ljust(LENGTH)])
    TAXON = ''.join([UPCASE(c) for c in TAXON.ljust(LENGTH)])
# !     Calcareous soil is with >15% (w:w) of CaCO3.
# !     Highly weathered is CEC:CLAY < 16 cmol/kg (meq/100g)
# !     Slightly weathered is CEC:CLAY >16 mmol/kg
# ! CHP the following statement should be in here or SOILDYN??
    if any(word in (SLDESC + TAXON) for word in ['ANDOSOL', 'ANDISOL', 'VOLCAN', 'ANDEPT']):
        VOLCANIC = True

    for L in range(1,NLAYR):
        # !       First check for calcareous soil
        if CaCO3[L] > 0.15:
            SOILLAYERTYPE[L] = 'CALCAREOUS       '
            continue
        elif CaCO3[L] < 0.0:
            if PH[L] > 7 and ISWITCH.ISWPHO != 'N' and MULTI < 2:
                MSG[1] = f"CaCO3 value missing for soil layer. {L + 1:2d}"
                MSG[2] = "Soil classification may be in error."
                MSG[2] = f"pH = {PH[L]:8.1f} Possible calcareous soil type."
                WARNING(2, ERRKEY, MSG)
        # !       Next check for andisol
        if VOLCANIC:
            SOILLAYERTYPE[L] = 'ANDISOL          '
            continue

        if CEC[L] > 0.0 and CLAY[L] > 0.0:
            ratio = CEC[L] / (CLAY[L] / 100.0)
            if ratio > 16.0:
                SOILLAYERTYPE[L] = 'SLIGHTLYWEATHERED'
            else:
                SOILLAYERTYPE[L] = 'HIGHLYWEATHERED  '
        else:
            SOILLAYERTYPE[L] = 'UNKNOWN          '
            if ISWITCH.ISWPHO != 'N' and MULTI < 2:
                MSG[1] = f"Insufficient data available to classify soil layer {L + 1:2d}"
                MSG[2] = "Default characteristics will be used."
                MSG[3] = f"  CaCO3 = {CaCO3[L]:8.2f} %"
                MSG[4] = f"  CEC   = {CEC[L]:8.2f} cmol/kg"
                MSG[5] = f"  pH    = {PH[L]:8.1f}"
                MSG[6] = f"  Clay  = {CLAY[L]:8.1f} %"
                MSG[7] = "This may affect results of soil phosphorus model."
                INFO(7, ERRKEY, MSG)
    if ISWITCH.ISWPHO == 'Y':
        MSG[1] = "Soil layer classifications (used for soil P model)"
        MSG[2] = "Layer Depth Soil_Layer_Type   Backup_Data"
        for L in range(1,NLAYR):
            depth = round(DS[L])
            layer_type = SOILLAYERTYPE[L].strip()
            if layer_type == 'CALCAREOUS':
                MSG[L + 2] = f"{L + 1:3d}{depth:7d}  {SOILLAYERTYPE[L]} CaCO3:{CaCO3[L]:6.2f}%"
            elif layer_type == 'ANDISOL':
                LEN1 = min(39, LENSTRING(SLDESC))
                LEN2 = min(47 - LEN1, LENSTRING(TAXON))
                MSG[L + 2] = f"{L + 1:3d}{depth:7d}  {SOILLAYERTYPE[L]} {SLDESC[:LEN1]}, {TAXON[:LEN2]}"
            else:
                MSG[L + 2] = (
                    f"{L + 1:3d}{depth:7d}  {SOILLAYERTYPE[L]}"
                    f" CaCO3:{CaCO3[L]:6.2f}%;"
                    f" CEC:{CEC[L]:6.1f} cmol/kg;"
                    f" CLAY:{CLAY[L]:6.1f}%"
                )
        INFO(NLAYR + 2, ERRKEY, MSG)

    return CaCO3, PH, CEC, CLAY, SOILLAYERTYPE
#       END SUBROUTINE SoilLayerClass

# Test driver for SoilLayerClass
NLAYR = 9
DS = [5.0, 15.0, 30.0, 45.0, 60.0, 90.0, 120.0, 150.0, 180.0]
SLDESC = 'Candler'
TAXON  = 'Hyperthermic uncoated'
MULTI = 1
ISWITCH = SwitchType('Y')
SoilLayerClass(ISWITCH,MULTI, DS, NLAYR, SLDESC, TAXON)