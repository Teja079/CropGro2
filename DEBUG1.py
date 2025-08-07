def SoilLayerClass(ISWITCH,MULTI, DS, NLAYR, SLDESC, TAXON,PH,CEC, CLAY,SOILLAYERTYPE):
    import numpy as np
    from WARNING import WARNING
    from ModuleDefs import NL
    from UTILS import NINT
#       USE ModuleDefs
#       IMPLICIT NONE
#       EXTERNAL UPCASE, LENSTRING, INFO
#
#       TYPE (SwitchType) , INTENT(IN) :: ISWITCH  !Simulation options
#       INTEGER NLAYR, L, LENGTH, LEN1, LEN2, MULTI, LenString, I
    CaCO3 = np.full(NL+1,-99.0, dtype=float)
    #DS = np.zeros(NL+1, dtype=float)
    #CEC = np.zeros(NL+1, dtype=float)
    #CLAY = np.zeros(NL+1, dtype=float)
    #SOILLAYERTYPE = np.empty(NL+1, dtype='U17')
    # SOILLAYERTYPE = [' ' * 17 for _ in range(NLAYR)]
#       CHARACTER*50 SLDESC, TAXON
#       CHARACTER*7, PARAMETER :: ERRKEY = 'SOILLayerClass'
#       CHARACTER*78 MSG(NL+4)
#       LOGICAL VOLCANIC
    ERRKEY = 'SOILLayerClass'
    MSG = [''] * (NLAYR + 4)
#      Define the type of soil layer.
#      First check soil name for occurrence of 'ANDOSOL', 'ANDISOL',
#      'VOLCAN' or 'ANDEPT'.  These indicate volcanic soils.
    VOLCANIC = False
    LENGTH = max(len(SLDESC.strip()), len(TAXON.strip()))

    SLDESC = SLDESC.upper()
    TAXON = TAXON.upper()

#      Calcareous soil is with >15% (w:w) of CaCO3.
#      Highly weathered is CEC:CLAY < 16 cmol/kg (meq/100g)
#      Slightly weathered is CEC:CLAY >16 mmol/kg
#  CHP the following statement should be in here or SOILDYN??
#     if 'SLDESC' + 'TAXON'.find('ANDOSOL') > 0 or
    if any(word in (SLDESC + TAXON) for word in ['ANDOSOL', 'ANDISOL', 'VOLCAN', 'ANDEPT']):
        VOLCANIC = True

    for L in range(1,NLAYR+1):
        # !       First check for calcareous soil
        if CaCO3[L] > 0.15:
            SOILLAYERTYPE[L] = 'CALCAREOUS       '
            continue
        elif CaCO3[L] < 0.0:
            if PH[L] > 7 and ISWITCH.ISWPHO != 'N' and MULTI < 2:
                MSG[1] = f"CaCO3 value missing for soil layer. {L:2d}"
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
                MSG[1] = f"Insufficient data available to classify soil layer {L:2d}"
                MSG[2] = "Default characteristics will be used."
                MSG[3] = f"  CaCO3 = {CaCO3[L]:8.2f} %"
                MSG[4] = f"  CEC   = {CEC[L]:8.2f} cmol/kg"
                MSG[5] = f"  pH    = {PH[L]:8.1f}"
                MSG[6] = f"  Clay  = {CLAY[L]:8.1f} %"
                MSG[7] = "This may affect results of soil phosphorus model."
                # INFO(7, ERRKEY, MSG)
    if ISWITCH.ISWPHO == 'Y':
        MSG[1] = "Soil layer classifications (used for soil P model)"
        MSG[2] = "Layer Depth Soil_Layer_Type   Backup_Data"
        for L in range(1,NLAYR):
            depth = NINT(DS[L])
            layer_type = SOILLAYERTYPE[L].strip()
            if layer_type == 'CALCAREOUS':
                MSG[L + 2] = f"{L+2:3d}{depth:7d}  {SOILLAYERTYPE[L]} CaCO3:{CaCO3[L]:6.2f}%"
            elif layer_type == 'ANDISOL':
                LEN1 = min(39, len(SLDESC))
                LEN2 = min(47 - LEN1, len(TAXON))
                MSG[L + 2] = f"{L + 2:3d}{depth:7d}  {SOILLAYERTYPE[L]} {SLDESC[:LEN1]}, {TAXON[:LEN2]}"
            else:
                MSG[L + 2] = (
                    f"{L + 2:3d}{depth:7d}  {SOILLAYERTYPE[L]}"
                    f" CaCO3:{CaCO3[L]:6.2f}%;"
                    f" CEC:{CEC[L]:6.1f} cmol/kg;"
                    f" CLAY:{CLAY[L]:6.1f}%"
                )
        # INFO(NLAYR + 2, ERRKEY, MSG)

    return CaCO3, PH, CEC, CLAY, SOILLAYERTYPE


from ModuleDefs import SwitchType
ISWITCH = SwitchType(
    ICO2='M',
    IDETL='Y',
    IFERI='R',
    IHARI='R',
    IIRRI='R',
    IPLTI='R',
    IRESI='R',
    ISIMI='S',
    ISWNIT='N',
    ISWPHO='N',
    ISWSYM='N',
    ISWTIL='N',
    ISWWAT='N',
    MEEVP='R',
    MEHYD='R',
    MEINF='S',
    MEPHO='L',
    MESEV='S',
    MESOL='2',
    METMP='D',
    MEWTH='M',
    NSWI=0
)
MULTI = 0
DS = [-99. ,  5. , 15. , 30. , 45. , 60. , 90., 120., 150. ,180.  , 0. ,-99. ,-99., -99., -99., -99., -99., -99., -99., -99. ,-99.]
NLAYR = 9
SLDESC = 'CANDLER                 '
TAXON= 'HYPERTHERMIC UNCOATED     '
PH = [-99.  ,  5.5  , 5.5 ,  5.3 ,  5.1 ,  5.1 ,  5.2 ,  5.3 ,  5.2 ,  5.2 ,-99. , -99., -99. , -99. , -99. , -99. , -99. , -99. , -99. , -99. , -99. ]
CEC=[-99. ,  15.5 , 15.5 , 15.5 , 15.5 , 15.5 , 15.5 , 15.5 , 15.5 , 15.5, -99. , -99., -99. , -99. , -99.,  -99. , -99. , -99. , -99. , -99. , -99. ]
CLAY =[-99. ,   1.3 ,  1.3  , 1.3  , 1.3  , 1.3  , 3.  ,  3.  ,  4.  ,  4. , -99.  ,-99., -99. , -99. , -99.,  -99. , -99. , -99.,  -99. , -99.,  -99. ]
SOILLAYERTYPE = ['                 ', '                 ', '                 ', '                 ' ,'                 ' ,'                 ', '                 ' ,'                 ' ,'                 ', '                 ', '                 ' ,'                 ', '                 ' ,'                 ' ,'                 ', '                 ' ,'                 ', '                 ', '                 ' ,'                 ']
print(SoilLayerClass(ISWITCH,MULTI, DS, NLAYR, SLDESC, TAXON,PH,CEC, CLAY,SOILLAYERTYPE))