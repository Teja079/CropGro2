#  Calculates seed composition for oil and protein
def SDCOMP(
    CARMIN, LIPOPT, LIPTB, PLIGSD, PMINSD, POASD,      # Input
    RCH2O, RLIG, RLIP, RMIN, RNO3C, ROA, SDLIP,        # Input
    SDPRO, SLOSUM, TAVG,                               # Input
    AGRSD1, AGRSD2, FNINSD, POTCAR, POTLIP) :          # Output

    LIPTEM, LIPOPT, LIPTB, GENSUM = [0.0] * 4
    POTPRO, CARMIN, SLOSUM, POTLIP, POTCAR = [0.0] * 5
    TAVG, SUMTEM, PSUMSD, TOTAL = [0.0] * 4
    PMINSD, POASD, PLIGSD, SDLIP, SDPRO, FNINSD = [0.0] * 6
    RNO3C, RLIG, RMIN, RCH2O, RLIP, ROA = [0.0] * 6

#***********************************************************************
#     The quadratic plateau for predicting percentage lipids
#-----------------------------------------------------------------------
    if (TAVG > LIPOPT):
        LIPTEM= 1.0
    elif ((TAVG < LIPOPT) and (TAVG > LIPTB)):
        LIPTEM = 1.0 - ((LIPOPT - TAVG) / (LIPOPT - LIPTB))**2
    else:
        LIPTEM= 0.0

    POTLIP = SDLIP * LIPTEM

#-----------------------------------------------------------------------
#     Determination of protein percentage
#-----------------------------------------------------------------------
    GENSUM = (SDPRO*100.0) + \
        (SDLIP*100.0) * (1.0 - (((LIPOPT - 25.) / (LIPOPT - LIPTB))**2))
    SUMTEM = 1.0 + SLOSUM * (TAVG - 25.0)
    PSUMSD = GENSUM * SUMTEM / 100.0
    POTPRO = PSUMSD - POTLIP

#-----------------------------------------------------------------------
#     Determination of carbohydrate percentage
#-----------------------------------------------------------------------
    POTCAR = 1.0 - POTLIP - POTPRO
    if (POTCAR < CARMIN):
        POTCAR =  CARMIN

    TOTAL  = POTLIP + POTPRO + POTCAR
    if (ABS(TOTAL) - 1.0 > 0.0005):
        POTPRO = POTPRO / TOTAL
        POTLIP = POTLIP / TOTAL
        POTCAR = POTCAR / TOTAL

        TOTAL  = POTLIP + POTPRO + POTCAR

    POTCAR = POTCAR - PMINSD - POASD - PLIGSD
    AGRSD1 = PMINSD*RMIN + PLIGSD*RLIG + POASD*ROA + \
        POTLIP*RLIP + POTCAR*RCH2O
    AGRSD2 = AGRSD1 + RNO3C*POTPRO
    FNINSD = POTPRO / 6.25

#***********************************************************************
    return

#-----------------------------------------------------------------------
# AGRSD1 CH2O requirement for seed growth, excluding cost for protein
#          content (g[CH2O] / g[seed])
# AGRSD2 CH2O requirement for seed growth, including cost for protein
#          content (g[CH2O] / g[seed])
# CARMIN Minimum carbohydrate fraction
# FNINSD Maximum fraction of N for growing seed tissue based on temperature
#          (g[N] / g[seed])
# GENSUM Protein plus lipid composition (%)
# LIPOPT Temperature above which lipid composition is at a maximum (°C)
# LIPTB  Temperature below which lipid composition is zero (°C)
# LIPTEM Factor to reduce lipid composition based on temperature (0-1).
#          Normalized quadratic plateau function.
# PLIGSD Proportion of seed tissue that is lignin (fraction)
# PMINSD Proportion of seed tissue that is mineral (fraction)
# POASD  Proportion of seed tissue that is organic acid (fraction)
# POTCAR Potential carbohydrate composition of seed based on temperature
#          (fraction)
# POTLIP Potential lipid composition of seed based on temperature
#          (fraction)
# POTPRO Potential protein composition of seed based on temperature
#          (fraction)
# PSUMSD Potential protein plus lipid composition (fraction)
# RCH2O  Respiration required for synthesizing CH2O structure
#          (g[CH2O] / g[tissue])
# RLIG   Respiration required for synthesizing lignin structure
#          (g[CH2O] / g[lignin])
# RLIP   Respiration required for synthesizing lipid structure
#          (g[CH2O] / g[lipid])
# RMIN   Respiration required for synthesizing mineral structure
#          (g[CH2O] / g[mineral])
# RNO3C  Respiration required for reducing NO3 to protein
#          (g[CH2O] / g[protein])
# ROA    Respiration required for synthesizing organic acids
#          (g[CH2O] / g[product])
# SDLIP  Maximum lipid composition in seed (fraction)
# SDPRO  Seed protein fraction at 25oC (g[protein] / g[seed])
# SLOSUM Slope of temperature vs. SUMTEM line (1/oC)
# SUMTEM Factor which affects protein composition based on average
#          temperature.
# TAVG   Average daily temperature (°C)
# TOTAL  Check for total composition equal to one.