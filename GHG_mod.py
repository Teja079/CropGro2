#     Contains data definitions for N2O generation routines in SOILNI
from ModuleDefs import NL
import numpy as np

# Data construct for control variables
@dataclass(frozen=True)
class N2O_type:
    WFPS= np.zeros(NL, dtype=float)