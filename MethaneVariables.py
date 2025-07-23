# MethaneVariables containing array variable declarations
import numpy as np

z = np.zeros(51,dtype=float)
epsilon = np.zeros(51,dtype=float)
lamda = np.zeros(51,dtype=float)
theta = np.zeros(51,dtype=float)
L = np.zeros(51,dtype=float)
rho = np.zeros(51,dtype=float)

DD = np.zeros((4,51),dtype=float)
LL = np.zeros((4,51),dtype=float)
VV = np.zeros((4,51),dtype=float)
yy = np.zeros((4,51),dtype=float)
inityy = np.zeros((4,51),dtype=float)

# from MODULE MethaneConstants
om : int = 1
o2 : int = 2
bb : int = 3
ch4 : int = 4

TSubstrate : float = 0.0