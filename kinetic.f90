
module mkinetic
use constants
implicit none

contains

function kinetic(n1,i1,j1,a1,k1,n2,i2,j2,a2,k2) result(ans)
 integer, intent(in) :: n1,i1,j1,n2,i2,j2
real(quad), intent(in) :: a1,a2,k1,k2
real(quad) :: ans
! - - - - - - - - - - - - - - - - - - 
integer :: key
real(quad) :: ap, spp 
real(quad) :: p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30
real(quad) :: e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12
! - - - - - - - - - - - - - - - - - - 
 
key = n1 + 2*i1 + 4*j1 + 8*n2 + 16*i2 + 32*j2

select case (key)
  case (0) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    ans = a2*(-a2 + ap)*p1*spp
  case (1) 
    ans = 0
  case (2) 
    ans = 0
  case (3) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = k1**2
    p4 = ap**2
    p13 = ap**(-3)
    e2 = exp(-(p1*p6)/4.)
    ans = (a2*e2*k1*p13*(2*p4 + a2*(-6*ap + p6))*spp)/4.
  case (4) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = k1**2
    p2 = ap**(-2)
    p4 = ap**2
    e2 = exp(-(p1*p6)/4.)
    ans = (a2*e2*p2*(2*p4 + a2*(-2*ap + p6))*spp)/2.
  case (5) 
    ans = 0
  case (6) 
    ans = 0
  case (7) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = k1**2
    p4 = ap**2
    p13 = ap**(-3)
    e12 = exp(-(p1*p6))
    ans = (a2*e12*k1*p13*(-3*a2*ap + p4 + 2*a2*p6)*spp)/2.
  case (8) 
    ans = 0
  case (9) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p2 = ap**(-2)
    ans = (3*a2*(-a2 + ap)*p2*spp)/2.
  case (10) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = k1**2
    p4 = ap**2
    p13 = ap**(-3)
    e2 = exp(-(p1*p6)/4.)
    ans = (a2*e2*k1*p13*(6*p4 + a2*(-6*ap + p6))*spp)/4.
  case (11) 
    ans = 0
  case (12) 
    ans = 0
  case (13) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = k1**2
    p16 = ap**(-4)
    p4 = ap**2
    p18 = k1**4
    e2 = exp(-(p1*p6)/4.)
    ans = -(a2*e2*p16*(6*p4*(-2*ap + p6) + a2*(p18 + 12*p4 - 12*ap*p6))*spp)/8.
  case (14) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = k1**2
    p4 = ap**2
    p13 = ap**(-3)
    e12 = exp(-(p1*p6))
    ans = (a2*e12*k1*p13*(-3*a2*ap + 3*p4 + 2*a2*p6)*spp)/2.
  case (15) 
    ans = 0
  case (16) 
    ans = 0
  case (17) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k2**2
    p13 = ap**(-3)
    p5 = a2**2
    p4 = ap**2
    p15 = k2**3
    e1 = exp(-(p1*p3)/4.)
    ans = (e1*p13*(2*a2*ap*k2*(3*ap - p3) - 6*ap*k2*p5 + p15*(p4 + p5))*spp)/4.
  case (18) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p2 = ap**(-2)
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p8 = (k1 + k2)**2
    e3 = exp(-(p1*p7)/4.)
    e4 = exp(-(p1*p8)/4.)
    ans = (p2*(e3*(2*a2*ap*(ap + (k1 - k2)*k2) + p3*p4 + p5*(-2*ap + p7)) - e4*(2*a2*ap*(ap - k2*(k1 + k2)) + p3*p4 + p5*(-2*ap + p8)))*spp)/4.
  case (19) 
    ans = 0
  case (20) 
    ans = 0
  case (21) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p8 = (k1 + k2)**2
    p13 = ap**(-3)
    e3 = exp(-(p1*p7)/4.)
    e4 = exp(-(p1*p8)/4.)
    ans = (p13*(e3*(-2*a2*ap*(ap*(k1 - 3*k2) + k2*p7) - (k1 - k2)*(p3*p4 + p5*(-6*ap + p7))) + e4*(2*a2*ap*(ap*(k1 + 3*k2) - k2*p8) + (k1 + k2)*(p3*p4 + p5*(-6*ap + p8))))*spp)/8.
  case (22) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p2 = ap**(-2)
    p1 = 1/ap
    p11 = (-2*k1 + k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p12 = (2*k1 + k2)**2
    e7 = exp(-(p1*p11)/4.)
    e8 = exp(-(p1*p12)/4.)
    ans = (p2*(e7*(2*a2*ap*(ap + (2*k1 - k2)*k2) + p3*p4 + (-2*ap + p11)*p5) - e8*(2*a2*ap*(ap - k2*(2*k1 + k2)) + p3*p4 + (-2*ap + p12)*p5))*spp)/8.
  case (23) 
    ans = 0
  case (24) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p13 = ap**(-3)
    e1 = exp(-(p1*p3)/4.)
    ans = (e1*k2*p13*(2*a2*ap*(5*ap - p3) + (-4*ap + p3)*p4 + (-6*ap + p3)*p5)*spp)/4.
  case (25) 
    ans = 0
  case (26) 
    ans = 0
  case (27) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p16 = ap**(-4)
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p4 = ap**2
    p5 = a2**2
    p19 = (k1 - k2)**4
    p3 = k2**2
    p6 = k1**2
    p20 = (k1 - k2)**3
    p8 = (k1 + k2)**2
    p21 = (k1 + k2)**4
    p22 = (k1 + k2)**3
    e3 = exp(-(p1*p7)/4.)
    e4 = exp(-(p1*p8)/4.)
    ans = (p16*(-(e3*(-2*a2*ap*(-(k2*p20) + 6*p4 - 3*ap*(-4*k1*k2 + 3*p3 + p6)) + p5*(p19 + 12*p4 - 12*ap*p7) + k2*p4*(4*ap*k1 - 6*ap*k2 + k2*p7))) + e4*(-2*a2*ap*(k2*p22 + 6*p4 - 3*ap*(4*k1*k2 + 3*p3 + p6)) + p5*(p21 + 12*p4 - 12*ap*p8) + k2*p4*(-4*ap*k1 - 6*ap*k2 + k2*p8)))*spp)/16.
  case (28) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p4 = ap**2
    p5 = a2**2
    p8 = (k1 + k2)**2
    p13 = ap**(-3)
    e3 = exp(-(p1*p7)/4.)
    e4 = exp(-(p1*p8)/4.)
    ans = (p13*(e3*(k2*(-4*ap + k2*(-k1 + k2))*p4 + (k1 - k2)*p5*(6*ap - p7) - 2*a2*ap*(3*ap*k1 - 5*ap*k2 + k2*p7)) - e4*(k2*(4*ap - k2*(k1 + k2))*p4 + (k1 + k2)*p5*(6*ap - p8) - 2*a2*ap*(3*ap*k1 + 5*ap*k2 - k2*p8)))*spp)/8.
  case (29) 
    ans = 0
  case (30) 
    ans = 0
  case (31) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p16 = ap**(-4)
    p1 = 1/ap
    p11 = (-2*k1 + k2)**2
    p4 = ap**2
    p5 = a2**2
    p27 = (-2*k1 + k2)**4
    p3 = k2**2
    p6 = k1**2
    p28 = (2*k1 - k2)**3
    p12 = (2*k1 + k2)**2
    p29 = (2*k1 + k2)**4
    p30 = (2*k1 + k2)**3
    e7 = exp(-(p1*p11)/4.)
    e8 = exp(-(p1*p12)/4.)
    ans = (p16*(-(e7*(k2*(8*ap*k1 - 6*ap*k2 + k2*p11)*p4 + (-12*ap*p11 + p27 + 12*p4)*p5 - 2*a2*ap*(-(k2*p28) + 6*p4 + 3*ap*(8*k1*k2 - 3*p3 - 4*p6)))) + e8*(k2*(-8*ap*k1 - 6*ap*k2 + k2*p12)*p4 + (-12*ap*p12 + p29 + 12*p4)*p5 - 2*a2*ap*(k2*p30 + 6*p4 - 3*ap*(8*k1*k2 + 3*p3 + 4*p6))))*spp)/32.
  case (32) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k2**2
    p2 = ap**(-2)
    p5 = a2**2
    p4 = ap**2
    e1 = exp(-(p1*p3)/4.)
    ans = (e1*p2*(2*a2*ap*(ap - p3) - 2*ap*p5 + p3*(p4 + p5))*spp)/2.
  case (33) 
    ans = 0
  case (34) 
    ans = 0
  case (35) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p8 = (k1 + k2)**2
    p13 = ap**(-3)
    e3 = exp(-(p1*p7)/4.)
    e4 = exp(-(p1*p8)/4.)
    ans = (p13*(e3*(2*a2*ap*(ap*(k1 - 3*k2) + k2*p7) + (k1 - k2)*(p3*p4 + p5*(-6*ap + p7))) + e4*(2*a2*ap*(ap*(k1 + 3*k2) - k2*p8) + (k1 + k2)*(p3*p4 + p5*(-6*ap + p8))))*spp)/8.
  case (36) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p2 = ap**(-2)
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p8 = (k1 + k2)**2
    e3 = exp(-(p1*p7)/4.)
    e4 = exp(-(p1*p8)/4.)
    ans = (p2*(e3*(2*a2*ap*(ap + (k1 - k2)*k2) + p3*p4 + p5*(-2*ap + p7)) + e4*(2*a2*ap*(ap - k2*(k1 + k2)) + p3*p4 + p5*(-2*ap + p8)))*spp)/4.
  case (37) 
    ans = 0
  case (38) 
    ans = 0
  case (39) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p11 = (-2*k1 + k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p12 = (2*k1 + k2)**2
    p13 = ap**(-3)
    e7 = exp(-(p1*p11)/4.)
    e8 = exp(-(p1*p12)/4.)
    ans = (p13*(e7*(2*a2*ap*(2*ap*k1 - 3*ap*k2 + k2*p11) + (2*k1 - k2)*(p3*p4 + (-6*ap + p11)*p5)) + e8*(2*a2*ap*(2*ap*k1 + 3*ap*k2 - k2*p12) + (2*k1 + k2)*(p3*p4 + (-6*ap + p12)*p5)))*spp)/16.
  case (40) 
    ans = 0
  case (41) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k2**2
    p16 = ap**(-4)
    p4 = ap**2
    p17 = k2**4
    p5 = a2**2
    e1 = exp(-(p1*p3)/4.)
    ans = -(e1*p16*(p3*(-6*ap + p3)*p4 - 2*a2*ap*(p17 - 9*ap*p3 + 6*p4) + (p17 - 12*ap*p3 + 12*p4)*p5)*spp)/8.
  case (42) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p4 = ap**2
    p5 = a2**2
    p8 = (k1 + k2)**2
    p13 = ap**(-3)
    e3 = exp(-(p1*p7)/4.)
    e4 = exp(-(p1*p8)/4.)
    ans = (p13*(e3*(k2*(4*ap + (k1 - k2)*k2)*p4 + (k1 - k2)*p5*(-6*ap + p7) + 2*a2*ap*(3*ap*k1 - 5*ap*k2 + k2*p7)) - e4*(k2*(4*ap - k2*(k1 + k2))*p4 + (k1 + k2)*p5*(6*ap - p8) - 2*a2*ap*(3*ap*k1 + 5*ap*k2 - k2*p8)))*spp)/8.
  case (43) 
    ans = 0
  case (44) 
    ans = 0
  case (45) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p16 = ap**(-4)
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p4 = ap**2
    p5 = a2**2
    p19 = (k1 - k2)**4
    p3 = k2**2
    p6 = k1**2
    p20 = (k1 - k2)**3
    p8 = (k1 + k2)**2
    p21 = (k1 + k2)**4
    p22 = (k1 + k2)**3
    e3 = exp(-(p1*p7)/4.)
    e4 = exp(-(p1*p8)/4.)
    ans = (p16*(-(e3*(-2*a2*ap*(-(k2*p20) + 6*p4 - 3*ap*(-4*k1*k2 + 3*p3 + p6)) + p5*(p19 + 12*p4 - 12*ap*p7) + k2*p4*(4*ap*k1 - 6*ap*k2 + k2*p7))) - e4*(-2*a2*ap*(k2*p22 + 6*p4 - 3*ap*(4*k1*k2 + 3*p3 + p6)) + p5*(p21 + 12*p4 - 12*ap*p8) + k2*p4*(-4*ap*k1 - 6*ap*k2 + k2*p8)))*spp)/16.
  case (46) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p11 = (-2*k1 + k2)**2
    p4 = ap**2
    p5 = a2**2
    p12 = (2*k1 + k2)**2
    p13 = ap**(-3)
    e7 = exp(-(p1*p11)/4.)
    e8 = exp(-(p1*p12)/4.)
    ans = (p13*(e7*(2*a2*ap*(6*ap*k1 - 5*ap*k2 + k2*p11) + k2*(4*ap + (2*k1 - k2)*k2)*p4 + (2*k1 - k2)*(-6*ap + p11)*p5) - e8*(-2*a2*ap*(6*ap*k1 + 5*ap*k2 - k2*p12) + k2*(4*ap - k2*(2*k1 + k2))*p4 + (2*k1 + k2)*(6*ap - p12)*p5))*spp)/16.
  case (47) 
    ans = 0
  case (48) 
    ans = 0
  case (49) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k2**2
    p13 = ap**(-3)
    p5 = a2**2
    p4 = ap**2
    p15 = k2**3
    e11 = exp(-(p1*p3))
    ans = (e11*p13*(a2*ap*k2*(3*ap - 4*p3) - 3*ap*k2*p5 + 2*p15*(p4 + p5))*spp)/2.
  case (50) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p2 = ap**(-2)
    p1 = 1/ap
    p9 = (k1 - 2*k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p10 = (k1 + 2*k2)**2
    e5 = exp(-(p1*p9)/4.)
    e6 = exp(-(p1*p10)/4.)
    ans = (p2*(-(e6*(2*a2*ap*(ap - 2*k2*(k1 + 2*k2)) + 4*p3*p4 + (-2*ap + p10)*p5)) + e5*(2*a2*ap*(ap + 2*(k1 - 2*k2)*k2) + 4*p3*p4 + p5*(-2*ap + p9)))*spp)/8.
  case (51) 
    ans = 0
  case (52) 
    ans = 0
  case (53) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p9 = (k1 - 2*k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p10 = (k1 + 2*k2)**2
    p13 = ap**(-3)
    e5 = exp(-(p1*p9)/4.)
    e6 = exp(-(p1*p10)/4.)
    ans = (p13*(e6*(2*a2*ap*(ap*(k1 + 6*k2) - 2*k2*p10) + (k1 + 2*k2)*(4*p3*p4 + (-6*ap + p10)*p5)) + e5*(-2*a2*ap*(ap*(k1 - 6*k2) + 2*k2*p9) - (k1 - 2*k2)*(4*p3*p4 + p5*(-6*ap + p9))))*spp)/16.
  case (54) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p2 = ap**(-2)
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p8 = (k1 + k2)**2
    e9 = exp(-(p1*p7))
    e10 = exp(-(p1*p8))
    ans = (p2*(e9*(a2*ap*(ap + 4*(k1 - k2)*k2) + 2*p3*p4 - p5*(ap - 2*p7)) + e10*(a2*ap*(-ap + 4*k2*(k1 + k2)) - 2*p3*p4 + p5*(ap - 2*p8)))*spp)/8.
  case (55) 
    ans = 0
  case (56) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k2**2
    p4 = ap**2
    p5 = a2**2
    p13 = ap**(-3)
    e11 = exp(-(p1*p3))
    ans = (e11*k2*p13*(a2*ap*(5*ap - 4*p3) + 2*p3*(p4 + p5) - ap*(2*p4 + 3*p5))*spp)/2.
  case (57) 
    ans = 0
  case (58) 
    ans = 0
  case (59) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p16 = ap**(-4)
    p1 = 1/ap
    p9 = (k1 - 2*k2)**2
    p4 = ap**2
    p5 = a2**2
    p23 = (k1 - 2*k2)**4
    p3 = k2**2
    p6 = k1**2
    p24 = (k1 - 2*k2)**3
    p10 = (k1 + 2*k2)**2
    p25 = (k1 + 2*k2)**4
    p26 = (k1 + 2*k2)**3
    e5 = exp(-(p1*p9)/4.)
    e6 = exp(-(p1*p10)/4.)
    ans = (p16*(e6*(4*k2*(-2*ap*(k1 + 3*k2) + k2*p10)*p4 + (-12*ap*p10 + p25 + 12*p4)*p5 - 2*a2*ap*(2*k2*p26 + 6*p4 - 3*ap*(8*k1*k2 + 12*p3 + p6))) - e5*(2*a2*ap*(2*k2*p24 - 6*p4 + 3*ap*(-8*k1*k2 + 12*p3 + p6)) + p5*(p23 + 12*p4 - 12*ap*p9) + 4*k2*p4*(2*ap*(k1 - 3*k2) + k2*p9)))*spp)/32.
  case (60) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p9 = (k1 - 2*k2)**2
    p4 = ap**2
    p5 = a2**2
    p10 = (k1 + 2*k2)**2
    p13 = ap**(-3)
    e5 = exp(-(p1*p9)/4.)
    e6 = exp(-(p1*p10)/4.)
    ans = (p13*(-(e6*(-2*a2*ap*(3*ap*k1 + 10*ap*k2 - 2*k2*p10) + 4*k2*(2*ap - k2*(k1 + 2*k2))*p4 + (k1 + 2*k2)*(6*ap - p10)*p5)) - e5*(4*k2*(2*ap + (k1 - 2*k2)*k2)*p4 + (k1 - 2*k2)*p5*(-6*ap + p9) + 2*a2*ap*(3*ap*k1 - 10*ap*k2 + 2*k2*p9)))*spp)/16.
  case (61) 
    ans = 0
  case (62) 
    ans = 0
  case (63) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p16 = ap**(-4)
    p1 = 1/ap
    p7 = (k1 - k2)**2
    p4 = ap**2
    p5 = a2**2
    p19 = (k1 - k2)**4
    p3 = k2**2
    p6 = k1**2
    p20 = (k1 - k2)**3
    p8 = (k1 + k2)**2
    p21 = (k1 + k2)**4
    p22 = (k1 + k2)**3
    e9 = exp(-(p1*p7))
    e10 = exp(-(p1*p8))
    ans = (p16*(-(e9*(a2*ap*(8*k2*p20 - 3*p4 + 6*ap*(-4*k1*k2 + 3*p3 + p6)) + 2*k2*p4*(2*ap*k1 - 3*ap*k2 + 2*k2*p7) + p5*(3*p4 + 4*(p19 - 3*ap*p7)))) + e10*(-(a2*ap*(8*k2*p22 + 3*p4 - 6*ap*(4*k1*k2 + 3*p3 + p6))) - 2*k2*p4*(2*ap*k1 + 3*ap*k2 - 2*k2*p8) + p5*(3*p4 + 4*(p21 - 3*ap*p8))))*spp)/16.
end select 
end function kinetic
end module mkinetic 
