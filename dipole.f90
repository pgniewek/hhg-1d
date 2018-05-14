
module mdipole
use constants
implicit none

contains

function dipole(n1,i1,j1,a1,k1,n2,i2,j2,a2,k2) result(ans)
 integer, intent(in) :: n1,i1,j1,n2,i2,j2
real(quad), intent(in) :: a1,a2,k1,k2
real(quad) :: ans
! - - - - - - - - - - - - - - - - - - 
integer :: key
real(quad) :: ap, spp 
real(quad) :: p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13
real(quad) :: e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13
! - - - - - - - - - - - - - - - - - - 
 
key = n1 + 2*i1 + 4*j1 + 8*n2 + 16*i2 + 32*j2

select case (key)
  case (0) 
    ans = 0
  case (1) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    ans = (p1*spp)/2.
  case (2) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = k1**2
    e7 = exp(-(p1*p7)/4.)
    ans = (e7*k1*p1*spp)/2.
  case (3) 
    ans = 0
  case (4) 
    ans = 0
  case (5) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = k1**2
    p10 = ap**(-2)
    e7 = exp(-(p1*p7)/4.)
    ans = (e7*p10*(2*ap - p7)*spp)/4.
  case (6) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = k1**2
    e8 = exp(-(p1*p7))
    ans = (e8*k1*p1*spp)/2.
  case (7) 
    ans = 0
  case (8) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    ans = (p1*spp)/2.
  case (9) 
    ans = 0
  case (10) 
    ans = 0
  case (11) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = k1**2
    p11 = ap**(-3)
    p13 = k1**3
    e7 = exp(-(p1*p7)/4.)
    ans = (e7*p11*(6*ap*k1 - p13)*spp)/8.
  case (12) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = k1**2
    p10 = ap**(-2)
    e7 = exp(-(p1*p7)/4.)
    ans = (e7*p10*(2*ap - p7)*spp)/4.
  case (13) 
    ans = 0
  case (14) 
    ans = 0
  case (15) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p7 = k1**2
    p11 = ap**(-3)
    p13 = k1**3
    e8 = exp(-(p1*p7))
    ans = (e8*p11*(3*ap*k1 - 2*p13)*spp)/4.
  case (16) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    e1 = exp(-(p1*p2)/4.)
    ans = (e1*k2*p1*spp)/2.
  case (17) 
    ans = 0
  case (18) 
    ans = 0
  case (19) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    p10 = ap**(-2)
    e3 = exp(-(p1*p3)/4.)
    e4 = exp(-(p1*p4)/4.)
    ans = (p10*(e3*(2*ap - p3) + e4*(-2*ap + p4))*spp)/8.
  case (20) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    e3 = exp(-(p1*p3)/4.)
    e4 = exp(-(p1*p4)/4.)
    ans = ((e3*(-k1 + k2) + e4*(k1 + k2))*p1*spp)/4.
  case (21) 
    ans = 0
  case (22) 
    ans = 0
  case (23) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p8 = (-2*k1 + k2)**2
    p9 = (2*k1 + k2)**2
    p10 = ap**(-2)
    e9 = exp(-(p1*p8)/4.)
    e10 = exp(-(p1*p9)/4.)
    ans = (p10*(e9*(2*ap - p8) + e10*(-2*ap + p9))*spp)/16.
  case (24) 
    ans = 0
  case (25) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    p11 = ap**(-3)
    p12 = k2**3
    e1 = exp(-(p1*p2)/4.)
    ans = (e1*p11*(6*ap*k2 - p12)*spp)/8.
  case (26) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    p10 = ap**(-2)
    e3 = exp(-(p1*p3)/4.)
    e4 = exp(-(p1*p4)/4.)
    ans = (p10*(e3*(2*ap - p3) + e4*(-2*ap + p4))*spp)/8.
  case (27) 
    ans = 0
  case (28) 
    ans = 0
  case (29) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    p11 = ap**(-3)
    e3 = exp(-(p1*p3)/4.)
    e4 = exp(-(p1*p4)/4.)
    ans = (p11*(e3*(k1 - k2)*(-6*ap + p3) + e4*(k1 + k2)*(6*ap - p4))*spp)/16.
  case (30) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p8 = (-2*k1 + k2)**2
    p9 = (2*k1 + k2)**2
    p10 = ap**(-2)
    e9 = exp(-(p1*p8)/4.)
    e10 = exp(-(p1*p9)/4.)
    ans = (p10*(e9*(2*ap - p8) + e10*(-2*ap + p9))*spp)/16.
  case (31) 
    ans = 0
  case (32) 
    ans = 0
  case (33) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    p10 = ap**(-2)
    e1 = exp(-(p1*p2)/4.)
    ans = (e1*p10*(2*ap - p2)*spp)/4.
  case (34) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    e3 = exp(-(p1*p3)/4.)
    e4 = exp(-(p1*p4)/4.)
    ans = ((e3*(k1 - k2) + e4*(k1 + k2))*p1*spp)/4.
  case (35) 
    ans = 0
  case (36) 
    ans = 0
  case (37) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    p10 = ap**(-2)
    e3 = exp(-(p1*p3)/4.)
    e4 = exp(-(p1*p4)/4.)
    ans = (p10*(2*ap*(e3 + e4) - e3*p3 - e4*p4)*spp)/8.
  case (38) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p8 = (-2*k1 + k2)**2
    p9 = (2*k1 + k2)**2
    e9 = exp(-(p1*p8)/4.)
    e10 = exp(-(p1*p9)/4.)
    ans = ((2*(e10 + e9)*k1 + (e10 - e9)*k2)*p1*spp)/8.
  case (39) 
    ans = 0
  case (40) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    p10 = ap**(-2)
    e1 = exp(-(p1*p2)/4.)
    ans = (e1*p10*(2*ap - p2)*spp)/4.
  case (41) 
    ans = 0
  case (42) 
    ans = 0
  case (43) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    p11 = ap**(-3)
    e3 = exp(-(p1*p3)/4.)
    e4 = exp(-(p1*p4)/4.)
    ans = (p11*(e3*(k1 - k2)*(6*ap - p3) + e4*(k1 + k2)*(6*ap - p4))*spp)/16.
  case (44) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    p10 = ap**(-2)
    e3 = exp(-(p1*p3)/4.)
    e4 = exp(-(p1*p4)/4.)
    ans = (p10*(2*ap*(e3 + e4) - e3*p3 - e4*p4)*spp)/8.
  case (45) 
    ans = 0
  case (46) 
    ans = 0
  case (47) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    p7 = k1**2
    p11 = ap**(-3)
    e13 = exp(-(p1*(p2 + 4*p7))/4.)
    ans = (e13*p11*spp*(2*k1*(6*ap - 3*p2 - 4*p7)*Cosh(k1*k2*p1) + k2*(-6*ap + p2 + 12*p7)*Sinh(k1*k2*p1)))/16.
  case (48) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    e2 = exp(-(p1*p2))
    ans = (e2*k2*p1*spp)/2.
  case (49) 
    ans = 0
  case (50) 
    ans = 0
  case (51) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p5 = (k1 - 2*k2)**2
    p6 = (k1 + 2*k2)**2
    p10 = ap**(-2)
    e5 = exp(-(p1*p5)/4.)
    e6 = exp(-(p1*p6)/4.)
    ans = (p10*(e5*(2*ap - p5) + e6*(-2*ap + p6))*spp)/16.
  case (52) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p5 = (k1 - 2*k2)**2
    p6 = (k1 + 2*k2)**2
    e5 = exp(-(p1*p5)/4.)
    e6 = exp(-(p1*p6)/4.)
    ans = (((-e5 + e6)*k1 + 2*(e5 + e6)*k2)*p1*spp)/8.
  case (53) 
    ans = 0
  case (54) 
    ans = 0
  case (55) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    p10 = ap**(-2)
    e11 = exp(-(p1*p3))
    e12 = exp(-(p1*p4))
    ans = (p10*(e11*(ap - 2*p3) - e12*(ap - 2*p4))*spp)/16.
  case (56) 
    ans = 0
  case (57) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    p11 = ap**(-3)
    p12 = k2**3
    e2 = exp(-(p1*p2))
    ans = (e2*p11*(3*ap*k2 - 2*p12)*spp)/4.
  case (58) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p5 = (k1 - 2*k2)**2
    p6 = (k1 + 2*k2)**2
    p10 = ap**(-2)
    e5 = exp(-(p1*p5)/4.)
    e6 = exp(-(p1*p6)/4.)
    ans = (p10*(e5*(2*ap - p5) + e6*(-2*ap + p6))*spp)/16.
  case (59) 
    ans = 0
  case (60) 
    ans = 0
  case (61) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p5 = (k1 - 2*k2)**2
    p6 = (k1 + 2*k2)**2
    p11 = ap**(-3)
    e5 = exp(-(p1*p5)/4.)
    e6 = exp(-(p1*p6)/4.)
    ans = (p11*(e5*(k1 - 2*k2)*(-6*ap + p5) + e6*(k1 + 2*k2)*(6*ap - p6))*spp)/32.
  case (62) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = (k1 - k2)**2
    p4 = (k1 + k2)**2
    p10 = ap**(-2)
    e11 = exp(-(p1*p3))
    e12 = exp(-(p1*p4))
    ans = (p10*(e11*(ap - 2*p3) - e12*(ap - 2*p4))*spp)/16.
  case (63) 
    ans = 0
end select 
end function dipole
end module mdipole 
