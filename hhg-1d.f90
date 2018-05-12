program HHG1D

implicit none
  
integer, parameter :: quad = selected_real_kind(33, 4931)

real(quad), parameter :: q_pi = 3.1415926535897932384626433832795

real(quad) :: a1,a2,k1,k2
real(quad) :: v1, v2, v3



a1 = 0.5
a2 = 0.7
k1 = 0.5
k2 = 0.0

v1 = overlap(1,0,0,a1,k1,1,0,0,a2,k1)
v2 = overlap(1,1,0,a1,k1,1,1,0,a2,k1)
v3 = overlap(1,1,1,a1,k1,1,1,1,a2,k1)

write(*,*)
write(*,*) v1
write(*,*) v2
write(*,*) v3
write(*,*)

contains 

function overlap(n1,i1,j1,a1,k1,n2,i2,j2,a2,k2) result(ans)
  integer, intent(in)   :: n1,i1,j1,n2,i2,j2
  real(quad), intent(in) :: a1,a2,k1,k2
  real(quad) :: ans
  ! - - - - - - - - - - - - - - - - - - 
  integer   :: key
  real(quad) :: ap, spp
  real(quad) :: p1,p2,p3,p4,p5,p6,p7,p8,p9,p10
  real(quad) :: e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12
  ! - - - - - - - - - - - - - - - - - - 
  
key = n1 + 2*i1 + 4*j1 + 8*n2 + 16*i2 + 32*j2

select case (key)
  case (0) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    ans = spp
  case (1) 
    ans = 0
  case (2) 
    ans = 0
  case (3) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k1**2
    e2 = exp(-(p1*p3)/4.)
    ans = (e2*k1*p1*spp)/2.
  case (4) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k1**2
    e2 = exp(-(p1*p3)/4.)
    ans = e2*spp
  case (5) 
    ans = 0
  case (6) 
    ans = 0
  case (7) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k1**2
    e12 = exp(-(p1*p3))
    ans = (e12*k1*p1*spp)/2.
  case (8) 
    ans = 0
  case (9) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    ans = (p1*spp)/2.
  case (10) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k1**2
    e2 = exp(-(p1*p3)/4.)
    ans = (e2*k1*p1*spp)/2.
  case (11) 
    ans = 0
  case (12) 
    ans = 0
  case (13) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k1**2
    p10 = ap**(-2)
    e2 = exp(-(p1*p3)/4.)
    ans = -(e2*p10*(-2*ap + p3)*spp)/4.
  case (14) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p3 = k1**2
    e12 = exp(-(p1*p3))
    ans = (e12*k1*p1*spp)/2.
  case (15) 
    ans = 0
  case (16) 
    ans = 0
  case (17) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    e1 = exp(-(p1*p2)/4.)
    ans = (e1*k2*p1*spp)/2.
  case (18) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    e3 = exp(-(p1*p4)/4.)
    e4 = exp(-(p1*p5)/4.)
    ans = ((e3 - e4)*spp)/2.
  case (19) 
    ans = 0
  case (20) 
    ans = 0
  case (21) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    e3 = exp(-(p1*p4)/4.)
    e4 = exp(-(p1*p5)/4.)
    ans = ((e3*(-k1 + k2) + e4*(k1 + k2))*p1*spp)/4.
  case (22) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p8 = (-2*k1 + k2)**2
    p9 = (2*k1 + k2)**2
    e7 = exp(-(p1*p8)/4.)
    e8 = exp(-(p1*p9)/4.)
    ans = ((e7 - e8)*spp)/4.
  case (23) 
    ans = 0
  case (24) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    e1 = exp(-(p1*p2)/4.)
    ans = (e1*k2*p1*spp)/2.
  case (25) 
    ans = 0
  case (26) 
    ans = 0
  case (27) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    p10 = ap**(-2)
    e3 = exp(-(p1*p4)/4.)
    e4 = exp(-(p1*p5)/4.)
    ans = (p10*(e3*(2*ap - p4) + e4*(-2*ap + p5))*spp)/8.
  case (28) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    e3 = exp(-(p1*p4)/4.)
    e4 = exp(-(p1*p5)/4.)
    ans = ((e3*(-k1 + k2) + e4*(k1 + k2))*p1*spp)/4.
  case (29) 
    ans = 0
  case (30) 
    ans = 0
  case (31) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p8 = (-2*k1 + k2)**2
    p9 = (2*k1 + k2)**2
    p10 = ap**(-2)
    e7 = exp(-(p1*p8)/4.)
    e8 = exp(-(p1*p9)/4.)
    ans = (p10*(e7*(2*ap - p8) + e8*(-2*ap + p9))*spp)/16.
  case (32) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    e1 = exp(-(p1*p2)/4.)
    ans = e1*spp
  case (33) 
    ans = 0
  case (34) 
    ans = 0
  case (35) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    e3 = exp(-(p1*p4)/4.)
    e4 = exp(-(p1*p5)/4.)
    ans = ((e3*(k1 - k2) + e4*(k1 + k2))*p1*spp)/4.
  case (36) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    e3 = exp(-(p1*p4)/4.)
    e4 = exp(-(p1*p5)/4.)
    ans = ((e3 + e4)*spp)/2.
  case (37) 
    ans = 0
  case (38) 
    ans = 0
  case (39) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p8 = (-2*k1 + k2)**2
    p9 = (2*k1 + k2)**2
    e7 = exp(-(p1*p8)/4.)
    e8 = exp(-(p1*p9)/4.)
    ans = ((2*(e7 + e8)*k1 + (-e7 + e8)*k2)*p1*spp)/8.
  case (40) 
    ans = 0
  case (41) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    p10 = ap**(-2)
    e1 = exp(-(p1*p2)/4.)
    ans = -(e1*p10*(-2*ap + p2)*spp)/4.
  case (42) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    e3 = exp(-(p1*p4)/4.)
    e4 = exp(-(p1*p5)/4.)
    ans = ((e3*(k1 - k2) + e4*(k1 + k2))*p1*spp)/4.
  case (43) 
    ans = 0
  case (44) 
    ans = 0
  case (45) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    p10 = ap**(-2)
    e3 = exp(-(p1*p4)/4.)
    e4 = exp(-(p1*p5)/4.)
    ans = (p10*(2*ap*(e3 + e4) - e3*p4 - e4*p5)*spp)/8.
  case (46) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p8 = (-2*k1 + k2)**2
    p9 = (2*k1 + k2)**2
    e7 = exp(-(p1*p8)/4.)
    e8 = exp(-(p1*p9)/4.)
    ans = ((2*(e7 + e8)*k1 + (-e7 + e8)*k2)*p1*spp)/8.
  case (47) 
    ans = 0
  case (48) 
    ans = 0
  case (49) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    e11 = exp(-(p1*p2))
    ans = (e11*k2*p1*spp)/2.
  case (50) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = (k1 - 2*k2)**2
    p7 = (k1 + 2*k2)**2
    e5 = exp(-(p1*p6)/4.)
    e6 = exp(-(p1*p7)/4.)
    ans = ((e5 - e6)*spp)/4.
  case (51) 
    ans = 0
  case (52) 
    ans = 0
  case (53) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = (k1 - 2*k2)**2
    p7 = (k1 + 2*k2)**2
    e5 = exp(-(p1*p6)/4.)
    e6 = exp(-(p1*p7)/4.)
    ans = (((-e5 + e6)*k1 + 2*(e5 + e6)*k2)*p1*spp)/8.
  case (54) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    e9 = exp(-(p1*p4))
    e10 = exp(-(p1*p5))
    ans = ((-e10 + e9)*spp)/8.
  case (55) 
    ans = 0
  case (56) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p2 = k2**2
    e11 = exp(-(p1*p2))
    ans = (e11*k2*p1*spp)/2.
  case (57) 
    ans = 0
  case (58) 
    ans = 0
  case (59) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = (k1 - 2*k2)**2
    p7 = (k1 + 2*k2)**2
    p10 = ap**(-2)
    e5 = exp(-(p1*p6)/4.)
    e6 = exp(-(p1*p7)/4.)
    ans = (p10*(e5*(2*ap - p6) + e6*(-2*ap + p7))*spp)/16.
  case (60) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p6 = (k1 - 2*k2)**2
    p7 = (k1 + 2*k2)**2
    e5 = exp(-(p1*p6)/4.)
    e6 = exp(-(p1*p7)/4.)
    ans = (((-e5 + e6)*k1 + 2*(e5 + e6)*k2)*p1*spp)/8.
  case (61) 
    ans = 0
  case (62) 
    ans = 0
  case (63) 
    ap = a1 + a2 
    spp = sqrt(q_pi/ap) 
    p1 = 1/ap
    p4 = (k1 - k2)**2
    p5 = (k1 + k2)**2
    p10 = ap**(-2)
    e9 = exp(-(p1*p4))
    e10 = exp(-(p1*p5))
    ans = (p10*(e9*(ap - 2*p4) - e10*(ap - 2*p5))*spp)/16.
end select 

end function overlap

end program HHG1D
