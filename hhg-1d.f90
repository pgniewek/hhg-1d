program HHG1D
  use constants
  use moverlap
  
implicit none

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

end program HHG1D
