module SplineMod

  implicit none
  
  private
  public :: q00
  
contains

!===============================================================================
   
   subroutine q00 (popsy, cag, rag, rsy)
   ! Quadratic-spline interpolation of rates with function-value boundary conditions at both ends
   ! Preserves total counts by age group (count = rate * pop)
   ! Expects rsy to already have values for ages <20 and >69

      double precision, dimension(0:100), intent(in) :: popsy ! population by single year of age
      double precision, dimension(10), intent(in) :: cag ! ce counts by age group, 2024 to 6569
      double precision, dimension(10), intent(in) :: rag ! ce rates by age group, 2024 to 6569
      double precision, dimension(0:99), intent(inout) :: rsy ! ce rates by single year of age
      double precision, dimension(10) :: pop ! population by age group
      double precision, dimension(10, 5) :: pop2d ! population by age group and local (within-group) age
      double precision, dimension(10) :: u, v, w ! intermediate expressions
      double precision, dimension(30, 30) :: cm !, cmt ! matrix of coefficients (***CMT IS TEST ONLY***)
      double precision, dimension(30) :: rv ! RHS vector
      double precision, dimension(30) :: cba ! vector of spline coefficients, ordered c1, b1, a1, c2, ...
      double precision, dimension(30, 30) :: lm, um ! LU decomposition matrices
      double precision :: rl, rh ! rates at low and high end
      integer :: g, h
  
      ! Compute boundary values
      call bndry00 (rsy, rl, rh, rag)
  
      ! Compute population by age group and (age grp, age)
      call convertInputs (popsy(20:69), pop, pop2d)
  
      ! Compute components of some coefficients
      call cond1coeffs (pop2d, u, v, w)
  
      ! Populate the matrix of coefficients and the RHS vector
      ! To keep the matrix closest to diagonal, adopt:
      !   1) The order of coefficients: c (constant), b (linear), a (quadratic)
      !   2) The order of equations:
      !        1. Low-end boundary condition
      !             c(1) = RL
      !        2. For each age group g, g = 1 to 9:
      !             1. Number preserving condition
      !                  w(g)*c(g)+v(g)*b(g)+u(g)*a(g) = P(g)R(g)
      !             2. Continuity condition
      !                  c(g)+5*b(g)+(5**2)*a(g) = c(g+1)
      !             3. Smoothness condition (first-derivative continuity)
      !                  b(g)+10*a(g) = b(g+1)
      !        3. Number-preserving condition for age group g=10
      !             w(10)*c(10)+v(10)*b(10)+u(10)*a(10) = P(10)R(10)
      !        4. High-end boundary condition
      !             c(10)+5*b(10)+(5**2)*a(10) = RH
      cm = 0d0
      rv = 0d0
      cm(1,1) = 1d0
      rv(1) = rl
      do g = 1, 9
         cm(3*g-1, 3*g-2) = w(g)
         cm(3*g-1, 3*g-1) = v(g)
         cm(3*g-1, 3*g) = u(g)
         rv(3*g-1) = cag(g)
         cm(3*g, 3*g-2) = 1d0
         cm(3*g, 3*g-1) = 5d0
         cm(3*g, 3*g) = 25d0
         cm(3*g, 3*g+1) = -1d0
         cm(3*g+1, 3*g-1) = 1d0
         cm(3*g+1, 3*g) = 10d0
         cm(3*g+1, 3*g+2) = -1d0
      end do
      cm(29, 28) = w(10)
      cm(29, 29) = v(10)
      cm(29, 30) = u(10)
      rv(29) = cag(10)
      cm(30, 28) = 1d0
      cm(30, 29) = 5d0
      cm(30, 30) = 25d0
      rv(30) = rh
  
      ! Decompose matrix of coefficients
      call LUdec (cm, lm, um)
      
      !!TEST: check decomposition
      !cmt = matmul(lm, um)
      !cmt = abs(cmt - cm)
      !print*, maxval(cmt)
      !pause

      ! Solve
      call triangSolve(lm, um, rv, cba)
  
      ! Compute SYOA rates (integrated over 1-year intervals)
      do g = 1, 10
         do h = 1, 5
            rsy(14+5*g+h) = cba(3*g)/3d0 * (h**3 - (h-1)**3) + cba(3*g-1)/2d0 * (h**2 - (h-1)**2) + cba(3*g-2)
            !print *, 14+5*g+h, rsy(14+5*g+h)
            !pause
         end do
      end do

   end subroutine q00
      
!===============================================================================
   
   subroutine bndry00 (a, lx, hx, x)
   ! Boundary conditions for values only
   ! Very simplistic for now

      double precision, dimension(0:99), intent(in) :: a ! values for all ages
      double precision, dimension(10), intent(in) :: x ! values by 5-year age groups
      double precision, intent(out) :: lx, hx ! values at boundaries (exact ages 20 and 70)
      double precision, dimension(2) :: ls, hs ! 2 years just outside the low and high boundaries
  
      ls = a(18:19)
      hs = a(70:71)
      
      lx = 0.6 * ls(2) + 0.4 * x(1)
      hx = 0.8 * hs(1) + 0.2 * x(10)
  
      !lx = ls(2) + 0.5 * (ls(2) - ls(1))
      !hx = hs(1) - 0.5 * (hs(2) - hs(1))
  
      ! Since the input rates may be garbage at this point, use this for testing:
      !lx = 0.75
      !hx = 0.35
      
      !print *, lx, hx
      !pause

   end subroutine bndry00

!===============================================================================

   subroutine convertInputs (arrIn, arrOut1, arrOut2)
   ! converts a 50-element 1-d array arrIn (syoa 20-69)
   ! to a (10,5)-element 2-d array (age group, local age) arrOut2
   ! and aggregates it to a 10-element array (age grp) arrOut1

      
      double precision, dimension(50), intent(in) :: arrIn
      double precision, dimension(10), intent(out) :: arrOut1
      double precision, dimension(10, 5), intent(out) :: arrOut2
      integer :: g, h
  
      do g = 1, 10
         arrOut1(g) = 0d0
         do h = 1, 5
            arrOut2(g, h) = arrIn((g-1)*5+h)
            arrOut1(g) = arrOut1(g) + arrIn((g-1)*5+h)
         end do
      end do
  
   end subroutine convertInputs

!===============================================================================
   
   subroutine cond1coeffs (pop, u, v, w)
   ! coefficients in the number-preserving condition

      double precision, dimension(10, 5), intent(in) :: pop
      double precision, dimension(10), intent(out) :: u, v, w
      integer :: g, h
  
      do g = 1, 10
         u(g) = 0d0
         v(g) = 0d0
         w(g) = 0d0
         do h = 1, 4
            u(g) = u(g) + (pop(g, h) - pop(g, h+1)) * h**3
            v(g) = v(g) + (pop(g, h) - pop(g, h+1)) * h**2
            w(g) = w(g) + (pop(g, h) - pop(g, h+1)) * h
         end do
         u(g) = u(g) + pop(g, 5) * h**3
         v(g) = v(g) + pop(g, 5) * h**2
         w(g) = w(g) + pop(g, 5) * h
         u(g) = u(g) / 3d0
         v(g) = v(g) / 2d0
      end do

   end subroutine cond1coeffs

!===============================================================================
   
   subroutine LUdec (a, l, u)
   ! LU decomposition of matrix a, with L having a unit diagonal

      double precision, dimension(30, 30), intent(in) :: a
      double precision, dimension(30, 30), intent(out) :: l, u
      integer :: i, j, k
  
      l = 0d0
      u = 0d0
  
      do i = 1, 30
         l(i, i) = 1d0
      end do
  
      do j = 1, 30
         do i = 1, j
            u(i, j) = a(i, j)
            do k = 1, i - 1
               u(i, j) = u(i, j) - l(i, k) * u (k, j)
            end do
         end do
         !! ERROR CHECKING - this is primitive, ideally should have a procedure to rearrange rows if necessary
         !print *, j, u(j, j)
         !if (abs(u(j, j)) < 1d-6) then
         !  print *, "Small pivot: LU decomposition may crash."
         !  pause
         !end if
         !! END ERROR CHECKING
         do i = j + 1, 30
            l(i, j) = a(i, j)
            do k = 1, j - 1
               l(i, j) = l(i, j) - l(i, k) * u(k, j)
            end do
            l(i, j) = l(i, j) / u(j, j)
         end do
      end do

   end subroutine LUdec
   
!===============================================================================

   subroutine triangSolve (l, u, b, x)
   ! solves a system of equations LUx=b, where L(U) is a lower(upper) triangular matrix
   ! assumes diagonal elements of L are 1 (so no division is needed)

      double precision, dimension(30, 30), intent(in) :: l, u
      double precision, dimension(30), intent(in) :: b
      double precision, dimension(30), intent(out) :: x
      double precision, dimension(30) :: y
      integer :: i, j
  
      x = 0d0
      y = 0d0
  
      ! Solve L (Ux) = b for y = Ux by forward substitution
      do i = 1, 30
         y(i) = b(i)
         do j = 1, i-1
            y(i) = y(i) - l(i,j) * y(j)
         end do
      end do
      
      ! Solve U x = y by backward substitution
      do i = 30, 1, -1
         x(i) = y(i) / u(i,i)
         do j = i+1, 30
            x(i) = x(i) - u(i,j) * x(j) / u(i,i)
         end do
      end do
  
   end subroutine triangSolve
   
!===============================================================================

end module SplineMod