module EconLibMod

   use EconParMod
   use WorkfileInterface
   use ifport
   use ifwbase
   include "OcactFortLib.inc"
   implicit none

   private
   public :: ModSel, GetAssumptions, AssignFileNames
   public :: OpenWorkfileVerbose
   public :: dateyear, dateqtr
   public :: carn, collapse, diff, lag, lagn, movavg, interpolate
   
   integer :: BKL, AFILE, ADFILE, DFILE
   integer :: selnum
   character (len=128) :: per1, per2, version
   integer :: qtr1a, qtr2a
   
contains

!===============================================================================

   subroutine ModSel(selname, anum, backup, selection)
   
      character (len=128) :: selname
      integer :: anum
      integer, optional :: backup
      character (len = 15) :: listName
      character (len = LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: a
      character (len = LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: assumpt10
      integer :: cta1
      integer :: strlen
      character (len = 4) :: selnumx
      integer, optional :: selection
      
      BKL = OpenWorkfile(trim(DAT_PATH) // "\bkl.wf1")

      write(*,'(////,a,//)') "The following are MODEEM &
         &assumptions and solutions."       
      
      listName = "ASSUMPT"
      strlen = len_trim(listName)
      listName(strlen+1:strlen+1) = IntToAsc(anum)
      call FetchList(BKL, trim(listName), a)      
      call FetchList(BKL, "ASSUMPT10", assumpt10)
      call CloseWorkfile(BKL, .false.)
      
      write(*,'(a)') "  Selection"
      write(*,'(a/)') "   Number          Description"
      do cta1 = 1, AscToInt(a(0))
         if(present(backup) .and. trim(assumpt10(cta1)) /= "N" ) then
            write(*,'("   *",i4,")   ",a)') cta1, trim(a(cta1))
         else
            write(*,'("    ",i4,")   ",a)') cta1, trim(a(cta1))
         end if
      end do
      write(*,'(/a)') "         OR"
      
      if (present(backup)) then
         write(*,'(/a)') "    * Indicates Solution was Backed-Up Previously"
      end if
      write(*,*) "     q    (q)uit"

      do
         write(*,'(/a$)') "Enter Number of Selection or 'q' to quit: "
         read(*,'(a4)') selnumx
         if (ToLower(trim(selnumx)) == 'q') then
            call SetWorkfileVerboseMode(.false.)
            call Abort()
         else if (IsDigit(trim(selnumx))) then
            selnum = AscToInt(selnumx)
            if (present(selection)) then
               selection = selnum
            end if
            selname = trim(a(selnum))
            exit
         else
            write(*,'(a/)') "Invalid Selection. Please try again."
         end if
      end do

   end subroutine ModSel
   
!===============================================================================

   subroutine GetAssumptions(assumpt, selection)
   
      character (len = LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: assumpt
      character (len = 128) :: input1
      character (len = LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: assumpt6
      integer, optional :: selection
      
      if (present(selection)) then
         selnum = selection
      end if
   
      BKL = OpenWorkfile(trim(DAT_PATH) // "\bkl.wf1")
      call FetchList(BKL, "ASSUMPT6", assumpt6)
      call CloseWorkfile(BKL, .false.)
     
      input1 = trim(assumpt6(selnum)) 
      
      AFILE = OpenWorkfile(trim(DAT_PATH) // "\" // trim(input1) // ".wf1")
      call FetchList(AFILE, "ASSUMPT", assumpt)
      call CloseWorkfile(AFILE, .false.)
      
   end subroutine GetAssumptions

!===============================================================================

  function dateyear(date) result(year)
  
     character (len=*) :: date
     integer :: year
     integer :: i
     
     do i = 1, len_trim(date)
        ! Find first non-numeric character and exit
        if (iachar(date(i:i)) < Z"30" .or. iachar(date(i:i)) > Z"39") exit
     end do
     
     year = AscToInt(date(1:i-1))
  
  end function dateyear


!===============================================================================

  function dateqtr(date) result(quarter)
  
     character (len=*) :: date
     integer :: quarter
     integer :: i
     
     do i = 1, len_trim(date)
        ! Find first non-numeric character and exit
        if (iachar(date(i:i)) < Z"30" .or. iachar(date(i:i)) > Z"39") exit
     end do
     
     quarter = AscToInt(date(i+1:len_trim(date)))
  
  end function dateqtr

!===============================================================================

   subroutine AssignFileNames(assumpt, efileName, dfileName, adfileName, &
                              afileName, ofile1Name, ofile2Name)
   
      character (len = LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: assumpt
      character (len = 128) :: afileName, adfileName, efileName, dfileName, &
                               ofile1Name, ofile2Name      
      character (len = LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: dfileList
      
      efileName = trim(assumpt(3))
      dfileName = trim(assumpt(4))
      adfileName = trim(assumpt(5))
      afileName = trim(assumpt(6))
      
      DFILE = OpenWorkfile(trim(DAT_PATH) // "\" // trim(dfileName) // ".wf1")
      call FetchList(DFILE, "dfile", dfileList)
      ofile1Name = trim(dfileList(3))
      ofile2Name = trim(dfileList(5))
      call CloseWorkfile(DFILE, .false.)
      
   end subroutine AssignFileNames

!===============================================================================

   function OpenWorkfileVerbose(fileName) result(FILE_NUMBER)

      character (len = *) :: fileName
      integer :: FILE_NUMBER

      FILE_NUMBER = OpenWorkfile(trim(DAT_PATH) // "\" // trim(fileName))
      write(20,'(a)') "Opened workfile, " // trim(DAT_PATH) // "\" // trim(fileName)

   end function OpenWorkfileVerbose

!===============================================================================  

!===============================================================================

   ! Compound annual growth rate for the period from i-n to i
   double precision function carn(n, X, i)
      ! DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"_carn@12" :: carn
      integer :: n, i
      double precision, dimension(:) :: X
      
      carn = 100d0 * ((X(i) / X(i-n))**(1d0/(dble(n)))-1d0)
  
   end function carn
   
!===============================================================================

   ! Collapse series X to series Y by averaging
   ! Note: Year 1901 is index 1, Month 1901M1 is index 12
   !       Quarter 1901Q1 is index 4
   subroutine collapse(Y, X)
      ! DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"_collapse@8" :: collapse
      double precision, dimension(:) :: X
      double precision, dimension(:) :: Y
      integer :: i, j, k, n, m, start, num
      
      m = size(X)   ! size of uncollapsed series (larger indices)
      n = size(Y)   ! size of collapsed series
      
      start = (n+1) / (MAX_YR+1)     ! starting index of collapsed series
      num = (m+1) / (n+1)     ! number of time periods in each collapsed point
      
      do i = start, n
          j = i * num         ! starting index for period to be collapsed
          k = j + num - 1     ! ending index for period to be collapsed
          Y(i) = sum(X(j:k)) / num
      end do
      
      do i = 1, m
         if (X(i) /= 0) exit   ! make sure first collapsed point has num complete periods
      end do
      if (i > 1 .and. mod(i,num) /= 0) Y(i/num) = 0d0
      
 
   end subroutine collapse

!===============================================================================

   ! Convert lower frequency  series X to higher frequency series Y by method
   ! Note: Year 1901 is index 1, Month 1901M1 is index 12
   !       Quarter 1901Q1 is index 4
   subroutine interpolate(Y, X, method, lineup)
      ! DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"_interpolate@16" :: interpolate
      double precision, dimension(:) :: X
      double precision, dimension(:) :: Y
      integer :: i, j, k, n, m, start, num, startx, starty
      character (len=*) :: method
      character (len=*) :: lineup
      
      m = size(X)   ! size of original series (lower indices)
      n = size(Y)   ! size of interpolated series
      
      startx = (m+1) / (MAX_YR+1)     ! starting index of original series
      starty = (n+1) / (MAX_YR+1)     ! starting index of interpolated series
      num = (n+1) / (m+1)      ! number of interpolated points per original point

      if (ToUpper(method) == "SPLINE") then
      else if (ToUpper(method) == "REPEAT") then
         do i = startx, m
            j = num * i
            Y(j:j+num-1) = X(i)
         end do
      else if (ToUpper(method) == "LINEAR") then
      else if (ToUpper(method) == "GEOMETRIC") then
      else if (ToUpper(method) == "PRORATE") then
      else if (ToUpper(method) == "PATTERN") then
      else if (ToUpper(method) == "LAGRANGE") then
      else
      end if
 
   end subroutine interpolate

!===============================================================================     

   ! N-th difference of successive values in series X
   ! Use diff subroutine if n=1 for rist difference
   subroutine diffn(X, Y, n)
      ! DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"_diffn@12" :: diffn
      double precision, dimension(:) :: X, Y
      integer :: n
      integer :: sx, sy, i, j, m      

      sx = size(X)
      sy = size(y)
      
      if (sx /= sy) then
         write(*,*) "Error: Cannot difference - series are different sizes."
         stop
      end if
   
      !if (.not. present(n)) then
      !   m = 1
      !else
         m = n
      !end if
      
      Y = X
      do j = sy, 1+m, -1        ! must iterate backwards to prevent overwriting
         Y(j) = Y(j) - Y(j-m)   ! current period values
      end do
  
   end subroutine diffn

!===============================================================================

   subroutine diff(X, Y)
      ! DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"_diff@8" :: diff
      double precision, dimension(:) :: X, Y
      
      call diffn(X,Y,1)
      
   end subroutine diff
      
!===============================================================================      

   ! Series X lagged by n periods
   ! Used lag subroutine for n=1
   subroutine lagn(X, Y, n)
      ! DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"_lagn@12" :: lagn
      double precision, dimension(:) :: X, Y
      integer :: sx, sy, i, j, m, n      

      sx = size(X)
      sy = size(y)
      
      if (sx /= sy) then
         write(*,*) "Error: Cannot lag - series are different sizes."
         stop
      end if
   
      !if (.not. present(n)) then
      !   m = 1
      !else
         m = n
      !end if
      
      Y = X
      do j = 1, sy-m
         Y(j+m) = X(j)
      end do
  
   end subroutine lagn
   
!==============================================================================

   subroutine lag(X, Y)
      ! DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"_lag@8" :: lag
      double precision, dimension(:) :: X, Y
   
      call lagn(X, Y, 1)
      
   end subroutine

!==============================================================================

   ! Moving average of series X
   ! Must specify  n the number of lags
   subroutine movavg(X, Y, n)
      ! DEC$ ATTRIBUTES DLLEXPORT, STDCALL, REFERENCE, ALIAS:"_movavg@12" :: movavg
      double precision, dimension(:) :: X, Y
      integer :: n
      integer :: sx, sy, i, j, m      

      sx = size(X)
      sy = size(y)
      
      if (sx /= sy) then
         write(*,*) "Error: Cannot compute moving average - &
            &series are different sizes."
         stop
      end if
   
      m = n
      Y = X
      do j = 1, sy-m+1
         Y(j+m-1) = sum(X(j:j+m-1)) / m
      end do
  
   end subroutine movavg     
   
!==============================================================================    

end module EconLibMod