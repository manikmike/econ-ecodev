module EconModSolAHistOutMod

   use EconParMod
   use EconModSolAVarMod
   use ifport
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSolAHistOutMain
   
   integer, parameter :: NUM_SERIES = 1215
   character (len = 18) , dimension(NUM_SERIES) :: seriesName
   integer, dimension(2,NUM_SERIES) :: per
   integer :: fileNum
   integer :: passNum = 0
   character (len = 128) :: username
   real (kind = 8), dimension(2,13,MAX_YR) :: covwkr
   real (kind = 8), dimension(2,0:100,4,MAX_YR) :: ce, ce_mef
   
contains

!===============================================================================

   subroutine EconModSolAHistOutMain()
   
      fileNum = 1
      passNum = passNum + 1
      if (passNum == 8) then
         if (WORKFILE_EXPORT) then
            call GetSeriesNames()
            call WriteOutputFileEV()
            call WriteProgramEV()
            call RunProgramEV()
         end if
      end if
             
   end subroutine EconModSolAHistOutMain

!===============================================================================

   subroutine GetSeriesNames()
   
      integer :: n
      
      do n = 1, NUM_SERIES
         read(0502, '(a18,2i5)') seriesName(n), per(1,n), per(2,n)
      end do
   
   end subroutine GetSeriesNames

!===============================================================================

   subroutine MapSeries(name, series)
   
      character (len = *) :: name
      real (kind = 8), dimension(:) :: series
      
      include "CompileSeriesMaps.fh"
      
      !DEC$ IF (COMPILE_SERIES_MAPS == 1)
      
      include "ModSolAHistSeriesMap.fh"

      else
         series = 0d0
      end if
      
      !DEC$ END IF
   
   end subroutine MapSeries

!===============================================================================

   subroutine WriteOutputFileEV()
   
      integer :: n, len
      character :: freq
      real (kind = 8), dimension(MAX_YR) :: annualSeries
      real (kind = 8), dimension(MAX_QTR) :: quarterlySeries
            
      do n = 1, NUM_SERIES
         len = len_trim(seriesName(n))
         freq = ToUpper(seriesName(n)(len:len))
         if (freq == "A") then
            write(5008,'(a)') "pageselect a"
            write(5008,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(5008,'(a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", 1900+per(1,n),  ") _" 
            call MapSeries(trim(seriesName(n)), annualSeries)
            call WriteSeriesEV(annualSeries, per(1,n), per(2,n))
         else if (freq == "Q") then
            write(5008,'(a)') "pageselect q"
            write(5008,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(5008,'(a,i0,a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", &
               1900+(per(1,n)/4),"q", mod(per(1,n),4)+1, ") _" 
            call MapSeries(trim(seriesName(n)), quarterlySeries)
            call WriteSeriesEV(quarterlySeries, per(1,n), per(2,n))
         end if
         write(5008,*)
      end do
      call CloseFile(5008)
   
   end subroutine WriteOutputFileEV

!===============================================================================
   
   subroutine WriteSeriesEV(series, i1, i2, fileNum)
   
      real (kind = 8), dimension(:) :: series
      integer :: i1, i2
      integer :: i, r, num
      integer, optional :: fileNum

      if (present(fileNum)) then
         num = fileNum
      else
         num = 5008
      end if
      
      do i = i1, i2, 4
         if (i + 4 <= i2) then
            write(num,'("  ",3(f29.13,","),f29.13,$)') series(i:i+3)
            write(num,'(a)') ", _"
            !write(num,'(a$)') "  "
         else
             r = i2 - i + 1
             if (r > 1) then 
                write(num,'("  ",' // IntToAsc(r-1) // '(f29.13,","),$)') series(i:i+r-2)         
                write(num,'(f29.13)') series(i+r-1)
             else
                write(num,'("  ",f29.13)') series(i+r-1)
             end if
         end if
         
      end do
   
   end subroutine WriteSeriesEV

!===============================================================================
   
   subroutine WriteProgramEV()
   
      ! Import Series
      write(5007,'(a)') "wfopen " // trim(OUT_PATH) // "\mul\" // trim(dfilename)
      write(5007,'(a)') "exec " // trim(OUT_PATH) // "\internal\ModSolAHistOut.prg"
      write(5007,'(a)') "wfsave(2) " // trim(OUT_PATH) // "\mul\" // trim(dfilename)
      write(5007,'(a)') "wfclose"
      
      call CloseFile(5007)
   
   end subroutine WriteProgramEV

!===============================================================================
   
   subroutine RunProgramEV()
   
      integer :: rv3
   
      write(*,'(a$)') "Importing solution to file " // trim(dfileName) // ".wf1"
      rv3 = ExecuteProgram(trim(OUT_PATH) // "\internal\ModSolAHistImport.prg")
      if (rv3 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
              
   end subroutine RunProgramEV

!===============================================================================   

end module EconModSolAHistOutMod