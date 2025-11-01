module EconModSol2OutMod

   use EconParMod
   use EconModSol2VarMod
   use ifport
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSol2OutMain
   
   integer, parameter :: NUM_SERIES = 792
   character (len = 18) , dimension(NUM_SERIES) :: seriesName
   integer, dimension(2,NUM_SERIES) :: per
   integer :: fileNum
   character (len = 128) :: username
   
contains

!===============================================================================

   subroutine EconModSol2OutMain()
   
      fileNum = 1
      if (WORKFILE_EXPORT) then
         call GetSeriesNames()
         call WriteOutputFileEV()
         call ModSol2ToStoch()
         call WriteProgramEV()
         call RunProgramEV()
      end if
             
   end subroutine EconModSol2OutMain

!===============================================================================

   subroutine GetSeriesNames()
   
      integer :: n
      
      do n = 1, NUM_SERIES
         read(0201, '(a18,2i5)') seriesName(n), per(1,n), per(2,n)
      end do
   
   end subroutine GetSeriesNames

!===============================================================================

   subroutine MapSeries(name, series)
   
      character (len = *) :: name
      real (kind = 8), dimension(:) :: series
      
      include "CompileSeriesMaps.fh"
      
      !DEC$ IF (COMPILE_SERIES_MAPS == 1)
      
      include "ModSol2SeriesMap.fh"

      else
         series = 0d0
      end if
      
      !DEC$ END IF
   
   end subroutine MapSeries

!===============================================================================

   subroutine ModSol2ToStoch()
   
      !integer :: iSex, iAge
      character(len=16) :: serName, blnkStr = "                "
      character(len=12) :: serData
      
      write (serData,'(a2,2i5)') " a", TRYEAR - 2, 1900 + endYr
      
      serName = "pgdp"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 pgdp_a(TRYEAR-1900-2:endYr)
      
      serName = "wsd"
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 wsd_a(TRYEAR-1900-2:endYr)
      
      serName = "wss"
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 wss_a(TRYEAR-1900-2:endYr)
      
      serName = "y"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 y_a(TRYEAR-1900-2:endYr)

   end subroutine ModSol2ToStoch

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
            write(2004,'(a)') "pageselect a"
            write(2004,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(2004,'(a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", 1900+per(1,n),  ") _" 
            call MapSeries(trim(seriesName(n)), annualSeries)
            call WriteSeriesEV(annualSeries, per(1,n), per(2,n))
         else if (freq == "Q") then
            write(2004,'(a)') "pageselect q"
            write(2004,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(2004,'(a,i0,a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", &
               1900+(per(1,n)/4),"q", mod(per(1,n),4)+1, ") _" 
            call MapSeries(trim(seriesName(n)), quarterlySeries)
            call WriteSeriesEV(quarterlySeries, per(1,n), per(2,n))
         end if
         write(2004,*)
      end do
      call CloseFile(2004)
   
   end subroutine WriteOutputFileEV

!===============================================================================
   
   subroutine WriteSeriesEV(series, i1, i2)
   
      real (kind = 8), dimension(:) :: series
      integer :: i1, i2
      integer :: i, r
      
      do i = i1, i2, 4
         if (i + 4 <= i2) then
            write(2004,'("  ",3(f29.13,","),f29.13,$)') series(i:i+3)
            write(2004,'(a)') ", _"
            !write(2004,'(a$)') "  "
         else
             r = i2 - i + 1
             if (r > 1) then 
                write(2004,'("  ",' // IntToAsc(r-1) // '(f29.13,","),$)') series(i:i+r-2)         
                write(2004,'(f29.13)') series(i+r-1)
             else
                write(2004,'("  ",f29.13)') series(i+r-1)
             end if
         end if
         
      end do
   
   end subroutine WriteSeriesEV

!===============================================================================
   
   subroutine WriteProgramEV()
   
      ! Import Series
      write(2005,'(a)') "wfopen " // trim(OUT_PATH) // "\mul\" // trim(afilename)
      write(2005,'(a)') "exec " // trim(OUT_PATH) // "\internal\ModSol2Out.prg"
      write(2005,'(a)') "wfsave(2) " // trim(OUT_PATH) // "\mul\" // trim(afilename)
      write(2005,'(a)') "wfclose"
      
      call CloseFile(2005)
   
   end subroutine WriteProgramEV

!===============================================================================
   
   subroutine RunProgramEV()
   
      integer :: rv3
   
      write(*,'(a$)') "Importing solution to file " // trim(afileName) // ".wf1"
      rv3 = ExecuteProgram(trim(OUT_PATH) // "\internal\ModSol2Import.prg")
      if (rv3 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
              
   end subroutine RunProgramEV

!===============================================================================   

end module EconModSol2OutMod