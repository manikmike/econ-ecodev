module EconOtl3OutMod

   use EconParMod
   use EconOtlVarMod
   use ifport
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconOtl3OutMain
   
   integer, parameter :: NUM_SERIES = 11277
   character (len = 18) , dimension(NUM_SERIES) :: seriesName
   integer, dimension(2,NUM_SERIES) :: per
   integer :: fileNum
   integer :: passNum = 0
   character (len = 128) :: username
   
contains

!===============================================================================

   subroutine EconOtl3OutMain()
   
      fileNum = 1
      passNum = passNum + 1
      if (passNum == 14) then
         call cwother()
         call Otl3ToStoch()
         if (WORKFILE_EXPORT) then
            call GetSeriesNames()
            call WriteOutputFileEV()
            call WriteProgramEV()
            call RunProgramEV()
         end if
      end if

   end subroutine EconOtl3OutMain

!===============================================================================

   subroutine GetSeriesNames()
   
      integer :: n
      
      do n = 1, NUM_SERIES
         read(0701, '(a18,2i5)') seriesName(n), per(1,n), per(2,n)
      end do
   
   end subroutine GetSeriesNames

!===============================================================================

   subroutine MapSeries(name, series)
   
      character (len = *) :: name
      real (kind = 8), dimension(:) :: series
      
      include "CompileSeriesMaps.fh"
      
      !DEC$ IF (COMPILE_SERIES_MAPS == 1)
      
      series = 0d0
      
      include "Otl3SeriesMap.fh"
   
      !else
      !   series = 0d0
      !end if
      
      !DEC$ END IF
   
   end subroutine MapSeries

!===============================================================================

   subroutine cwother()

      integer :: yr, age, sex, a
      real (kind = 8), dimension(MAX_YR) :: teo_temp

      do yr = 64, min(OUTPUT_LAST_YR, endYr)
         do age = 16, 100
            if (yr >= OUTPUT_TRYR - 2) then
               write(7101,'(i4,1x,i3,2i10)') 1900+yr, age, &
                  nint(1d6 * teo_mefc_sy(1:2, age, yr))
            else
               if (needHist) then
                     if (yr == 64) then ! only read in series once
                        do sex = 1, 2
                           call FetchSeries(OTLFILE, "TEO_MEFC_"//trim(sexLabel(sex))//&
                              trim(IntToAsc(age))//".A", teo_temp)
                           teo_mefc_sy(sex,age,64:80) = teo_temp(64:80)
                        end do
                     end if
                     write(7102,'(i4,1x,i3,2i10)') 1900+yr, age, &
                        nint(1d6 * teo_mefc_sy(1:2, age, yr))
               end if
            end if
         end do
      end do

   end subroutine cwother

!===============================================================================

   subroutine Otl3ToStoch()
   
      integer :: iSex, iAge
      character(len=16) :: serName, blnkStr = "                "
      character(len=12) :: serData
      
      write (serData,'(a2,2i5)') " a", TRYEAR - 2, 1900 + endYr
      
      serName = "teo_no_s_16o" ! NO LONGER USED BY STOCH
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 teo_noi_s(0,24,TRYEAR-1900-2:endYr) + teo_nol_s(0,24,TRYEAR-1900-2:endYr)
      
      serName = "teo_esf_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 teo_esf(0,24,TRYEAR-1900-2:endYr)
      
      serName = "teo_und_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 teo_und(0,24,TRYEAR-1900-2:endYr)
      
      do iSex = 2, 1, -1
        do iAge = 16, 100 ! SYOA
          serName = "teo_asf2_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_asf2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_asj2_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_asj2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_awjf_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_awjf_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_awt_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_awt_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_mef_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_mef_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_mefc_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_mefc_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_noi_s_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_noi_s_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_nol_s_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_nol_s_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "teo_und_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     teo_und_sy(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
      end do
      
   end subroutine Otl3ToStoch

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
            write(7004,'(a)') "pageselect a"
            write(7004,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(7004,'(a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", 1900+per(1,n),  ") _" 
            call MapSeries(trim(seriesName(n)), annualSeries)
            call WriteSeriesEV(annualSeries, per(1,n), per(2,n))
         else if (freq == "Q") then
            write(7004,'(a)') "pageselect q"
            write(7004,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(7004,'(a,i0,a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", &
               1900+(per(1,n)/4),"q", mod(per(1,n),4)+1, ") _" 
            call MapSeries(trim(seriesName(n)), quarterlySeries)
            call WriteSeriesEV(quarterlySeries, per(1,n), per(2,n))
         end if
         write(7004,*)
      end do
      call CloseFile(7004)
   
   end subroutine WriteOutputFileEV

!===============================================================================
   
   subroutine WriteSeriesEV(series, i1, i2)
   
      real (kind = 8), dimension(:) :: series
      integer :: i1, i2
      integer :: i, r
      
      do i = i1, i2, 4
         if (i + 4 <= i2) then
            write(7004,'("  ",3(f29.13,","),f29.13,$)') series(i:i+3)
            write(7004,'(a)') ", _"
            !write(7004,'(a$)') "  "
         else
             r = i2 - i + 1
             if (r > 1) then 
                write(7004,'("  ",' // IntToAsc(r-1) // '(f29.13,","),$)') series(i:i+r-2)         
                write(7004,'(f29.13)') series(i+r-1)
             else
                write(7004,'("  ",f29.13)') series(i+r-1)
             end if
         end if
         
      end do
   
   end subroutine WriteSeriesEV

!===============================================================================
   
   subroutine WriteProgramEV()
   
      ! Import Series
      write(7005,'(a)') "wfopen " // trim(OUT_PATH) // "\mul\" // trim(OTL_FILE)
      write(7005,'(a)') "exec " // trim(OUT_PATH) // "\internal\Otl3Out.prg"
      write(7005,'(a)') "wfsave(2) " // trim(OUT_PATH) // "\mul\" // trim(OTL_FILE)
      write(7005,'(a)') "wfclose"
      
      call CloseFile(7005)
   
   end subroutine WriteProgramEV

!===============================================================================
   
   subroutine RunProgramEV()
   
      integer :: rv3
   
      write(*,'(a$)') "Importing solution to file " // trim(OTL_FILE) // ".wf1"
      rv3 = ExecuteProgram(trim(OUT_PATH) // "\internal\Otl3Import.prg")
      if (rv3 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
              
   end subroutine RunProgramEV

!===============================================================================        

end module EconOtl3OutMod