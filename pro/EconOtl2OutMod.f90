module EconOtl2OutMod

   use EconParMod
   use EconOtlVarMod
   use ifport
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconOtl2OutMain
   
   integer, parameter :: NUM_SERIES = 26663
   character (len = 18) , dimension(NUM_SERIES) :: seriesName
   integer, dimension(2,NUM_SERIES) :: per
   integer :: fileNum
   integer :: passNum = 0
   character (len = 128) :: username
   
contains

!===============================================================================

   subroutine EconOtl2OutMain()
   
      fileNum = 1
      passNum = passNum +1
      if (passNum == 7) then
         if (WORKFILE_EXPORT) then
            call GetSeriesNames()
            call WriteOutputFileEV()
            call Otl2ToStoch()
            call WriteProgramEV()
            call RunProgramEV()
         end if
      end if
             
   end subroutine EconOtl2OutMain

!===============================================================================

   subroutine GetSeriesNames()
   
      integer :: n
      
      do n = 1, NUM_SERIES
         read(0401, '(a18,2i5)') seriesName(n), per(1,n), per(2,n)
      end do
   
   end subroutine GetSeriesNames

!===============================================================================

   subroutine MapSeries(name, series)
   
      character (len = *) :: name
      real (kind = 8), dimension(:) :: series
      
      include "CompileSeriesMaps.fh"
      
      !DEC$ IF (COMPILE_SERIES_MAPS == 1)
      
      series = 0d0
      
      include "Otl2SeriesMap_A-M.fh"
      
      include "Otl2SeriesMap_N-Z.fh"
      
      !else
      !   series = 0d0
      !end if
      
      !DEC$ END IF
   
   end subroutine MapSeries

!===============================================================================

   subroutine Otl2ToStoch()
   
      integer :: iSex, iAge
      character(len=16) :: serName, blnkStr = "                "
      character(len=12) :: serData
      
      write (serData,'(a2,2i5)') " a", TRYEAR - 2, 1900 + endYr
      
      serName = "eo_esf_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_esf(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_nas_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_nas(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_aw_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_aw(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_naw_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_naw(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_no_1_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_no_1(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_no_2_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_no_2(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_no_m2_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_noi_m2(0,24,TRYEAR-1900-2:endYr) + eo_nol_m2(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_no_m_16o" ! CHECK THE VALUES OF THIS ONE! ****NOT USED BY STOCH?*****
      !write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
      !           eo_no(0,24,TRYEAR-1900-2:endYr) - eo_esf(0,24,TRYEAR-1900-2:endYr) - eo_und(0,24,TRYEAR-1900-2:endYr)
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_noi_m1(0,24,TRYEAR-1900-2:endYr) + eo_nol_m1(0,24,TRYEAR-1900-2:endYr) + &
                 eo_noi_m2(0,24,TRYEAR-1900-2:endYr) + eo_nol_m2(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_no_m16o" ! NOT USED BY STOCH
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_no(1,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_no_s2_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_noi_s2(0,24,TRYEAR-1900-2:endYr) + eo_nol_s2(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_no_s_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_noi_s2(0,24,TRYEAR-1900-2:endYr) + eo_nol_s2(0,24,TRYEAR-1900-2:endYr) + &
                 eo_noi_s1(0,24,TRYEAR-1900-2:endYr) + eo_nol_s1(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_no_u2_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_noi_u2(0,24,TRYEAR-1900-2:endYr) + eo_nol_u2(0,24,TRYEAR-1900-2:endYr)
      
      serName = "eo_und_16o"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eo_und(0,24,TRYEAR-1900-2:endYr)
      
      do iSex = 2, 1, -1
        do iAge = 16, 100 ! SYOA
          serName = "eo_as_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     !eo_as_sy(iSex,iAge,TRYEAR-1900-2:endYr) ! COMMENTED OUT IN ECON???
                     eo_as1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_as2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "eo_aw_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     !eo_aw_sy(iSex,iAge,TRYEAR-1900-2:endYr) ! COMMENTED OUT IN ECON???
                     eo_aw1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_aw2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "eo_nas_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     !eo_nas_sy(iSex,iAge,TRYEAR-1900-2:endYr) ! COMMENTED OUT IN ECON???
                     eo_nas1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_nas2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "eo_naw_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     !eo_naw_sy(iSex,iAge,TRYEAR-1900-2:endYr) ! COMMENTED OUT IN ECON???
                     eo_naw1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_naw2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "eo_no_m_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     eo_noi_m1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_noi_m2_sy(iSex,iAge,TRYEAR-1900-2:endYr) + &
                     eo_nol_m1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_nol_m2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "eo_no_s_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     eo_noi_s1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_noi_s2_sy(iSex,iAge,TRYEAR-1900-2:endYr) + &
                     eo_nol_s1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_nol_s2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
          serName = "eo_no_u_" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     eo_noi_u1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_noi_u2_sy(iSex,iAge,TRYEAR-1900-2:endYr) + &
                     eo_nol_u1_sy(iSex,iAge,TRYEAR-1900-2:endYr) + eo_nol_u2_sy(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
      end do
      
      serName = "ws_eo"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 ws_eo(TRYEAR-1900-2:endYr)
      
      serName = "ws_eo_esf"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 ws_eo_esf(TRYEAR-1900-2:endYr)
      
      serName = "ws_eo_mefc"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 ws_eo_mefc(TRYEAR-1900-2:endYr)
      
   end subroutine Otl2ToStoch

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
            write(4004,'(a)') "pageselect a"
            write(4004,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(4004,'(a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", 1900+per(1,n),  ") _" 
            call MapSeries(trim(seriesName(n)), annualSeries)
            call WriteSeriesEV(annualSeries, per(1,n), per(2,n))
         else if (freq == "Q") then
            write(4004,'(a)') "pageselect q"
            write(4004,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(4004,'(a,i0,a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", &
               1900+(per(1,n)/4),"q", mod(per(1,n),4)+1, ") _" 
            call MapSeries(trim(seriesName(n)), quarterlySeries)
            call WriteSeriesEV(quarterlySeries, per(1,n), per(2,n))
         end if
         write(4004,*)
      end do
      call CloseFile(4004)
   
   end subroutine WriteOutputFileEV

!===============================================================================
   
   subroutine WriteSeriesEV(series, i1, i2)
   
      real (kind = 8), dimension(:) :: series
      integer :: i1, i2
      integer :: i, r
      
      do i = i1, i2, 4
         if (i + 4 <= i2) then
            write(4004,'("  ",3(f29.13,","),f29.13,$)') series(i:i+3)
            write(4004,'(a)') ", _"
            !write(4004,'(a$)') "  "
         else
             r = i2 - i + 1
             if (r > 1) then 
                write(4004,'("  ",' // IntToAsc(r-1) // '(f29.13,","),$)') series(i:i+r-2)         
                write(4004,'(f29.13)') series(i+r-1)
             else
                write(4004,'("  ",f29.13)') series(i+r-1)
             end if
         end if
         
      end do
   
   end subroutine WriteSeriesEV

!===============================================================================
   
   subroutine WriteProgramEV()
   
      ! Import Series
      write(4005,'(a)') "wfopen " // trim(OUT_PATH) // "\mul\" // trim(OTL_FILE)
      write(4005,'(a)') "exec " // trim(OUT_PATH) // "\internal\Otl2Out.prg"
      write(4005,'(a)') "wfsave(2) " // trim(OUT_PATH) // "\mul\" // trim(OTL_FILE)
      write(4005,'(a)') "wfclose"
      
      call CloseFile(4005)
   
   end subroutine WriteProgramEV

!===============================================================================
   
   subroutine RunProgramEV()
   
      integer :: rv3
   
      write(*,'(a$)') "Importing solution to file " // trim(OTL_FILE) // ".wf1"
      rv3 = ExecuteProgram(trim(OUT_PATH) // "\internal\Otl2Import.prg")
      if (rv3 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
              
   end subroutine RunProgramEV

!===============================================================================      

end module EconOtl2OutMod