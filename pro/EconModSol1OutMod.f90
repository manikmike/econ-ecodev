module EconModSol1OutMod

   use EconParMod
   use EconModSol1VarMod
   use ifport
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSol1OutMain
   
   integer, parameter :: NUM_SERIES = 1794
   character (len = 18) , dimension(NUM_SERIES) :: seriesName
   integer, dimension(2,NUM_SERIES) :: per
   
contains

!===============================================================================

   subroutine EconModSol1OutMain()
   
      call unemp()
      call nomgdp()
      call epop62()
      call p_lcadj()
      if (WORKFILE_EXPORT) then
         call GetSeriesNames()
         call WriteOutputFileEV()
         call ModSol1ToStoch()
         call WriteProgramEV()
         call RunProgramEV()
      end if
             
   end subroutine EconModSol1OutMain

!===============================================================================

   subroutine GetSeriesNames()
   
      integer :: n
      
      do n = 1, NUM_SERIES
         read(0101, '(a18,2i5)') seriesName(n), per(1,n), per(2,n)
      end do
   
   end subroutine GetSeriesNames

!===============================================================================

   subroutine MapSeries(name, series)
   
      character (len = *) :: name
      real (kind = 8), dimension(:) :: series
      
      include "CompileSeriesMaps.fh"
      
      !DEC$ IF (COMPILE_SERIES_MAPS == 1)
      
      include "ModSol1SeriesMap.fh"

      else
         series = 0d0
      end if
      
      !DEC$ END IF
   
   end subroutine MapSeries

!===============================================================================

   subroutine unemp()
   
      integer :: yr, sex, group
      
      do yr = 90, min(OUTPUT_LAST_YR, endYr)
         do sex = 1, 2
            if (yr >= OUTPUT_TRYR - 1) then
               write(1103,'(i4,10f11.6)') 1900+yr,ru_a(sex,27,yr),(ru_a(sex,group,yr),group=3,11)
            else
               if (needHist) write(1104,'(i4,10f11.6)') 1900+yr,ru_a(sex,27,yr),(ru_a(sex,group,yr),group=3,11)
            end if
         end do   
      end do
   
   end subroutine unemp

!===============================================================================
   
   subroutine epop62()
   
      integer :: yr, sex
      real (kind = 8), dimension(MAX_YR) :: temp1, temp2
      
      call FetchSeries(BKDR1, "PM62.A", temp1)
      psy_a(1,62,sample(1):103) = temp1(sample(1):103)

      call FetchSeries(BKDR1, "PF62.A", temp2)
      psy_a(2,62,sample(1):103) = temp2(sample(1):103)

      do yr = 77, min(OUTPUT_LAST_YR, endYr)
         if (yr > OUTPUT_TRYR - 1) then
            write(1107,'(i4,2f15.6)') 1900+yr, (psy_a(sex,62,yr) * (1d0 - ru_a(sex,11,yr) / 100d0),sex=1,2)
         else
            if (needHist) write(1108,'(i4,2f15.6)') 1900+yr, (psy_a(sex,62,yr) * (1d0 - ru_a(sex,11,yr) / 100d0),sex=1,2)
         end if
      end do
   
   end subroutine epop62
   
!===============================================================================   

   subroutine nomgdp()
   
      integer :: yr

      do yr = 46, min(OUTPUT_LAST_YR, endYr)
         if (yr >= OUTPUT_TRYR - 1) then
            write(1105,'(i4,i15)') 1900+yr, nint(1d3*gdp_a(yr))
         else
            if (needHist) write(1106,'(i4,i15)') 1900+yr, nint(1d3*gdp_a(yr))
         end if
      end do
   
   end subroutine nomgdp

!===============================================================================

   subroutine p_lcadj()

      real (kind = 8), dimension(MAX_QTR) :: adj
      integer :: sex, ageGrp, age, marStat, yr, i
      integer, dimension(13) :: lo  = (/ 55, 60, 65, 70, 75, 80, 85, 90, 95,  65,  70,  75,  85 /)
      integer, dimension(13) :: hi  = (/ 59, 64, 69, 74, 79, 84, 89, 94, 99, 100, 100, 100, 100 /)
      integer, dimension(13) :: grp = (/ 10, 11, 12, 13, 14, 15, 32, 33, 34,  22,  23,  17,  16 /) 

      sample = (/ startQtr, endQtr /)

      do sex = 1, 2
         do ageGrp = 1, 9
            adj = lcadj(sex,ageGrp,:) / n(sex,ageGrp,:)
            call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//"_LCADJ.Q", adj)
            if (ageGrp >= 3 .and. ageGrp <=9) then
               do marStat = 1, 3
                  adj = pms(sex,ageGrp,marStat,:) * (lcadj(sex,ageGrp,:) / n(sex,ageGrp,:)) / p(sex,ageGrp,:)
                  call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))// &
                     trim(marStatLabel(marStat))//"_LCADJ.Q", adj)
               end do
               if (sex == 2 .and. ageGrp <=7) then
                  do marStat = 1, 3
                     adj = pmsc6u(sex,ageGrp,marStat,:) * (lcadj(sex,ageGrp,:) / n(sex,ageGrp,:)) / p(sex,ageGrp,:)
                     call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))// &
                        trim(marStatLabel(marStat))//"C6U_LCADJ.Q", adj)
                     adj = pmsnc6(sex,ageGrp,marStat,:) * (lcadj(sex,ageGrp,:) / n(sex,ageGrp,:)) / p(sex,ageGrp,:)
                     call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))// &
                        trim(marStatLabel(marStat))//"NC6_LCADJ.Q", adj)
                  end do
               end if
            end if
         end do     
      end do

      do sex = 1, 2
         do age = 55, 79
            adj = lcadjsy(sex,age,:) / nisy(sex,age,:)
            call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//"_LCADJ.Q", adj)
         end do     
      end do

      do sex = 1, 2
            ageGrp = 31
            adj = lcadj(sex,ageGrp,:) / n(sex,ageGrp,:)
            call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//"_LCADJ.Q", adj)
      end do

      do sex = 1, 2
         do age = 80, 100
            adj = psy(sex,age,:) * (lcadj(sex,31,:) / n(sex,31,:)) / p(sex,31,:) 
            call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//"_LCADJ.Q", adj)
         end do     
      end do

      do sex = 1, 2
         do yr = startQtr, endQtr
            lcadj(sex,24,yr) = sum(lcadj(sex,1:9,yr)) + &
               lcadj(sex,31,yr) + sum(lcadjsy(sex,55:79,yr))
            adj(yr) = lcadj(sex,24,yr) / n(sex,24,yr)
         end do
         call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//"16O_LCADJ.Q", adj)
      end do

      adj = (lcadj(1,24,:) + lcadj(2,24,:)) / (n(1,24,:) + n(2,24,:))
      call StoreSeries(LFPR_PROJ, "P16O_LCADJ.Q", adj)
         
      do sex = 1, 2
         do i = 1, 13
            do yr = startQtr, endQtr
               if (hi(i) < 80) then
                  lcadj(sex,grp(i),yr) = sum(lcadjsy(sex,lo(i):hi(i),yr))
                  adj(yr) = lcadj(sex,grp(i),yr) / n(sex,grp(i),yr)
               else if (lo(i) < 80 .and. hi(i) == 100) then
                  lcadj(sex,grp(i),yr) = sum(lcadjsy(sex,lo(i):79,yr)) + lcadj(sex,31,yr)
                  adj(yr) = lcadj(sex,grp(i),yr) / n(sex,grp(i),yr)
               else ! hi(i) >= 80  .and. (lo(i) >= 80 .or. hi(i) /= 100)
                  adj(yr) = p(sex,grp(i),yr) * (lcadj(sex,31,yr) / n(sex,31,yr)) / p(sex,31,yr)
               end if
            end do
            call StoreSeries(LFPR_PROJ, "P"//trim(sexLabel(sex))//trim(ageGrpLabel(grp(i)))//"_LCADJ.Q", adj)
         end do
      end do

      call CloseWorkfile(LFPR_PROJ, .true.)

   end subroutine p_lcadj

!===============================================================================

   subroutine ModSol1ToStoch()
   
      integer :: iSex, iAge
      character(len=16) :: serName, blnkStr = "                "
      character(len=12) :: serData
      real (kind=8), dimension(MAX_YR) :: adj, leadjwgt = 4d-1
      real (kind=8), dimension(3:7, MAX_YR) :: nc3t5f, ncu3f
      
      call FetchSeries(OFILE1, "NC3T5F2024.A", nc3t5f(3,:))
      call FetchSeries(OFILE1, "NC3T5F2529.A", nc3t5f(4,:))
      call FetchSeries(OFILE1, "NC3T5F3034.A", nc3t5f(5,:))
      call FetchSeries(OFILE1, "NC3T5F3539.A", nc3t5f(6,:))
      call FetchSeries(OFILE1, "NC3T5F4044.A", nc3t5f(7,:))
      call FetchSeries(OFILE1, "NCU3F2024.A", ncu3f(3,:))
      call FetchSeries(OFILE1, "NCU3F2529.A", ncu3f(4,:))
      call FetchSeries(OFILE1, "NCU3F3034.A", ncu3f(5,:))
      call FetchSeries(OFILE1, "NCU3F3539.A", ncu3f(6,:))
      call FetchSeries(OFILE1, "NCU3F4044.A", ncu3f(7,:))

      write (serData,'(a2,2i5)') " a", TRYEAR - 2, 1900 + endYr
      
      serName = "e"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 e_a(0,0,TRYEAR-1900-2:endYr)
      
      serName = "edmil"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 edmil_a(TRYEAR-1900-2:endYr)
      
      do iSex = 2, 1, -1
        do iAge = 1, 13
          serName = "e" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ageGrpLabel(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     e_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
        iAge = 17 ! 75o
        serName = "e" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   e_a(iSex,iAge,TRYEAR-1900-2:endYr)
        iAge = 23 ! 70o
        serName = "e" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   e_a(iSex,iAge,TRYEAR-1900-2:endYr)
      end do
      
      do iAge = 3, 7 ! age groups 2024 to 4044, for the number of children series
        serName = "nc3t5f" // trim(ageGrpLabel(iAge))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   nc3t5f(iAge,TRYEAR-1900-2:endYr)
        serName = "ncu3f" // trim(ageGrpLabel(iAge))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   ncu3f(iAge,TRYEAR-1900-2:endYr)
      end do
      
      serName = "lc"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 lc_a(0,0,TRYEAR-1900-2:endYr)

      do iSex = 2, 1, -1
        do iAge = 1, 13
          serName = "l" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ageGrpLabel(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     lc_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
        iAge = 17 ! 75o
        serName = "l" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   lc_a(iSex,iAge,TRYEAR-1900-2:endYr)
        iAge = 23 ! 70o
        serName = "l" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   lc_a(iSex,iAge,TRYEAR-1900-2:endYr)
      end do
      
      do iSex = 2, 1, -1
        do iAge = 1, 17 ! 5-yr age grps through 80-84, and 85o and 75o
          serName = "p" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     p_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
        iAge = 23 ! 70o
        serName = "p" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   p_a(iSex,iAge,TRYEAR-1900-2:endYr)
      end do
      do iSex = 2, 1, -1
        do iAge = 55, 100 ! SYOA
          serName = "p" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     psy_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
      end do
      
      !LE Adjustment
      do iSex = 2, 1, -1
        do iAge = 3, 9 ! 5-yr age grps from 20-24 through 50-54
          serName = "p" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge))) // "adj"
          !call FetchSeries(DFILE, "P" // trim(sexLabel(iSex)) // trim(ageGrpLabel(iAge)) // "ADJ.Q", adj)
          call FetchSeries(DFILE, "P" // trim(sexLabel(iSex)) // trim(ageGrpLabel(iAge)) // "ADJ.A", adj)
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     4d-1 * adj(TRYEAR-1900-2:endYr)
        end do
        do iAge = 14, 16 ! 5-yr age grps 75-79, 80-84, and 85o
          serName = "p" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge))) // "adj"
          !call FetchSeries(DFILE, "P" // trim(sexLabel(iSex)) // trim(ageGrpLabel(iAge)) // "ADJ.Q", adj)
          !call FetchSeries(DFILE, "P" // trim(sexLabel(iSex)) // trim(ageGrpLabel(iAge)) // "ADJ.A", adj)
          adj = 0d0 ! Life Expectancy adjustment factors for these age groups aren't produced
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     4d-1 * adj(TRYEAR-1900-2:endYr)
        end do
      end do
      do iSex = 2, 1, -1
        do iAge = 55, 100 ! SYOA
          serName = "p" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge)) // "adj"
          !call FetchSeries(DFILE, "P" // trim(sexLabel(iSex)) // trim(IntToAsc(iAge)) // "ADJ.Q", adj)   
          call FetchSeries(DFILE, "P" // trim(sexLabel(iSex)) // trim(IntToAsc(iAge)) // "ADJ.A", adj)   
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     4d-1 * adj(TRYEAR-1900-2:endYr)
        end do
      end do
      
      do iSex = 2, 1, -1
        do iAge = 1, 15 ! RU has different numbering of age groups
          serName = "r" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel2(iAge)))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     ru_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
      end do
      
      serName = "ru"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 ru_a(0,0,TRYEAR-1900-2:endYr)

      serName = "gdp"
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 gdp_a(TRYEAR-1900-2:endYr)

      serName = "prod"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 prod_a(TRYEAR-1900-2:endYr)

      serName = "leadjwgt"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 leadjwgt(TRYEAR-1900-2:endYr)

   end subroutine ModSol1ToStoch

!===============================================================================
   
   subroutine EconModSol1OnlyOutMain()
   
      call GetSeriesNames()
      call WriteOutputFileEV()
             
   end subroutine EconModSol1OnlyOutMain

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
            write(1004,'(a)') "pageselect a"
            write(1004,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(1004,'(a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", 1900+per(1,n),  ") _" 
            call MapSeries(trim(seriesName(n)), annualSeries)
            if (seriesName(n)(1:2) == "PF" .or. seriesName(n)(1:2) == "PM" .or. seriesName(n)(1:3) == "P16") then 
               call CheckForNegativeLFPR(annualSeries, per(1,n), per(2,n), n)
            end if
            call WriteSeriesEV(annualSeries, per(1,n), per(2,n))
         else if (freq == "Q") then
            write(1004,'(a)') "pageselect q"
            write(1004,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(1004,'(a,i0,a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", &
               1900+(per(1,n)/4),"q", mod(per(1,n),4)+1, ") _" 
            call MapSeries(trim(seriesName(n)), quarterlySeries)
            if (seriesName(n)(1:2) == "PF" .or. seriesName(n)(1:2) == "PM" .or. seriesName(n)(1:3) == "P16") then 
               call CheckForNegativeLFPR(quarterlySeries, per(1,n), per(2,n), n)
            end if
            call WriteSeriesEV(quarterlySeries, per(1,n), per(2,n))
         end if
         write(1004,*)
      end do
      call CloseFile(1004)
   
   end subroutine WriteOutputFileEV

!===============================================================================

   subroutine CheckForNegativeLFPR(series, i1, i2, n)

      real (kind = 8), dimension(:) :: series      
      integer :: i1, i2, n
      integer :: yr

      do yr = i1, i2
         if (series(yr) < 0) then
            write(*,'(a)') " Error: Series "//trim(seriesName(n))//" contains a negative value"
            ! call SetWorkfileVerboseMode(.false.)
            ! call CloseAllFiles()
            ! stop
         end if
      end do

   end subroutine CheckForNegativeLFPR

!===============================================================================
   
   subroutine WriteSeriesEV(series, i1, i2)
   
      real (kind = 8), dimension(:) :: series
      integer :: i1, i2
      integer :: i, r
      
      do i = i1, i2, 4
         if (i + 4 <= i2) then
            write(1004,'("  ",3(f29.13,","),f29.13,$)') series(i:i+3)
            write(1004,'(a)') ", _"
            !write(1004,'(a$)') "  "
         else
             r = i2 - i + 1
             if (r > 1) then 
                write(1004,'("  ",' // IntToAsc(r-1) // '(f29.13,","),$)') series(i:i+r-2)         
                write(1004,'(f29.13)') series(i+r-1)
             else
                write(1004,'("  ",f29.13)') series(i+r-1)
             end if
         end if
         
      end do
   
   end subroutine WriteSeriesEV

!===============================================================================
   
   subroutine WriteProgramEV()
   
      ! Import Series
      write(1005,'(a)') "wfopen " // trim(OUT_PATH) // "\mul\" // trim(afilename)
      write(1005,'(a)') "exec " // trim(OUT_PATH) // "\internal\ModSol1Out.prg"
      write(1005,'(a)') "wfsave(2) " // trim(OUT_PATH) // "\mul\" // trim(afilename)
      write(1005,'(a)') "wfclose"
      
      call CloseFile(1005)
   
   end subroutine WriteProgramEV

!===============================================================================
   
   subroutine RunProgramEV()
   
      integer :: rv3
   
      write(*,'(a$)') "Importing solution to file " // trim(afileName) // ".wf1"
      rv3 = ExecuteProgram(trim(OUT_PATH) // "\internal\ModSol1Import.prg")
      if (rv3 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
              
   end subroutine RunProgramEV

!===============================================================================   
   
end module EconModSol1OutMod