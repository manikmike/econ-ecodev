module EconOtl1Mod

   use EconParMod
   use EconLibMod
   use EconModSolAVarMod ! uses EconModSol2VarMod and EconOtl1VarMod
   use EconOtl1OutMod
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconOtl1Main
   
contains

!===============================================================================

   ! Create historical values for the ratio of E / N by 
   ! single-year-of-age and gender and stores the values in an OTL file

   subroutine EconOtl1Main()

      integer :: age, sex, yr, yr2
      real (kind = 8), dimension(MAX_YR) :: ltemp, etemp, ntemp, ptemp

      write(*,'(//a//)') "Solving Otl1, Please Wait..."

      call FetchSeries(BKDO1, "LC.A", ltemp)
      yr2 = sample(2)

      ! Age 16 to 79
      !   1964 to 1993

      re_sy(1:2,16,64:93) = p_a(1:2,1,64:93) * (1d0 - ru_a(1:2,1,64:93) / 100d0)
      re_sy(1:2,17,64:93) = p_a(1:2,1,64:93) * (1d0 - ru_a(1:2,1,64:93) / 100d0)

      re_sy(1:2,18,64:93) = p_a(1:2,2,64:93) * (1d0 - ru_a(1:2,2,64:93) / 100d0)
      re_sy(1:2,19,64:93) = p_a(1:2,2,64:93) * (1d0 - ru_a(1:2,2,64:93) / 100d0)

      do age = 20, 24
         do sex = 1, 2
           re_sy(sex,age,64:93) = p_a(sex,3,64:93) * (1d0 - ru_a(sex,3,64:93) / 100d0)
         end do
      end do

      do age = 25, 29
         do sex = 1, 2
            re_sy(sex,age,64:76) = p_a(sex,4,64:76) * (1d0 - ru_a(sex,18,64:76) / 100d0)
            re_sy(sex,age,77:93) = p_a(sex,4,77:93) * (1d0 - ru_a(sex,4,77:93) / 100d0)
         end do
      end do

      do age = 30, 34
         do sex = 1, 2
            re_sy(sex,age,64:76) = p_a(sex,5,64:76) * (1d0 - ru_a(sex,18,64:76) / 100d0)
            re_sy(sex,age,77:93) = p_a(sex,5,77:93) * (1d0 - ru_a(sex,5,77:93) / 100d0)
         end do
      end do

      do age = 35, 39
         do sex = 1, 2
            re_sy(sex,age,64:76) = p_a(sex,6,64:76) * (1d0 - ru_a(sex,19,64:76) / 100d0)
            re_sy(sex,age,77:93) = p_a(sex,6,77:93) * (1d0 - ru_a(sex,6,77:93) / 100d0)
         end do
      end do

      do age = 40, 44
         do sex = 1, 2
            re_sy(sex,age,64:76) = p_a(sex,7,64:76) * (1d0 - ru_a(sex,19,64:76) / 100d0)
            re_sy(sex,age,77:93) = p_a(sex,7,77:93) * (1d0 - ru_a(sex,7,77:93) / 100d0)
         end do
      end do

      do age = 45, 49
         do sex = 1, 2
            re_sy(sex,age,64:76) = p_a(sex,8,64:76) * (1d0 - ru_a(sex,20,64:76) / 100d0)
            re_sy(sex,age,77:93) = p_a(sex,8,77:93) * (1d0 - ru_a(sex,8,77:93) / 100d0)
         end do
      end do

      do age = 50, 54
         do sex = 1, 2
            re_sy(sex,age,64:76) = p_a(sex,9,64:76) * (1d0 - ru_a(sex,20,64:76) / 100d0)
            re_sy(sex,age,77:93) = p_a(sex,9,77:93) * (1d0 - ru_a(sex,9,77:93) / 100d0)
         end do
      end do

      do age = 55, 59
         do sex = 1, 2
            ptemp = 0
            call FetchSeries(BKDR1, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", ptemp)
            psy_a(sex,age,64:103) = ptemp(64:103)
            re_sy(sex,age,77:93) = psy_a(sex,age,77:93) * (1d0 - ru_a(sex,10,77:93) / 100d0)
            re_sy(sex,age,65:76) = psy_a(sex,age,65:76) * (1d0 - ru_a(sex,21,65:76) / 100d0)
            re_sy(sex,age,64:65) = re_sy(sex,age,65)
         end do
      end do

      do age = 60, 64
         do sex = 1, 2
            ptemp = 0
            call FetchSeries(BKDR1, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", ptemp)
            psy_a(sex,age,64:103) = ptemp(64:103)
            re_sy(sex,age,77:93) = psy_a(sex,age,77:93) * (1d0 - ru_a(sex,11,77:93) / 100d0)
            re_sy(sex,age,65:76) = psy_a(sex,age,65:76) * (1d0 - ru_a(sex,21,65:76) / 100d0)
            re_sy(sex,age,64:65) = re_sy(sex,age,65)
         end do
      end do

      do age = 65, 69
         do sex = 1, 2
            ptemp = 0
            call FetchSeries(BKDR1, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", ptemp)
            psy_a(sex,age,64:103) = ptemp(64:103)
            re_sy(sex,age,77:93) = psy_a(sex,age,77:93) * (1d0 - ru_a(sex,12,77:93) / 100d0)
            re_sy(sex,age,65:76) = psy_a(sex,age,65:76) * (1d0 - ru_a(sex,22,65:76) / 100d0)
            re_sy(sex,age,64:65) = re_sy(sex,age,65)
         end do
      end do

      do age = 70, 74
         do sex = 1, 2
            ptemp = 0
            call FetchSeries(BKDR1, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", ptemp)
            psy_a(sex,age,64:103) = ptemp(64:103)
            re_sy(sex,age,81:93) = psy_a(sex,age,81:93) * (1d0 - ru_a(sex,13,81:93) / 100d0)
            re_sy(sex,age,65:80) = psy_a(sex,age,65:80) * (1d0 - ru_a(sex,22,65:80) / 100d0)
            re_sy(sex,age,64:65) = re_sy(sex,age,65)
         end do
      end do

      do sex = 1, 2
         ptemp = 0
         call FetchSeries(BKDR1, "P"//trim(sexLabel(sex))//"75O.A", ptemp)
         p_a(sex,17,65:88) = ptemp(65:88)
         do age = 75, 79
            ptemp = 0
            call FetchSeries(BKDR1, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", ptemp)
            psy_a(sex,age,64:103) = ptemp(64:103)
            re_sy(sex,age,89:93) = psy_a(sex,age,89:93) * (1d0 - ru_a(sex,22,89:93) / 100d0)
            re_sy(sex,age,65:88) = p_a(sex,17,65:88) * (1d0 - ru_a(sex,22,65:88) / 100d0)
            re_sy(sex,age,63:65) = re_sy(sex,age,65)
         end do
      end do

      ! Age 16 to 79
      !   1994 to end

      ! This call will fail in the loop for some reason, so add a preemptive call here
      ! There may be an issue with thread synchronization with EViews wfselect command
      call FetchSeries(CPSO_NILF, "EM16.A", etemp)
      do sex = 1, 2
         do age = 16, 79
            call FetchSeries(OTLFILE, "RE"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", re_sy(sex,age,:))
            call FetchSeries(CPSO_NILF, "E"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", etemp)
            call FetchSeries(CPSO_NILF, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", ntemp)
            re_sy(sex,age,94:yr2) = etemp(94:yr2) / ntemp(94:yr2)
         end do
      end do

      ! Age 80 to 100
      !   1964 to end
      !   Values determined on cohort basis

      do sex = 1, 2
         do age = 80, 100
            call FetchSeries(OTLFILE, "RE"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", re_sy(sex,age,:))
            do yr = 64, yr2
               re_sy(sex,age,yr) = 0.965d0 * re_sy(sex,age-1,yr-1)
            end do
         end do
      end do

      !   1964 to 1983
      !     - Missing Values arbitrarily assigned

      age = 80
      do yr = 64, 83
         age = age + 1
         yr2 = yr + 1
         do sex = 1, 2
            re_sy(sex,age,64:yr) = re_sy(sex,age,yr2)
         end do
      end do

      ! call EconOtl1OutMain() ! moved to EconOtl2Mod for projected values

      write(*,'(/a/)') "Otl1 procedure finished"

   end subroutine EconOtl1Main

!===============================================================================

end module EconOtl1Mod