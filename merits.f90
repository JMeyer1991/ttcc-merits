! calculations for promotion pathways using buildings and facilities in
! Toontown: Corporate Clash

module merits

   implicit none

   private

   public bb_bldg, dpt_bldg, facil, get_mname

contains

   subroutine calc_merits(needed, yield, options, dpt)
      character(1), intent(in) :: dpt
      character(50) :: mname
      character(50), intent(in) :: options(:)
      integer :: i, remain, reps
      integer, intent(in) :: needed, yield(:)

      call get_mname(dpt, mname)

      remain = needed

      do while (remain > 0)
         do i = 1, size(yield)
            if(yield(i) >= remain .or. i == size(yield)) then
               reps = remain / yield(i) + 1
               print "(A, A, A, I2, A, I5, A, A, A)", "* ", trim(options(i)), &
                  " (x", reps, ") (~", int(yield(i)), " ", trim(mname), &
                  " each)"
               remain = remain - yield(i) * reps
               exit
            end if
         end do
      end do
   end subroutine calc_merits

   subroutine get_mname(dpt, mname)
      character(1), intent(in) :: dpt
      character(50), intent(out) :: mname

      select case (dpt)
         case ("S", "s")
            mname = "Invoices"
         case ("C", "c")
            mname = "Cogbucks"
         case ("L", "l")
            mname = "Patents"
         case ("B", "b")
            mname = "Stock Options"
         case default
            mname = "merits"
         end select
   end subroutine get_mname

   ! calculates promotion pathway using Boardbot buildings
   subroutine bb_bldg(needed, boost, dpt)
      character(1), intent(in) :: dpt
      character(50), dimension(6), parameter :: options = &
         ["1-story", "2-story", "3-story", "4-story", "5-story", "6-story"]
      integer, intent(in) :: needed
      integer, dimension(6) :: boosted_yield
      integer, dimension(6), parameter :: yield = [5, 26, 100, 220, 440, 1400]
      real, intent(in) :: boost

      print "(A)", "Boardbot Buildings"
      print "(A)", "------------------"

      boosted_yield = nint(yield * boost)
      call calc_merits(needed, boosted_yield, options, dpt)
   end subroutine bb_bldg

   subroutine dpt_bldg(needed, boost, dpt)
      character(1), intent(in) :: dpt
      character(50), dimension(6), parameter :: options = &
         ["1-story", "2-story", "3-story", "4-story", "5-story", "6-story"]
      integer, intent(in) :: needed
      integer, dimension(6) :: boosted_yield
      integer, dimension(6), parameter :: yield = &
         [15, 65, 250, 550, 1100, 3100]
      real, intent(in) :: boost
      
      print "(A)", "Departmental Buildings"
      print "(A)", "----------------------"

      boosted_yield = nint(yield * boost)

      call calc_merits(needed, boosted_yield, options, dpt)
   end subroutine dpt_bldg

   subroutine facil(needed, boost, dpt)
      character(1), intent(in) :: dpt
      character(50), allocatable :: options(:)
      integer, allocatable :: boosted_yield(:)
      integer, allocatable :: yield(:)
      integer, intent(in) :: needed
      real, intent(in) :: boost

      if (dpt == "b" .or. dpt == "B") then
         allocate(options(9))
         options = &
            ["Minimal Silver Sprocket                           ", &
             "Silver Sprocket with Entrance Battles             ", &
             "Full Silver Sprocket                              ", &
             "Minimal Golden Gear                               ", &
             "Golden Gear with Entrance Battles                 ", &
             "Full Golden Gear                                  ", &
             "Minimal Diamond Dynamo                            ", &
             "Diamond Dynamo with Entrance Battles              ", &
             "Full Diamond Dynamo                               "]
         allocate(yield(9))
         allocate(boosted_yield(9))
         yield = [2800, 3300, 5000, 6500, 7700, 8500, 11000, 15500, 20000]
         
         print "(A)", "Bossbot Facilities"
         print "(A)", "------------------"
      else if (dpt == "c" .or. dpt == "C") then
         allocate(options(3))
         options = &
            ["Coin Mint                                         ", &
             "Dollar Mint                                       ", &
             "Bullion Mint                                      "]
         allocate(yield(3))
         allocate(boosted_yield(9))
         yield = [1250, 2200, 3100]

         print "(A)", "Cashbot Facilities"
         print "(A)", "------------------"
      else if (dpt == "l" .or. dpt == "L") then
         allocate(options(3))
         options = &
            ["Lawfice A113                                      ", &
             "Lawfice B221                                      ", &
             "Lawfice C418                                      "]
         allocate(yield(3))
         allocate(boosted_yield(9))
         yield = [3000, 5000, 7250]

         print "(A)", "Lawbot Facilities"
         print "(A)", "-----------------"
      else if (dpt == "s" .or. dpt == "S") then
         allocate(options(4))
         options = &
            ["Short Front Factory                               ", &
             "Short Side Factory                                ", &
             "Long Front Factory                                ", &
             "Long Side Factory                                 "]
         allocate(yield(4))
         allocate(boosted_yield(4)) 
         yield = [1300, 1500, 2500, 3000]

         print "(A)", "Sellbot Facilities"
         print "(A)", "------------------"
      else
         print *, "ERROR: Invalid department selected."
         stop
      end if
      
      boosted_yield = nint(yield * boost)

      call calc_merits(needed, boosted_yield, options, dpt)

   end subroutine facil

end module merits
