! calculations for promotion pathways using buildings and facilities in
! Toontown: Corporate Clash

module merits

   implicit none

   private

   public bb_bldg
   public dpt_bldg
   public facil

contains

   subroutine calc_merits(needed, yield, options, dpt)
      character(1), intent(in) :: dpt
      integer :: i
      character(50) :: mname
      integer, intent(in) :: needed
      character(50), intent(in) :: options(:)
      integer :: remain
      integer :: reps
      real, intent(in) :: yield(:)

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

   ! calculates promotion pathway using Boardbot buildings
   subroutine bb_bldg(needed, boost, dpt)
      real, intent(in) :: boost
      character(1), intent(in) :: dpt
      integer, intent(in) :: needed
      character(50), dimension(6), parameter :: options = &
         ["1-story", "2-story", "3-story", "4-story", "5-story", "6-story"]
      integer, dimension(6), parameter :: yield = [5, 26, 100, 220, 440, 1400]

      print "(A)", "Boardbot Buildings"
      print "(A)", "------------------"

      call calc_merits(needed, yield * boost, options, dpt)
   end subroutine bb_bldg

   subroutine dpt_bldg(needed, boost, dpt)
      real, intent(in) :: boost
      character(1), intent(in) :: dpt
      integer, intent(in) :: needed
      character(50), dimension(6), parameter :: options = &
         ["1-story", "2-story", "3-story", "4-story", "5-story", "6-story"]
      integer, dimension(6), parameter :: yield = &
         [15, 65, 250, 550, 1100, 3100]

      print "(A)", "Departmental Buildings"
      print "(A)", "----------------------"

      call calc_merits(needed, yield * boost, options, dpt)
   end subroutine dpt_bldg

   subroutine facil(needed, boost, dpt)
      real, intent(in) :: boost
      character(1), intent(in) :: dpt
      integer, intent(in) :: needed
      character(50), allocatable :: options(:)
      integer, allocatable :: yield(:)

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
         yield = [2800, 3300, 5000, 6500, 7700, 8500, 11000, 15500, 20000]
         
         print "(A)", "Bossbot Facilities"
         print "(A)", "------------------"

         call calc_merits(needed, yield * boost, options, dpt)
      else if (dpt == "c" .or. dpt == "C") then
         allocate(options(3))
         options = &
            ["Coin Mint                                         ", &
             "Dollar Mint                                       ", &
             "Bullion Mint                                      "]
         allocate(yield(3))
         yield = [1250, 2200, 3100]

         print "(A)", "Cashbot Facilities"
         print "(A)", "------------------"

         call calc_merits(needed, yield * boost, options, dpt)
      else if (dpt == "l" .or. dpt == "L") then
         allocate(options(3))
         options = &
            ["Lawfice A113                                      ", &
             "Lawfice B221                                      ", &
             "Lawfice C418                                      "]
         allocate(yield(3))
         yield = [3000, 5000, 7250]

         print "(A)", "Lawbot Facilities"
         print "(A)", "-----------------"

         call calc_merits(needed, yield * boost, options, dpt)
      else if (dpt == "s" .or. dpt == "S") then
         allocate(options(4))
         options = &
            ["Short Front Factory                               ", &
             "Short Side Factory                                ", &
             "Long Front Factory                                ", &
             "Long Side Factory                                 "]
         yield = [1300, 1500, 2500, 3000]

         print "(A)", "Sellbot Facilities"
         print "(A)", "------------------"

         call calc_merits(needed, yield * boost, options, dpt)
      else
         print *, "ERROR: Invalid department selected."
      end if
   end subroutine facil

end module merits
