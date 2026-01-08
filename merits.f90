! calculations for promotion pathways using buildings and facilities in
! Toontown: Corporate Clash

module merits

   implicit none

   private

   public bb_bldg
   public dpt_bldg
   !public facil

contains

   subroutine calc_merits(needed, yield, options)
      integer :: i
      integer, intent(in) :: needed
      character(50), intent(in) :: options(:)
      integer :: remain
      real, intent(in) :: yield(:)

      remain = needed

      do while (remain > 0)
         do i = 1, size(yield)
            if(yield(i) >= remain .or. i == size(yield)) then
               print "(A, A)", "* ", options(i)
               remain = remain - yield(i)
               exit
            end if
         end do
      end do
   end subroutine calc_merits

   ! calculates promotion pathway using Boardbot buildings
   subroutine bb_bldg(needed, boost)
      real, intent(in) :: boost
      integer, intent(in) :: needed
      character(50), dimension(6), parameter :: options = &
         ["1-story", "2-story", "3-story", "4-story", "5-story", "6-story"]
      integer, dimension(6), parameter :: yield = [5, 26, 100, 220, 440, 1400]

      print "(A)", "Boardbot Buildings"
      print "(A)", "------------------"

      call calc_merits(needed, yield * boost, options)
   end subroutine bb_bldg

   subroutine dpt_bldg(needed, boost)
      real, intent(in) :: boost
      integer, intent(in) :: needed
      character(50), dimension(6), parameter :: options = &
         ["1-story", "2-story", "3-story", "4-story", "5-story", "6-story"]
      integer, dimension(6), parameter :: yield = &
         [15, 65, 250, 550, 1100, 3100]

      print "(A)", "Departmental Buildings"
      print "(A)", "----------------------"

      call calc_merits(needed, yield * boost, options)
   end subroutine dpt_bldg


end module merits
