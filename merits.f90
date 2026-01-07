! calculations for promotion pathways using buildings and facilities in
! Toontown: Corporate Clash

module merits

   implicit none

   private

   public bb_bldg
   !public dpt_bldg
   !public facil

contains

   ! calculates promotion pathway using Boardbot buildings
   subroutine bb_bldg(needed, boost)
      real, intent(in) :: boost
      integer :: i
      integer, intent(in) :: needed
      integer :: remain
      integer, dimension(6), parameter :: yield = [5, 26, 100, 220, 440, 1400]

      remain = needed

      print "(A)", "Boardbot Buildings"
      print "(A)", "------------------"

      do while (remain > 0)
         do i = 1, size(yield)
            if(yield(i) * boost >= remain .or. i == size(yield)) then
               print "(A, I1, A)", "* ", i, "-story"
               remain = remain - yield(i) * boost
               exit
            end if
         end do
      end do
   end subroutine bb_bldg

end module merits
