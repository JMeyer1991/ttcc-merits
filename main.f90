program ttcc_merits
   use merits
   
   implicit none

   character(50) :: arg              ! placeholder for parsing arguments
   real          :: boost            ! 1 + num_boosts * 0.25
   integer       :: current_merits   ! curent merits in chosen department
   character(1)  :: dpt              ! selected department
   integer       :: i                ! generic index variable
   character(50) :: mname            ! department merit name
   integer       :: num_boosts       ! number of boosters currently active
   logical       :: persist          ! indicates whether the -p flag is active
   integer       :: remaining_merits ! target_merits - current_merits
   integer       :: target_merits    ! total merits needed for promotion

   persist = .false. ! persistent mode is off by default

   do i = 1, command_argument_count()
      call get_command_argument(i, arg)

      select case (trim(arg))
         case ('-p', '-persist')
            persist = .true. ! turn on persistent mode
         case default
            print "(A, A)", "Unknown Argument: ", trim(arg)
      end select
   end do
   
   do
      print "(A)", "Select a department."
      
      if (persist) then
         print "(A)", "(S)ellbot  (C)ashbot  (L)awbot  (B)ossbot  E(x)it"
      else
         print "(A)", "(S)ellbot  (C)ashbot  (L)awbot  (B)ossbot"
      end if
      
      read *, dpt

      if (dpt == "x" .or. dpt == "X") then
         exit
      else
         call get_mname(dpt, mname)
      end if

      print "(A, A, A)", "How many ", trim(mname), " do you currently have?"
      read      *, current_merits

      print "(A, A, A)", "How many total ", trim(mname), &
         " does your next promotion require?"
      read      *, target_merits

      print "(A)", "How many boosters do you currently have?"
      read      *, num_boosts
      boost = 1 + num_boosts * 0.25

      remaining_merits = target_merits - current_merits
      print "(A, I5, A, A, A)", "You still need ", remaining_merits, " ", &
         trim(mname), "."

      print *
      call facil(remaining_merits, boost, dpt)
      print *
      call dpt_bldg(remaining_merits, boost, dpt)
      print *
      call bb_bldg(remaining_merits, boost, dpt)

      if (.not. persist) then
         exit
      else
         print *
      end if
   end do
end program ttcc_merits
