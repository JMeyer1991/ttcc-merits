program ttcc_merits
   use merits
   
   implicit none

   character(50) :: arg              ! placeholder for parsing arguments
   real          :: boost            ! 1 + num_boosts * 0.25
   character(5)  :: current_char     ! current merits expressed as character
   logical       :: current_flag     ! current merits specified from cli
   integer       :: current_merits   ! curent merits in chosen department
   character(1)  :: dpt              ! selected department
   logical       :: dpt_flag         ! department specified from command line
   integer       :: i                ! generic index variable
   integer       :: io_status        ! internal I/O status
   character(50) :: mname            ! department merit name
   integer       :: num_boosts       ! number of boosters currently active
   logical       :: persist          ! indicates whether the -p flag is active
   integer       :: remaining_merits ! target_merits - current_merits
   character(5)  :: target_char      ! target_merits as character
   logical       :: target_flag      ! target merits specified from cli
   integer       :: target_merits    ! total merits needed for promotion

   persist = .false.
   dpt_flag = .false.
   current_flag = .false.
   target_flag = .false.

   do i = 1, command_argument_count()
      call get_command_argument(i, arg)

      select case (trim(arg))
         case ('-d', '-dpt')
            dpt_flag = .true. ! indicate that department has been specified
            call get_command_argument(i + 1, dpt)
         case ('-h', '-have')
            call get_command_argument(i + 1, current_char)
            read (current_char, '(I5)', iostat=io_status) current_merits
            if (io_status == 0) then
               current_flag = .true.
            end if
         case ('-p', '-persist')
            persist = .true. ! turn on persistent mode
         case ('-r', '-req')
            call get_command_argument(i + 1, target_char)
            read (target_char, '(I5)', iostat=io_status) target_merits
            if(io_status == 0) then
               target_flag = .true.
            end if
         case ('--help')
            print "(A, A)", "-d <dpt> or -dpt <dpt>    ", &
               "department to obtain merits in"
            print "(A, A)", "-h <num> or -have <num>   ", &
               "current number of merits"
            print "(A, A)", "-p or -persist            ", &
               "use the interactive interface continuously"
            print "(A, A)", "-r <num> or -req <num>    ", &
               "target number of merits for promotion"
            print "(A, A)", "--help                    ", &
               "display this help information and exit"
            stop
      end select
   end do
   
   do
      if (dpt_flag) then
         continue
      else if (persist) then
         print "(A)", "Select a department."
         print "(A)", "(S)ellbot  (C)ashbot  (L)awbot  (B)ossbot  E(x)it"
         read *, dpt
      else
         print "(A)", "Select a department."
         print "(A)", "(S)ellbot  (C)ashbot  (L)awbot  (B)ossbot"
         read *, dpt
      end if

      if (dpt == "x" .or. dpt == "X") then
         exit
      else
         call get_mname(dpt, mname)
      end if

      if (.not. current_flag) then
         print "(A, A, A)", "How many ", trim(mname), &
            " do you currently have?"
         read      *, current_merits
      end if

      if (.not. target_flag) then
         print "(A, A, A)", "How many total ", trim(mname), &
            " does your next promotion require?"
         read      *, target_merits
      end if

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
         dpt_flag = .false.
         current_flag = .false.
         target_flag = .false.
      end if
   end do
end program ttcc_merits
