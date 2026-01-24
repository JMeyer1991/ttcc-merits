program ttcc_merits
   use merits
   
   implicit none

   character(1) :: dpt
   character(5) :: charint
   character(50) :: arg, mname
   integer :: current_merits, i, io_status, num_boosts, remaining_merits, &
      target_merits
   logical :: boost_flag, current_flag, dpt_flag, persist, target_flag
   real :: boost

   ! set all flags to .false. by default
   persist = .false.
   dpt_flag = .false.
   current_flag = .false.
   target_flag = .false.
   boost_flag = .false.

   ! parse command arguments, ignoring invalid ones
   do i = 1, command_argument_count()
      ! get the next argument in the sequence
      call get_command_argument(i, arg)

      ! enable flags and set other variables as required
      select case (trim(arg))
         ! set the boost based on a -b <num> flag
         case ('-b', '-boost')
            call get_command_argument(i + 1, charint)
            read (charint, '(I1)', iostat=io_status) num_boosts
            if(io_status == 0) then
               boost = 1 + num_boosts * 0.25
               boost_flag = .true. ! indicate that boost has been specified
            end if
         ! set the department based on a -d <dpt> flag
         case ('-d', '-dpt')
            dpt_flag = .true. ! indicate that department has been specified
            call get_command_argument(i + 1, dpt)
         ! set the current merits based on a -h flag
         case ('-h', '-have')
            call get_command_argument(i + 1, charint)
            read (charint, '(I5)', iostat=io_status) current_merits
            if (io_status == 0) then
               current_flag = .true.
            end if
         ! activate persist mode
         case ('-p', '-persist')
            persist = .true. ! turn on persist mode
         ! set the required merits based on a -r <num> flag
         case ('-r', '-req')
            call get_command_argument(i + 1, charint)
            read (charint, '(I5)', iostat=io_status) target_merits
            if(io_status == 0) then
               target_flag = .true.
            end if
         ! display command help and exit
         case ('--help')
            print "(A, A)", "-b <num> or -boost <num>   ", &
               "number of boosters currently active"
            print "(A, A)", "-d <dpt> or -dpt <dpt>     ", &
               "department to obtain merits in"
            print "(A, A)", "-h <num> or -have <num>    ", &
               "current number of merits"
            print "(A, A)", "-p or -persist             ", &
               "use the interactive interface continuously"
            print "(A, A)", "-r <num> or -req <num>     ", &
               "target number of merits for promotion"
            print "(A, A)", "--help                     ", &
               "display this help information and exit"
            stop
      end select
   end do
   
   ! main runtime loop
   do
      ! skip department selection if -d was used
      if (dpt_flag) then
         continue
      ! display the exit option if persist mode is active
      else if (persist) then
         print "(A)", "Select a department."
         print "(A)", "(S)ellbot  (C)ashbot  (L)awbot  (B)ossbot  E(x)it"
         read *, dpt
      ! display the standard menu if persist mode is not active and no
      ! department has been selected
      else
         print "(A)", "Select a department."
         print "(A)", "(S)ellbot  (C)ashbot  (L)awbot  (B)ossbot"
         read *, dpt
      end if

      ! exit if "x" is entered
      if (dpt == "x" .or. dpt == "X") then
         exit
      ! if a department letter was entered, get the name for its merits
      else
         call get_mname(dpt, mname)
      end if

      ! prompt the user for current merits if -h was not used
      if (.not. current_flag) then
         print "(A, A, A)", "How many ", trim(mname), &
            " do you currently have?"
         read      *, current_merits
      end if

      ! prompt the user for required merits if -r was not used
      if (.not. target_flag) then
         print "(A, A, A)", "How many total ", trim(mname), &
            " does your next promotion require?"
         read      *, target_merits
      end if

      ! prompt the user for boosts if -b was not used
      if (.not. boost_flag) then
         print "(A)", "How many boosters do you currently have?"
         read      *, num_boosts
         boost = 1 + num_boosts * 0.25
      end if

      ! calculate remaining merits to next promotion and display the result
      remaining_merits = target_merits - current_merits
      print "(A, I5, A, A, A)", "You still need ", remaining_merits, " ", &
         trim(mname), "."

      ! calculate options for obtaining merits
      print *
      call facil(remaining_merits, boost, dpt)
      print *
      call dpt_bldg(remaining_merits, boost, dpt)
      print *
      call bb_bldg(remaining_merits, boost, dpt)

      ! exit the program if persist mode is not activated
      if (.not. persist) then
         exit
      ! prepare for the next loop by resetting flags (in case they are
      ! specified in conjunction with -p)
      else
         print *
         dpt_flag = .false.
         current_flag = .false.
         target_flag = .false.
         boost_flag = .false.
      end if
   end do
end program ttcc_merits
