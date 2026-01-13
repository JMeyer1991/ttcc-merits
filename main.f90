program ttcc_merits
   ! calculate the number of merits needed for a cog disguise promotion
   ! in Toontown: Corporate Clash and recommend the most efficient options
   ! for attaining them

   use merits
   
   implicit none

   real          :: boost            ! 1 + num_boosts * 0.25
   integer       :: current_merits   ! curent merits in chosen department
   character(1)  :: department       ! selected department
   character(50) :: mname            ! department merit name
   integer       :: num_boosts       ! number of boosters currently active
   integer       :: remaining_merits ! target_merits - current_merits
   integer       :: target_merits    ! total merits needed for promotion

   ! ask the user which department they are trying to attain merits in
   print "(A)", "Select a department."
   print "(A)", "(S)ellbot  (C)ashbot  (L)awbot  (B)ossbot"
   read      *, department

   call get_mname(department, mname)

   ! ask the user how many merits they currently have
   print "(A, A, A)", "How many ", trim(mname), " do you currently have?"
   read      *, current_merits

   ! ask the user how many total merits their promotion requires
   print "(A, A, A)", "How many total ", trim(mname), &
      " does your next promotion require?"
   read      *, target_merits

   ! ask the user how many active boosters they have; caclculate boost
   print "(A)", "How many boosters do you currently have?"
   read      *, num_boosts
   boost = 1 + num_boosts * 0.25

   ! calculate remaining merits and display the result
   remaining_merits = target_merits - current_merits
   print "(A, I5, A, A, A)", "You still need ", remaining_merits, " ", &
      trim(mname), "."

   print *
   call facil(remaining_merits, boost, department)
   print *
   call dpt_bldg(remaining_merits, boost, department)
   print *
   call bb_bldg(remaining_merits, boost, department)
end program ttcc_merits
