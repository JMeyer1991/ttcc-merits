program ttcc_merits
   ! calculate the number of merits needed for a cog disguise promotion
   ! in Toontown: Corporate Clash and recommend the most efficient options
   ! for attaining them

   real          :: boost            ! 1 + num_boosts * 0.25
   integer       :: current_merits   ! curent merits in chosen department
   character(1)  :: department       ! selected department
   integer       :: num_boosts       ! number of boosters currently active
   integer       :: rec_bbuilding    ! recommended size of Boardbot bdlg
   integer       :: rec_building     ! recommended size of building
   character(50) :: rec_facility     ! recommended cog facility
   integer       :: remaining_merits ! target_merits - current_merits
   integer       :: target_merits    ! total merits needed for promotion

   ! ask the user which department they are trying to attain merits in
   print "(A)", "Select a department."
   print "(A)", "(S)ellbot  (C)ashbot  (L)awbot  (B)ossbot"
   read      *, department

   ! ask the user how many merits they currently have
   print "(A)", "How many merits do you currently have?"
   read      *, current_merits

   ! ask the user how many total merits their promotion requires
   print "(A)", "How many total merits does your next promotion require?"
   read      *, target_merits

   ! ask the user how many active boosters they have; caclculate boost
   print "(A)", "How many boosters do you currently have?"
   read      *, num_boosts
   boost = 1 + num_boosts * 0.25

   ! calculate remaining merits and display the result
   remaining_merits = target_merits - current_merits
   print "(A, I5, A)", "You still need ", remaining_merits, " merits."

   ! recommend a cog facility from the selected department
   if (department == "S" .or. department == "s") then
      if (1300 * boost >= remaining_merits) then
         rec_facility = "short Front Factory"
      else if (1500 * boost >= remaining_merits) then
         rec_facility = "short Side Factory"
      else if (2500 * boost >= remaining_merits) then
         rec_facility = "long Front Factory"
      else
         rec_facility = "long Side Factory"
      end if
   else if (department == "C" .or. department == "c") then
      if (1250 * boost >= remaining_merits) then
         rec_facility = "Coin Mint"
      else if (2200 * boost >= remaining_merits) then
         rec_facility = "Dollar Mint"
      else
         rec_facility = "Bullion Mint"
      end if
   else if (department == "L" .or. department == "l") then
      if (3000 * boost >= remaining_merits) then
         rec_facility = "Lawfice A113"
      else if (5000 * boost >= remaining_merits) then
         rec_facility = "Lawfice B221"
      else
         rec_facility = "Lawfice C418"
      end if
   else if (department == "B" .or. department == "b") then
      if (2800 * boost >= remaining_merits) then
         rec_facility = "minimal Silver Sprocket"
      else if (3300 * boost >= remaining_merits) then
         rec_facility = "Silver Sprocket with entrance battles"
      else if (5000 * boost >= remaining_merits) then
         rec_facility = "full Silver Sprocket"
      else if (6500 * boost >= remaining_merits) then
         rec_facility = "minimal Golden Gear"
      else if (7700 * boost >= remaining_merits) then
         rec_facility = "Golden Gear with entrance battles"
      else if (8500 * boost >= remaining_merits) then
         rec_facility = "full Golden Gear"
      else if (11000 * boost >= remaining_merits) then
         rec_facility = "minimal Diamond Dynamo"
      else if (15500 * boost >= remaining_merits) then
         rec_facility = "Diamond Dynamo with entrance battles"
      else
         rec_facility = "full Diamond Dynamo"
      end if
   else
      print "(A)", "Invalid Department"
   end if

   ! calculate recommended building size
   if (15 * boost >= remaining_merits) then
      rec_building = 1
   else if (65 * boost >= remaining_merits) then
      rec_building = 2
   else if (250 * boost >= remaining_merits) then
      rec_building = 3
   else if (550 * boost >= remaining_merits) then
      rec_building = 4
   else if (1100 * boost >= remaining_merits) then
      rec_building = 5
   else
      rec_building = 6
   end if

   ! calculate recommended Boardbot building size
   if (5 * boost >= remaining_merits) then
      rec_bbuilding = 1
   else if (26 * boost >= remaining_merits) then
      rec_bbuilding = 2
   else if (100 * boost >= remaining_merits) then
      rec_bbuilding = 3
   else if (220 * boost >= remaining_merits) then
      rec_bbuilding = 4
   else if (440 * boost >= remaining_merits) then
      rec_bbuilding = 5
   else
      rec_bbuilding = 6
   end if

   print "(A)"       , "You may want to complete:"
   print "(A, A)"    , "* ", rec_facility
   print "(A, I1, A)", "* ", rec_building, "-story departmental building"
   print "(A, I1, A)", "* ", rec_bbuilding, "-story Boardbot building"
end program ttcc_merits
