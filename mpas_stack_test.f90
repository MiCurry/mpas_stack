program mpas_stack_test

   use mpas_stack, only : node, payload_t, mpas_stack_push, mpas_stack_is_empty
   use mpas_stack, only : mpas_stack_free

   implicit none

   type, extends(payload_t) :: my_item
      integer :: my_num
      logical :: bool_flag = .FALSE.
   end type my_item

   type(node), pointer :: stack1, stack2, stack_cur
   class(my_item), pointer :: item1, item2, item3, item4, item5, item6
   class(my_item), pointer :: item

   logical, dimension(10) :: bool_check_array1, bool_check_array2
   integer, dimension(10) :: int_check_array1, int_check_array2
   integer :: i, j
   integer :: ierr

   write(0,*) "STARTING THE PROGRAM"

   allocate(item1)
   item1 % my_num = 1

   stack1 => mpas_stack_push(stack1, item1)
   item => my_pop(stack1)

   write(0,*) ""
   write(0,*) "Testing a single item. Pushing it on the stack, and then off the stack"
   write(0,*) "And insuring the stack reports that it is empty"
   write(0,*) ""

   if ( item % my_num /= item1 % my_num ) then
      write(0,*) "This was not the number we expected for item1!"
      write(0,*) "Got: ", item % my_num, "Wanted: ", item1 % my_num
      stop
   else if ( item % bool_flag .NEQV. item1 % bool_flag ) then
      write(0,*) "This was not the boolean we expected for item 1!"
      write(0,*) "Got: ", item % bool_flag, "Wanted: ", item1 % bool_flag 
      stop
   else if(.not. mpas_stack_is_empty(stack1)) then
      write(0,*) "The stack said it was not empty, but it should have been!"
      stop
   else
      write(0,*) "First test successfull"
   endif

   allocate(item2)
   item2 % my_num = 2

   allocate(item3)
   item3 % my_num = 3

   allocate(item4)
   item4 % my_num = 4
   item4 % bool_flag = .True.

   allocate(item5)
   item5 % my_num = 5
   item4 % bool_flag = .True.

   allocate(item6)
   item6 % my_num = 6

   write(0,*) ""
   write(0,*) "Pushing multiple values onto stack1 and insuring they are correct values"
   write(0,*) ""

   stack1 => mpas_stack_push(stack1, item1)
   stack1 => mpas_stack_push(stack1, item2)

   int_check_array1(1) = 2
   int_check_array1(2) = 1
   bool_check_array1(1) = .FALSE.
   bool_check_array1(2) = .FALSE.
    
   stack2 => mpas_stack_push(stack2, item1)
   stack2 => mpas_stack_push(stack2, item2)

   i = 1
   do while(.NOT. mpas_stack_is_empty(stack1))
      item => my_pop(stack1)

      ! Check to see if the integers are in the correct order
      if (item % my_num /= int_check_array1(i)) then
         write(0,*) "Test ", i, " Failed! "
         write(0,*) "This item did not match what was expected!"
         write(0,*) "Got: ", item % my_num, "Wanted: ", int_check_array1(i)
         stop
      else
         write(0,*) "Test ", i, " Correct! "
         write(0,*) "Got: ", item % my_num, "Wanted: ", int_check_array1(i)
      endif
      write(0,*) ""

      ! Check to see if the bool flag are in the correct order 
      if (item % bool_flag .NEQV. bool_check_array1(i)) then
         write(0,*) "Test ", i, " Failed! "
         write(0,*) "This item did not match what was expected!"
         write(0,*) "Got: ", item % bool_flag, "Wanted: ", bool_check_array1(i)
         stop
      else
         write(0,*) "Test ", i, " CORRECT! "
         write(0,*) "Got: ", item % bool_flag, "Wanted: ", bool_check_array1(i)
         item % bool_flag = .TRUE.
      endif

      i = i + 1
      write(0,*) ""
   enddo


   write(0,*) ""
   write(0,*) "Now testing the second stack to see if the values of the payloads have changed"

   bool_check_array1(1) = .TRUE.
   bool_check_array1(2) = .TRUE.

   ! Check the second stack
   i = 1 ! Set i to zero
   do while(.NOT. mpas_stack_is_empty(stack2))
      item => my_pop(stack2)

      ! Check to see if the integers are in the correct order
      if (item % my_num /= int_check_array1(i)) then
         write(0,*) "Test ", i, " Failed! "
         write(0,*) "This item did not match what was expected!"
         write(0,*) "Got: ", item % my_num, "Wanted: ", int_check_array1(i)
         stop
      else
         write(0,*) "Test ", i, " Correct! "
         write(0,*) "Got: ", item % my_num, "Wanted: ", int_check_array1(i)
      endif
      write(0,*) ""

      ! Check to see if the bool flag are in the correct order 
      if (item % bool_flag .NEQV. bool_check_array1(i)) then
         write(0,*) "Test ", i, " Failed! "
         write(0,*) "This item did not match what was expected!"
         write(0,*) "Got: ", item % bool_flag, "Wanted: ", bool_check_array1(i)
         stop
      else
         write(0,*) "Test ", i, " CORRECT! "
         write(0,*) "Got: ", item % bool_flag, "Wanted: ", bool_check_array1(i)
         item % bool_flag = .TRUE.
      endif

      i = i + 1 ! Increment by one
      write(0,*) ""
   enddo

   !
   !
   !
   !

   write(0,*) ""
   write(0,*) "Now pushing in a bunch of values, plus pushing some items twice!"

   stack1 => mpas_stack_push(stack1, item2)
   stack1 => mpas_stack_push(stack1, item6)

   stack1 => mpas_stack_push(stack1, item6)
   stack1 => mpas_stack_push(stack1, item1)

   stack1 => mpas_stack_push(stack1, item2)
   stack1 => mpas_stack_push(stack1, item3)

   stack1 => mpas_stack_push(stack1, item3)
   stack1 => mpas_stack_push(stack1, item4)

   int_check_array1(1) = 4
   int_check_array1(2) = 3

   int_check_array1(3) = 3
   int_check_array1(4) = 2

   int_check_array1(5) = 1
   int_check_array1(6) = 6

   int_check_array1(7) = 6
   int_check_array1(8) = 2

   i = 1
   do while(.NOT. mpas_stack_is_empty(stack1))
      item => my_pop(stack1)

      ! Check to see if the integers are in the correct order
      if (item % my_num /= int_check_array1(i)) then
         write(0,*) "Test ", i, " Failed! "
         write(0,*) "This item did not match what was expected!"
         write(0,*) "Got: ", item % my_num, "Wanted: ", int_check_array1(i)
         stop
      else
         write(0,*) "Test ", i, " Correct! "
         write(0,*) "Got: ", item % my_num, "Wanted: ", int_check_array1(i)
      endif

      i = i + 1
      write(0,*) ""
   enddo


   ! Should produce 'The stack was emtpy' per our my_pop funciton
   write(0,*) "Testing poping and empty stack. Should produce 'the stack was emtpy'"
   write(0,*) "Which we specified in our `my_pop` function"
   write(0,*) ""
   item => my_pop(stack1)
   if(associated(item)) then
      write(0,*) "The item should not have been associated!"
      stop
   endif

   deallocate(item1); deallocate(item2); deallocate(item3);deallocate(item4)
   deallocate(item5); deallocate(item6);

   ! Testing mpas_stack_free
   write(0,*) ""
   write(0,*) "Testing mpas_stack_free!"

   do i = 1, 20, 1
      allocate(item1)   
      item1 % my_num = i
      stack1 => mpas_stack_push(stack1, item1)
   enddo

   call mpas_stack_free(stack1, free_payload=.TRUE.)
   write(0,*) "The stack was freed!"

   if (mpas_stack_is_empty(stack1)) then
      write(0,*) "The stack says that is was is empty"
   else
      write(0,*) "The stack was not freed succesfully"
      stack_cur => stack1
      j = 0
      do while(associated(stack_cur))
         j = j + 1
         if (associated(stack_cur % next)) then
            stack_cur => stack_cur % next
         else
            exit
         endif
      enddo
      write(0,*) "The stack had ", j, " items still allocataded!"
   endif

   write(0,*) ""
   write(0,*) "Testing mpas_stack_free without freeing the payloads"
   allocate(item1)
   item1 % my_num = 1
   stack1 => mpas_stack_push(stack1, item1)
   allocate(item2)
   item2 % my_num = 2
   stack1 => mpas_stack_push(stack1, item2)
   allocate(item3)
   item3 % my_num = 3
   stack1 => mpas_stack_push(stack1, item3)
   allocate(item4)
   item4 % my_num = 4
   stack1 => mpas_stack_push(stack1, item4)

   call mpas_stack_free(stack1, free_payload=.FALSE.)
   if (mpas_stack_is_empty(stack1)) then

      write(0,*) "Stack Freed, but not the payloads...Checking if the payloads exist..."

      if ( .NOT. associated(item1)) then 
         write(0,*) "FAILED! Item1 was not associated!" 
         stop
      else
         write(0,*) "Item1: ", associated(item1), item1 % my_num
      endif

      if ( .NOT. associated(item2)) then 
         write(0,*) "FAILED! Item2 was not associated!" 
         stop
      else
         write(0,*) "Item2: ", associated(item2), item2 % my_num
      endif

      if ( .NOT. associated(item3)) then 
         write(0,*) "FAILED! Item3 was not associated!" 
         stop
      else
         write(0,*) "Item3: ", associated(item3), item3 % my_num
      endif

      if ( .NOT. associated(item3)) then 
         write(0,*) "FAILED! Item3 was not associated!" 
         stop
      else
         write(0,*) "Item3: ", associated(item3), item3 % my_num
      endif

   else
      write(0,*) "FAILED! The stack was not freed succsfully!"
      stack_cur => stack1
      j = 0
      do while(associated(stack_cur))
         j = j + 1
         if (associated(stack_cur % next)) then
            stack_cur => stack_cur % next
         else
            exit
         endif
      enddo
      write(0,*) "The stack had ", j, " items still allocataded!"
      write(0,*) "Stopping..."
      stop
   endif

   deallocate(item1); deallocate(item2); deallocate(item3); deallocate(item4)

   write(0,*) "ALL TESTS COMPLETE"

   contains

   ! My own function to pop off my node type
   function my_pop(stack) result(item)

      use mpas_stack, only : node, payload_t, mpas_stack_pop

      implicit none

      type(node), intent(inout), pointer :: stack

      type(my_item), pointer :: item
      class(payload_t), pointer :: top

      item => null()
      if (mpas_stack_is_empty(stack)) then
         write(0,*) "The stack was empty!"
         return
      endif
   
      top => mpas_stack_pop(stack)
      
      select type(top)
         type is(my_item)
            item => top
         class default
            write(0,*) "BEERP!"
            stop
      end select

   end function my_pop
end program mpas_stack_test
