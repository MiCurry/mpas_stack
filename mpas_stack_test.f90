program mpas_stack_test

   use mpas_stack, only : mpas_stack_type, mpas_stack_payload_type
   use mpas_stack, only : mpas_stack_push, mpas_stack_pop, mpas_stack_is_empty, mpas_stack_free

   implicit none

   type, extends(mpas_stack_payload_type) :: my_item
      integer :: my_num
      logical :: bool_flag
   end type my_item

   type(mpas_stack_type), pointer :: stack1 => null()
   class(my_item), pointer :: item1, item2
   class(my_item), dimension(:), pointer :: items
   class(my_item), pointer :: item
   class(mpas_stack_payload_type), pointer :: top

   integer, dimension(:), pointer :: int_check_array
   integer :: i, n

   write(0,*) "Testing mpas_stack"

   !
   ! Test that an empty stack is empty
   !
   if (.not. mpas_stack_is_empty(stack1)) then
      write(0,*) "FAILED: Empty stack reported to be non-empty"
   endif

   item1 => null()
   stack1 => mpas_stack_push(stack1, item1)
   item => my_pop(stack1)
   if (associated(item)) then
       write(0,*) "FAILED: Item1 should have not been associated!"
   endif

   !
   ! Add a single element to the stack
   !
   allocate(item1)
   item1 % my_num = 1
   item1 % bool_flag = .True.
   stack1 => mpas_stack_push(stack1, item1)

   ! Check to see that the stack is no longer empty
   if (mpas_stack_is_empty(stack1)) then
      write(0,*) "FAILED: Stack reported as empty but should contain an element"
   endif

   ! Remove the single element from the stack
   item => my_pop(stack1)
   if (item % my_num /= 1 .and. item % bool_flag) then
      write(0,*) "FAILED: Item popped from the stack was not correct"
      write(0,*) "FAILED: element my_num was : ", item % my_num, " and bool was: ", item % bool_flag
      write(0,*) "FAILED: where they should have been: 1, and .True."
   endif

   ! Free the empty stack
   call mpas_stack_free(stack1, .True.)
   if (associated(stack1)) then
      write(0,*) "FAILED: Stack was associated when it should have been unassociated!"
   endif

   deallocate(item1)

   ! Call pop on an empty stack
   top => mpas_stack_pop(stack1)
   if (associated(top)) then
      write(0,*) "FAILED: Top was associated, when it should have been NULL"
   endif

   ! Check to see if the stack is empty
   if (.not. mpas_stack_is_empty(stack1)) then
      write(0,*) "FAILED: Stack was not empty when it should have been"
   endif


   !
   ! Pass in 2 elements to the stack and ensure they pop off in FILO order 
   !
   allocate(item1)
   allocate(item2)

   item1 % my_num = 1
   item1 % bool_flag = .True.

   item2 % my_num = 2
   item2 % bool_flag = .False.

   stack1 => mpas_stack_push(stack1, item1)
   if (mpas_stack_is_empty(stack1)) then
      write(0,*) "FAILED: Stack reported empty after pushing item1 onto stack1"
   endif

   stack1 => mpas_stack_push(stack1, item2)
   if (mpas_stack_is_empty(stack1)) then
      write(0,*) "FAILED: Stack reported empty after pushing item1 onto stack1"
   endif

   ! Pop off an item and ensure that its item2
   item => my_pop(stack1)
   if (item % my_num /= 2 .and. .not. item % bool_flag) then
      write(0,*) "FAILED: Item popped from the stack was not correct"
      write(0,*) "FAILED: element my_num was : ", item % my_num, " and bool was: ", item % bool_flag
      write(0,*) "FAILED: where they should have been: 2, and .False."
   endif

   ! Pop off an item and ensure that its item1
   item => my_pop(stack1)
   if (item % my_num /= 1 .and. item % bool_flag) then
      write(0,*) "FAILED: Item popped from the stack was not correct"
      write(0,*) "FAILED: element my_num was : ", item % my_num, " and bool was: ", item % bool_flag
      write(0,*) "FAILED: where they should have been: 1, and .True."
   endif

   deallocate(item1)
   deallocate(item2)

   ! Check to see the stack is empty
   if (.not. mpas_stack_is_empty(stack1)) then
      write(0,*) "FAILED: Stack reported not empty when it should have been empty"
   endif

   ! Free the empty stack
   call mpas_stack_free(stack1, .True.)
   if (associated(stack1)) then
      write(0,*) "FAILED: Stack was associated when it should have been unassociated!"
   endif


   !
   ! Check mpas_stack_free to see if it deallocates everything
   !
   allocate(item1)
   allocate(item2)
   item1 % my_num = 1
   item2 % my_num = 2

   stack1 => mpas_stack_push(stack1, item1)
   stack1 => mpas_stack_push(stack1, item2)

   ! Free the stack and check to see if its associated
   call mpas_stack_free(stack1)
   if (associated(stack1)) then
      write(0,*) "FAILED: Stack1 was not freed successfully"
   endif

   ! Check too see if the stack is empty
   if (.not. mpas_stack_is_empty(stack1)) then
      write(0,*) "FAILED: Stack reported that it was not empty"
   endif

   ! Check to see if item1 and item2 are associated
   ! item1 and item2 are successfully deallocated after calling mpas_stack_free; however,
   ! item1 and item2 are still *associated*. Should this be the correct behavior? Or should
   ! the stack do something different?
   if (associated(item1)) then
      write(0,*) "FAILED: Item1 was associated when it should have been unassociated"
   endif

   if (associated(item2)) then
      write(0,*) "FAILED: Item2 was associated when it should have been unassociated"
   endif

   item1 => null()
   item2 => null()

   !
   ! Pushing three items, and pushing the same item twice
   !
   allocate(item1)
   item1 % my_num = 1
   item1 % bool_flag = .True.

   allocate(item2)
   item2 % my_num = 2
   item2 % bool_flag = .False.

   stack1 => mpas_stack_push(stack1, item1)
   stack1 => mpas_stack_push(stack1, item2)
   stack1 => mpas_stack_push(stack1, item1)

   item => my_pop(stack1)
   if (item % my_num /= 1 .and. .not. item % bool_flag) then
      write(0,*) "FAILED: Item popped from the stack was not correct"
      write(0,*) "FAILED: element my_num was : ", item % my_num, " and bool was: ", item % bool_flag
      write(0,*) "FAILED: where they should have been: 1, and .True."
   endif

   item => my_pop(stack1)
   if (item % my_num /= 2 .and. item % bool_flag) then
      write(0,*) "FAILED: Item popped from the stack was not correct"
      write(0,*) "FAILED: element my_num was : ", item % my_num, " and bool was: ", item % bool_flag
      write(0,*) "FAILED: where they should have been: 2, and .False."
   endif

   item => my_pop(stack1)
   if (item % my_num /= 1 .and. .not. item % bool_flag) then
      write(0,*) "FAILED: Item popped from the stack was not correct"
      write(0,*) "FAILED: element my_num was : ", item % my_num, " and bool was: ", item % bool_flag
      write(0,*) "FAILED: where they should have been: 1, and .True."
   endif

   ! Free the stack
   call mpas_stack_free(stack1)
   if (associated(stack1)) then
      write(0,*) "FAILED: Stack was associated when it should have been unassociated!"
   endif

   deallocate(item1)
   deallocate(item2)

   !
   ! Testing with a large amounts of elements
   !
   n = 10000000
   write(0,*) "Inserting a large amount of elements: ", n
   allocate(items(n))
   allocate(int_check_array(n))
   do i = 1, n
      items(i) % my_num = i
      int_check_array(i) = i
      stack1 => mpas_stack_push(stack1, items(i))
   enddo

   write(0,*) "Popping a large amount of elements: ", n
   do i = 1, n
      item => my_pop(stack1)
      if (item % my_num /= int_check_array(n-i+1)) then
         write(0,*) "FAILED: Item popped from the stack was not correct"
         write(0,*) "FAILED: element my_num was : ", item % my_num
         write(0,*) "FAILED: where they should have been: ", int_check_array(n-i+1)
      endif
   enddo

   ! Free the stack
   call mpas_stack_free(stack1)
   if (associated(stack1)) then
      write(0,*) "FAILED: Stack was associated when it should have been unassociated!"
   endif

   deallocate(items)
   deallocate(int_check_array)

   write(0,*) "Tests complete"

   contains

   ! My own function to pop off my mpas_stack_type type
   function my_pop(stack) result(item)

      use mpas_stack, only : mpas_stack_type, mpas_stack_payload_type, mpas_stack_pop

      implicit none

      type(mpas_stack_type), intent(inout), pointer :: stack

      class(my_item), pointer :: item
      class(mpas_stack_payload_type), pointer :: top

      item => null()
      if (mpas_stack_is_empty(stack)) then
         item=>null()
         return
      endif
   
      top => mpas_stack_pop(stack)
      if (.not. associated(top)) then
          top => null()
          return
      endif
      
      select type(top)
         type is(my_item)
            item => top
         class default
            write(0,*) "BEERP!"
            stop
      end select

   end function my_pop

end program mpas_stack_test
