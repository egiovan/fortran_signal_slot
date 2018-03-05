module observer_implementation_mod
use signal_slot_mod
implicit none

! two different observer has been implemented, each one with two slots.
! the slots are functions with two arguments:
! the first is of class observer_t and you need a select type to
! transform it to the dynamic type of the actual argument. The same for the 
! second argument which is a class(*) value and should be transformed 
! to the expected type 

    type,extends(observer_t) :: my_first_observer_t
        real :: b = 0
        integer :: bb = 0
    end type
    
    type,extends(observer_t) :: my_second_observer_t
        real :: c = 0
        integer :: cc = 0
    end type 

contains 
    
    subroutine update_1_int(observer, val)
        class(observer_t),intent(inout) :: observer
        class(*),intent(in) :: val
        
        select type (observer)
        class is (my_first_observer_t)
            select type (val)
            type is (integer)
                observer % bb = val
            class default
                error stop 'Error: the type of the values is wrong'
            end select
        class default
            error stop 'Error: the type of the observer is wrong '
        end select
         
    end subroutine
!-----------------------------------------------------------------------
    subroutine update_2_int(observer, val)
        class(observer_t),intent(inout) :: observer
        class(*),intent(in) :: val

        select type (observer)
        class is (my_second_observer_t)
            select type (val)
            type is (integer)
                observer % cc = val * 2
            class default
                error stop 'Error: the type of the values is wrong'
            end select
        class default
            error stop 'Error: the type of the observer is wrong '
        end select 

    end subroutine
!-----------------------------------------------------------------------
    subroutine update_1_real(observer, val)
        class(observer_t),intent(inout) :: observer
        class(*),intent(in) :: val
        select type (observer)
        
        class is (my_first_observer_t)
            select type (val)
            type is (real)
                observer % b = val
            class default
                error stop 'Error: the type of the values is wrong'
            end select
        class default
            error stop 'Error: the type of the observer is wrong '
        end select 

    end subroutine
!-----------------------------------------------------------------------
    subroutine update_2_real(observer, val)
        class(observer_t),intent(inout) :: observer
        class(*),intent(in) :: val
        
        select type (observer)
        class is (my_second_observer_t)
            select type (val)
            type is (real)
                observer % c = val * 2.0
            class default
                error stop 'Error: the type of the values is wrong'
            end select
        class default
            error stop 'Error: the type of the observer is wrong '
        end select 
        
    end subroutine

end module
