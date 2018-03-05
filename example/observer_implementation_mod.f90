module observer_implementation_mod
use signal_slot_mod
implicit none

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
            end select
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
            end select
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
            end select
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
            end select
        end select 
        
    end subroutine

end module
