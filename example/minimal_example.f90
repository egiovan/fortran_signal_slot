module mini_example_mod
    use signal_slot_mod, only: signal_t, observer_t
    implicit none

    type,  extends(observer_t) :: my_observer_t
    end type

contains

    subroutine my_update(observer, val)
        class(observer_t),intent(inout) :: observer
        class(*),intent(in) :: val
        select type (val)
            type is (integer)
                print *, 'I received:', val
        end select
    end subroutine
end module

program mini_example
    use mini_example_mod, only: my_observer_t, my_update
    use signal_slot_mod, only: signal_t

    type(signal_t) :: signal
    type(my_observer_t), target :: my_observer

    call signal % connect(my_observer, my_update)
    call signal % notify_slots(12)

end program

    