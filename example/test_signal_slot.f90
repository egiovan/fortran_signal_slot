program test_signalslot
use  observer_implementation_mod
use  observable_implementation_mod
implicit none

    type(my_first_observer_t), target :: my_first_observer
    type(my_second_observer_t), target :: my_second_observer
    
    type(myobservable_t) :: myobservable

! the signals are connected to the slots

    call myobservable % intsignal % connect(my_first_observer, update_1_int)
    call myobservable % realsignal % connect(my_first_observer, update_1_real)
    
    call myobservable % intsignal % connect(my_second_observer, update_2_int)
    call myobservable % realsignal % connect(my_second_observer, update_2_real)
   
! the observable is changed and the signals are emitted

    call myobservable % set_value(5.0, 2)
    
    print *, my_first_observer % b, my_first_observer % bb
    print *, my_second_observer % c, my_second_observer % cc

! the slots are disconnected

    call myobservable % intsignal % disconnect_slots
    call myobservable % realsignal % disconnect_slots

! the signal are not emitted anymore

    call myobservable % set_value(9.0, 4)
    
    print *, my_first_observer % b, my_first_observer % bb
    print *, my_second_observer % c, my_second_observer % cc


end program
