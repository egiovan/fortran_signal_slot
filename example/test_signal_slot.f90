program test_signalslot
use  observer_implementation_mod
use  observable_implementation_mod
implicit none

    type(my_first_observer_t), target :: my_first_observer
    type(my_second_observer_t), target :: my_second_observer
    
    type(myobservable_t) :: myobservable

    call myobservable % intsignal % register_slot(my_first_observer, update_1_int)
    call myobservable % realsignal % register_slot(my_first_observer, update_1_real)
    
    call myobservable % intsignal % register_slot(my_second_observer, update_2_int)
    call myobservable % realsignal % register_slot(my_second_observer, update_2_real)
   
    call myobservable % set_value(5.0, 2)
    
    print *, my_first_observer % b, my_first_observer % bb
    print *, my_second_observer % c, my_second_observer % cc

    
end program
